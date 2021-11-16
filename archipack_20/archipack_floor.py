# -*- coding:utf-8 -*-

# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####

# <pep8 compliant>

# ----------------------------------------------------------
# Author: Jacob Morris - Stephen Leger (s-leger)
# ----------------------------------------------------------
import time
import bpy
from bpy.types import Operator, PropertyGroup, Mesh, Panel
from bpy.props import (
    FloatProperty, CollectionProperty, StringProperty,
    BoolProperty, IntProperty, EnumProperty,
    FloatVectorProperty, IntVectorProperty
    )
from mathutils import Vector, Matrix
from random import uniform
from math import radians, cos, sin, pi, sqrt
import bmesh
from .bmesh_utils import BmeshEdit as bmed
from .archipack_manipulator import Manipulable, archipack_manipulator
from .archipack_preset import ArchipackPreset, PresetMenuOperator
from .archipack_abstraction import ensure_select_and_restore
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackCreateTool,
    ArchipackObject,
    ArchipackPanel,
    stop_auto_manipulate
)
from .archipack_cutter import (
    CutAblePolygon, CutAbleGenerator,
    ArchipackCutter,
    ArchipackCutterPart,
    update_operation
    )
from .archipack_dimension import DimensionProvider
from .archipack_curveman import ArchipackUserDefinedPath
from .archipack_segments2 import ArchipackSegment
from .archipack_throttle import throttle
from .archipack_material import build_mat_enum

import logging
logger = logging.getLogger("archipack")


X_AXIS = Vector((1, 0, 0))
Z_AXIS = Vector((0, 0, 1))
MAT_GROUT = 0
MAT_FLOOR = 1


material_enums = []
mat_enum, mat_index_getter, mat_index_setter= build_mat_enum('idmat', material_enums)


# ------------------------------------------------------------------
# Define property class to store object parameters and update mesh
# ------------------------------------------------------------------


class FloorGenerator(CutAblePolygon, CutAbleGenerator):

    def __init__(self, o=None):
        CutAbleGenerator.__init__(self, o)
        self.line = None
        self.rotation_matrix = None

    def limits(self, d):
        itM = self.rotation_matrix.inverted()
        x_min, y_min, self.x_max, self.y_max = self.bounding_rect(itM)
        self.x_min = x_min - d.offset_x
        self.xsize = self.x_max - self.x_min + d.offset_x
        self.y_min = y_min - d.offset_y
        self.ysize = self.y_max - self.y_min + d.offset_y
        logger.debug("FloorGenerator.limits xsize: %s" % self.xsize)

    def cut(self, o, d, realtime):
        """
            either external or holes cuts
        """
        self.as_lines()
        self.limits(d)
        self.is_convex()
        if not realtime:
            itM = o.matrix_world.inverted()
            for c in o.children:
                _d = archipack_floor_cutter.datablock(c)
                if _d is not None:
                    tM = itM @ c.matrix_world
                    g = _d.ensure_direction(tM)
                    self.slice(g)

            # also use slabs holes
            if o.parent:
                # find slab in children of reference point
                for c in o.parent.children:
                    if c.data is not None and "archipack_slab" in c.data:
                        for b in c.children:
                            if b.data is not None and "archipack_slab_cutter" in b.data:
                                _d = b.data.archipack_slab_cutter[0]
                                tM = itM @ b.matrix_world
                                g = _d.ensure_direction(tM)
                                self.slice(g)

    @property
    def random_color(self):
        return Vector((uniform(0.0, 1.0), uniform(0.0, 1.0), uniform(0.0, 1.0), 1))

    def bool_fast_cut(self, context, o, g, keep_inside, volume=False):
        # keep inside = intersect and swap
        # keep outside = difference
        if keep_inside:
            operation = "INTERSECT"
        else:
            operation = "DIFFERENCE"

        pts = []
        # closest part
        g.get_pts(pts)

        verts = []
        # vertical surface

        for v in pts:
            x, y, z = v
            verts.extend([Vector((x, y, -2)), Vector((x, y, 2))])

        nv = int(len(verts) / 2 - 1)
        faces = [(2 * i, 2 * i + 1, 2 * i + 3, 2 * i + 2) for i in range(nv)]
        faces.append((-2, -1, 1, 0))

        bm = bmed.buildmesh(None, verts, faces, temporary=True)

        if volume:
            edges = [ed for ed in bm.edges if ed.verts[0].co.z > 0.5 and ed.verts[1].co.z > 0.5]
            bmesh.ops.edgeloop_fill(bm, edges=edges, mat_nr=0, use_smooth=False)
            edges = [ed for ed in bm.edges if ed.verts[0].co.z < 0.5 and ed.verts[1].co.z < 0.5]
            bmesh.ops.edgeloop_fill(bm, edges=edges, mat_nr=0, use_smooth=False)
            bmesh.ops.recalc_face_normals(bm, faces=bm.faces)

        bmed.select(bm, True, True, True)
        bmed.bmesh_join(o, [bm], normal_update=False)

        with ensure_select_and_restore(context, o, [o], object_mode="EDIT"):
            # not safe as expected !!
            bpy.ops.mesh.intersect_boolean(operation=operation, use_swap=keep_inside)

    def floor(self, context, o, d, pattern, active):
        """
         active: throttle mode enabled
        """
        verts, faces, matids, uvs, vcolors = [], [], [], [], []

        if d.bevel:
            bevel = d.bevel_amount
        else:
            bevel = 0

        # mesh for realtime
        realtime = active or d.pattern == "realtime"

        thickness = d.thickness
        bottom = 0

        if d.add_grout:
            thickness = min(d.thickness - d.mortar_depth, d.thickness - 0.0001)
            bottom = min(d.thickness - (d.mortar_depth + bevel), d.thickness - 0.0001)

        self.top = d.thickness

        if not realtime:
            if d.pattern == "user":
                list_of_bmesh = self.user_defined(d, pattern, Matrix(), self.rotation_matrix)
                bm = bmed.bmesh_join(o, list_of_bmesh, temporary=True)
                bmed.select(bm, False, False, False)

            else:
                self.generate_pattern(d, verts, faces, matids, uvs, vcolors)
                bm = bmed.buildmesh(o, verts, faces, matids, uvs, vcolors,
                        weld=False, clean=False,  temporary=True)

            # use mesh.ops.boolean to cut
            # when hidden fallback to slow method
            if d.use_fast_boolean and o.visible_get():

                bmed._end(bm, o)
                for hole in self.holes:
                    self.bool_fast_cut(context, o, hole, False, volume=d.pattern=="user")
                self.bool_fast_cut(context, o, self, True, volume=d.pattern=="user")
                bm = bmed._start(o)

            else:
                self.cut_holes(bm, self)
                self.cut_boundary(bm, self)

            bm.verts.ensure_lookup_table()

            if d.solidify:
                # solidify and floor bottom
                geom = bm.faces[:]
                verts = bm.verts[:]
                edges = bm.edges[:]
                bmesh.ops.solidify(bm, geom=geom, thickness=0.0001)
                for v in verts:
                    v.co.z = bottom

                # bevel
                if d.bevel:
                    for v in bm.verts:
                        v.select = True
                    for v in verts:
                        v.select = False
                    for v in bm.edges:
                        v.select = True
                    for v in edges:
                        v.select = False
                    geom = [v for v in bm.verts if v.select]
                    geom.extend([v for v in bm.edges if v.select])
                    bmesh.ops.bevel(bm,
                        geom=geom,
                        offset=d.bevel_amount,
                        offset_type='OFFSET',
                        segments=1,     # d.bevel_res
                        profile=0.5,
                        vertex_only=False,
                        clamp_overlap=False,
                        material=-1,
                        loop_slide=True,
                        mark_seam=False,
                        mark_sharp=False)

            bm.to_mesh(o.data)
            bm.free()

        else:
            bmed.emptymesh(o)

        # Grout
        if realtime or d.add_grout:
            verts = []
            self.get_verts(verts)

            if realtime:
                for v in verts:
                    v.z = self.top

            bm = bmesh.new()
            for v in verts:
                bm.verts.new(v)
            bm.verts.ensure_lookup_table()
            for i in range(1, len(verts)):
                bm.edges.new((bm.verts[i - 1], bm.verts[i]))
            bm.edges.new((bm.verts[-1], bm.verts[0]))
            bm.edges.ensure_lookup_table()
            bmesh.ops.contextual_create(bm, geom=bm.edges)

            self.cut_holes(bm, self)

            # realtime might use extrude instead ?
            if realtime:
                # flip normals
                for poly in bm.faces:
                    poly.normal_flip()

            else:
                geom = bm.faces[:]
                bmesh.ops.solidify(bm, geom=geom, thickness=thickness)

            # unwrap uvs
            layer = bm.loops.layers.uv.verify()
            for poly in bm.faces:
                vz = poly.normal
                p = poly.verts[0].co
                if vz.length < 0.5:
                    # fallback for faces with null normal
                    print("Archipack: warning floor %s has a segment with null length !" % o.name)
                    tM = Matrix.Translation(p)
                else:
                    if abs(vz.z) > 0.5:
                        vx = X_AXIS
                    else:
                        vx = vz.cross(Z_AXIS)

                    vy = vx.cross(vz)
                    tM = Matrix([
                        [vx.x, vy.x, vz.x, p.x],
                        [vx.y, vy.y, vz.y, p.y],
                        [vx.z, vy.z, vz.z, p.z],
                        [0, 0, 0, 1]
                    ])
                itM = tM.inverted()
                for j, loop in enumerate(poly.loops):
                    loop[layer].uv = (itM @ loop.vert.co).to_2d()

            bmed.bmesh_join(o, [bm], normal_update=True)

    # ---------------------------------------------------
    # Patterns
    # ---------------------------------------------------

    def regular_tile(self, d, verts, faces, matids, uvs, vcolors):
        """
         ____  ____  ____
        |    ||    ||    | Regular tile, rows can be offset, either manually or randomly
        |____||____||____|
           ____  ____  ____
          |    ||    ||    |
          |____||____||____|
        """
        off = False
        o = 1
        if d.offset != 0:
            o = 1 / (100 / d.offset)
        y = self.y_min

        while y < self.y_max:
            x = self.x_min
            tl2 = d.tile_length
            if y < self.y_max < y + d.tile_length:
                tl2 = self.y_max - y

            while x < self.x_max:
                tw2 = d.tile_width

                if x < self.x_max < x + d.tile_width:
                    tw2 = self.x_max - x
                elif x == self.x_min and off and not d.random_offset:
                    tw2 = d.tile_width * o
                elif x == self.x_min and d.random_offset:
                    v = d.tile_width * d.offset_variance * 0.0049
                    tw2 = (d.tile_width / 2) + uniform(-v, v)

                self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y, tw2, tl2)
                x += tw2 + d.spacing

            y += tl2 + d.spacing
            off = not off

    def hopscotch(self, d, verts, faces, matids, uvs, vcolors):
        """
         ____  _  Large tile, plus small one on top right corner
        |    ||_|
        |____| ____  _  But shifted up so next large one is right below previous small one
              |    ||_|
              |____|
        """
        sp = d.spacing

        # movement variables
        row = 0

        tw = d.tile_width
        tl = d.tile_length
        s_tw = (tw - sp) / 2  # small tile width
        s_tl = (tl - sp) / 2  # small tile length
        y = self.y_min - s_tl

        pre_y = y
        while y < self.y_max + s_tl or (row == 2 and y - sp < self.y_max):
            x = self.x_min
            step_back = True

            if row == 1:  # row start indented slightly
                x = self.x_min + s_tw + sp

            while x < self.x_max:
                if row == 0 or row == 1:
                    # adjust for if there is a need to cut off the bottom of the tile
                    if y < self.y_min - s_tl:
                        self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y, tw, tl + y - self.y_min)  # large one
                    else:
                        self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y, tw, tl)  # large one

                    self.add_plane(d, verts, faces, matids, uvs, vcolors, x + tw + sp, y + s_tl + sp, s_tw, s_tl)  # small one

                    if step_back:
                        x += tw + sp
                        y -= s_tl + sp
                    else:
                        x += tw + s_tw + 2 * sp
                        y += s_tl + sp

                    step_back = not step_back
                else:
                    if x == self.x_min:  # half width for starting position
                        self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y, s_tw, tl)  # large one
                        # small one on right
                        self.add_plane(d, verts, faces, matids, uvs, vcolors, x + s_tw + sp, y + s_tl + sp, s_tw, s_tl)
                        # small one on bottom
                        self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y - sp - s_tl, s_tw, s_tl)
                        x += (2 * s_tw) + tw + (3 * sp)
                    else:
                        self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y, tw, tl)  # large one
                        # small one on right
                        self.add_plane(d, verts, faces, matids, uvs, vcolors, x + tw + sp, y + s_tl + sp, s_tw, s_tl)
                        x += (2 * tw) + (3 * sp) + s_tw

            if row == 0 or row == 2:
                y = pre_y + tl + sp
            else:
                y = pre_y + s_tl + sp
            pre_y = y

            row = (row + 1) % 3  # keep wrapping rows

    def stepping_stone(self, d, verts, faces, matids, uvs, vcolors):
        """
         ____  __  ____
        |    ||__||    | Row of large one, then two small ones stacked beside it
        |    | __ |    |
        |____||__||____|
         __  __  __  __
        |__||__||__||__| Row of smalls
        """
        sp = d.spacing
        y = self.y_min
        row = 0

        tw = d.tile_width
        tl = d.tile_length
        s_tw = (tw - sp) / 2
        s_tl = (tl - sp) / 2

        while y < self.y_max:
            x = self.x_min

            while x < self.x_max:
                if row == 0:  # large one then two small ones stacked beside it
                    self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y, tw, tl)
                    self.add_plane(d, verts, faces, matids, uvs, vcolors, x + tw + sp, y, s_tw, s_tl,)
                    self.add_plane(d, verts, faces, matids, uvs, vcolors, x + tw + sp, y + s_tl + sp, s_tw, s_tl)
                    x += tw + s_tw + (2 * sp)
                else:  # row of small ones
                    self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y, s_tw, s_tl)
                    self.add_plane(d, verts, faces, matids, uvs, vcolors, x + s_tw + sp, y, s_tw, s_tl)
                    x += tw + sp

            if row == 0:
                y += tl + sp
            else:
                y += s_tl + sp

            row = (row + 1) % 2

    def hexagon(self, d, verts, faces, matids, uvs, vcolors):
        """
          __  Hexagon tiles
        /   \
        \___/
        """
        sp = d.spacing
        width = d.tile_width
        dia = (width / 2) / cos(radians(30))
        #               top of current, half way up next,    vertical spacing component
        vertical_spacing = dia * (1 + sin(radians(30))) + (sp * sin(radians(60)))  # center of one row to next row
        da = pi / 3
        base_points = [(sin(i * da), cos(i * da)) for i in range(6)]

        y = self.y_min
        offset = False
        while y - width / 2 < self.y_max:  # place tile as long as bottom is still within bounds
            if offset:
                x = self.x_min + width / 2
            else:
                x = self.x_min - sp / 2

            while x - width / 2 < self.x_max:  # place tile as long as left is still within bounds
                f = len(verts)

                if d.vary_thickness and d.thickness_variance > 0:
                    v = d.thickness / 100 * d.thickness_variance
                    z = uniform(self.top, self.top + v)
                else:
                    z = self.top

                for pt in base_points:
                    verts.append(self.rotation_matrix @ Vector((dia * pt[0] + x, dia * pt[1] + y, z)))

                faces.append([f] + [i for i in range(f + 1, len(verts))])
                vcolors.append(self.random_color)
                uvs.append(base_points)
                self.add_matid(d, matids)

                x += width + sp

            y += vertical_spacing
            offset = not offset

    def windmill(self, d, verts, faces, matids, uvs, vcolors):
        """
         __  ____
        |  ||____| This also has a square one in the middle, totaling 5 tiles per pattern
        |__|   __
         ____ |  |
        |____||__|
        """
        sp = d.spacing

        tw = d.tile_width
        tl = d.tile_length
        s_tw = (tw - sp) / 2
        s_tl = (tl - sp) / 2

        y = self.y_min
        while y < self.y_max:
            x = self.x_min

            while x < self.x_max:
                self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y, tw, s_tl, rotate_uv=True)  # bottom
                self.add_plane(d, verts, faces, matids, uvs, vcolors, x + tw + sp, y, s_tw, tl)  # right
                self.add_plane(d, verts, faces, matids, uvs, vcolors, x + s_tw + sp, y + tl + sp, tw, s_tl, rotate_uv=True)  # top
                self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y + s_tl + sp, s_tw, tl)  # left
                self.add_plane(d, verts, faces, matids, uvs, vcolors, x + s_tw + sp, y + s_tl + sp, s_tw, s_tl)  # center

                x += tw + s_tw + (2 * sp)
            y += tl + s_tl + (2 * sp)

    def boards(self, d, verts, faces, matids, uvs, vcolors):
        """
        ||| Typical wood boards
        |||
        """
        x = self.x_min
        bw, bl = d.board_width, d.board_length
        off = False
        o = 1 / (100 / d.offset) if d.offset != 0 else 0

        while x < self.x_max:
            if d.vary_width:
                v = bw * (d.width_variance / 100) * 0.99
                bw2 = bw + uniform(-v, v)
            else:
                bw2 = bw

            if bw2 + x > self.x_max:
                bw2 = self.x_max - x
            y = self.y_min

            counter = 1
            while y < self.y_max:
                bl2 = bl
                if d.vary_length:
                    v = bl * (d.length_variance / 100) * 0.99
                    bl2 = bl + uniform(-v, v)
                elif y == self.y_min and off and not d.random_offset:
                    bl2 = bl * o
                elif y == self.y_min and d.random_offset:
                    v = bl * d.offset_variance * 0.0049
                    bl2 = (bl / 2) + uniform(-v, v)

                if (counter >= d.max_boards) or y + bl2 > self.y_max:
                    bl2 = self.y_max - y

                self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y, bw2, bl2)
                y += bl2 + d.length_spacing
                counter += 1
            off = not off
            x += bw2 + d.width_spacing

    def user_defined(self, d, pattern, space, tM):
        """
        """
        x = self.x_min
        patterns = []
        if pattern is not None and pattern.type == "MESH":
            bw, bl, z = pattern.dimensions
            off = False
            while x < self.x_max:
                bw2 = bw

                if bw2 + x > self.x_max:
                    bw2 = self.x_max - x
                y = self.y_min

                counter = 1
                while y < self.y_max:
                    bm = bmed._start(pattern)
                    bl2 = bl
                    bmesh.ops.transform(bm,
                                        matrix=tM @ Matrix.Translation(Vector((x, y, 0))),
                                        space=space,
                                        verts=bm.verts)
                    patterns.append(bm)
                    y += bl2
                    counter += 1
                off = not off
                x += bw2

        return patterns

    def square_parquet(self, d, verts, faces, matids, uvs, vcolors):
        """
        ||--||-- Alternating groups oriented either horizontally, or forwards and backwards.
        ||--||-- self.spacing is used because it is the same spacing for width and length
        --||--|| Board width is calculated using number of boards and the length.
        --||--||
        """
        x = self.x_min
        start_orient_length = True

        # figure board width
        bl = d.short_board_length
        bw = (bl - (d.boards_in_group - 1) * d.spacing) / d.boards_in_group
        while x < self.x_max:
            y = self.y_min
            orient_length = start_orient_length
            while y < self.y_max:

                if orient_length:
                    start_x = x

                    for i in range(d.boards_in_group):
                        if x < self.x_max and y < self.y_max:
                            self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y, bw, bl)
                            x += bw + d.spacing

                    x = start_x
                    y += bl + d.spacing

                else:
                    for i in range(d.boards_in_group):
                        if x < self.x_max and y < self.y_max:
                            self.add_plane(d, verts, faces, matids, uvs, vcolors, x, y, bl, bw, rotate_uv=True)
                            y += bw + d.spacing

                orient_length = not orient_length

            start_orient_length = not start_orient_length
            x += bl + d.spacing

    def herringbone(self, d, verts, faces, matids, uvs, vcolors):
        """
        Boards are at 45 degree angle, in chevron pattern, ends are angled
        """
        width_dif = d.board_width / cos(radians(45))
        x_dif = d.short_board_length * cos(radians(45))
        y_dif = d.short_board_length * sin(radians(45))
        total_y_dif = width_dif + y_dif
        sp_dif = d.spacing / cos(radians(45))

        y = self.y_min - y_dif
        while y < self.y_max:
            x = self.x_min

            while x < self.x_max:
                # left side

                self.add_face(d, verts, faces, matids, uvs, vcolors,
                    (x, y, 0), (x + x_dif, y + y_dif, 0),
                    (x + x_dif, y + total_y_dif, 0), (x, y + width_dif, 0), rotate_uv=True)

                x += x_dif + d.spacing

                # right side
                if x < self.x_max:
                    self.add_face(d, verts, faces, matids, uvs, vcolors,
                        (x, y + y_dif, 0), (x + x_dif, y, 0),
                        (x + x_dif, y + width_dif, 0), (x, y + total_y_dif, 0), rotate_uv=True)
                    x += x_dif + d.spacing

            y += width_dif + sp_dif  # adjust spacing amount for 45 degree angle

    def herringbone_parquet(self, d, verts, faces, matids, uvs, vcolors):
        """
        Boards are at 45 degree angle, in chevron pattern, ends are square, not angled
        """

        an_45 = 0.5 * sqrt(2)

        x_dif = d.short_board_length * an_45
        y_dif = d.short_board_length * an_45
        y_dif_45 = d.board_width * an_45
        x_dif_45 = d.board_width * an_45
        total_y_dif = y_dif + y_dif_45

        sp_dif = (d.spacing / an_45) / 2  # divide by two since it is used for both x and y
        width_dif = d.board_width / an_45

        y = self.y_min - y_dif
        while y - y_dif_45 < self.y_max:  # continue as long as bottom left corner is still good
            x = self.x_min

            while x - x_dif_45 < self.x_max:  # continue as long as top left corner is still good
                # left side

                self.add_face(d, verts, faces, matids, uvs, vcolors,
                    (x, y, 0),
                    (x + x_dif, y + y_dif, 0),
                    (x + x_dif - x_dif_45, y + total_y_dif, 0),
                    (x - x_dif_45, y + y_dif_45, 0), rotate_uv=True)

                x += x_dif - x_dif_45 + sp_dif
                y0 = y + y_dif - y_dif_45 - sp_dif

                if x < self.x_max:
                    self.add_face(d, verts, faces, matids, uvs, vcolors,
                        (x, y0, 0),
                        (x + x_dif, y0 - y_dif, 0),
                        (x + x_dif + x_dif_45, y0 - y_dif + y_dif_45, 0),
                        (x + x_dif_45, y0 + y_dif_45, 0), rotate_uv=True)

                    x += x_dif + x_dif_45 + sp_dif

                else:  # we didn't place the right board, so step ahead far enough the the while loop for x breaks
                    break

            y += width_dif + (2 * sp_dif)

    def add_matid(self, d, matids):
        matids.append(d.id_mat(MAT_FLOOR))

    def add_plane(self, d, verts, faces, matids, uvs, vcolors, x, y, w, l, rotate_uv=False):
        """
        Adds vertices and faces for a place, clip to outer boundaries if clip is True
        :param x: start x position
        :param y: start y position
        :param w: width (in x direction)
        :param l: length (in y direction)
        """

        x1 = x + w
        y1 = y + l

        if d.vary_thickness and d.thickness_variance > 0:
            v = d.thickness / 100 * d.thickness_variance
            z = uniform(self.top, self.top + v)
        else:
            z = self.top

        p = len(verts)
        verts.extend([self.rotation_matrix @ Vector(p) for p in [(x, y, z), (x1, y, z), (x1, y1, z), (x, y1, z)]])
        faces.append([p + 3, p + 2, p + 1, p])
        vcolors.append(self.random_color)

        if d.normalize_uvs:
            w, l = 1, 1

        if rotate_uv:
            uvs.append([(0, 0), (0, w), (l, w), (l, 0)])
        else:
            uvs.append([(0, 0), (w, 0), (w, l), (0, l)])

        self.add_matid(d, matids)

    def add_face(self, d, verts, faces, matids, uvs, vcolors, p0, p1, p2, p3, rotate_uv=False):
        """
        Adds vertices and faces for a place, clip to outer boundaries if clip is True
        :param x: start x position
        :param y: start y position
        :param w: width (in x direction)
        :param l: length (in y direction)
        """

        if d.vary_thickness and d.thickness_variance > 0:
            v = d.thickness / 100 * d.thickness_variance
            z = uniform(self.top, self.top + v)
        else:
            z = self.top

        p = len(verts)
        verts.extend([self.rotation_matrix @ Vector((v[0], v[1], z)) for v in [p0, p1, p2, p3]])
        faces.append([p + 3, p + 2, p + 1, p])
        vcolors.append(self.random_color)

        if d.normalize_uvs:
            w, l = 1, 1
        else:
            w = (verts[-2] - verts[-1]).length
            l = (verts[-3] - verts[-2]).length

        if rotate_uv:
            uvs.append([(0, 0), (0, w), (l, w), (l, 0)])
        else:
            uvs.append([(0, 0), (w, 0), (w, l), (0, l)])

        self.add_matid(d, matids)

    def add_manipulator(self, name, pt1, pt2, pt3):
        m = self.manipulators.add()
        m.prop1_name = name
        m.set_pts([pt1, pt2, pt3])

    def generate_pattern(self, d, verts, faces, matids, uvs, vcolors):

        if d.pattern == "boards":
            self.boards(d, verts, faces, matids, uvs, vcolors)
        elif d.pattern == "square_parquet":
            self.square_parquet(d, verts, faces, matids, uvs, vcolors)
        elif d.pattern == "herringbone":
            self.herringbone(d, verts, faces, matids, uvs, vcolors)
        elif d.pattern == "herringbone_parquet":
            self.herringbone_parquet(d, verts, faces, matids, uvs, vcolors)
        elif d.pattern == "regular_tile":
            self.regular_tile(d, verts, faces, matids, uvs, vcolors)
        elif d.pattern == "hopscotch":
            self.hopscotch(d, verts, faces, matids, uvs, vcolors)
        elif d.pattern == "stepping_stone":
            self.stepping_stone(d, verts, faces, matids, uvs, vcolors)
        elif d.pattern == "hexagon":
            self.hexagon(d, verts, faces, matids, uvs, vcolors)
        elif d.pattern == "windmill":
            self.windmill(d, verts, faces, matids, uvs, vcolors)


def update(self, context):
    if self.auto_update:
        self.update(context, realtime_update=False)


def update_manipulators(self, context):
    if self.auto_update:
        self.update(context, manipulable_refresh=True)


def update_path(self, context):
    self.update_path(context)


class archipack_floor_part(Archipacki18n, ArchipackSegment, PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_floor[0]


class archipack_floor(Archipacki18n, ArchipackUserDefinedPath, ArchipackObject, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display main settings', 'NONE', 0),
            ('PARTS', 'Shape', 'Display floor segments settings', 'NONE', 1),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 2)
        ),
        default='MAIN',
    )
    parts: CollectionProperty(type=archipack_floor_part)

    pattern: EnumProperty(
        name='Floor Pattern',
        items=(("boards", "Boards", ""),
               ("square_parquet", "Square Parquet", ""),
               ("herringbone_parquet", "Herringbone Parquet", ""),
               ("herringbone", "Herringbone", ""),
               ("regular_tile", "Regular Tile", ""),
               ("hopscotch", "Hopscotch", ""),
               ("stepping_stone", "Stepping Stone", ""),
               ("hexagon", "Hexagon", ""),
               ("windmill", "Windmill", ""),
               ("realtime", "Realtime", "Realtime optimized simple mesh"),
               ("user", "User defined", "")
        ),
        default="boards",
        update=update
    )
    rotation: FloatProperty(
        name='Rotation',
        subtype='ANGLE', unit='ROTATION',
        min=-pi,
        max=pi,
        update=update
    )
    spacing: FloatProperty(
        name='Spacing',
        description='The amount of space between boards or tiles in both directions',
        unit='LENGTH', subtype='DISTANCE',
        min=0,
        default=0.005,
        precision=5,
        update=update
    )
    thickness: FloatProperty(
        name='Thickness',
        description='Thickness',
        unit='LENGTH', subtype='DISTANCE',
        min=0.0,
        default=0.005,
        precision=5,
        update=update
    )
    vary_thickness: BoolProperty(
        name='Random Thickness',
        description='Vary thickness',
        default=False,
        update=update
    )
    thickness_variance: FloatProperty(
        name='Variance',
        description='How much vary by',
        min=0, max=100,
        default=25,
        precision=5,
        subtype='PERCENTAGE',
        update=update
    )

    board_width: FloatProperty(
        name='Width',
        description='The width',
        unit='LENGTH', subtype='DISTANCE',
        min=0.02,
        default=0.2,
        precision=5,
        update=update
    )
    vary_width: BoolProperty(
        name='Random Width',
        description='Vary width',
        default=False,
        update=update
    )
    width_variance: FloatProperty(
        name='Variance',
        description='How much vary by',
        subtype='PERCENTAGE',
        min=1, max=100, default=50,
        precision=5,
        update=update
    )
    width_spacing: FloatProperty(
        name='Width Spacing',
        description='The amount of space between boards in the width direction',
        unit='LENGTH', subtype='DISTANCE',
        min=0,
        default=0.002,
        precision=5,
        update=update
    )

    board_length: FloatProperty(
        name='Length',
        description='The length of the boards',
        unit='LENGTH', subtype='DISTANCE',
        precision=5,
        min=0.02,
        default=2,
        update=update
    )
    short_board_length: FloatProperty(
        name='Length',
        description='The length of the boards',
        unit='LENGTH', subtype='DISTANCE',
        precision=5,
        min=0.02,
        default=2,
        update=update
    )
    vary_length: BoolProperty(
        name='Random Length',
        description='Vary board length',
        default=False,
        update=update
    )
    length_variance: FloatProperty(
        name='Variance',
        description='How much board length can vary by',
        subtype='PERCENTAGE',
        min=1, max=100, default=50,
        precision=5, update=update
    )
    max_boards: IntProperty(
        name='Max Boards',
        description='Max number of boards in one row',
        min=1,
        default=100,
        update=update
    )
    length_spacing: FloatProperty(
        name='Length Spacing',
        description='The amount of space between boards in the length direction',
        unit='LENGTH', subtype='DISTANCE',
        min=0,
        default=0.002,
        precision=5,
        update=update
    )

    # parquet specific
    boards_in_group: IntProperty(
        name='Boards in Group',
        description='Number of boards in a group',
        min=1, default=4,
        update=update
    )

    # tile specific
    tile_width: FloatProperty(
        name='Width',
        description='Width of the tiles',
        unit='LENGTH', subtype='DISTANCE',
        min=0.002,
        default=0.2,
        precision=5,
        update=update
    )
    tile_length: FloatProperty(
        name='Length',
        description='Length of the tiles',
        unit='LENGTH', subtype='DISTANCE',
        precision=5,
        min=0.02,
        default=0.3,
        update=update
    )

    # grout
    add_grout: BoolProperty(
        name='Add Grout',
        description='Add grout',
        default=False,
        update=update
    )
    mortar_depth: FloatProperty(
        name='Depth',
        description='The depth of the mortar from the surface of the tile',
        unit='LENGTH', subtype='DISTANCE',
        precision=5,
        step=0.005,
        min=0,
        default=0.001,
        update=update
    )
    offset_x: FloatProperty(
        name='Offset x',
        unit='LENGTH', subtype='DISTANCE',
        description='Offset pattern on x axis',
        min=0, default=0,
        precision=5,
        update=update
    )
    offset_y: FloatProperty(
        name='Offset y',
        unit='LENGTH', subtype='DISTANCE',
        description='Offset pattern on y axis',
        min=0, default=0,
        precision=5,
        update=update
    )
    # regular tile
    random_offset: BoolProperty(
        name='Random Offset',
        description='Random amount of offset for each row of tiles',
        update=update, default=False
    )
    offset: FloatProperty(
        name='Offset',
        description='How much to offset each row of tiles',
        min=0, max=100, default=0,
        precision=5,
        update=update
    )
    offset_variance: FloatProperty(
        name='Variance',
        description='How much to vary the offset each row of tiles',
        min=0.001, max=100, default=50,
        precision=5,
        update=update
    )

    # bevel
    bevel: BoolProperty(
        name='Bevel',
        update=update,
        default=False,
        description='Bevel upper faces'
    )
    bevel_amount: FloatProperty(
        name='Bevel',
        description='Bevel amount',
        unit='LENGTH', subtype='DISTANCE',
        min=0.0001, default=0.001,
        precision=5, step=0.05,
        update=update
    )
    solidify: BoolProperty(
        name="Solidify",
        default=True,
        update=update
    )
    z: FloatProperty(
        name="dumb z",
        description="Dumb z for manipulator placeholder",
        default=0.01,
        options={'SKIP_SAVE'}
    )
    x_offset: FloatProperty(
        name='Offset',
        description='How much to offset boundary',
        default=0,
        precision=5,
        update=update
    )
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update_manipulators
    )
    normalize_uvs: BoolProperty(
        name="Normalize uvs",
        description="Use normalized uvs (use full texture on each tile)",
        default=False,
        update=update
    )
    # Note: 2 to 7 are for future use
    idmat: IntVectorProperty(
        default=[
            0, 1,
            2, 3, 4, 5, 6, 7
        ],
        size=8
    )
    always_closed = True
    use_fast_boolean: BoolProperty(
        name="Fast boolean",
        description="Use fast boolean to cut boundary and holes, might not be as safe as slow one",
        default=True,
        update=update
    )
    user_defined_pattern: StringProperty(
        name="User defined",
        default="",
        update=update
    )

    def get_generator(self, o=None):
        g = FloorGenerator(o)
        g.rotation_matrix = Matrix.Rotation(self.rotation, 4, Vector((0, 0, 1)))
        g.add_parts(self)
        # g.line = g.make_offset(self.x_offset)
        return g

    def from_spline(self, context, o, curve, ccw=False, cw=False):
        ArchipackUserDefinedPath.from_spline(self, context, o, curve, ccw=True)

    def add_manipulator(self, name, pt1, pt2, pt3):
        m = self.manipulators.add()
        m.prop1_name = name
        m.set_pts([pt1, pt2, pt3])

    def update_manipulators(self):
        self.manipulators.clear()  # clear every time, add new ones
        self.add_manipulator("length", (0, 0, 0), (0, self.length, 0), (-0.4, 0, 0))
        self.add_manipulator("width", (0, 0, 0), (self.width, 0, 0), (0.4, 0, 0))

        z = self.thickness

        if self.pattern == "boards":
            self.add_manipulator("board_length", (0, 0, z), (0, self.board_length, z), (0.1, 0, z))
            self.add_manipulator("board_width", (0, 0, z), (self.board_width, 0, z), (-0.2, 0, z))
        elif self.pattern == "square_parquet":
            self.add_manipulator("short_board_length", (0, 0, z), (0, self.short_board_length, z), (-0.2, 0, z))
        elif self.pattern in ("herringbone", "herringbone_parquet"):
            dia = self.short_board_length * cos(radians(45))
            dia2 = self.board_width * cos(radians(45))
            self.add_manipulator("short_board_length", (0, 0, z), (dia, dia, z), (0, 0, z))
            self.add_manipulator("board_width", (dia, 0, z), (dia - dia2, dia2, z), (0, 0, z))
        else:
            tl = self.tile_length
            tw = self.tile_width

            if self.pattern in ("regular_tile", "hopscotch", "stepping_stone"):
                self.add_manipulator("tile_width", (0, tl, z), (tw, tl, z), (0, 0, z))
                self.add_manipulator("tile_length", (0, 0, z), (0, tl, z), (0, 0, z))
            elif self.pattern == "hexagon":
                self.add_manipulator("tile_width", (tw / 2 + self.spacing, 0, z), (tw * 1.5 + self.spacing, 0, z),
                                     (0, 0, 0))
            elif self.pattern == "windmill":
                self.add_manipulator("tile_width", (0, 0, z), (tw, 0, 0), (0, 0, z))
                self.add_manipulator("tile_length", (0, tl / 2 + self.spacing, z), (0, tl * 1.5 + self.spacing, z),
                                     (0, 0, z))

    def setup_manipulators(self):

        if len(self.manipulators) < 1:
            s = self.manipulators.add()
            s.type_key = "SIZE"
            s.prop1_name = "z"
            s.normal = Vector((0, 1, 0))

        self.setup_parts_manipulators('z')

    def update(self, context, manipulable_refresh=False, realtime_update=True):
        """
        NOTE: realtime_update prevent crash with ui when changes are done on multiple objects
        TODO: find root cause -> possible tread issue with throttle
        :param context:
        :param manipulable_refresh:
        :param realtime_update:
        :return:
        """
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        tim = time.time()

        # clean up manipulators before any data model change
        if manipulable_refresh:
            self.manipulable_disable(o)

        # self.update_parts()

        if self.num_parts < 3:
            self.restore_context(context)
            return

        if realtime_update:
            throttle.add(context, o, self)

        g = self.get_generator()
        g.locate_manipulators(self)

        g.set_offset(self.x_offset)
        realtime = throttle.is_active(o.name)

        t = time.time()
        g.cut(o, self, realtime)
        logger.debug("Floor.update() cut:%.4f seconds", time.time() - t)
        t = time.time()
        pattern = self.get_scene_object(context, self.user_defined_pattern)
        g.floor(context, o, self, pattern, realtime)
        self.shade_smooth(context, o, 0.209)

        logger.debug("Floor.update() floor:%.4f seconds", time.time() - t)
        self.update_dimensions(context, o)

        # enable manipulators rebuild
        if manipulable_refresh:
            self.manipulable_refresh = True

        logger.debug("Floor.update() throttle:%s %s :%.4f seconds", realtime, o.name, time.time() - tim)
        # restore context
        self.restore_context(context)

    def manipulable_setup(self, context, o):
        # locate manipulators
        self.get_generator()
        self.setup_manipulators()
        self.manipulable_setup_parts(context, o)
        for m in self.manipulators:
            self.manip_stack.append(m.setup(context, o, self))


class archipack_floor_cutter_segment(ArchipackCutterPart, PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_floor_cutter[0]


class archipack_floor_cutter(ArchipackCutter, ArchipackObject, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display cutter settings', 'NONE', 0),
            ('PARTS', 'Shape', 'Display cutter segments settings', 'NONE', 1)
        ),
        default='MAIN',
    )
    parts: CollectionProperty(type=archipack_floor_cutter_segment)

    def update_parent(self, context, o):
        if o is not None:
            d = archipack_floor.datablock(o.parent)
            if d is not None:
                cutables = [o.parent]
                self.filter_cutables(context, o, cutables)
                for c in cutables:
                    with ensure_select_and_restore(context, c, [c]) as (ctx, act, sel):
                        d.update(ctx)
                self.store_cutables(o, cutables)


# ------------------------------------------------------------------
# Define panel class to show object parameters in ui panel (N)
# ------------------------------------------------------------------


class ARCHIPACK_PT_floor(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_floor"
    bl_label = "Flooring"

    @classmethod
    def poll(cls, context):
        # ensure your object panel only show when active object is the right one
        return archipack_floor.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        if not archipack_floor.filter(o):
            return
        layout = self.layout

        d = archipack_floor.datablock(o)

        self.draw_common(context, layout)

        box = layout.box()
        row = box.row(align=True)
        not_realtime = d.pattern != "realtime"
        # Presets operators
        self.draw_op(context, layout, row, "archipack.floor_preset_menu",
                     text=bpy.types.ARCHIPACK_OT_floor_preset_menu.bl_label, icon="PRESET")
        self.draw_op(context, layout, row, "archipack.floor_preset", icon='ADD', text="")
        self.draw_op(context, layout, row, "archipack.floor_preset", icon='REMOVE', text="").remove_active = True

        self.draw_op(context, layout, layout, 'archipack.floor_cutter', icon="MOD_BOOLEAN").parent = o.name

        self.draw_prop(context, layout, layout, d, "tabs", expand=True)
        box = layout.box()

        if d.tabs == 'PARTS':
            expand = d.template_user_path(context, box, focus=False)
            if expand:
                self.draw_prop(context, layout, box, d, 'x_offset')
            d.template_parts(context, layout)

        elif d.tabs == 'MAIN':
            self.draw_prop(context, layout, box, d, 'pattern', text="")

            if not_realtime:
                self.draw_prop(context, layout, box, d, 'rotation')
                self.draw_prop(context, layout, box, d, "offset_x")
                self.draw_prop(context, layout, box, d, "offset_y")

            box.separator()
            if d.pattern == 'user':
                self.draw_label(context, layout, box, "Pattern object")
                box.prop_search(d, "user_defined_pattern", context.scene, "objects",
                                text="",
                                icon='OUTLINER_OB_MESH')

            else:
                self.draw_prop(context, layout, box, d, 'thickness')

            if not_realtime:

                if d.pattern != 'user':
                    self.draw_prop(context, layout, box, d, 'vary_thickness', icon='RNDCURVE')
                    if d.vary_thickness:
                        self.draw_prop(context, layout, box, d, 'thickness_variance')

                box.separator()
                self.draw_prop(context, layout, box, d, 'solidify', icon='MOD_SOLIDIFY')

                if d.pattern != 'user':
                    box.separator()
                    if d.pattern == 'boards':
                        self.draw_prop(context, layout, box, d, 'max_boards')
                        self.draw_prop(context, layout, box, d, 'board_length')
                        self.draw_prop(context, layout, box, d, 'vary_length', icon='RNDCURVE')
                        if d.vary_length:
                            self.draw_prop(context, layout, box, d, 'length_variance')
                        box.separator()

                        # width
                        self.draw_prop(context, layout, box, d, 'board_width')
                        # vary width
                        self.draw_prop(context, layout, box, d, 'vary_width', icon='RNDCURVE')
                        if d.vary_width:
                            self.draw_prop(context, layout, box, d, 'width_variance')
                        box.separator()
                        self.draw_prop(context, layout, box, d, 'length_spacing')
                        self.draw_prop(context, layout, box, d, 'width_spacing')

                    elif d.pattern in {'square_parquet', 'herringbone_parquet', 'herringbone'}:
                        self.draw_prop(context, layout, box, d, 'short_board_length')

                        if d.pattern != "square_parquet":
                            self.draw_prop(context, layout, box, d, "board_width")
                        self.draw_prop(context, layout, box, d, "spacing")

                        if d.pattern == 'square_parquet':
                            self.draw_prop(context, layout, box, d, 'boards_in_group')
                    elif d.pattern in {'regular_tile', 'hopscotch', 'stepping_stone', 'hexagon', 'windmill'}:
                        # width and length and mortar
                        if d.pattern != "hexagon":
                            self.draw_prop(context, layout, box, d, "tile_length")
                        self.draw_prop(context, layout, box, d, "tile_width")
                        self.draw_prop(context, layout, box, d, "spacing")

                    if d.pattern in {"regular_tile", "boards"}:
                        box.separator()
                        self.draw_prop(context, layout, box, d, "random_offset", icon="RNDCURVE")
                        if d.random_offset:
                            self.draw_prop(context, layout, box, d, "offset_variance")
                        else:
                            self.draw_prop(context, layout, box, d, "offset")

                # grout
                box.separator()
                self.draw_prop(context, layout, box, d, 'add_grout', icon='MESH_GRID')
                if d.add_grout:
                    self.draw_prop(context, layout, box, d, 'mortar_depth')

                # bevel
                box.separator()
                self.draw_prop(context, layout, box, d, 'bevel', icon='MOD_BEVEL')
                if d.bevel:
                    self.draw_prop(context, layout, box, d, 'bevel_amount')

            self.draw_prop(context, layout, box, d, 'use_fast_boolean')

        elif d.tabs == 'MATERIALS':
            if "archipack_material" in o:
                o.archipack_material[0].draw(context, box)
            box = layout.box()
            self.draw_prop(context, layout, box, d, 'normalize_uvs')


class ARCHIPACK_PT_floor_cutter(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_floor_cutter"
    bl_label = "Floor Cutter"

    @classmethod
    def poll(cls, context):
        return archipack_floor_cutter.poll(context.active_object)

    def draw(self, context):
        d = archipack_floor_cutter.datablock(context.active_object)
        if d is None:
            return
        layout = self.layout

        self.draw_common(context, layout)

        d.draw(context, layout, draw_offset=True)


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


class ARCHIPACK_OT_floor(ArchipackCreateTool, Operator):
    bl_idname = "archipack.floor"
    bl_label = "Floor"
    bl_description = "Floor"

    def create(self, context):
        """
            expose only basic params in operator
            use object property for other params
        """
        m = bpy.data.meshes.new("Floor")
        o = bpy.data.objects.new("Floor", m)
        d = m.archipack_floor.add()
        # make manipulators selectable
        d.manipulable_selectable = True
        x = 4
        angle_90 = pi / 2
        d.set_parts(4)
        for i, p in enumerate(d.parts):
            p.a0 = angle_90
            p.length = x
        d.parts[0].a0 = - angle_90
        # Link object into scene
        self.link_object_to_scene(context, o)
        o.color = (1, 0.25, 0, 1)

        # select and make active
        self.select_object(context, o, True)
        self.add_material(context, o)
        self.load_preset(d)
        return o

    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            with stop_auto_manipulate(context):
                o = self.create(context)
                o.location = self.get_cursor_location(context)
            self.add_to_reference(context, o)
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_floor_from_curve(ArchipackCreateTool, Operator):
    bl_idname = "archipack.floor_from_curve"
    bl_label = "Floor curve"
    bl_description = "Create a floor from a curve"

    @classmethod
    def poll(self, context):
        return context.active_object is not None and context.active_object.type == 'CURVE'
    # -----------------------------------------------------
    # Draw (create UI interface)
    # -----------------------------------------------------
    # noinspection PyUnusedLocal

    def create(self, context):
        curve = context.active_object
        bpy.ops.archipack.floor(filepath=self.filepath)
        o = context.active_object
        d = archipack_floor.datablock(o)
        d.user_defined_path = curve.name
        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            self.create(context)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_floor_from_wall(ArchipackCreateTool, Operator):
    bl_idname = "archipack.floor_from_wall"
    bl_label = "->Floor"
    bl_description = "Create floor(s) from a wall"

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o is not None and o.data is not None and 'archipack_wall2' in o.data

    def create(self, context):
        m = bpy.data.meshes.new("Floor")
        o = bpy.data.objects.new("Floor", m)
        d = m.archipack_floor.add()
        d.manipulable_selectable = True
        d.auto_update = False
        self.link_object_to_scene(context, o)
        o.color = (1, 0.25, 0, 1)

        self.select_object(context, o, True)
        self.add_material(context, o)
        self.load_preset(d)
        d.auto_update = False
        return o

    def floor_from_wall(self, context, w, wd):
        """
         Create flooring from surrounding wall
         Use slab cutters, windows and doors, T childs walls
        """
        tim = time.time()
        # wall is either a single or collection of polygons
        try:
            io, wall, childs = wd.as_geom(context, w, 'FLOORS', [], [], [])

        except RecursionError as ex:
            self.report({"ERROR"}, "RecursionError while building geometry: %s" % ex)
            import traceback
            traceback.print_exc()
            return None

        except Exception as ex:
            self.report({"ERROR"}, "Error while building geometry: %s" % ex)
            import traceback
            traceback.print_exc()
            return None

        o = None
        # Multipolygon
        if wall.type_id == 6:
            polys = wall.geoms
        else:
            polys = [wall]
        sel = []
        logger.debug("floor_from_wall() curves :%.4f seconds", time.time() - tim)

        for poly in polys:

            boundary = io._to_curve(poly.exterior, "{}-boundary".format(w.name), '2D')
            boundary.location.z -= wd.z_offset

            logger.debug("floor_from_wall() boundary :%.4f seconds", time.time() - tim)

            o = self.create(context)
            sel.append(o)
            # o.matrix_world = w.matrix_world.copy()
            d = archipack_floor.datablock(o)
            d.thickness = wd.z_offset
            logger.debug("floor_from_wall() create :%.4f seconds", time.time() - tim)
            d.user_defined_path = boundary.name

            logger.debug("floor_from_wall() user_defined_path :%.4f seconds", time.time() - tim)
            self.delete_object(context, boundary)
            logger.debug("floor_from_wall() delete_object :%.4f seconds", time.time() - tim)
            d.user_defined_path = ""
            logger.debug("floor_from_wall() floor :%.4f seconds", time.time() - tim)
            for hole in poly.interiors:
                curve = io._to_curve(hole, "{}-cut".format(o.name), '3D')
                bpy.ops.archipack.floor_cutter(parent=o.name, curve=curve.name)
                c = context.active_object
                cd = archipack_floor_cutter.datablock(c)
                cd.user_defined_path = ""
                self.delete_object(context, curve)
                self.unselect_object(context, c)

            logger.debug("floor_from_wall() cutters :%.4f seconds", time.time() - tim)

            # link to reference point here to retrieve slabs holes too when updating
            self.select_object(context, o, True)
            self.select_object(context, w, True)
            bpy.ops.archipack.add_reference_point()
            self.unselect_object(context, w)

            # select and make active
            self.select_object(context, o, True)
            # make low throttle so user feedback on creation is real time
            throttle.add(context, o, d, duration=0.01)
            d.auto_update = True
            self.unselect_object(context, o)
            logger.debug("floor_from_wall() %s :%.4f seconds", o.name, time.time() - tim)

        for obj in sel:
            self.select_object(context, obj)

        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            wall = context.active_object
            bpy.ops.object.select_all(action="DESELECT")
            wd = wall.data.archipack_wall2[0]
            with stop_auto_manipulate(context):
                o = self.floor_from_wall(context, wall, wd)
            self.select_object(context, wall, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_floor_cutter(ArchipackCreateTool, Operator):
    bl_idname = "archipack.floor_cutter"
    bl_label = "Floor Cutter"
    bl_description = "Floor Cutter"

    parent: StringProperty("")
    curve: StringProperty("")

    def create(self, context):
        m = bpy.data.meshes.new("Floor Cutter")
        o = bpy.data.objects.new("Floor Cutter", m)
        d = m.archipack_floor_cutter.add()
        d.manipulable_selectable = True
        parent = self.get_scene_object(context, self.parent)
        curve = self.get_scene_object(context, self.curve)

        logger.debug("ARCHIPACK_OT_floor_cutter.create()", self.parent, parent)

        if parent is not None:
            o.parent = parent
            bbox = parent.bound_box
            angle_90 = pi / 2
            x0, y0, z = bbox[0]
            x1, y1, z = bbox[6]
            x = max(1, 0.2 * (x1 - x0))

            o.matrix_world = parent.matrix_world @ Matrix.Translation(Vector((-3 * x, 0, 0)))
            # d.auto_update = False
            d.set_parts(4)
            for i, p in enumerate(d.parts):
                p.a0 = angle_90
                p.length = x
            d.parts[0].a0 = - angle_90
            # d.auto_update = True
        else:
            o.location = self.get_cursor_location(context)

        # Link object into scene
        self.link_object_to_scene(context, o)
        o.color = (1, 0, 0, 1)

        # select and make active
        self.select_object(context, o, True)
        self.load_preset(d)
        update_operation(d, context)

        if curve is not None:
            d.user_defined_path = curve.name
        else:
            d.update(context)
        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            with stop_auto_manipulate(context):
                o = self.create(context)
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


# ------------------------------------------------------------------
# Define operator class to load presets (with polls)
# ------------------------------------------------------------------


class ARCHIPACK_OT_floor_preset_create(PresetMenuOperator, Operator):
    bl_description = "Show Floor presets and create object at cursor location"
    bl_idname = "archipack.floor_preset_create"
    bl_label = "Floor"
    preset_subdir = "archipack_floor"


class ARCHIPACK_OT_floor_preset_menu(PresetMenuOperator, Operator):
    bl_description = "Create Floor from presets"
    bl_idname = "archipack.floor_preset_menu"
    bl_label = "Floor preset"
    preset_subdir = "archipack_floor"


class ARCHIPACK_OT_floor_preset_from_wall(PresetMenuOperator, Operator):
    bl_description = "Create floor(s) from a wall"
    bl_idname = "archipack.floor_preset_from_wall"
    bl_label = "-> Floor"
    preset_subdir = "archipack_floor"
    preset_operator : StringProperty(
        options={'SKIP_SAVE'},
        default="archipack.floor_from_wall"
    )

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o and o.data and "archipack_wall2" in o.data


class ARCHIPACK_OT_floor_preset_from_curve(PresetMenuOperator, Operator):
    bl_description = "Create a Floor from curve"
    bl_idname = "archipack.floor_preset_from_curve"
    bl_label = "-> Floor"
    preset_subdir = "archipack_floor"
    preset_operator : StringProperty(
        options={'SKIP_SAVE'},
        default="archipack.floor_from_curve"
    )

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o and o.type == 'CURVE'


class ARCHIPACK_OT_floor_preset(ArchipackPreset, Operator):
    bl_description = "Add / remove a Floor Preset"
    bl_idname = "archipack.floor_preset"
    bl_label = "Floor preset"
    preset_menu = "ARCHIPACK_OT_floor_preset_menu"

    @property
    def blacklist(self):
        return ['parts', 'n_parts', 'thickness']


def register():
    bpy.utils.register_class(archipack_floor_cutter_segment)
    bpy.utils.register_class(archipack_floor_cutter)
    Mesh.archipack_floor_cutter = CollectionProperty(type=archipack_floor_cutter)
    bpy.utils.register_class(ARCHIPACK_OT_floor_cutter)
    bpy.utils.register_class(ARCHIPACK_PT_floor_cutter)

    bpy.utils.register_class(archipack_floor_part)
    bpy.utils.register_class(archipack_floor)
    Mesh.archipack_floor = CollectionProperty(type=archipack_floor)
    bpy.utils.register_class(ARCHIPACK_PT_floor)
    bpy.utils.register_class(ARCHIPACK_OT_floor)
    bpy.utils.register_class(ARCHIPACK_OT_floor_preset_from_wall)
    bpy.utils.register_class(ARCHIPACK_OT_floor_preset_from_curve)
    bpy.utils.register_class(ARCHIPACK_OT_floor_preset_menu)
    bpy.utils.register_class(ARCHIPACK_OT_floor_preset_create)
    bpy.utils.register_class(ARCHIPACK_OT_floor_preset)
    bpy.utils.register_class(ARCHIPACK_OT_floor_from_curve)
    bpy.utils.register_class(ARCHIPACK_OT_floor_from_wall)


def unregister():
    bpy.utils.unregister_class(archipack_floor_cutter_segment)
    bpy.utils.unregister_class(archipack_floor_cutter)
    del Mesh.archipack_floor_cutter
    bpy.utils.unregister_class(ARCHIPACK_OT_floor_cutter)
    bpy.utils.unregister_class(ARCHIPACK_PT_floor_cutter)

    bpy.utils.unregister_class(archipack_floor_part)
    bpy.utils.unregister_class(archipack_floor)
    del Mesh.archipack_floor
    bpy.utils.unregister_class(ARCHIPACK_PT_floor)
    bpy.utils.unregister_class(ARCHIPACK_OT_floor)
    bpy.utils.unregister_class(ARCHIPACK_OT_floor_preset_from_wall)
    bpy.utils.unregister_class(ARCHIPACK_OT_floor_preset_from_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_floor_preset_menu)
    bpy.utils.unregister_class(ARCHIPACK_OT_floor_preset_create)
    bpy.utils.unregister_class(ARCHIPACK_OT_floor_preset)
    bpy.utils.unregister_class(ARCHIPACK_OT_floor_from_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_floor_from_wall)
