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
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110- 1301, USA.
#
# ##### END GPL LICENSE BLOCK #####

# <pep8 compliant>

# ----------------------------------------------------------
# Author: Stephen Leger (s-leger)
#
# ----------------------------------------------------------
import time
import bpy
import bmesh
from random import uniform
from math import sin, cos, pi, tan, radians
from mathutils import Vector, Matrix
from bpy.types import Operator, PropertyGroup, Mesh, Panel
from bpy.props import (
    FloatProperty, BoolProperty, IntProperty, StringProperty, IntVectorProperty,
    FloatVectorProperty, CollectionProperty, EnumProperty
)
from .bmesh_utils import BmeshEdit as bmed
from .archipack_abstraction import ensure_select_and_restore
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackPanel,
    ArchipackObject,
    ArchipackCreateTool,
    ArchipackDrawTool,
    ArchipackObjectsManager,
    stop_auto_manipulate
    )
from .archipack_snap import snap_point
from .archipack_keymaps import Keymaps
from .archipack_polylines import Io, Polygonizer
from .archipack_dimension import DimensionProvider
from .archipack_curveman import ArchipackUserDefinedPath
from .archipack_generator import Line, Arc, Generator
from .archipack_segments2 import ArchipackSegment
from .archipack_throttle import throttle
from .archipack_manipulator import (
    Manipulable, archipack_manipulator,
    GlPolygon, GlPolyline,
    GlLine, GlText, FeedbackPanel
    )
from .archipack_preset import ArchipackPreset, PresetMenuOperator
from .archipack_polylines import CoordSys, Qtree
from .archipack_material import build_mat_enum
from .archipack_floor import FloorGenerator
from .archipack_iconmanager import icons as icon_man
from .archipack_prefs import get_prefs
from .archipack_autoboolean import ArchipackBoolManager
from .archipack_jsonio import to_dict
from .archipack_dimension import ARCHIPACK_OT_dimension_auto

import logging
logger = logging.getLogger("archipack")
logger_childs = logging.getLogger("archipack")


# clipboard for finishes
clipboard = {}
X_AXIS = Vector((1, 0, 0))
Z_AXIS = Vector((0, 0, 1))


# material slots in id_mat array
MAT_PART_INSIDE = 0
MAT_PART_OUTSIDE = 1
MAT_INSIDE = 0
MAT_OUTSIDE = 1
MAT_TOP = 2
MAT_BOTTOM = 3
MAT_START = 4
MAT_END = 5
MAT_FINISH_1 = 0
MAT_FINISH_2 =1


material_enums = []
mat_enum, mat_index_getter, mat_index_setter = build_mat_enum('idmat', material_enums)

FINISH_INSIDE = 0
FINISH_OUTSIDE = 1
finish_enums = []


def finish_enum(self, context):

    # Ui only show for active object so this is OK
    if context is None:
        return finish_enums

    finish_enums.clear()
    finish_enums.append(("0", "None", "No finishing"))
    finish_names = set()
    defined = set()
    i = 1
    source = self
    if hasattr(self, "parent_data"):
        source = self.parent_data

    for finish in source.finish:
        _n = finish.finish_name
        if _n not in finish_names:
            finish_names.add(_n)
            finish_enums.append((str(i), _n, _n))
            defined.add(i)
            i += 1

    n_finish = len(finish_enums) - 1

    for index in self.finish_id:
        if index not in defined and index > n_finish:
            defined.add(str(index))
            finish_enums.append((str(index), "Missing", "Missing finish"))

    return finish_enums


def finish_getter(index):
    def getter(self):
        val = self.finish_id[index]
        if val < len(finish_enums):
            return val
        return 0

    return getter


def finish_setter(index):
    def setter(self, value):
        self.finish_id[index] = value
        return None

    return setter


finish2_enums = []


def finish2_enum(self, context):

    # Ui only show for active object so this is OK
    if context is None:
        return finish2_enums

    finish2_enums.clear()
    finish2_enums.append(("0", "None", "No finishing"))
    finish2_names = set()
    defined = set()
    i = 1
    source = self
    if hasattr(self, "parent_data"):
        source = self.parent_data

    for finish in source.finish2:
        _n = finish.finish_name
        if _n not in finish2_names:
            finish2_names.add(_n)
            finish2_enums.append((str(i), _n, _n))
            defined.add(i)
            i += 1

    n_finish = len(finish2_enums) - 1

    for index in self.finish2_id:
        if index not in defined and index > n_finish:
            defined.add(str(index))
            finish2_enums.append((str(index), "Missing", "Missing finish"))

    return finish2_enums


def finish2_getter(index):
    def getter(self):
        val = self.finish2_id[index]
        if val < len(finish2_enums):
            return val
        return 0

    return getter


def finish2_setter(index):
    def setter(self, value):
        self.finish2_id[index] = value
        return None

    return setter


class Q_tree(Qtree):
    """
     A quadtree to minimize relocate intersections
    """
    def _rec_str(self, child, depth):
        if len(child.children) > 0:
            _s = "\n".join([self._rec_str(c, depth+1) for c in child.children])
        else:
            _s = "\n".join(["depth:{} i:{} name:{} idx:{} seg:{} _type:{}".format(
                depth,
                n.item,
                self._geoms[n.item][0],
                self._geoms[n.item][1],
                str(self._geoms[n.item][2]),
                self._geoms[n.item][3]
                ) for i, n in enumerate(child.nodes)])
        return _s

    def __str__(self):
        return "Q_tree\n{}".format(self._rec_str(self, 0))

    def _extend_bounds(self, x0, y0, x1, y1, extend):
        return (min(x0, x1) - extend,
                min(y0, y1) - extend,
                max(x0, x1) + extend,
                max(y0, y1) + extend)

    def _getbounds_pts(self, p0, p1):
        x0, y0 = p0.x, p0.y
        x1, y1 = p1.x, p1.y
        return x0, y0, x1, y1

    def _getbounds_arc(self, c, r):
        x0, y0 = c.x - r, c.y - r
        x1, y1 = c.x + r, c.y + r
        return x0, y0, x1, y1

    def getbounds_seg(self, seg, extend=0):
        if hasattr(seg, "_r"):
            x0, y0, x1, y1 = self._getbounds_arc(seg.c, seg._r)
        else:
            x0, y0, x1, y1 = self._getbounds_pts(seg.p0, seg.p1)
        return self._extend_bounds(x0, y0, x1, y1, extend)

    def getbounds_pts(self, p0, p1, extend):
        x0, y0, x1, y1 = self._getbounds_pts(p0, p1)
        return self._extend_bounds(x0, y0, x1, y1, extend)

    def getbounds_pt(self, pt, extend=0):
        x, y = pt.x, pt.y
        return (x - extend,
                  y - extend,
                  x + extend,
                  y + extend)

    def intersects_seg(self, seg, extend):
        bounds = self.getbounds_seg(seg, extend)
        selection = list(self._intersect(bounds))
        count = len(selection)
        return count, sorted(selection)

    def intersects_pts(self, p0, p1, extend):
        bounds = self.getbounds_pts(p0, p1, extend)
        selection = list(self._intersect(bounds))
        count = len(selection)
        return count, sorted(selection)

    def intersects_pt(self, pt, extend):
        bounds = self.getbounds_pt(pt, extend)
        selection = list(self._intersect(bounds))
        count = len(selection)
        return count, sorted(selection)

    def insert_seg(self, seg, data):
        idx = self.ngeoms
        self._geoms.append(data)
        bounds = self.getbounds_seg(seg)
        self._insert(idx, bounds)

    def insert_point(self, pt, data):
        idx = self.ngeoms
        self._geoms.append(data)
        bounds = self.getbounds_pt(pt)
        self._insert(idx, bounds)


def debugTree(context, coordsys, tree):
    coords = [[seg.p0, seg.p1] for name, i, seg, _type in tree._geoms]
    io = Io(context, context.scene, coordsys)
    geoms = io.coords_to_linestring(Matrix(), coords, False)
    io.to_curve(context.scene, coordsys, geoms)


class LastState:
    """
    Store last state on change
    """
    __slots__ = ('_attr', '_value')
    
    def __init__(self):
        self.reset()

    def reset(self):
        self._attr = None
        self._value = 0

    def init(self, d, attr):
        """
        :param d: object holding attribute
        :param attr: changed attribute name
        :return:
        """
        self._attr = attr
        self._value = getattr(d, attr)

    def changed(self):
        """
        :return: attr name, last value
        """
        attr, value = self._attr, self._value
        self.reset()
        return attr, value


class Wall:

    def __init__(self, wall_z):
        self.wall_z = wall_z
        self.slices = []
        self.finish_inside = 0
        self.finish_outside = 0
        self.material_inside = 0
        self.material_outside = 0
        self.mat_finish_inside = 0
        self.mat_finish_outside = 0


class StraightWall(Wall, Line):

    __slots__ = ('slices', 'wall_z',
                 'finish_inside', 'finish_outside',
                 'material_inside', 'material_outside',
                 'z_step', 't_step', 'n_step')

    def __init__(self, p, wall_z, last=None):
        Line.__init__(self, p, last=last)
        Wall.__init__(self, wall_z)


class CurvedWall(Wall, Arc):

    __slots__ = ('slices', 'wall_z',
                 'finish_inside', 'finish_outside',
                 'material_inside', 'material_outside',
                 'z_step', 't_step', 'n_step')

    def __init__(self, p, radius, da, wall_z, last=None):
        Arc.__init__(self, p, radius, da, last=last)
        Wall.__init__(self, wall_z)


class WallGenerator(Generator):

    __slots__ = ('last_type', 'faces_type', 'outside', 'inside', 'axis')

    def __init__(self, o=None):
        Generator.__init__(self, o)
        self.last_type = 'NONE'
        self.faces_type = 'NONE'
        # Lines with offset
        self.outside = None
        self.inside = None
        self.axis = None

    def create_segment(self, d, part, co, last, last_type, p1=None, next=None):

        # start a new wall
        if part.type == 0:
            _k = StraightWall(co, d.z, last=last)
        else:
            _k = CurvedWall(co, part.radius, part.da,  d.z, last=last)

        z = 0
        if next is not None and len(next.slices) > 0:
            z = next.slices[0].z

        _k.slices = part.get_slices(d, z)

        if part.material_override:
            _k.material_inside = part.id_mat(MAT_PART_INSIDE)
            _k.material_outside = part.id_mat(MAT_PART_OUTSIDE)
        else:
            _k.material_inside = d.id_mat(MAT_INSIDE)
            _k.material_outside = d.id_mat(MAT_OUTSIDE)

        if len(d.finish) < 1:

            if part.finish_override:
                _source = part
            else:
                _source = d

            _k.finish_inside = _source.finish2_id[FINISH_INSIDE]
            _k.finish_outside = _source.finish2_id[FINISH_OUTSIDE]

        else:
            if part.finish_override:
                _source = part
            else:
                _source = d

            _k.finish_inside = _source.finish_id[FINISH_INSIDE]
            _k.finish_outside = _source.finish_id[FINISH_OUTSIDE]

        return _k

    def init_sides(self, d):
        """
        Create lines for left, right and axis
        :param d:
        :return:
        """
        
        x_offset = d.x_offset
        offset = d.offset
        width = d.width

        # Init using last state on change
        # allow childs setup before update
        attr, value = d.changed.changed()
        
        if attr is not None:
            if attr == 'base_line':
                x_offset = value - 1
            elif attr == 'offset':
                offset = value
            # elif attr == 'width':
            #    width = value

        axis = offset + x_offset * 0.5 * width
        half_width = d.direction * 0.5 * width

        logger.debug("Wall.init_sides() %s use last state:%s", d.id_data.name, attr is not None)

        self.axis = self.make_offset(axis)
        self.outside = self.make_offset(axis + half_width)
        self.inside = self.make_offset(axis - half_width)

        # snap to walls
        if not d.is_closed and self.hassegs:
            t = d.extremes
            s0, s1 = self.axis.segs[0], self.axis.segs[-2]
            s0.p0 = s0.lerp(t[0])
            s1.p1 = s1.lerp(t[1])
            s0, s1 = self.outside.segs[0], self.outside.segs[-2]
            s0.p0 = s0.lerp(t[2])
            s1.p1 = s1.lerp(t[3])
            s0, s1 = self.inside.segs[0], self.inside.segs[-2]
            s0.p0 = s0.lerp(t[4])
            s1.p1 = s1.lerp(t[5])

    def make_wall(self, d, verts, faces, matids, vcolors, vgroup_top, finishes):

        f = 0
        col = Vector((1, 1, 1, 1))
        _segs = self.valid_segs
        # nb_segs = self.numsegs

        # Trick here: get x, y, coords from offset
        # get z from axis
        outside = self.outside.valid_segs
        inside = self.inside.valid_segs

        z0 = -d.z_offset

        material_top, material_bottom = d.id_mat(MAT_TOP), \
                                        d.id_mat(MAT_BOTTOM)

        # angle from last to current in order to identify continuity in finishings
        # and set finishing pattern origins

        out_o = outside[0].p0
        in_o = inside[0].p0

        w0 = _segs[-1]
        finish_i_offset = 0
        finish_o_offset = 0

        for part, w, l, r in zip(d.parts, _segs, outside, inside):

            if abs(w.delta_angle(w0)) > 0.001:
                in_o = r.p0
                out_o = l.p0
                # TODO: handle global auto offset in this case

            finish_outside_offset = 0
            finish_inside_offset = 0

            if part.finish_override:
                if part.finish_outside_auto:
                    finish_outside_offset += finish_o_offset
                    finish_o_offset += part.finish_outside_offset - l.length
                finish_outside_offset += part.finish_outside_offset
                if part.finish_inside_auto:
                    finish_inside_offset += finish_i_offset
                    finish_i_offset += part.finish_inside_offset - r.length
                finish_inside_offset += part.finish_inside_offset
            else:
                if d.finish_outside_auto:
                    finish_outside_offset += finish_o_offset
                    finish_o_offset -= l.length
                if d.finish_inside_auto:
                    finish_inside_offset += finish_i_offset
                    finish_i_offset -= r.length

            w0 = w
            # w.param_t(d.step_angle)
            last_t = -1
            for j, s in enumerate(w.slices):
                # range(w.n_step):
                # t = w.t_step[j]

                t, z, a = s

                if last_t == t:
                    continue

                last_t = t

                # logger.debug("Wall.make_wall(i:%s j:%s t:%s)", i, j, t)
                zt = z0 + w.wall_z + z # w.get_z(t)
                if 1 > t > 0:
                    n = w.normal(t).rotate(a)
                    res, p, u, tl = n.intersect_ext(l, side="INSIDE")
                    res, p, u, tr = n.intersect_ext(r, side="INSIDE")
                    res = 1 > tl > 0 and 1 > tr > 0

                else:
                    tl, tr = t, t
                    res = True

                if res:

                    x, y, z = l.lerp(tl)
                    verts.extend([(x, y, z0), (x, y, zt)])
                    x, y, z = r.lerp(tr)
                    verts.extend([(x, y, zt), (x, y, z0)])

                    vgroup_top.extend([f + 1, f + 2])

                    faces.extend([
                        (f, f + 4, f + 5, f + 1),
                        (f + 1, f + 5, f + 6, f + 2),
                        (f + 2, f + 6, f + 7, f + 3),
                        (f + 3, f + 7, f + 4, f)
                    ])
                    vcolors.extend([col] * 4)
                    f += 4
                    finishes.extend([
                        (w.finish_outside, finish_outside_offset, out_o),
                        (0, 0, None),
                        (w.finish_inside, finish_inside_offset, in_o),
                        (0, 0, None)
                    ])

                    matids.extend([
                        w.material_outside,
                        material_top,
                        w.material_inside,
                        material_bottom
                    ])

        vgroup_top.extend([f + 1, f + 2])

        # when wall is open, add last section of vertices
        if d.is_closed:
            faces[-4:] = [
                (-4, 0, 1, -3),
                (-3, 1, 2, -2),
                (-2, 2, 3, -1),
                (-1, 3, 0, -4)
                ]
        else:
            w = _segs[-1]
            # first slice of last seg
            t, z, a = self.segs[-1].slices[0]
            # t = w.t_step[w.n_step]
            zt = z0 + w.wall_z + z #w.get_z(t)
            x, y, z = outside[-1].p1 #lerp(t)
            verts.extend([(x, y, z0), (x, y, zt)])
            x, y, z = inside[-1].p1 #lerp(t)
            verts.extend([(x, y, zt), (x, y, z0)])

            faces.extend([
                (1, 2, 3, 0),
                (-4, -1, -2, -3)
            ])
            matids.extend([
                d.id_mat(MAT_START),
                d.id_mat(MAT_END)
            ])
            vcolors.extend([col] * 2)
            finishes.extend([(0, 0, None), (0, 0, None)])

        # flip normals when CW
        if d.draw_direction == 'CW':
            for i, f in enumerate(faces):
                f0, f1, f2, f3  = f
                faces[i] = (f3, f2, f1, f0)

        self.locate_manipulators(d, d.direction)

    def debug(self, verts):
        for wall in self.segs:
            for i in range(33):
                x, y, z = wall.lerp(i / 32)
                verts.append((x, y, 0))

    def make_surface(self, o, verts, height):
        bm = bmesh.new()
        for v in verts:
            bm.verts.new(v)
        bm.verts.ensure_lookup_table()
        for i in range(1, len(verts)):
            bm.edges.new((bm.verts[i - 1], bm.verts[i]))
        bm.edges.new((bm.verts[-1], bm.verts[0]))
        bm.edges.ensure_lookup_table()
        bmesh.ops.contextual_create(bm, geom=bm.edges)
        geom = bm.faces[:]
        bmesh.ops.solidify(bm, geom=geom, thickness=height)
        bm.to_mesh(o.data)
        bm.free()


class PatternGenerator(FloorGenerator):
    """
    Extend floor patterns generator to support wall
    """
    def face_limits(self, d, face, tM, pattern):
        # put offset here before rotation !!!
        self.rotation_matrix = tM
        itM = tM.inverted()
        bounds = [itM @ loop.vert.co for loop in face.loops]
        x, y, z = zip(*bounds)
        x_min, y_min, self.x_max, self.y_max = min(x), min(y), max(x), max(y)

        # coord object origin in face system
        # face_delta = itM @ Vector()

        # make minx start at multiple of profile
        # world        minx  |        c      x_max|
        # |     |     |     |mod| |     |     |     |
        # x                                   |mod|
        # x

        # ox = mod_x - ((finish.offset_x + face_delta.x - minx) % mod_x)
        mod_x, mod_y = 1, 1
        # mod_y * 2 to support offset occuring every 2 lines
        if d.pattern == "regular_tile":
            mod_x = d.tile_width + d.spacing
            mod_y = 2 * (d.tile_length + d.spacing)

        elif d.pattern == "windmill":
            mod_x = 3 * (d.tile_width + d.spacing)
            mod_y = 3 * (d.tile_length + d.spacing)

        elif d.pattern  == "stepping_stone":
            mod_x = 3 * (d.tile_width + d.spacing)
            mod_y = 3 * (d.tile_length + d.spacing)

        elif d.pattern == "hopscotch":
            mod_x = 6 * (d.tile_width + d.spacing)
            mod_y = 6 * (d.tile_length + d.spacing)

        elif d.pattern == "hexagon":
            dia = (d.tile_width / 2) / cos(radians(30))
            #               top of current, half way up next,    vertical spacing component
            vertical_spacing = dia * (1 + sin(radians(30))) + (d.spacing * sin(radians(60)))  # center of one row to next row
            mod_x = 2 * (d.tile_width + d.spacing)
            mod_y = 2 * vertical_spacing

        elif d.pattern == 'boards':
            mod_x = 2 * (d.board_width + d.width_spacing)
            mod_y = d.board_length + d.length_spacing

        elif d.pattern == 'square_parquet':
            mod_x = mod_y = 2 * (d.spacing + d.short_board_length)

        elif d.pattern == 'herringbone_parquet':
            mod_x = 2 * (d.short_board_length + d.spacing) / (2 ** 0.5)
            mod_y = 2 * (d.board_width + d.spacing) / (2 ** 0.5)

        elif d.pattern == 'herringbone':
            mod_x = 2 * (d.short_board_length + d.spacing) / (2 ** 0.5)
            mod_y = 2 * (d.board_width + d.spacing) / (2 ** 0.5)

        elif d.pattern == 'user':
            mod_x, mod_y, z = pattern.dimensions

        # align with pattern
        ox = (x_min % mod_x)
        oy = (y_min % mod_y)

        # ensure overlap at start (left)
        overlap = 3 * d.thickness
        if ox < overlap:
            ox += mod_x

        if oy < overlap:
            oy += mod_y

        self.x_min = x_min - ox
        self.y_min = y_min - oy

        # ensure overlap at end (right)
        self.x_max += overlap
        self.y_max += overlap

    def make_finishing(self, o, d, pattern, face, tM, patterns):
        """
         active: throttle mode enabled
        """
        self.face_limits(d, face, tM, pattern)

        verts, faces, matids, uvs, vcolors = [], [], [], [], []

        if d.bevel:
            bevel = d.bevel_amount
        else:
            bevel = 0

        # mesh for realtime
        realtime = d.pattern == "realtime"
        
        # add a gap between surfaces so boolean can handle geometry
        gap = 0.0002
        

        # minumum offset from face
        bottom = d.offset_z + gap

        if d.add_grout:
            # minumum offset from face
            bottom = max(gap, d.thickness - (d.mortar_depth + bevel))

        # dist from face in face normal direction flat pattern is generated here
        self.top = d.thickness + d.offset_z

        if d.solidify:
            # solidify later add 0.0001 outside
            self.top -= gap

        vector_up = Vector((0, 0, 1))
        face_matrix = self.rotation_matrix.inverted()
        # o.matrix_world @
        if not realtime:

            if d.pattern == 'user':

                list_of_bmesh = self.user_defined(d, pattern,
                                                  Matrix(),
                                                  self.rotation_matrix)
                bm = bmed.bmesh_join(o, list_of_bmesh, temporary=True)
                bmed.select(bm, False, False, False)

            else:
                self.generate_pattern(d, verts, faces, matids, uvs, vcolors)
                bm = bmed.buildmesh(None, verts, faces, matids, uvs, vcolors, temporary=True)

            if d.solidify:
                vb = bottom * vector_up

                # solidify and move bottom near face
                geom = bm.faces[:]
                verts = bm.verts[:]
                edges = bm.edges[:]
                bmesh.ops.solidify(bm, geom=geom, thickness=gap)
                # 0 scale so inside fit with face even with random thickness
                bmesh.ops.scale(bm, vec=Vector((1, 1, 0)), space=face_matrix, verts=verts)
                # keep a small gap so boolean still work
                bmesh.ops.translate(bm, vec=vb, space=face_matrix, verts=verts)

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

            patterns.append(bm)

        # Grout
        if d.add_grout:
            thickness = bottom - gap
            if thickness > gap or realtime:
                verts = [v.co.to_3d() for v in face.verts]
                mat = d.id_mat(MAT_FINISH_2)
                bm = bmesh.new()

                if realtime:
                    vt = self.top * vector_up
                    bmesh.ops.translate(bm, vec=vt, space=face_matrix, verts=bm.verts)

                for v in verts:
                    bm.verts.new(v)
                bm.verts.ensure_lookup_table()
                for i in range(1, len(verts)):
                    bm.edges.new((bm.verts[i - 1], bm.verts[i]))
                bm.edges.new((bm.verts[-1], bm.verts[0]))
                bm.edges.ensure_lookup_table()
                bmesh.ops.contextual_create(bm, geom=bm.edges)

                # ensure face normal orientation
                if (bm.faces[0].normal - face.normal).length < 0.01:
                    bmesh.ops.reverse_faces(bm, faces=bm.faces, flip_multires=False)

                # realtime might use extrude instead ?
                if not realtime:
                    geom = bm.faces[:]
                    bmesh.ops.translate(bm, vec=gap * vector_up, space=face_matrix, verts=bm.verts)
                    bmesh.ops.solidify(bm, geom=geom, thickness=thickness - gap)

                # unwrap uvs
                layer = bm.loops.layers.uv.verify()
                for f in bm.faces:
                    f.material_index = mat
                    vz = f.normal
                    if vz.length < 0.5:
                        # fallback for faces with null normal
                        tM = Matrix()
                    else:
                        if abs(vz.z) > 0.5:
                            vx = X_AXIS
                        else:
                            vx = vz.cross(Z_AXIS)

                        vy = vx.cross(vz)
                        tM = Matrix([
                            [vx.x, vy.x, vz.x, 0],
                            [vx.y, vy.y, vz.y, 0],
                            [vx.z, vy.z, vz.z, 0],
                            [0, 0, 0, 1]
                        ])
                    itM = tM.inverted()
                    for j, loop in enumerate(f.loops):
                        loop[layer].uv = (itM @ loop.vert.co).to_2d()

                patterns.append(bm)

    def add_matid(self, d, matids):
        matids.append(d.id_mat(MAT_FINISH_1))


def update(self, context):
    self.update(context)
    return None


def update_without_throttle(self, context):
    self.update(context, allow_throttle=False)
    return None


def update_childs(self, context):
    self.manipulable_refresh = True
    self.update(context, update_childs=True, manipulable_refresh=True)
    return None


def update_manipulators(self, context):
    self.manipulable_refresh = True
    self.update(context, manipulable_refresh=True)
    return None


def update_slices(self, context):
    if not self.fit_roof:
        self.clear_slices()
    self.update(context)
    return None


def update_relocate(self, context):
    self.update(context, setup_childs=True, relocate_childs=True)
    return None


def change_setter(attr):
    def setter(self, value):
        self.changed.init(self, attr)
        setattr(self, attr, value)
        return None
    return setter


def change_getter(attr):
    def getter(self):
        return getattr(self, attr)
    return getter


class archipack_wall2_slice(Archipacki18n, PropertyGroup):
    z: FloatProperty(
        name="Height",
        default=0,
        precision=5,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    d: FloatProperty(
        name="Location",
        default=0,
        min=0,
        precision=5,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    a: FloatProperty(
        name="Angle",
        default=0,
        min=-75 / 180 * pi, max=75 / 180 * pi,
        subtype='ANGLE', unit='ROTATION',
        update=update
    )
    auto_update: BoolProperty(
        default=True,
        options={'SKIP_SAVE'}
    )

    @property
    def parent_data(self):
        return self.id_data.archipack_wall2[0]

    def update(self, context, manipulable_refresh=False):
        if self.auto_update:
            self.parent_data.update(context, manipulable_refresh)

    def draw(self, context, layout, part, index):
        row = layout.row(align=True)
        self.draw_label(context, layout, row, "Slice", postfix=str(index + 1))
        if index > 0:
            op = self.draw_op(context, layout, row, "archipack.wall2_remove_slice", icon='REMOVE', text="")
            op.part = part
            op.index = index
        self.draw_prop(context, layout, layout, self, "z")
        if index > 0:
            self.draw_prop(context, layout, layout, self, "d")
            self.draw_prop(context, layout, layout, self, "a")


class archipack_wall2_part(Archipacki18n, ArchipackSegment, PropertyGroup):

    slices: CollectionProperty(type=archipack_wall2_slice)

    material_outside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Out",
        items=mat_enum,
        get=mat_index_getter(MAT_PART_OUTSIDE),
        set=mat_index_setter(MAT_PART_OUTSIDE),
        update=update
    )

    finish_override: BoolProperty(
        default=False,
        update=update_relocate
    )
    finish_inside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Inside",
        description="Finish inside",
        get=finish_getter(FINISH_INSIDE),
        set=finish_setter(FINISH_INSIDE),
        items=finish_enum,
        update=update_relocate
    )
    finish_outside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Outside",
        description="Finish outside",
        get=finish_getter(FINISH_OUTSIDE),
        set=finish_setter(FINISH_OUTSIDE),
        items=finish_enum,
        update=update_relocate
    )
    finish_id: IntVectorProperty(
        size=2,
        default=[0, 0]
    )
    finish2_inside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Inside",
        description="Finish inside",
        get=finish2_getter(FINISH_INSIDE),
        set=finish2_setter(FINISH_INSIDE),
        items=finish2_enum,
        update=update_relocate
    )
    finish2_outside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Outside",
        description="Finish outside",
        get=finish2_getter(FINISH_OUTSIDE),
        set=finish2_setter(FINISH_OUTSIDE),
        items=finish2_enum,
        update=update_relocate
    )
    finish2_id: IntVectorProperty(
        size=2,
        default=[0, 0]
    )
    finish_inside_offset: FloatProperty(
        name="Offset inside",
        description="Offset of inside finishing on x axis",
        default=0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    finish_outside_offset: FloatProperty(
        name="Offset outside",
        description="Offset of outside finishing on x axis",
        default=0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    finish_outside_auto: BoolProperty(
        name="Auto offset",
        description="Auto offset along wall",
        default=False,
        update=update
    )
    finish_inside_auto: BoolProperty(
        name="Auto offset",
        description="Auto offset along wall",
        default=False,
        update=update
    )
    manipulators: CollectionProperty(type=archipack_manipulator)

    # ui related
    expand_slices: BoolProperty(
        default=False,
        options={'SKIP_SAVE'}
    )

    @property
    def parent_data(self):
        return self.id_data.archipack_wall2[0]

    def update(self, context, manipulable_refresh=False, setup_childs=False, relocate_childs=True):

        # Reset change side
        self.change_side = 'RIGHT'
        self.lock = False

        if self.auto_update:
            self.parent_data.update(context, manipulable_refresh, setup_childs=setup_childs, relocate_childs=relocate_childs)

    @property
    def _length(self):
        if self.type == 1:
            length = 2.0 * self.radius * abs(self.da)
        else:
            length = self.length
        return length

    def remove_double_slices(self):
        slices = [(s.d, i) for i, s in enumerate(self.slices)]
        slices.sort(key=lambda x: x[0])
        last = -1
        to_remove = []
        for d, i in slices:
            if abs(d - last) < 0.001:
                to_remove.append(i)
            else:
                last = d
        to_remove.sort()
        for i in reversed(to_remove):
            self.slices.remove(i)

    def get_slices(self, d, z_next):
        """Interpolate given slices data on curved segments
        :param d:
        :param z_next:
        :return: array of slices (t, z, a)
        """
        length = self._length
        slices = [(s.d / length, s.z, s.a) for s in self.slices if s.d < length]
        if self.type == 1:
            steps = max(1, round(abs(self.da) / d.step_angle, 0))
            t = 1.0 / steps
            for i in range(1, int(steps)):
                slices.append((i * t, None, 0))
            slices.sort(key=lambda x: x[0])
            slices.append((1, z_next, 0))
            t0 = 0
            z0 = self.slices[0].z
            a0 = self.slices[0].a
            for i, s in enumerate(slices):
                t, z, a = s
                j = i
                while z is None:
                    j += 1
                    t, z, a = slices[j]
                dt = t - t0
                if dt > 0:
                    da = (a - a0) / dt
                    dz = (z - z0) / dt
                else:
                    da = 0
                    dz = 0
                for k in range(i, j):
                    t1, z1, a1 = slices[k]
                    slices[k] = (t1, z0 + dz * (t1 - t0) , a0 + da * (t1 - t0))
                z0 = z
                a0 = a
                t0 = t
            slices.pop()
        else:
            slices.sort(key=lambda x: x[0])
        return slices

    def draw_finishes(self, context, layout, index, use_finish2):
        box = layout.box()
        self.draw_prop(context, layout, box, self, "finish_override", text="Seg override", postfix=str(index + 1),
                           toggle=True)
        if self.finish_override:
            if use_finish2:
                self.draw_prop(context, layout, box, self, "finish2_inside")
                row = layout.row()
                self.draw_prop(context, layout, row, self, "finish_inside_offset")
                self.draw_prop(context, layout, row, self, "finish_inside_auto", icon="AUTO", text="")

                self.draw_prop(context, layout, box, self, "finish2_outside")
                row = layout.row()
                self.draw_prop(context, layout, row, self, "finish_outside_offset")
                self.draw_prop(context, layout, row, self, "finish_outside_auto", icon="AUTO", text="")

            else:
                self.draw_prop(context, layout, box, self, "finish_inside")
                self.draw_prop(context, layout, box, self, "finish_outside")

    def draw_material(self, context, layout, index):
        box = layout.box()
        self.draw_prop(context, layout, box, self, "material_override", text="Seg override", postfix=str(index + 1),
                       toggle=True)
        if self.material_override:
            self.draw_prop(context, layout, box, self, "material", text="Int")
            self.draw_prop(context, layout, box, self, "material_outside")

    def draw(self, context, layout, index, draw_type=True, closed=False):

        row = layout.row(align=True)
        icon = "TRIA_RIGHT"
        if self.expand:
            icon = "TRIA_DOWN"

        self.draw_prop(context, layout, row, self, 'expand', icon=icon, emboss=True, text="Seg", postfix=str(index + 1))
        self.draw_prop(context, layout, row, self, "type_ui", text="")

        if self.expand:
            self.draw_insert(context, layout, index, closed)
            if self.type == 1:
                self.draw_prop(context, layout, layout, self, "r_ui")
                self.draw_prop(context, layout, layout, self, "da_ui")
            else:
                self.draw_prop(context, layout, layout, self, "l_ui")
            self.draw_prop(context, layout, layout, self, "a_ui")

            row = layout.row(align=True)
            icon = "TRIA_RIGHT"
            if self.expand_slices:
                icon = "TRIA_DOWN"
            self.draw_prop(context, layout, row, self, 'expand_slices', icon=icon, emboss=True,
                           text="Slices", postfix="({})".format(len(self.slices)))
            self.draw_op(context, layout, row, "archipack.wall2_add_slice", icon='ADD', text="").part = index
            if self.expand_slices:
                for i, slice in enumerate(self.slices):
                    slice.draw(context, layout, index, i)


class archipack_wall2_relocate_child(PropertyGroup):
    """
     Store child points relationship to parent segments
     as distance from 2 nearest segments
    """
    child_name: StringProperty(
        description="Name of child object"
    )
    child_idx: IntProperty(
        default=-1,
        description="Index of linked child part (startpoint) when -1 set object location"
    )
    seg0: IntProperty(
        description="Index of parent segment 0 the point is bound to"
    )
    seg1: IntProperty(
        description="Index of parent segment 1 the point is bound to"
    )
    parent_name0: StringProperty(default="")
    parent_name1: StringProperty(default="")
    d0: FloatProperty(
        description="Distance from parent segment 0 the point is bound to"
    )
    d1: FloatProperty(
        description="Distance from parent segment 1 the point is bound to"
    )
    t: FloatProperty(
        description="Distance from start of closest segment normalized"
    )
    side: StringProperty(default="")

    def __str__(self):
        return "c:{} {} p0:{} {} {} p1:{} {} {} side:{}".format(
            self.child_name,
            self.child_idx,
            self.parent_name0,
            self.seg0,
            self.d0,
            self.parent_name1,
            self.seg1,
            self.d1,
            self.side
        )

    def filter_child(self, o):
        d = None
        k = None
        if o and o.data:
            od = o.data
            for key in od.keys():
                if "archipack_" in key and key[10:] in (
                        "window",
                        "door",
                        "custom",
                        "wall2",
                        "slab",
                        "fence",
                        "floor",
                        "molding",
                        "area",
                        "slab_cutter",
                        "floor_cutter",
                        "roof_cutter"
                        ):
                    try:
                        d = getattr(od, key)[0]
                        k = key
                    except:
                        pass
                    break
        return d, k

    def get_child(self, context):
        d = None
        # 2.8 ???
        o = context.scene.objects.get(self.child_name)
        d, k = self.filter_child(o)
        return o, d

    def get_parent0(self, context):
        d = None
        # 2.8 ???
        o = context.scene.objects.get(self.parent_name0)
        d, k = self.filter_child(o)
        return o, d

    def get_parent1(self, context):
        d = None
        # 2.8 ???
        o = context.scene.objects.get(self.parent_name1)
        d, k = self.filter_child(o)
        return o, d


class archipack_wall2_child(PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)
    child_name: StringProperty()
    wall_idx: IntProperty(
        description="Index of wall part"
    )
    pos: FloatVectorProperty(subtype='XYZ')
    flip: BoolProperty(default=False)

    def filter_child(self, o):
        d = None
        if o and o.data:
            od = o.data
            for key in od.keys():
                if "archipack_" in key and key[10:] in (
                        "window",
                        "door",
                        "custom"
                ):
                    try:
                        d = getattr(od, key)[0]
                    except:
                        pass
                    break
        return d

    def get_child(self, context):
        c = context.scene.objects.get(self.child_name)
        d = self.filter_child(c)
        return c, d


class archipack_wall2_finish2(Archipacki18n, PropertyGroup):
    finish_name: StringProperty(
        name="",
        description="Name for this finish, may share name with other finish components",
        default="Finish",
        update=update
    )

    idmat: IntVectorProperty(
        default=[0, 1],
        size=2
    )
    material_index: EnumProperty(
        options={'SKIP_SAVE'},
        name="Int",
        items=mat_enum,
        update=update,
        get=mat_index_getter(MAT_FINISH_1),
        set=mat_index_setter( MAT_FINISH_1)
    )
    material_index2: EnumProperty(
        options={'SKIP_SAVE'},
        name="Out",
        items=mat_enum,
        update=update,
        get=mat_index_getter(MAT_FINISH_2),
        set=mat_index_setter( MAT_FINISH_2)
    )
    altitude: FloatProperty(
        name="Altitude",
        description="Altitude from bottom of wall",
        default=0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    z: FloatProperty(
        name="Height",
        description="Height of finish",
        min=0.01,
        default=20,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
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
        default=1,
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

    user_defined_pattern: StringProperty(
        name="User defined",
        default="",
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
    offset_z: FloatProperty(
        name='Offset z',
        unit='LENGTH', subtype='DISTANCE',
        description='Offset pattern on z axis',
        default=0,
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

    x_offset: FloatProperty(
        name='Offset',
        description='How much to offset boundary',
        default=0,
        precision=5,
        update=update
    )
    expand: BoolProperty(
        default=False,
        options={'SKIP_SAVE'}
    )
    force_edges: BoolProperty(
        default=False,
        update=update,
        name="Force corners",
        description="Force corners cuts continuity"
    )
    normalize_uvs: BoolProperty(
        default=False,
        update=update,
        name="Normalize uvs",
        description="Normalize uv maps coordinates (use whole map on each face)"
    )
    @property
    def parent_data(self):
        return self.id_data.archipack_wall2[0]

    def id_mat(self, index):
        _idx = self.idmat[index]
        if _idx < len(self.id_data.materials):
            return _idx
        return 0

    def draw(self, context, layout, index):
        box = layout.box()
        row = box.row(align=True)
        icon = "TRIA_RIGHT"
        if self.expand:
            icon = "TRIA_DOWN"
        self.draw_prop(context, layout, row, self, 'expand', icon=icon, emboss=True, text="", postfix=str(index + 1))
        self.draw_prop(context, layout, row, self, 'finish_name')
        self.draw_op(context, layout, row, "archipack.wall2_remove_finish2", icon='REMOVE', text="").index = index
        self.draw_op(context, layout, row, "archipack.wall2_copy_finish2", icon='COPYDOWN', text="").finish_name = self.finish_name

        if self.expand:

            self.draw_prop(context, layout, box, self, 'altitude')
            self.draw_prop(context, layout, box, self, 'z')
            # rotation still doesnt work
            # self.draw_prop(context, layout, self, 'rotation')
            self.draw_label(context, layout, box, "Pattern offset")
            self.draw_prop(context, layout, box, self, 'offset_x')
            self.draw_prop(context, layout, box, self, 'offset_y')
            self.draw_prop(context, layout, box, self, 'offset_z')
            self.draw_prop(context, layout, box, self, 'rotation')
            self.draw_prop(context, layout, box, self, 'force_edges')

            box = layout.box()
            self.draw_label(context, layout, box, "Material")
            self.draw_prop(context, layout, box, self, 'material_index', text="")
            self.draw_label(context, layout, box, "Material (mortar)")
            self.draw_prop(context, layout, box, self, 'material_index2', text="")

            box = layout.box()
            self.draw_prop(context, layout, box, self, 'pattern', text="")

            box.separator()

            if self.pattern == 'user':
                self.draw_label(context, layout, box, "Pattern object")
                box.prop_search(self, "user_defined_pattern", context.scene, "objects",
                                text="",
                                icon='OUTLINER_OB_MESH')
            else:
                self.draw_prop(context, layout, box, self, 'thickness')

                self.draw_prop(context, layout, box, self, 'vary_thickness', icon='RNDCURVE')
                if self.vary_thickness:
                    self.draw_prop(context, layout, box, self, 'thickness_variance')

            box.separator()
            self.draw_prop(context, layout, box, self, 'solidify', icon='MOD_SOLIDIFY')

            if self.pattern == 'boards':
                box.separator()
                self.draw_prop(context, layout, box, self, 'max_boards')
                self.draw_prop(context, layout, box, self, 'board_length')
                self.draw_prop(context, layout, box, self, 'vary_length', icon='RNDCURVE')
                if self.vary_length:
                    self.draw_prop(context, layout, box, self, 'length_variance')
                box.separator()

                # width
                self.draw_prop(context, layout, box, self, 'board_width')
                # vary width
                self.draw_prop(context, layout, box, self, 'vary_width', icon='RNDCURVE')
                if self.vary_width:
                    self.draw_prop(context, layout, box, self, 'width_variance')
                box.separator()
                self.draw_prop(context, layout, box, self, 'length_spacing', text="Height spacing")
                self.draw_prop(context, layout, box, self, 'width_spacing')

            elif self.pattern in {'square_parquet', 'herringbone_parquet', 'herringbone'}:
                box.separator()
                self.draw_prop(context, layout, box, self, 'short_board_length')

                if self.pattern != "square_parquet":
                    self.draw_prop(context, layout, box, self, "board_width")
                self.draw_prop(context, layout, box, self, "spacing")

                if self.pattern == 'square_parquet':
                    self.draw_prop(context, layout, box, self, 'boards_in_group')
            elif self.pattern in {'regular_tile', 'hopscotch', 'stepping_stone', 'hexagon', 'windmill'}:
                box.separator()
                # width and length and mortar
                if self.pattern != "hexagon":
                    self.draw_prop(context, layout, box, self, "tile_length", text="Height")
                self.draw_prop(context, layout, box, self, "tile_width")
                self.draw_prop(context, layout, box, self, "spacing")

            if self.pattern in {"regular_tile", "boards"}:
                box.separator()
                self.draw_prop(context, layout, box, self, "random_offset", icon="RNDCURVE")
                if self.random_offset:
                    self.draw_prop(context, layout, box, self, "offset_variance")
                else:
                    self.draw_prop(context, layout, box, self, "offset")

            # grout
            box.separator()
            self.draw_prop(context, layout, box, self, 'add_grout', icon='MESH_GRID')
            if self.add_grout:
                self.draw_prop(context, layout, box, self, 'mortar_depth')

            # bevel
            box.separator()
            self.draw_prop(context, layout, box, self, 'bevel', icon='MOD_BEVEL')
            if self.bevel:
                self.draw_prop(context, layout, box, self, 'bevel_amount')

            if self.pattern != 'user':
                self.draw_prop(context, layout, box, self, 'normalize_uvs')


    def update(self, context, manipulable_refresh=False, relocate_childs=True):
        # use relocate when changing material index 1
        self.parent_data.update(context, manipulable_refresh, relocate_childs=relocate_childs)


class archipack_wall2_finish(Archipacki18n, PropertyGroup):
    finish_name: StringProperty(
        name="",
        description="Name for this finish, may share name with other finish components",
        default="Finish",
        update=update
    )

    idmat: IntVectorProperty(
        default=[0, 1],
        size=2
    )
    material_index: EnumProperty(
        options={'SKIP_SAVE'},
        name="Int",
        items=mat_enum,
        update=update,
        get=mat_index_getter(MAT_FINISH_1),
        set=mat_index_setter( MAT_FINISH_1)
    )
    material_index2: EnumProperty(
        options={'SKIP_SAVE'},
        name="Out",
        items=mat_enum,
        update=update,
        get=mat_index_getter(MAT_FINISH_2),
        set=mat_index_setter( MAT_FINISH_2)
    )
    altitude: FloatProperty(
        name="Altitude",
        description="Altitude from bottom of wall",
        default=0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    z: FloatProperty(
        name="Height",
        description="Height of finish",
        min=0.01,
        default=20,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    rotation: FloatProperty(
        name="Rotation",
        description="Rotate pattern",
        default=0,
        min=-pi / 4,
        max=pi / 4,
        subtype='ANGLE', unit='ROTATION',
        update=update
    )
    pattern_type: EnumProperty(
        name="Pattern",
        items=(
            ('V_BOARD', "Vertical board", "Vertical board"),
            ('H_BOARD', "Horizontal board", "Horizontal board"),
            ('TILES', "Tiles", "Ceramic tiles"),
            ('LOGGING', "Logs", "Round logs"),
            ('USER_DEFINED', "User defined", "User defined object as finishing")
        ),
        default='H_BOARD',
        update=update
    )
    tile_x: FloatProperty(
        name="Width",
        description="Tile width",
        min=0.01,
        default=0.3,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    tile_y: FloatProperty(
        name="Height",
        description="Tile height",
        default=0.6,
        min=0.01,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    tile_z: FloatProperty(
        name="Thickness",
        description="Tile thickness",
        default=0.005,
        min=0.001,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    tile_space: FloatProperty(
        name="Mortar",
        description="Mortar width",
        default=0.002,
        min=0.0001,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    offset_x: FloatProperty(
        name="Offset h",
        description="Pattern horizontal offset",
        min=0.0,
        default=0.0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    offset_y: FloatProperty(
        name="Offset v",
        description="Pattern vertical offset",
        default=0.0,
        min=0.0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    board_x: FloatProperty(
        name="Width",
        description="Pattern width",
        min=0.01,
        default=0.15,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    board_z: FloatProperty(
        name="Thickness",
        description="Pattern thickness",
        default=0.02,
        min=0.001,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    joint_x: FloatProperty(
        name="Width",
        description="Gap width",
        min=0.01,
        default=0.15,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    joint_z: FloatProperty(
        name="Thickness",
        description="Gap thickness",
        default=0.02,
        min=0.001,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    joint_bevel: FloatProperty(
        name="Bevel",
        description="Gap bevel",
        default=0.02,
        min=0.001,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    expand: BoolProperty(
        default=False,
        options={'SKIP_SAVE'}
    )
    user_pattern: StringProperty(
        default="",
        update=update
    )
    force_edges: BoolProperty(
        default=False,
        update=update,
        name="Force corners",
        description="Force corners cuts continuity"
    )
    normalize_uvs: BoolProperty(
        default=False,
        update=update,
        name="Normalize uvs",
        description="Normalize uv maps coordinates (use whole map on each face)"
    )

    @property
    def thickness(self):
        if self.pattern_type in {'H_BOARD', 'V_BOARD'}:
            z = self.board_z
        elif self.pattern_type == 'LOGGING':
            z = self.board_x
        elif self.pattern_type == 'TILES':
            z = self.tile_z
        else:
            z = 0.05
        return z

    @property
    def parent_data(self):
        return self.id_data.archipack_wall2[0]

    def id_mat(self, index):
        _idx = self.idmat[index]
        if _idx < len(self.id_data.materials):
            return _idx
        return 0

    def draw(self, context, layout, index):
        row = layout.row(align=True)
        icon = "TRIA_RIGHT"
        if self.expand:
            icon = "TRIA_DOWN"
        self.draw_prop(context, layout, row, self, 'expand', icon=icon, emboss=True, text="", postfix=str(index + 1))
        self.draw_prop(context, layout, row, self, 'finish_name')
        self.draw_op(context, layout, row, "archipack.wall2_remove_finish", icon='REMOVE', text="").index = index
        self.draw_op(context, layout, row, "archipack.wall2_copy_finish", icon='COPYDOWN', text="").finish_name = self.finish_name

        if self.expand:
            self.draw_prop(context, layout, layout, self, 'altitude')
            self.draw_prop(context, layout, layout, self, 'z')
            # rotation still doesnt work
            # self.draw_prop(context, layout, self, 'rotation')
            self.draw_label(context, layout, layout, "Pattern offset")
            self.draw_prop(context, layout, layout, self, 'offset_x')
            self.draw_prop(context, layout, layout, self, 'offset_y')
            self.draw_prop(context, layout, layout, self, 'force_edges')

            row = layout.row(align=True)
            self.draw_prop(context, layout, row, self, 'pattern_type', text="")
            if self.pattern_type == 'TILES':
                self.draw_prop(context, layout, layout, self, 'tile_x')
                self.draw_prop(context, layout, layout, self, 'tile_y')
                self.draw_prop(context, layout, layout, self, 'tile_z')
                self.draw_prop(context, layout, layout, self, 'tile_space')
                self.draw_prop(context, layout, layout, self, 'normalize_uvs')
            elif 'BOARD' in self.pattern_type or self.pattern_type == 'LOGGING':
                self.draw_prop(context, layout, layout, self, 'board_x')

            if 'BOARD' in self.pattern_type:
                self.draw_prop(context, layout, layout, self, 'board_z')
                self.draw_label(context, layout, layout, "Gap")
                self.draw_prop(context, layout, layout, self, 'joint_x')
                self.draw_prop(context, layout, layout, self, 'joint_z')
                self.draw_prop(context, layout, layout, self, 'joint_bevel')

            layout.label(text="Material")
            self.draw_prop(context, layout, layout, self, 'material_index', text="")
            if self.pattern_type == 'TILES':
                layout.label(text="Material (mortar)")
                self.draw_prop(context, layout, layout, self, 'material_index2', text="")

            if self.pattern_type == 'USER_DEFINED':
                row = layout.row()
                row.prop_search(self, "user_pattern", context.scene, "objects", text="")

    def update(self, context, manipulable_refresh=False, relocate_childs=False):
        self.parent_data.update(context, manipulable_refresh, relocate_childs=relocate_childs)


class archipack_wall2(Archipacki18n, ArchipackUserDefinedPath, ArchipackObject, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display main settings', 'NONE', 0),
            ('PARTS', 'Axis', 'Display walls segments settings', 'NONE', 1),
            ('FINISHINGS', 'Finish', 'Display finishings settings', 'NONE', 2),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 3),
            ('TOOLS', '', 'Display tools', 'MODIFIER', 4)
        ),
        default='MAIN',
    )


    parts: CollectionProperty(type=archipack_wall2_part)
    step_angle: FloatProperty(
        description="Curved parts segmentation",
        name="Step angle",
        min=1 / 180 * pi,
        max=pi,
        default=6 / 180 * pi,
        subtype='ANGLE', unit='ROTATION',
        update=update
    )

    width_ui: FloatProperty(
        options={'SKIP_SAVE'},
        name="Width",
        min=0.01,
        default=0.2, precision=5,
        unit='LENGTH', subtype='DISTANCE',
        get=change_getter('width'),
        set=change_setter('width'),
        update=update_relocate
    )
    width: FloatProperty(
        name="Width",
        min=0.01,
        default=0.2, precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )
    z: FloatProperty(
        name='Height',
        min=0.1,
        default=2.7, precision=5,
        unit='LENGTH', subtype='DISTANCE',
        description='height', update=update,
    )
    base_line: IntProperty(
        name="Base",
        description="Wall part on base line",
        default=0
    )
    base_line_ui: EnumProperty(
        options={'SKIP_SAVE'},
        name="Base",
        description="Wall part on base line",
        items=(
            ('OUTSIDE', "Outside", "Outside"),
            ('AXIS', "Axis", "Axis"),
            ('INSIDE', "Inside", "Inside")),
        default='OUTSIDE',
        get=change_getter('base_line'),
        set=change_setter('base_line'),
        update=update_relocate
    )
    offset: FloatProperty(
        name="Offset",
        description="Lateral offset of wall",
        unit='LENGTH', subtype='DISTANCE',
        default=0, precision=5, step=1
    )
    offset_ui: FloatProperty(
        options={'SKIP_SAVE'},
        name="Offset",
        description="Lateral offset of wall",
        unit='LENGTH', subtype='DISTANCE',
        default=0, precision=5, step=1,
        get=change_getter('offset'),
        set=change_setter('offset'),
        update=update_relocate
    )

    z_offset: FloatProperty(
        name="Floor thickness",
        unit='LENGTH', subtype='DISTANCE',
        description='Thickness of floors',
        min=0,
        default=0.1, precision=4, step=1,
        update=update
    )

    always_closed = False

    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update_manipulators
    )
    auto_synch: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        name="Auto synchro",
        description="Synchronize objects"
    )
    dimensions: BoolProperty(
        default=False,
        name="Dimensions",
        description="Buid static dimensions",
        update=update_childs
    )
    # dumb manipulators to show sizes between childs
    childs_manipulators: CollectionProperty(type=archipack_manipulator)
    # store to manipulate windows and doors
    childs: CollectionProperty(type=archipack_wall2_child)

    # store childs with parts points locations to relocate
    reloc_childs: CollectionProperty(
        options={'SKIP_SAVE'},
        type=archipack_wall2_relocate_child
    )

    reloc_walls: CollectionProperty(
        options={'SKIP_SAVE'},
        type=archipack_wall2_relocate_child
    )

    finish2: CollectionProperty(type=archipack_wall2_finish2)


    finish: CollectionProperty(type=archipack_wall2_finish)
    finish_expand: BoolProperty(
        name="Finishings",
        description="Display finishings options",
        default=False,
        options={'SKIP_SAVE'}
    )
    finish_enable: BoolProperty(
        name="Finishings",
        description="Enable finishings",
        default=True,
        update=update
    )
    finish_inside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Inside",
        description="Finish inside (0 to disable)",
        get=finish_getter(FINISH_INSIDE),
        set=finish_setter(FINISH_INSIDE),
        items=finish_enum,
        update=update
    )
    finish_outside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Outside",
        description="Finish outside (0 to disable)",
        get=finish_getter(FINISH_OUTSIDE),
        set=finish_setter(FINISH_OUTSIDE),
        items=finish_enum,
        update=update
    )
    finish_id: IntVectorProperty(
        size=2,
        default=[0, 0]
    )

    finish2_inside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Inside",
        description="Finish2 inside",
        get=finish2_getter(FINISH_INSIDE),
        set=finish2_setter(FINISH_INSIDE),
        items=finish2_enum,
        update=update_relocate
    )
    finish2_outside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Outside",
        description="Finish2 outside",
        get=finish2_getter(FINISH_OUTSIDE),
        set=finish2_setter(FINISH_OUTSIDE),
        items=finish2_enum,
        update=update_relocate
    )
    finish2_id: IntVectorProperty(
        size=2,
        default=[0, 0]
    )
    finish_outside_auto: BoolProperty(
        name="Auto offset",
        description="Auto offset along wall",
        default=False,
        update=update
    )
    finish_inside_auto: BoolProperty(
        name="Auto offset",
        description="Auto offset along wall",
        default=False,
        update=update
    )
    fit_roof: BoolProperty(
        name="Auto fit roof",
        description="Automatically fit surrounding roof",
        default=False,
        update=update_slices
    )
    # param t of intersections for axis outside and inside
    extremes: FloatVectorProperty(
        size=6,
        default=[0, 1, 0, 1, 0, 1]
    )

    extend = 0

    idmat: IntVectorProperty(
        size=6,
        default=[
            0, 1,
            2, 2,
            0, 0
        ]
    )
    material_inside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Int",
        items=mat_enum,
        get=mat_index_getter(MAT_INSIDE),
        set=mat_index_setter( MAT_INSIDE),
        update=update
    )
    material_outside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Out",
        items=mat_enum,
        get=mat_index_getter(MAT_OUTSIDE),
        set=mat_index_setter( MAT_OUTSIDE),
        update=update
    )
    material_top: EnumProperty(
        options={'SKIP_SAVE'},
        name="Top",
        items=mat_enum,
        get=mat_index_getter(MAT_TOP),
        set=mat_index_setter( MAT_TOP),
        update=update
    )
    material_bottom: EnumProperty(
        options={'SKIP_SAVE'},
        name="Bottom",
        items=mat_enum,
        get=mat_index_getter(MAT_BOTTOM),
        set=mat_index_setter( MAT_BOTTOM),
        update=update
    )
    material_start: EnumProperty(
        options={'SKIP_SAVE'},
        name="Start",
        items=mat_enum,
        get=mat_index_getter(MAT_START),
        set=mat_index_setter( MAT_START),
        update=update
    )
    material_end: EnumProperty(
        options={'SKIP_SAVE'},
        name="End",
        items=mat_enum,
        get=mat_index_getter(MAT_END),
        set=mat_index_setter( MAT_END),
        update=update
    )

    snap_wall_start: BoolProperty(
        name="Start",
        description="Auto snap wall start",
        default=True,
        update=update
    )
    snap_wall_end: BoolProperty(
        name="End",
        description="Auto snap wall end",
        default=True,
        update=update
    )

    changed = LastState()

    draw_direction: EnumProperty(
        name="Direction",
        description="Wall draw direction",
        items=(
            ('CCW', "CCW", "Draw in ccw order", 0),
            ('CW', "CW", "Draw in cw order", 1)
        ),
        default='CCW'
    )

    @property
    def direction(self):
        return {'CCW':1, 'CW':-1}[self.draw_direction]

    @property
    def x_offset(self):
        """ Lateral offset of axis from base line
        base line is in range [0:2]
        :return: x_offset in range [-1:1]
        """
        return self.direction * (self.base_line - 1)

    def clear_slices(self):
        for p in self.parts:
            p.slices.clear()
            p.slices.add()

    def check_slices(self):
        for p in self.parts:
            if len(p.slices) < 1:
                p.slices.add()

    def after_insert_part(self, context, o, where, distance):
        self.setup_childs(context, o, update_all=True)
        self.select_object(context, o, True)

    def after_remove_part(self, context, o, where):
        self.setup_childs(context, o, update_all=True)
        self.select_object(context, o, True)

    def after_make_first(self, context, o, where):
        self.setup_childs(context, o, update_all=True)
        self.select_object(context, o, True)

    def get_generator(self, o=None):
        """
         o: Object or Matrix, make generator in coordsys
         axis: Generate offset at wall axis
        """

        # if o is not None:
        #    print("get_generator %s" % o.name)

        self.check_slices()

        g = WallGenerator(o)
        g.add_parts(self)
        g.init_sides(self)
        return g

    def reverse(self, context, o):

        g = self.get_generator()
        loc, rot, scale = o.matrix_world.decompose()

        _line = [g.outside, g, g.inside][self.base_line]
        _line.reverse()

        # move origin to new point
        o.location += rot.to_matrix() @ _line.reset_origin()

        self.offset = -self.offset
        self.changed.init(self, 'base_line')
        self.base_line = 2 - self.base_line
        _line.update_parts(self)
        self.manipulable_refresh = True
        self.update(context)
        self.setup_childs(context, o, update_all=True)
        self.relocate_childs(context, o)

    def setup_manipulators(self):

        if len(self.manipulators) == 0:
            # make manipulators selectable
            s = self.manipulators.add()
            s.prop1_name = "width_ui"

            s = self.manipulators.add()
            s.prop1_name = "n_parts"
            s.type_key = 'COUNTER'

            s = self.manipulators.add()
            s.prop1_name = "z"
            s.normal = (0, 1, 0)

        # if self.t_part != "" and len(self.manipulators) < 4:
        #    s = self.manipulators.add()
        #    s.prop1_name = "x"
        #    s.type_key = 'DELTA_LOC'

        self.setup_parts_manipulators('z')

    def from_spline(self, context, o, curve, ccw=False, cw=False):
        ArchipackUserDefinedPath.from_spline(self, context, o, curve, ccw=True)

    def apply_modifier(self, o, modif):
        """move modifier on top of stack and apply
        """
        index = o.modifiers.find(modif.name)
        if index > -1:
            for i in range(index):
                bpy.ops.object.modifier_move_up(modifier=modif.name)
            bpy.ops.object.modifier_apply(apply_as='DATA', modifier=modif.name)

    def update_parts(self):
        # Override to handle parts slices
        self.check_slices()

        for p in self.parts:
            self.create_uid(p, increment=2)

        self.setup_manipulators()

    def unwrap_uv(self, o):
        # uv unwrap
        bm = bmed._start(o)
        bmed.ensure_bmesh(bm)

        layer = bm.loops.layers.uv.verify()
        for i, face in enumerate(bm.faces):
            if i % 4 == 0:
                p = face.verts[0].co
            vz = face.normal
            if vz.length < 0.5:
                # fallback for faces with null normal
                tM = Matrix.Translation(p)
            else:
                if abs(vz.z) < 0.5:
                    vx = vz.cross(Z_AXIS)
                else:
                    vx = vz.cross(X_AXIS)
                vy = vx.cross(vz)
                tM = Matrix([
                    [vx.x, vy.x, vz.x, p.x],
                    [vx.y, vy.y, vz.y, p.y],
                    [vx.z, vy.z, vz.z, p.z],
                    [0, 0, 0, 1]
                ])
            itM = tM.inverted()
            for j, loop in enumerate(face.loops):
                loop[layer].uv = (itM @ loop.vert.co).to_2d()
        bmed._end(bm, o)

    def update(self,
            context,
            manipulable_refresh=False,
            setup_childs=False,
            update_childs=False,
            relocate_childs=False,
            allow_throttle=True
            ):

        tim = time.time()

        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return


        if self.manipulable_refresh:
            # prevent crash by removing all manipulators refs to datablock before changes
            self.manipulable_disable(o)

        # setup childs using old data on width, offset side and base_line changes
        self.setup_childs(context, o, update_all=setup_childs)
        logger.debug("Wall2.update() %s setup_childs(1) :%.2f seconds", o.name, time.time() - tim)

        # print("wall2 update start")

        verts = []
        faces = []
        matids = []
        vcolor = []
        vgroup_top = []
        finishes = []

        # self.update_parts()

        vgroup = o.vertex_groups.get('Top')
        if vgroup is not None:
            o.vertex_groups.remove(vgroup)
        logger.debug("Wall2.update() %s vertex_groups.remove :%.2f seconds", o.name, time.time() - tim)

        # Snap neighboors
        self.snap_wall_sides(o)
        logger.debug("Wall2.update() %s snap_wall_sides :%.2f seconds", o.name, time.time() - tim)

        # Fit Roof
        roof = None
        roofs = []
        rd = None
        if self.fit_roof:
            roofs = self.find_roofs(context, o)
            if len(roofs) > 0:
                self.clear_slices()

                for roof in roofs:
                    rd = roof.data.archipack_roof[0]
                    rd.make_wall_fit(context, roof, o, clear_slices=False)

        if self.manipulable_refresh or update_childs:
            self.setup_childs(context, o, update_all=True)
            logger.debug("Wall2.update() %s setup_childs(2):%s  :%.2f seconds", o.name, update_childs, time.time() - tim)

        g = self.get_generator()
        g.make_wall(self, verts, faces, matids, vcolor, vgroup_top, finishes)
        logger.debug("Wall2.update() %s 2 make_wall :%.2f seconds", o.name, time.time() - tim)
        # print("vgroup_top:", vgroup_top)

        bmed.buildmesh(o, verts, faces, matids, vcolors=vcolor)
        self.shade_smooth(context, o, 0.20944)
        logger.debug("Wall2.update() 3 buildmesh :%.2f seconds", time.time() - tim)

        offset = self.offset + self.x_offset * 0.5 * self.width

        # Width
        self.manipulators[0].set_pts([
            g.segs[0].normal(0, offset + 0.5 * self.width).v,
            g.segs[0].normal(0, offset - 0.5 * self.width).v,
            (-0.5, 0, 0)
            ])

        # Parts COUNTER
        self.manipulators[1].set_pts([g.segs[-2].lerp(1.1),
            g.segs[-2].lerp(1.1 + 0.5 / g.segs[-2].length),
            (-1, 0, 0)
            ])

        # Height
        self.manipulators[2].set_pts([
            Vector((0, 0, -self.z_offset)),
            Vector((0, 0, self.z - self.z_offset)),
            (-1, 0, 0)
            ], normal=self.direction * g.segs[0].straight(1, 0).v)

        # update materials of holes
        for mod in o.modifiers:
            if mod.type == 'BOOLEAN' and mod.object is not None:
                self.link_materials(context, o, mod.object)
                for hole_mod in mod.object.modifiers:
                    if hole_mod.type == 'BOOLEAN' and hole_mod.object is not None:
                        self.link_materials(context, o, hole_mod.object)

        # if self.t_part != "":
        #    t = 0.3 / g.segs[0].length
        #    self.manipulators[3].set_pts([
        #        g.segs[0].normal(t, 0.1).p1,
        #        g.segs[0].normal(t, -0.1).p1,
        #        (1, 0, 0)
        #        ])

        logger.debug("Wall2.update() %s 4 manipulators :%.2f seconds", o.name, time.time() - tim)

        # update child location and size
        if relocate_childs and self.auto_synch:
            self.relocate_childs(context, o, g)

        # store gl points
        self.update_childs(context, o, g)

        logger.debug("Wall2.update() %s 5 relocate :%.2f seconds", o.name, time.time() - tim)

        # Add / remove providers to wall dimensions
        self.update_dimension(context, o, g)
        logger.debug("Wall2.update() 6 dimensions :%.2f seconds", time.time() - tim)

        # Update all dimensions
        if relocate_childs or update_childs:
            self.update_dimensions(context, o)

        logger.debug("Wall2.update() 7 dimensions :%.2f seconds", time.time() - tim)
        vgroup = o.vertex_groups.get("Top")
        if vgroup is None:
            vgroup = o.vertex_groups.new(name="Top")

        vgroup_bot = [v.index for v in o.data.vertices]
        vgroup.add(vgroup_bot, 0, 'REPLACE')
        vgroup.add(vgroup_top, 1, 'REPLACE')

        # logger.debug("Wall2.update() throttle:%s  :%.2f seconds", throttle.is_active(o.name), time.time() - tim)
        if self.fit_roof and len(roofs) > 0:
            # self.apply_modifier(o, modif)
            if not throttle.is_active(o.name):
                for roof in roofs:
                    rd = roof.data.archipack_roof[0]
                    logger.debug("Wall2.update() 9 fit roof :%.2f seconds", time.time() - tim)
                    self.fit_roof_modifier(o, roof, rd, apply=True)

        self.unwrap_uv(o)

        if  len(self.finish) < 1:
            if len(self.finish2) > 0 and self.finish_enable:

                if allow_throttle:
                    throttle.add(context, o, self)

                if not throttle.is_active(o.name):
                    logger.debug("Wall2.update() 10 finishings :%.2f seconds", time.time() - tim)

                    # finish
                    bm = bmed._start(o)
                    bm.verts.index_update()
                    bm.faces.index_update()

                    finish_indexes = {}
                    i = 1
                    for finish in self.finish2:
                        _n = finish.finish_name
                        if _n not in finish_indexes:
                            finish_indexes[_n] = i
                            i += 1
                    for finish in self.finish2:
                        _n = finish.finish_name
                        if _n not in finish_indexes:
                            finish_indexes[_n] = -1

                    patterns = []

                    for finish2 in self.finish2:
                        self.build_floor_finish(context, o, finish2, finish_indexes, bm, patterns, finishes)

                    bm.free()
                    bmed.bmesh_join(o, patterns, normal_update=True)

        elif self.finish_enable:

            if allow_throttle:
                throttle.add(context, o, self)

            if not throttle.is_active(o.name):
                logger.debug("Wall2.update() 10 finishings :%.2f seconds", time.time() - tim)

                # finish
                bm = bmed._start(o)
                bm.verts.index_update()
                bm.faces.index_update()

                finish_indexes = {}
                i = 1
                for finish in self.finish:
                    _n = finish.finish_name
                    if _n not in finish_indexes:
                        finish_indexes[_n] = i
                        i += 1
                for finish in self.finish:
                    _n = finish.finish_name
                    if _n not in finish_indexes:
                        finish_indexes[_n] = -1

                patterns = []
                for finish in self.finish:
                    self.build_finish(context, o, finish, finish_indexes, bm, patterns, finishes)

                bm.free()
                bmed.bmesh_join(o, patterns, normal_update=True)

        # if manipulable_refresh:
        #    self.manipulable_refresh = True

        logger.debug("Wall2.update() %s done :%.2f seconds", o.name, time.time() - tim)

        # print("wall2 update end")

        self.restore_context(context)
    
    @property
    def random_color(self):
        return Vector((uniform(0.0, 1.0), uniform(0.0, 1.0), uniform(0.0, 1.0), 1))

    # Finish
    def vertical_boards(self, finish, profil, sx, sy, minx, maxx, miny, maxy, offset, verts, faces, uvs, vcolor):
        profil_z = finish.board_x
        n_y = 1 + int(sx / profil_z)
        u = maxx - minx
        p0 = profil[-1] - Vector((0, profil_z, 0))
        uvs_v = []
        for p1 in profil:
            uvs_v.append((p1 - p0).length)
            p0 = p1
        v_len = sum(uvs_v)
        # start at 0,0
        col = self.random_color
        verts.append(Vector((minx, miny, offset)))
        verts.append(Vector((minx, maxy, offset)))
        faces.append((0, 1, 3, 2))
        uvs.append([(0, 0), (sx, 0), (sx, u), (0, u)])
        vcolor.append(col)
        f = 2
        for i in range(n_y):
            if finish.normalize_uvs:
                u1 = 1
            else:
                u1 = u
            v0 = 0
            x0 = minx + i * profil_z
            for co, v in zip(profil, uvs_v):
                if finish.normalize_uvs:
                    v1 = 1 / v
                else:
                    v1 = v
                verts.append(Vector((x0 + co.z, miny, co.y)))
                verts.append(Vector((x0 + co.z, maxy, co.y)))
                faces.append((f, f + 1, f + 3, f + 2))
                uvs.append([(v0, 0), (v0, u1), (v1, u1), (v1, 0)])
                v0 = v1
                vcolor.append(col)
                f += 2
            col = self.random_color
        x0 = minx + n_y * profil_z
        verts.append(Vector((x0, miny, offset)))
        verts.append(Vector((x0, maxy, offset)))
        faces.append((f, f + 1, 1, 0))
        uvs.append([(0, 0), (sx, 0), (sx, u), (0, u)])
        vcolor.append(col)

    def horizontal_boards(self, finish, profil, sx, sy, minx, maxx, miny, maxy, offset, verts, faces, uvs, vcolor):
        profil_z = finish.board_x
        n_z = 1 + int(sy / profil_z)
        u = maxx - minx
        p0 = profil[-1] - Vector((0, profil_z, 0))
        uvs_v = []
        for p1 in profil:
            uvs_v.append((p1 - p0).length)
            p0 = p1
        v_len = sum(uvs_v)
        col = self.random_color
        # start at 0,0
        verts.append(Vector((minx, miny, offset)))
        verts.append(Vector((maxx, miny, offset)))
        faces.append((0, 2, 3, 1))
        uvs.append([(0, 0), (sx, 0), (sx, u), (0, u)])
        vcolor.append(col)
        f = 2
        for i in range(n_z):
            if finish.normalize_uvs:
                u1 = 1
            else:
                u1 = u
            v0 = 0
            z0 = miny + i * profil_z
            for co, v in zip(profil, uvs_v):
                if finish.normalize_uvs:
                    v1 = 1 / v
                else:
                    v1 = v
                z = min(maxy, z0 + co.z)
                verts.append(Vector((minx, z, co.y)))
                verts.append(Vector((maxx, z, co.y)))
                faces.append((f, f + 2, f + 3, f + 1))
                uvs.append([(v0, 0), (v1, 0), (v1, u1), (v0, u1)])
                v0 = v1
                vcolor.append(col)
                f += 2
            col = self.random_color
        z0 = min(maxy, miny + n_z * profil_z)
        verts.append(Vector((minx, z0, offset)))
        verts.append(Vector((maxx, z0, offset)))
        faces.append((f, 0, 1, f + 1))
        uvs.append([(0, 0), (sx, 0), (sx, u), (0, u)])
        vcolor.append(col)

    def ceiling(self, finish, sx, sy, minx, maxx, miny, maxy, offset, verts, faces, matids, uvs, vcolor):
        n_y = 1 + int(sy / finish.tile_y)
        n_x = 1 + int(sx / finish.tile_x)

        # start at 0,0
        #
        #
        #   x_x_______x_x   x_______x x
        #   x x_______x x   x_______x x
        #   | |       | |   |       | |
        #   x x_______x x   x_______x x
        #   x_x_______x_x
        #     x0     x1 x2
        #

        # half join
        h = 0.5 * finish.tile_space
        # ceiling
        x1 = finish.tile_x - 2 * h
        x2 = finish.tile_x - h
        y = 0
        z0 = finish.tile_z + offset

        y = miny
        dx = finish.tile_x
        dy = [h, finish.tile_y - h, finish.tile_y]

        verts.append(Vector((minx, y, z0)))
        for j in range(n_x):
            x0 = minx + h + j * dx
            verts.append(Vector((x0, y, z0)))
            verts.append(Vector((x0 + x1, y, z0)))
            verts.append(Vector((x0 + x2, y, z0)))

        f = 0
        fx = 1 + n_x * 3
        for i in range(n_y):
            y0 = miny + i * finish.tile_y
            for k in range(3):
                v = dy[k]
                y = y0 + v
                # 1st col
                mat = 1
                if k == 1:
                    mat = 0
                verts.append(Vector((minx, y, z0)))
                if k > 0 and y > maxy:
                    if k == 1:
                        y = maxy - h
                    else:
                        y = maxy
                for j in range(n_x):
                    col = self.random_color
                    if finish.normalize_uvs:
                        u1, v1 = 1, 1
                    else:
                        u1, v1 = dx, v
                    
                    x0 = minx + h + j * dx
                    verts.append(Vector((x0, y, z0)))
                    verts.append(Vector((x0 + x1, y, z0)))
                    verts.append(Vector((x0 + x2, y, z0)))
                    # faces.append((f, f + 1, f + fx + 1, f + fx))
                    faces.append((f, f + fx, f + fx + 1, f + 1))
                    f += 1
                    faces.append((f, f + fx, f + fx + 1, f + 1))
                    f += 1
                    faces.append((f, f + fx, f + fx + 1, f + 1))
                    f += 1
                    matids.extend([1, mat, 1])
                    uvs.extend([
                        [(0, 0), (0, v), (h, v), (h, 0)],
                        [(0, 0), (0, v1), (u1, v1), (u1, 0)],
                        [(0, 0), (0, v), (h, v), (h, 0)]
                    ])
                    vcolor.extend([col] * 3)
                f += 1

    def make_user_pattern(self,
                        sx, sy,
                        minx, maxx, miny, maxy, offset,
                        finish,
                        user_x, user_y, user_verts, user_faces, user_matids, user_uvs,
                        verts, faces, matids):

        n_y = 2 + int(sy / user_y)
        n_x = 2 + int(sx / user_x)

        for nx in range(n_x):
            x = minx + nx * user_x
            for ny in range(n_y):
                y = miny + ny * user_y
                tM = Matrix.Translation(Vector((x, y, offset)))
                f = len(verts)
                verts.extend([tM @ p for p in user_verts])
                faces.extend([tuple([i + f for i in p]) for p in user_faces])
                matids.extend(user_matids)

    def build_floor_finish(self, context, o, finish, finish_indexes, bm, patterns, finishes):
        """
        Floor based pattern finish
        :param context:
        :param o:
        :param finish:
        :param finish_indexes:
        :param bm:
        :param patterns:
        :param finishes:
        :return:
        """
        location_index = finish_indexes[finish.finish_name]

        patman = PatternGenerator()

        if finish.pattern == "user" and self.get_scene_object(context, finish.user_defined_pattern) is None:
            return

        for face in bm.faces:
            idx, offset, origin = finishes[face.index]
            if idx == location_index:
                # store temp bmesh
                temp = []

                n = face.normal
                # never try on horizontal faces
                if abs(n.z) > 0.01:
                    continue

                # horizontal on the left of the face normal x axis of the matrix
                c = n.cross(-Z_AXIS)

                # print(n, c)

                # center of the face
                # fc = face.calc_center_bounds()

                # matrix of a plane at segment origin where z axis is normal and y axis is up
                # prerotate of finish rotation
                origin_mat = Matrix([
                    [c.x, 0, n.x, origin.x],
                    [c.y, 0, n.y, origin.y],
                    [0.0, 1, 0.0, finish.altitude],
                    [0.0, 0, 0.0, 1]
                ]) @ Matrix.Rotation(finish.rotation, 4, "Z")

                # offset of face center in origin mat, we only care of z axis
                # x, y, z = origin_mat.inverted() @ fc

                # move origin matrix along z axis so it is coplanar with face
                # offset origin in rotated coordsys
                tM = origin_mat @ Matrix.Translation(Vector((offset + finish.offset_x, finish.offset_y, 0)))
                pattern = self.get_scene_object(context, finish.user_defined_pattern)
                patman.make_finishing(o, finish, pattern, face, tM, temp)

                # verts index in face
                f_index = [v.index for v in face.verts]

                # Slice using linked faces
                for edge in face.edges:

                    for link_face in edge.link_faces:
                        if link_face is not face:
                            slice_co = edge.verts[0].co
                            if abs(link_face.normal.z) < 0.1:
                                y = edge.verts[1].co - slice_co
                                # edge is not guarantee to be in the right order
                                # ensure edge is in face order
                                last = f_index[-1]
                                for idx in f_index:
                                    if edge.verts[0].index == idx and edge.verts[1].index == last:
                                        break
                                    if edge.verts[1].index == idx and edge.verts[0].index == last:
                                        y = -y
                                        break
                                    last = idx

                                # intersection of faces using same finishing
                                if finishes[link_face.index][0] == location_index or finish.force_edges:
                                    # slice plane use both normals
                                    # perpendicular to edge, ensure always looking inside
                                    slice_no = (n + link_face.normal).normalized().cross(y)
                                    slice_co -= 0.00005 * slice_no
                                else:
                                    # faces doesn't share finishings
                                    slice_no = n.cross(y)
                            else:
                                # either a top or bottom face
                                # slice plane use this face normal
                                slice_no = link_face.normal
                                # slice bottom part when pattern is under wall bottom
                                if link_face.normal.z < -0.5:
                                    slice_co = Vector((0, 0, finish.altitude))

                            # slice pattern using co and no
                            for part in temp:
                                self.slice_pattern(part, slice_co, slice_no)

                # slice top
                slice_co = Vector((0, 0, finish.altitude + finish.z))
                slice_no = Vector((0, 0, 1))
                for part in temp:
                    self.slice_pattern(part, slice_co, slice_no)

                # remove outside parts of loop
                patterns.extend(temp)

    def build_finish(self, context, o, finish, finish_indexes, bm, patterns, finishes):
        # offset from wall twice of remove double to prevent merge of faces
        z = 0
        offset = 0.0002
        if finish.pattern_type in {'H_BOARD', 'V_BOARD'}:
            z = finish.board_z
            x = finish.board_x
            z1 = max(0.001, z - finish.joint_z)      # 0.6 * z
            x2 = min(finish.joint_x, x - 0.0002)      # 0.7 * x
            x1 = min(x2 - finish.joint_bevel, x2 - 0.0002)         # 0.8 * x
            profil = [Vector(p) for p in [
                    (0, z1 + offset, 0), (0, z1 + offset, x1),
                    (0, z + offset, x2), (0, z + offset, x)
                    ]]

        elif finish.pattern_type == 'LOGGING':
            z = finish.board_x
            x = finish.board_x
            r = 0.5 * z
            a0 = -pi / 2
            segs = 16
            a = pi / segs
            profil = [Vector((0, r * cos(a0 + a * i) + 2 * offset, r + r * sin(a0 + a * i))) for i in range(segs)]

        elif finish.pattern_type == 'TILES':
            z = finish.tile_z

        elif finish.pattern_type == 'USER_DEFINED':
            pattern_obj = self.get_scene_object(context, finish.user_pattern)
            if pattern_obj is None:
                return
            user_x, user_y, z = pattern_obj.dimensions
            d = pattern_obj.data
            user_verts = [Vector((-v.co.x, v.co.y, v.co.z)) for v in d.vertices]
            user_faces = [tuple(p.vertices[:]) for p in d.polygons]
            if len(d.uv_layers) > 0:
                uv_layer = d.uv_layers[0].data
                user_uvs = [[uv_layer[v].uv for v in p.loop_indices] for p in d.polygons]
            else:
                user_uvs = [[user_verts[v].to_2d() for v in f] for f in user_faces]
            user_idmat = [p.material_index for p in d.polygons]

        mod_x = 1000000
        if finish.pattern_type == 'V_BOARD':
            mod_x = finish.board_x
        elif finish.pattern_type == 'TILES':
            mod_x = finish.tile_x

        z_bot = finish.altitude
        z_top = z_bot + finish.z

        # target segments material index
        location_index = finish_indexes[finish.finish_name]
        material_index = finish.id_mat(MAT_FINISH_1)
        material_index2 = finish.id_mat(MAT_FINISH_2)
        # lateral profile in yz axis (last will be next first one)
        # so this is profile -last vertex
        z_axis = Vector((0, 0, 1))

        for face in bm.faces:
            idx, offset, origin = finishes[face.index]
            if idx == location_index:

                n = face.normal
                # never try on vertical faces
                if abs(n.z) > 0.01:
                    continue

                # horizontal on the left of the face normal x axis of the matrix
                c = n.cross(z_axis)

                # center of the face
                fc = face.calc_center_bounds()

                # face coordsys
                # origin at face center
                # x axis is face side
                # y axis is up
                # z axis is face normal

                # prerotate
                face_mat = Matrix([
                    [c.x, 0, n.x, fc.x],
                    [c.y, 0, n.y, fc.y],
                    [0.0, 1, 0.0, fc.z],
                    [0.0, 0, 0.0, 1]
                ]) @ Matrix.Rotation(finish.rotation, 4, "Z")


                """
                # horizontal
                x_face = face_normal.cross(z_axis)
                # this is z_axis !!
                y_face = x_face.cross(face_normal)
                origin = face.calc_center_bounds()

                # Matrix of a plane at object's 0,0,0 and with z matching face normal
                # Prerotate to allow pattern rotations
                face_rot = Matrix([
                    [x_face.x, y_face.x, face_normal.x, 0],
                    [x_face.y, y_face.y, face_normal.y, 0],
                    [x_face.z, y_face.z, face_normal.z, 0],
                    [0, 0, 0, 1]
                    ]) @ Matrix.Rotation(finish.rotation, 4, z_axis)

                # z of face center in face_rot coordsys
                delta_z = (face_rot.inverted() @ origin).z

                # Pretranslate matrix along z to face
                face_mat = face_rot @ Matrix.Translation(
                        Vector((0, 0, delta_z))
                        )
                """
                face_inverse = face_mat.inverted()

                # coord object origin in face system
                face_delta = face_inverse @ Vector()

                # loop in face coordsys
                bounds = [face_inverse @ loop.vert.co for loop in face.loops]

                # in face_inverse space
                cx = [co.x for co in bounds]
                cy = [co.y for co in bounds]

                minx = min(cx)
                maxx = max(cx)

                miny = z_bot - fc.z
                maxy = min(z_top, max(cy))

                # make minx start at multiple of profile
                # world        minx |        c
                # |    |    |    |    |
                # x              |  Xm|
                # x

                ox = mod_x - ((offset + finish.offset_x + face_delta.x - minx) % mod_x)
                sx = maxx - minx + ox
                minx -= ox

                # extend pattern for depth on edges (arbitrary * 5)
                dx = z * 5
                minx -= dx
                maxx += dx
                sx += 2 * dx

                # enlarge for rotation
                dy = 2 * abs(tan(finish.rotation)) * sx
                miny -= dy
                maxy += dy

                # make miny start at multiple of profile

                oy = finish.offset_y
                sy = maxy - miny + oy
                miny -= oy

                if sx <= 0 or sy <= 0:
                    continue

                # fill in pattern
                verts = []
                faces = []
                matids = []
                vcolor = []
                uvs = []

                if finish.pattern_type == 'H_BOARD':
                    self.horizontal_boards(finish, profil, sx, sy, minx, maxx, miny, maxy, offset,
                                           verts, faces, uvs, vcolor)
                elif finish.pattern_type == 'V_BOARD':
                    self.vertical_boards(finish, profil, sx, sy, minx, maxx, miny, maxy, offset,
                                         verts, faces, uvs, vcolor)
                elif finish.pattern_type == 'LOGGING':
                    self.horizontal_boards(finish, profil, sx, sy, minx, maxx, miny, maxy, offset,
                                           verts, faces, uvs, vcolor)
                elif finish.pattern_type == 'TILES':
                    self.ceiling(finish, sx, sy, minx, maxx, miny, maxy, offset,
                                 verts, faces, matids, uvs, vcolor)
                elif finish.pattern_type == 'USER_DEFINED':
                    vcolor = None
                    uvs = None
                    self.make_user_pattern(
                        sx, sy,
                        minx, maxx, miny, maxy, offset,
                        finish,
                        user_x, user_y, user_verts, user_faces, user_idmat, user_uvs,
                        verts, faces, matids)

                # build our pattern
                pattern = bmed.buildmesh(o, verts, faces, uvs=uvs, vcolors=vcolor, temporary=True)
                _faces = pattern.faces

                if finish.pattern_type == 'TILES':
                    pattern.normal_update()
                    # 20 is for mortar
                    mats = [material_index, material_index2]
                    for i, mat in enumerate(matids):
                        _faces[i].material_index = mats[mat]

                    # inset
                    inset = 0.001
                    geom = [f for f in _faces if f.material_index == material_index]

                    if len(geom) > 0:
                        bmesh.ops.inset_individual(
                            pattern,
                            faces=geom,
                            thickness=inset,
                            depth=-inset,
                            use_even_offset=False,
                            use_interpolate=False,
                            use_relative_offset=False
                            )

                        _faces.ensure_lookup_table()

                    # extrude boundarys
                    geom = [ed for ed in pattern.edges[:] if ed.is_boundary]
                    if len(geom) > 0:
                        ret = bmesh.ops.extrude_edge_only(
                            pattern,
                            edges=geom)

                        geom = [v for v in ret["geom"] if isinstance(v, bmesh.types.BMVert)]
                        del ret
                        bmesh.ops.translate(
                            pattern,
                            verts=geom,
                            vec=Vector((0, 0, offset - finish.tile_z))
                            )

                elif finish.pattern_type == 'USER_DEFINED':

                    pattern.normal_update()

                    for i, mat in enumerate(matids):
                        _faces[i].material_index = mat

                    # weld
                    geom = [v for v in pattern.verts[:] if v.is_boundary]
                    if len(geom) > 0:
                        bmesh.ops.remove_doubles(
                            pattern,
                            verts=geom,
                            dist=0.0001
                            )

                    # extrude boundarys
                    geom = [ed for ed in pattern.edges[:] if ed.is_boundary]
                    if len(geom) > 0:
                        ret = bmesh.ops.extrude_edge_only(
                            pattern,
                            edges=geom)

                        geom = [v for v in ret["geom"] if isinstance(v, bmesh.types.BMVert)]
                        del ret
                        bmesh.ops.translate(
                            pattern,
                            verts=geom,
                            vec=Vector((0, 0, -0.5 * offset))
                            )

                else:
                    for i in range(len(_faces)):
                        _faces[i].material_index = material_index

                geom = [ed for ed in pattern.edges[:] if ed.is_boundary]
                if len(geom) > 0:
                    bmesh.ops.holes_fill(
                            pattern,
                            edges=geom,
                            sides=len(geom)
                            )

                # move our pattern in object's space
                pattern.transform(face_mat)

                # verts index in face
                f_index = [v.index for v in face.verts]

                # Slice using linked faces
                for edge in face.edges:

                    for link_face in edge.link_faces:
                        if link_face is not face:
                            slice_co = edge.verts[0].co
                            if abs(link_face.normal.z) < 0.1:
                                y = edge.verts[1].co - slice_co
                                # edge is not garantee to be in the right order
                                # ensure edge is in face order
                                last = f_index[-1]
                                for idx in f_index:
                                    if edge.verts[0].index == idx and edge.verts[1].index == last:
                                        break
                                    if edge.verts[1].index == idx and edge.verts[0].index == last:
                                        y = -y
                                        break
                                    last = idx

                                # intersection of faces using same finishing
                                if finishes[link_face.index] == location_index or finish.force_edges:
                                    # slice plane use both normals
                                    # perpendicular to edge, ensure always looking inside
                                    slice_no = (n + link_face.normal).normalized().cross(y)
                                    slice_co -= 0.00005 * slice_no
                                else:
                                    # faces doesn't share finishings
                                    slice_no = n.cross(y)
                            else:
                                # either a top or bottom face
                                # slice plane use this face normal
                                slice_no = link_face.normal
                                # slice bottom part when pattern is under wall bottom
                                if link_face.normal.z < -0.5:
                                    slice_co = Vector((0, 0, finish.altitude))

                            # slice pattern using co and no
                            self.slice_pattern(pattern, slice_co, slice_no)

                # slice top
                slice_co = Vector((0, 0, finish.altitude + finish.z))
                slice_no = Vector((0, 0, 1))
                self.slice_pattern(pattern, slice_co, slice_no)

                # remove outside parts of loop
                patterns.append(pattern)

    def slice_pattern(self, pattern, slice_co, slice_no):

        bmed.bisect(pattern,
                    slice_co,
                    slice_no,
                    clear_outer=True
                )

        geom = [ed for ed in pattern.edges[:] if not ed.is_manifold]
        if len(geom) > 0:
            bmesh.ops.holes_fill(
                pattern,
                edges=geom,
                sides=len(geom)
                )

    def fit_roof_modifier(self, o, roof, rd, apply=False):
        if self.fit_roof and roof is not None:
            target = rd.find_shrinkwrap(roof)
            if target is not None:
                modif = o.modifiers.new('Roof', 'SHRINKWRAP')
                modif.wrap_method = 'PROJECT'
                modif.vertex_group = 'Top'
                modif.use_project_z = True
                modif.use_negative_direction = True
                modif.target = target
                if apply:
                    self.apply_modifier(o, modif)

    def add_child(self, name, wall_idx, pos, flip):
        # print("add_child %s %s" % (name, wall_idx))
        c = self.childs.add()
        c.child_name = name
        c.wall_idx = wall_idx
        c.pos = pos
        c.flip = flip
        m = c.manipulators.add()
        m.type_key = 'DELTA_LOC'
        m.prop1_name = "x"
        m = c.manipulators.add()
        m.type_key = 'SNAP_SIZE_LOC'
        m.prop1_name = "x"
        m.prop2_name = "x"

    def _find_relocate_childs(self, o, childs):
        d, k = archipack_wall2_relocate_child.filter_child(self, o)
        if d:
            if k not in childs:
                childs[k] = {}
            childs[k][o] = d

    def find_relocate_childs(self, o):
        # find childs and sort by kind
        childs = {}
        if o.parent:
            for c in o.parent.children:
                self._find_relocate_childs(c, childs)
        return childs

    def add_relocate_child(
            self,
            tree,
            name,
            c_idx,
            pt,
            all_segs,
            maxdist,
            seg_idx=None,
            allow_single=False):
        """
         Find 2 closest segments
         Store distance to segments, segment indexes
         maxdist: distance of point from segment in front / back
         seg_idx: index of parent segment to project on closest
                  allow inner walls projection over outside
         allow_single: walls inside volume might have only isolated segment
                  this option enable link to isolated seg using a t and distance rule
        """
        closest = []

        count, selection = tree.intersects_pt(pt, maxdist)

        # logger.debug("tree.intersects_pt(%s) :%.4f seconds",  count, (time.time() - tim))

        for idx in selection:
            parent_name, parent_idx, seg, side = tree._geoms[idx]
            if parent_name != name and parent_name in all_segs:
                res, dist, t = seg.point_sur_segment(pt)
                abs_d = abs(dist)
                t_max = maxdist / seg.length

                # squared dist taking account of point location over segment
                d2 = dist ** 2
                if t > 1:
                    d2 += ((t - 1) * seg.length) ** 2
                elif t < 0:
                    d2 += (t * seg.length) ** 2

                if abs_d < maxdist and -t_max < t < 1 + t_max:
                    # logger.debug("%s %s %s %s", name, parent_name, d2, dist ** 2)
                    closest.append((d2, -dist, parent_name, parent_idx, t))

        # get 2 closest segments
        # childs walls use projection of themself through seg_idx

        if len(closest) > 1 or (seg_idx is not None and len(closest) > 0):
            closest.sort(key=lambda s: s[0])
            c = self.reloc_childs.add()
            c.child_name = name
            c.child_idx = c_idx
            if seg_idx is not None:
                # pick near segment with greateast angle (angle closest to 90)
                s0 = all_segs[name].segs[seg_idx]
                i = 0
                a_min = pi
                for abs_d, d, p_name, p_idx, t in closest:
                    s1 = all_segs[p_name].segs[p_idx]
                    a = abs(abs(s1.delta_angle(s0)) - pi / 2)
                    if a < a_min:
                        a_min = a
                        c.d0, c.parent_name0, c.seg0, c.t = d, p_name, p_idx, t
                # use child segment (eg for wall projection)
                c.d1, c.parent_name1, c.seg1 = 0, name, seg_idx
            else:
                d, d1, p1, i1, t1 = closest[0][0:5]
                # try to exclude colinear segments
                i = 1
                s0 = all_segs[p1].segs[i1]
                d2, p2, i2 = closest[1][1:4]
                n_closest = len(closest) - 1
                a = abs(all_segs[p2].segs[i2].delta_angle(s0))
                while i < n_closest and (a > 3.1405 or a < 0.001):
                    if closest[i][0] < d:
                        d, d1, p1, i1, t1 = closest[i][0:5]
                        s0 = all_segs[p1].segs[i1]
                    i += 1
                    d2, p2, i2 = closest[i][1:4]
                    a = abs(all_segs[p2].segs[i2].delta_angle(s0))

                c.d0, c.parent_name0, c.seg0, c.t = d1, p1, i1, t1
                c.d1, c.parent_name1, c.seg1 = d2, p2, i2

                # logger.debug("Multi {} c:{} p0:{} w0:{} p1:{} w1:{} d0:{} d1:{}".format(
                #   name, c_idx, c.parent_name0, c.seg0, c.parent_name1, c.seg1, c.d0, c.d1
                # ))

        elif allow_single and len(closest) > 0:
            # use wall's endpoint when isolated with a t + dist rule
            closest.sort(key=lambda s: s[0])
            c = self.reloc_childs.add()
            c.child_name = name
            c.child_idx = c_idx
            c.t = closest[0][4]
            c.d0, c.parent_name0, c.seg0 = closest[0][1:4]
            # logger.debug("Single {} c:{} p0:{} w0:{} d0:{} t:{}".format(
            #    name, c_idx, c.parent_name0, c.seg0, c.d0, c.t
            #    ))
        # else:
        #    logger.debug("Skip", name, len(closest), pt, seg_idx)

    def find_closest_segments(self, name, tree, pt, maxdist):
        """ Find at least 2 closest segments of pt in tree
        :param name: child name
        :param tree: segments tree
        :param pt: point to seek
        :param maxdist: maxdist for seek
        :return: found, array of (idx of seg in tree._geoms, distance, t in seg)
        """
        closest = []
        count, selection = tree.intersects_pt(pt, maxdist)
        for idx in selection:
            p_name, p_idx, seg, side = tree._geoms[idx]
            if p_name != name:
                res, d, t = seg.point_sur_segment(pt)
                p0 = seg.lerp(min(1, max(0, t)))
                abs_d = (p0 - pt).length
                if abs_d < maxdist:
                    closest.append((idx, d, t))
        found = len(closest) > 1
        if found:
            closest.sort(key=lambda s: s[1])

        return found, closest

    def find_closest_segment(self, name, tree, pt, maxdist):
        """ Find closest segment of pt in tree
        :param name: child name
        :param tree: segments tree
        :param pt: point to seek
        :param maxdist: maxdist for seek
        :return: found, idx of seg in tree._geoms, distance, t in seg
        """
        dist = maxdist
        closest = -1
        t_closest = 0
        d_closest = 1e32
        count, selection = tree.intersects_pt(pt, maxdist)
        for idx in selection:
            p_name, p_idx, seg, side = tree._geoms[idx]
            if p_name != name:
                res, d, t = seg.point_sur_segment(pt)
                p0 = seg.lerp(min(1, max(0, t)))
                abs_d = (p0 - pt).length
                if abs_d < dist:
                    dist = abs_d
                    closest = idx
                    t_closest = t
                    d_closest = d
        return closest > -1, closest, d_closest, t_closest

    def find_closest_startpoint(self, name, tree, pt, maxdist):
        """ Find closest segment start point of pt in point_tree
        :param name: child name
        :param tree: segments start point tree
        :param pt: point to seek
        :param maxdist: maxdist for seek
        :return: found, idx in tree._geoms
        """
        dist = maxdist
        closest = -1
        count, selection = tree.intersects_pt(pt, maxdist)
        for idx in selection:
            p_name, p_idx, seg, side = tree._geoms[idx]
            if p_name != name:
                # match segments start points
                d = (seg.p0 - pt).length
                if d < dist:
                    dist = d
                    closest = idx

        return closest > -1, closest

    def add_single_relocate_child(self, tree, name, c_idx, idx):
        """ Relocate child witch rely on single segment of parent
        :param tree:
        :param name:
        :param c_idx:
        :param idx:
        :return:
        """
        c = self.reloc_childs.add()
        c.parent_name0, c.seg0, seg, c.side = tree._geoms[idx]
        c.child_name = name
        c.child_idx = c_idx
        return c

    def add_startpoint_child(self,
                       tree,
                       name,
                       c_idx,
                       s0,
                       maxdist):
        """
        :param tree: walls segments start point tree
        :param name: child name
        :param c_idx: child segment idx
        :param s0: segment
        :param maxdist:
        :return:
        """
        found, closest = self.find_closest_startpoint(name, tree, s0.p0, maxdist)
        if found:
            self.add_single_relocate_child(tree, name, c_idx, closest)
        else:
            logger_childs.debug("Add startpoint child %s c_idx:%s maxdist:%s failed", name, c_idx, maxdist)

        return found
    
    def add_single_segment_child(self,
                       tree,
                       name,
                       c_idx,
                       s0,
                       maxdist):
        """
        :param tree: walls segments tree
        :param name: child name
        :param c_idx: child segment idx
        :param s0: segment
        :param maxdist:
        :return:
        """
        found, closest, d, t = self.find_closest_segment(name, tree, s0.p0, maxdist)
        if found:
            c = self.add_single_relocate_child(tree, name, c_idx, closest)
            c.t = t
        else:
            logger_childs.debug("Add single segment child %s c_idx:%s maxdist:%s failed", name, c_idx, maxdist)

        return found
    
    def add_twall_child(self,
                       tree,
                       name,
                       c_idx,
                       g,
                       maxdist):
        """TODO: better detection of real t-child -> childs with both sides intersecting parent
        :param tree: walls segments tree
        :param name: child name
        :param c_idx: child segment idx (0 or last)
        :param s0: segment
        :param maxdist:
        :return:
        """
        if c_idx == 0:
            near = 'START'
        else:
            near = 'END'

        sx = g.axis.valid_segs[c_idx]
        # so = g.outside.valid_segs[c_idx]
        s0 = g.valid_segs[c_idx]

        it, t, p, closest = self._snap_wall_ext(sx, tree, maxdist * 1.5, near, {'inside', 'outside'}, name)
        if closest == -1:
            return

        # it, t, p, closest = self._snap_wall_ext(so, tree, maxdist, near, {'inside', 'outside'}, name)
        # if closest == -1:
        #    return

        # find if generator intersects other's seg
        it, t, p, closest = self._snap_wall_ext(s0, tree, maxdist, near, {'inside', 'outside'}, name)

        if it:
            c = self.reloc_walls.add()
            c.parent_name0, c.seg0, seg, c.side = tree._geoms[closest]
            c.child_name = name
            c.child_idx = c_idx
            c.parent_name1, c.seg1 = name, c_idx
        else:
            logger_childs.debug("Add twall child %s c_idx:%s maxdist:%s near:%s failed", name, c_idx, maxdist, near)

    def add_two_segments_child(self,
                               tree,
                               name,
                               c_idx,
                               s0,
                               maxdist
                               ):
        """
        :param tree: walls segments tree
        :param name: child name
        :param c_idx: child segment idx
        :param s0: segment
        :param maxdist:
        :return:
        """
        found, closest = self.find_closest_segments(name, tree, s0.p0, maxdist)
        if found:

            idx0, d0, t0 = closest.pop(0)
            p_name0, p_idx0, s1, side0 = tree._geoms[idx0]

            idx1, d1, t1 = closest.pop(0)
            p_name1, p_idx1, s2, side1 = tree._geoms[idx1]
            a = abs(s2.delta_angle(s1))

            # TODO: exclude collinear segments
            for idx, d, t in closest:
                p_name, p_idx, s2, side = tree._geoms[idx]

            c = self.add_single_relocate_child(tree, name, c_idx, idx0)
            c.d0, c.t = d0, t0
            c.d1, c.parent_name1, c.seg1 = d1, p_name1, p_idx1
        else:
            logger_childs.debug("Add two segment child %s c_idx:%s rule:%s maxdist:%s failed", name, c_idx, maxdist)

        return found

    def detect_openings(self, tree, name, tM, maxdist, flippable=True):
        """
         Find closest segment
         Store distance to segments, segment indexes
        """
        p = tM.translation.to_3d()

        found, closest, d, t = self.find_closest_segment(name, tree, p, maxdist)

        if found:
            p_name, p_idx, seg, side = tree._geoms[closest]
            flip = False
            if flippable:
                # door inside in y direction
                inside = -tM.col[1].to_3d()
                # wall inside
                n = seg.normal(t, -self.direction).v
                # 2 vecs in same direction -> length = 2 / opposite direction length = 0
                flip = (inside.normalized() + n).length > 0.5
                # print("front:", inside, " dir:", n, " l:", (inside.normalized() + n).length)

            m = self.childs_manipulators.add()
            m.type_key = 'DUMB_SIZE'
            # logger.debug("wall2.detect_openings() %s t:%s d:%s", name, t, d)
            return True, (
                name,
                p_idx,
                (t * seg.length, d, 0),
                flip,
                t)

        return False, ()

    def setup_childs(self, context, o, g=None, update_all=False, openings_only=False):
        """
            Store childs
            create manipulators
            call after a boolean oop
            generators use wall coordsys

            wall childs must evaluate on each run -> store on
            openings too (or might use a relocate rule)

            openings only: setup only openings for this wall to synch dimensions on hole delete
        """

        if not self.auto_synch or\
                o.parent is None:

            self.changed.reset()

            logger_childs.debug("archipack_wall2.setup_childs() %s  skip relocate_childs:%s childs:%s",
                     o.name,
                     len(self.reloc_childs),
                     len(self.childs))
            return {}

        logger.debug("archipack_wall2.setup_childs() %s  relocate_childs:%s childs:%s walls:%s",
                     o.name,
                     len(self.reloc_childs),
                     len(self.childs),
                     len(self.reloc_walls))

        tim = time.time()

        childs = self.find_relocate_childs(o)

        # logger.debug("archipack_wall2.setup_childs() %s  find_relocate_childs :%.4f seconds", o.name, (time.time() - tim))

        # retrieve objects to init quadtree
        objs = [o.parent]
        for k, cat in childs.items():
            objs.extend(cat.keys())

        if g is None:
            logger.debug("setup_childs: get_generator()")
            g = self.get_generator()

        # generators in wall coordsys
        itM = o.matrix_world.inverted()

        # init a quadtree to minimize intersections tests on setup
        coordsys = CoordSys(objs, itM=itM)

        # collect walls sides segs and openings points
        walls_generators = {}

        # print(coordsys)
        maxdist = self.width

        if update_all or openings_only:
            self.setup_openings(context, childs, coordsys, g, itM, maxdist, o, walls_generators)

        if openings_only:
            return childs

        # Order matter !
        # First update walls then other
        # as others depends on walls
        # so relocate_child order matter

        walls_generators[o.name] = g

        use_point_tree = "archipack_slab" in childs or\
                         "archipack_molding" in childs or\
                         "archipack_floor" in childs or\
                         "archipack_area" in childs
        use_wall_tree = use_point_tree or  "archipack_fence" in childs

        # logger.debug("archipack_wall2.setup_childs() %s  build tree phase 2 :%.4f seconds", o.name, (time.time() - tim))

        # Processing t childs walls
        self.reloc_walls.clear()

        # setup openings on other walls
        # get all walls segs so each wall is able to see all others
        if 'archipack_wall2' in childs:

            for c, cd in childs['archipack_wall2'].items():
                if c.name != o.name:
                    use_wall_tree = True
                    # also wont synch itself when disabled

                    # generator require valid manipulators
                    cd.setup_manipulators()
                    cg = cd.get_generator(itM @ c.matrix_world)

                    # process only openings on other walls
                    if update_all:
                        cd.setup_openings(context, childs, coordsys, cg, itM, maxdist, c, walls_generators)

                    walls_generators[c.name] = cg

            if use_wall_tree:

                wall_tree = Q_tree(coordsys, max_depth=8)
                logger_childs.debug("walls_generators %s", walls_generators.keys())
                # Init wall segs for walls
                for name, _g in walls_generators.items():
                    for i, seg in enumerate(_g.inside.valid_segs):
                        wall_tree.insert_seg(seg, (name, i, seg, 'inside'))
                    for i, seg in enumerate(_g.outside.valid_segs):
                        wall_tree.insert_seg(seg, (name, i, seg, 'outside'))

                self.setup_tchilds(childs, wall_tree, maxdist, o, walls_generators)

                # debugTree(context, coordsys, wall_tree)

        # only process further at setup time
        if not update_all:
            return childs

        self.reloc_childs.clear()

        # logger.debug("archipack_wall2.setup_childs() %s  processing subs :%.4f seconds", o.name, (time.time() - tim))

        # curved segments are not supported

        # for p in self.parts:
        #    if p.type == 1:
        #        return

        # logger.debug("archipack_wall2.setup_childs() %s  processing soft :%.4f seconds", o.name, (time.time() - tim))

        if use_point_tree:
            point_tree = Q_tree(coordsys, max_depth=8)
            for name, _g in walls_generators.items():
                for i, seg in enumerate(_g.outside.segs):
                    point_tree.insert_point(seg.p0, (name, i, seg, 'outside'))
                for i, seg in enumerate(_g.inside.segs):
                    point_tree.insert_point(seg.p0, (name, i, seg, 'inside'))

            # debugTree(context, coordsys, point_tree)

            # print("Point tree", point_tree)

        # floor and moulding
        for k, cat in childs.items():
            if k in (
                    "archipack_molding",
                    "archipack_floor",
                    "archipack_area"):
                for c, cd in cat.items():
                    cd.setup_manipulators()
                    cg = cd.get_generator(itM @ c.matrix_world)
                    # allow single segments to support inside walls ends
                    name = c.name
                    for c_idx, seg in enumerate(cg.segs):
                        # point in world coordsys
                        found = self.add_startpoint_child(
                            point_tree,
                            name,
                            c_idx,
                            seg,
                            maxdist)

                        found = True
                        if not found:
                            # probably found a curved segment
                            # TODO: match part of curved segment -> [start / end t]
                            self.add_single_segment_child(
                                wall_tree,
                                name,
                                c_idx,
                                seg,
                                maxdist)

        if 'archipack_slab' in childs:
            # Explicit rule for slab, using wall segs only with larger dist
            # might use same rule for roof boundary cutters
            for c, cd in childs['archipack_slab'].items():
                cd.setup_manipulators()
                cg = cd.get_generator(itM @ c.matrix_world)
                name = c.name
                for c_idx, seg in enumerate(cg.segs):
                    # point in world coordsys
                    found = self.add_startpoint_child(
                        point_tree,
                        name,
                        c_idx,
                        seg,
                        maxdist)
                    found = True
                    if not found:
                        self.add_two_segments_child(
                            wall_tree,
                            name,
                            c_idx,
                            seg,
                            1e32)

        # maxdist = 2 * self.width
        # NOTE:
        # cutters might move following parent
        # so we need to reloacate every points
        # in order to keep cutters location in synch
        # update cutters

        # Cutters are childs, and this relationship update only after
        # so we must update the cutter location by hand
        for k, cat in childs.items():
            if "cutter" in k:
                for c, cd in cat.items():
                    cd.setup_manipulators()
                    cg = cd.get_generator(itM @ c.matrix_world)
                    # allow single segments to support inside walls ends
                    for c_idx, seg in enumerate(cg.segs):
                        # point in world coordsys
                        self.add_two_segments_child(
                            wall_tree,
                            c.name,
                            c_idx,
                            seg,
                            maxdist
                            )

        logger_childs.debug("archipack_wall2.setup_childs() done %s reloc_childs:%s %.4f seconds\n", o.name, len(self.reloc_childs),(time.time() - tim))

        return childs

    def setup_openings(self, context, childs, coordsys, g, itM, maxdist, o, walls_generators):

        self.childs.clear()
        self.childs_manipulators.clear()
        # detect openings on this wall
        # require manipulators setup
        # each wall own her openings
        if "archipack_window" in childs or "archipack_door" in childs or "archipack_custom" in childs:

            wall_with_childs = [0] * (self.num_parts + 1)
            relocate = []

            # outside segs for windows and doors
            segs = g.outside.valid_segs

            # Init Tree for own openings
            sides_tree = Q_tree(coordsys, max_depth=8)
            for i, seg in enumerate(segs):
                sides_tree.insert_seg(seg, (o.name, i, seg, 'outside'))

            # logger.debug("archipack_wall2.setup_childs() %s  init tree :%.4f seconds", o.name, (time.time() - tim))
            w_min_z = o.matrix_world.translation.z - self.z_offset
            w_max_z = w_min_z + self.z

            for k, cat in childs.items():
                if k == "archipack_window":
                    for c, cd in cat.items():
                        tM = itM @ c.matrix_world

                        # min_z = c.matrix_world.translation.z + cd.altitude
                        # max_z = min_z + cd.z
                        # if max_z < w_min_z or min_z > w_max_z:
                        #    continue

                        res, reloc = self.detect_openings(sides_tree, c.name, tM, maxdist, flippable=False)
                        if res:
                            relocate.append(reloc)
                            wall_with_childs[reloc[1]] = 1
                        # add 2 segs on both ends of openings
                        if cd.altitude == 0:
                            cg = cd.get_generator(tM)
                            walls_generators[c.name] = cg
                            # all_segs[c.name] = cg.segs

                elif k in ("archipack_door", "archipack_custom"):
                    for c, cd in cat.items():
                        tM = itM @ c.matrix_world
                        """
                        if k == "archipack_door":
                            min_z = c.matrix_world.translation.z
                            max_z = min_z + cd.z
                            if max_z < w_min_z or min_z > w_max_z:
                                continue
                        """
                        res, reloc = self.detect_openings(sides_tree, c.name, tM, maxdist, flippable=True)
                        if res:
                            relocate.append(reloc)
                            wall_with_childs[reloc[1]] = 1
                            if k == "archipack_door":
                                # flip define material ids of hole
                                c['archipack_flip'] = reloc[3]
                                if cd.flip != reloc[3]:
                                    # self.select_object(context, c, True)
                                    cd.flip = reloc[3]
                                    cd.interactive_hole(context, c)
                                    # self.unselect_object(context, c)
                        # add 2 segs on both ends of openings
                        if "archipack_door" in c.data or cd.altitude == 0:
                            cg = cd.get_generator(tM)
                            walls_generators[c.name] = cg
                            # all_segs[c.name] = cg.segs

            # logger.debug("archipack_wall2.setup_childs() %s  process openings :%.4f seconds", o.name, (time.time() - tim))

            # sort openings along segments
            relocate.sort(key=lambda x: (x[1], x[4]))
            for child in relocate:
                name, wall_idx, pos, flip, t = child
                logger_childs.debug("Setup_childs %s opening %s", o.name, name)
                self.add_child(name, wall_idx, pos, flip)

            # logger.debug("archipack_wall2.setup_childs() %s  add childs :%.4f seconds", o.name, (time.time() - tim))

            # add a dumb size from last child to end of wall segment
            for i in range(sum(wall_with_childs)):
                m = self.childs_manipulators.add()
                m.type_key = 'DUMB_SIZE'

            # self.select_object(context, o, True)
            self.update_childs(context, o)

    def setup_tchilds(self, childs, wall_tree, maxdist, o, walls_generators):
        # debugTree(context, coordsys, wall_tree)
        # Project t childrens so they follow main without angle change
        # as we do use a projection at relocate time, only need to identify walls segs using axis here
        # require a limited space check and use t child seg projection over parent
        # so find closest parent seg
        for c, cd in childs['archipack_wall2'].items():
            if c.name != o.name and not cd.is_closed:
                # check t childs ends segment only
                g = walls_generators[c.name]
                self.add_twall_child(
                    wall_tree,
                    c.name,
                    0,
                    g,
                    maxdist)

                self.add_twall_child(
                    wall_tree,
                    c.name,
                    -1,
                    g,
                    maxdist)

    def add_generator(self, name, c, d, generators, itM, force=False):
        if name != "" and c is not None and d is not None and (name not in generators or force):
            generators[name] = [
                c,
                d,
                d.get_generator(itM @ c.matrix_world),
                False
                ]

    def post_relocate(self, context):
        o = self.find_in_selection(context)
        if o is not None:
            logger_childs.debug("archipack_wall2.post_relocate() %s", o.name)
            self.relocate_childs(context, o)
            # update wall's finish
            if self.finish_enable and len(self.finish) > 0:
                self.update(context)
            self.restore_auto_manipulate(context)

    def get_or_update_generator(self, itM, generators, name):
        """
        return clean generator, get new one when dirty
        :param generators:
        :param name:
        :return:
        """
        c, d, g, dirty = generators[name]

        if dirty:
            # c.matrix_world.translation = c.location
            self.add_generator(name, c, d, generators, itM, force=True)
            g = generators[name][2]

        return c, d, g

    def add_relocate_generator(self, context, itM, child, soft, child_names, generators):
        name = child.child_name
        if name not in soft:
            c, d = child.get_child(context)
            if c is None:
                return
            child_names.append(name)
            soft.add(name)
            self.add_generator(name, c, d, generators, itM)

        # soft[name].append(child)

        c, d = child.get_parent0(context)
        self.add_generator(child.parent_name0, c, d, generators, itM)

        # parent_name1 could be empty
        c, d = child.get_parent1(context)
        self.add_generator(child.parent_name1, c, d, generators, itM)

    def bend_window(self, d, child, g):
        _so = g.outside.segs[child.wall_idx]
        _p = _so.lerp(child.pos.x / _so.length)
        _si = g.inside.segs[child.wall_idx]
        res, _, ti0 = _si.point_sur_segment(_p)
        wx = 0.5 * d.x
        bend = False
        was_bent = len(d.bend) > 0
        d.bend.clear()
        closed = g.closed
        n_segs = g.numsegs

        # location of pivot point in y axis
        y0 = 0.5 * d.y
        if d.window_type == 'FLAT':
            y = d.hole_center_y + 0.5 * d.frame_y
            y2 = y0 - (y + d.panel_y)
        else:
            y = d.hole_center_y + d.rail_frame_depth
            y2 = y0 - (y + 2 * d.panel_y)

        y1 = y0 + y

        if child.pos.x < wx:
            # negative side
            _so0 = _so
            wall_idx = child.wall_idx - 1
            x = min(ti0 * _si.length, child.pos.x)
            while x < wx:
                if wall_idx < 0:
                    if not closed:
                        break
                    wall_idx = n_segs - 1

                b = d.bend.add()
                _si1 = g.inside.segs[wall_idx]
                _so1 = g.outside.segs[wall_idx]

                b.a = _so0.delta_angle(_si1)
                if b.a < 0:
                    x2 = y1 * tan(0.5 * -b.a)
                else:
                    x2 = y2 * tan(0.5 * b.a)

                if x < 0:
                    x2 = -x2

                x += x2
                #       from axis
                #          0
                #  wx |    |    x
                b.x = -x
                # b.a determine le segment utilise comme point de depart
                # le pivot est toujours au plus court de deux
                res, _, ti = _si1.point_sur_segment(_so1.p0)
                res, _, to = _so1.point_sur_segment(_si1.p0)
                x += min([_si1.length, _si1.length - ti * _si1.length, _so1.length, _so1.length - to * _so1.length])
                x += x2

                bend = True
                _so0 = _so1
                wall_idx -= 1

        wall_idx = child.wall_idx + 1
        if child.pos.x + wx > _so.length:
            _si0 = _si

            x = min(_si.length - (ti0 * _si.length), _so.length - child.pos.x)
            while x < wx:
                if wall_idx >= n_segs:
                    if not closed:
                        break
                    wall_idx = 0

                b = d.bend.add()
                _si1 = g.inside.segs[wall_idx]
                _so1 = g.outside.segs[wall_idx]
                b.a = _so1.delta_angle(_si0)

                if b.a < 0:
                    x2 = y1 * tan(0.5 * -b.a)
                else:
                    x2 = y2 * tan(0.5 * b.a)

                if x < 0:
                    x2 = -x2

                x += x2
                # b.a = g.segs[child.wall_idx + 1].delta_angle(_si)
                # from axis
                #   s.len
                #  x |  wx
                b.x = x
                res, _, ti = _si1.point_sur_segment(_so1.p1)
                res, _, to = _so1.point_sur_segment(_si1.p1)
                x += min([_si1.length,  _so1.length, ti * _si1.length, to * _so1.length])
                x += x2

                bend = True
                _si0 = _si1
                wall_idx += 1

        if was_bent and len(d.bend) == 0:
            bend = True

        return bend

    def relocate_childs(self, context, o, g=None):
        """
            Move and resize childs after wall edition
            childs here are either doors or windows
            Unlike other routines,
            generators use world coordsys
            T childs walls only update doors and windows
            rely on stored childs
        """
        tim = time.time()
        # throttle enable rules
        do_throttle_child = False #len(self.childs) > 50
        do_throttle_reloc = False #len(self.reloc_childs) > 50

        if not self.auto_synch or (len(self.childs) + len(self.reloc_childs) + len(self.reloc_walls)) == 0:
            logger_childs.debug("archipack_wall2.relocate_childs() %s nothig to do", o.name)
            return

        # throttle relocation of childs
        if do_throttle_child or do_throttle_reloc:
            throttle.add(context, o, self, 2.0, update_func="post_relocate")
            throttle.stack[o.name].update_func = "post_relocate"

        # only throttle openings when number of childs > 10
        if do_throttle_child and throttle.is_active(o.name):
            logger_childs.debug("archipack_wall2.relocate_childs() %s throttle childs(%s)", len(self.childs), o.name)
            return

        logger_childs.debug("\n*****************************************\n")
        logger_childs.debug("archipack_wall2.relocate_childs() %s start", o.name)

        if g is None:
            g = self.get_generator()

        # Update translation from location by hand before scene.update()
        # o.matrix_world.translation = o.location
        tM = o.matrix_world
        itM = tM.inverted()

        # Generators: object, datablock, generator, mat world inverse, dirty flag
        generators = {}

        finish_mat = []
        finish_thick = []

        if  len(self.finish) < 1:
            finish_names = {}
            i = 0
            for finish in self.finish2:
                _n = finish.finish_name
                if _n not in finish_names:
                    finish_mat.append(finish.idmat[0])
                    finish_thick.append(finish.thickness + finish.offset_z)
                    finish_names[_n] = i
                    i += 1
                else:
                    idx = finish_names[_n]
                    finish_thick[idx] = max(finish_thick[idx], finish.thickness + finish.offset_z)
        else:
            finish_names = set()
            for finish in self.finish:
                _n = finish.finish_name
                if _n not in finish_names:

                    finish_mat.append(finish.idmat[0])
                    finish_thick.append(finish.thickness)
                finish_names.add(_n)
        # support for No finishing
        finish_mat.append(0)
        finish_thick.append(0)

        # relocate openings
        for child in self.childs:

            c, d = child.get_child(context)

            if c is None:
                continue

            # logger.debug("Relocate_opening %s %s", o.name, c.name)

            seg = g.outside.segs[child.wall_idx]
            t = child.pos.x / seg.length

            # inside
            n = seg.normal(t, -self.direction)
            side = 1

            if child.flip:
                side = -side

            # x axis is segment direction
            xx, xy, z = side * n.cross(n.v)
            # y look inside
            yx, yy, z = side * n.v

            if d is not None:
                _seg = g.segs[child.wall_idx]

                bend = False
                if "archipack_window" in c.data:
                    bend = self.bend_window(d, child, g)

                # -1 as finish 0 is None and finishes index are 1 based
                mat_finish_inside = finish_mat[_seg.finish_inside - 1]
                mat_finish_outside = finish_mat[_seg.finish_outside - 1]
                finishing_int = finish_thick[_seg.finish_inside - 1]
                finishing_out = finish_thick[_seg.finish_outside - 1]
                # update material index, side and width of objects
                if (d.y != self.width or
                    ((hasattr(d, 'auto_mat') and d.auto_mat) and (
                        d.mat_hole_inside != _seg.material_inside or
                        d.mat_hole_outside != _seg.material_outside or
                        d.mat_finish_inside != mat_finish_inside or
                        d.mat_finish_outside != mat_finish_outside)
                    ) or
                    (hasattr(d, 'finishing_int') and d.finishing_int != finishing_int) or
                    (hasattr(d, 'finishing_out') and d.finishing_out != finishing_out)):

                    self.select_object(context, c, True)
                    d.auto_update = False
                    update_hole = False

                    if hasattr(d, 'auto_mat') and d.auto_mat:

                        if d.mat_hole_inside != _seg.material_inside:
                            update_hole = True
                            d.mat_hole_inside = _seg.material_inside

                        if d.mat_hole_outside != _seg.material_outside:
                            update_hole = True
                            d.mat_hole_outside = _seg.material_outside

                        if d.mat_finish_inside != mat_finish_inside:
                            update_hole = True
                            d.mat_finish_inside = mat_finish_inside

                        if d.mat_finish_outside != mat_finish_outside:
                            update_hole = True
                            d.mat_finish_outside = mat_finish_outside

                    if hasattr(d, 'finishing_int') and d.finishing_int != finishing_int:
                        update_hole = True
                        d.finishing_int = finishing_int

                    if hasattr(d, 'finishing_out') and d.finishing_out != finishing_out:
                        update_hole = True
                        d.finishing_out = finishing_out

                    if update_hole:
                        d.interactive_hole(context, c)

                    d.y = self.width
                    d.auto_update = True

                    self.unselect_object(context, c)

                elif bend:
                    self.select_object(context, c, True)
                    d.update(context)
                    self.unselect_object(context, c)

                offset = 0.5 * self.width

            else:
                offset = child.pos.y

            x, y, z = n.lerp(offset)

            self.select_object(context, o, True)
            wM = tM @ g.outside.matrix_about_segment(child.wall_idx, t, offset, -self.direction, child.pos.z, side)
            # preTranslate
            """
            wM = tM @ Matrix([
                [xx, yx, 0, x],
                [xy, yy, 0, y],
                [0, 0, 1, child.pos.z],
                [0, 0, 0, 1]
            ])
            """
            # if c.parent:
            #    c.location = c.parent.matrix_world.inverted() @ wM.translation
            c.matrix_world = wM

            if d is not None:
                self.add_generator(c.name, c, d, generators, itM)

        # logger.debug("archipack_wall2.relocate_childs() openings %s :%.4f seconds", len(self.childs), time.time() - tim)

        # Throttle relocation of reloc_childs
        if do_throttle_reloc and throttle.is_active(o.name):
            # adjust throttle timer
            # throttle.add(context, o, self, 2.0 * (time.time() - tim), update_func="post_relocate")
            logger_childs.debug("archipack_wall2.relocate_childs() throttle reloc (%s) :%.4f seconds", len(self.reloc_childs), time.time() - tim)
            return

        # build a dict for each child
        soft = set()

        # process childs in the right order
        child_names = []
        for child in self.reloc_walls:
            self.add_relocate_generator(context, itM, child, soft, child_names, generators)

        for child in self.reloc_childs:
            self.add_relocate_generator(context, itM, child, soft, child_names, generators)

        # logger.debug("archipack_wall2.relocate_childs() generators(%s) :%.4f seconds", len(generators), time.time() - tim)

        # for name, item in generators.items():
        #    c, d, cg, dirty = item
        #    cg.as_curve(context, name=name)

        n_changes = 0

        for name in child_names:

            # XXX blender's BUG here ? child as dict sometimes fails to retrieve right data ?????
            childs = [rc for rc in self.reloc_walls if rc.child_name == name]
            childs.extend([rc for rc in self.reloc_childs if rc.child_name == name])

            # logger.debug("Relocate_childs start :%.2f seconds", time.time() - tim)
            # logger.debug("Relocate_childs %s child:%s", o.name, name)
            c, d, cg = self.get_or_update_generator(itM, generators, name)

            # apply changes to generator
            n_segs = cg.numsegs
            changed = False
            for cd in childs:

                _line = self.get_or_update_generator(itM, generators, cd.parent_name0)[2]

                # find closest segments: use sides of main wall for t childs
                if cd.side != "":
                    _line = getattr(_line, cd.side)

                if cd.seg0 >= len(_line.segs):
                    print("Error in", cd.parent_name0, "invalid index", cd.seg0, name)
                    continue

                s0 = _line.segs[cd.seg0]

                if cd.parent_name1 == "":
                    # Single point rule molding or floor
                    # p in world coordsys
                    if cd.t > 0 and hasattr(s0, "_r"):
                        # map to arc using t param on arc
                        # TODO: handle partial arc remapping
                        p = s0.lerp(cd.t)
                    else:
                        p = s0.p0
                else:
                    # Two segments intersection rule
                    # T childs use themself as 2nd segment without offset

                    _line = self.get_or_update_generator(itM, generators, cd.parent_name1)[2]
                    s1 = _line.segs[cd.seg1]

                    if cd.d0 != 0:
                        s0 = s0.offset(cd.d0)

                    if cd.d1 != 0:
                        s1 = s1.offset(cd.d1)

                    # p in world coordsys
                    res, p, u, v = s0.intersect_ext(s1)

                    # XXX doesn't work for 2 segs rule !!
                    if u > 1 or u < 0:
                        logger_childs.debug("intersect fails:%s u:%s v:%s", name, u, v)
                        p = 0

                # if intersection fails p = 0
                if p != 0:
                    if cd.child_idx < len(cg.segs):
                        _seg = cg.segs[cd.child_idx]
                        if (_seg.p0 - p).length > 0.001:
                            n_changes += 1
                            changed = True

                        if _seg.has_last and hasattr(_seg._last, "_r"):
                            _seg._last.p1 = p
                        else:
                            _seg.p0 = p

                        if cd.child_idx == 0:
                            _p = tM @ p
                            d.move_object(c, _p)
                    else:
                        print(name, " child_idx mismatch :", cd.child_idx, ">", len(cg.segs))
                        print(cd)
                    """
                    else:
                        # !!
                        if (cg.segs[cd.child_idx - 1].p1 - p).length > 0.001:
                            n_changes += 1
                            changed = True
                        cg.segs[cd.child_idx - 1].p1 = p
                    """
                logger_childs.debug("%s changed:%s p:%s cd: %s", name, changed, p, cd)

            if changed:
                logger_childs.debug("archipack_wall2.relocate_childs() update %s :%.4f seconds", name, time.time() - tim)

                # update data from generator
                cg.update_parts(d)

                # logger.debug("Relocate_childs change :%.2f seconds", time.time() - tim)

                # throttle.add(context, c, d, duration=0.01, update_func="update")
                # self.select_object(context, c, True)
                with ensure_select_and_restore(context, c, [c]):
                    d.update(context)

                # flag generator as dirty
                generators[name][3] = True
                # logger.debug("Relocate_childs update :%.2f seconds", time.time() - tim)
                # self.unselect_object(context, c)

        self.select_object(context, o, True)

        logger_childs.debug("archipack_wall2.relocate_childs() done %s (%s of %s) :%.4f seconds",
                     o.name,
                     n_changes,
                     len(self.reloc_childs) + len(self.reloc_walls),
                     time.time() - tim)

    def update_childs(self, context, o, g=None):
        """
            setup gl points for childs
        """
        # print("update_childs")

        if o.parent is None:
            return

        if g is None:
            g = self.get_generator()

        # swap manipulators so they always face outside
        manip_side = self.direction * 0.5

        n_childs = len(self.childs_manipulators)

        itM = o.matrix_world.inverted()
        m_idx = 0
        for wall_idx, wall in enumerate(g.outside.valid_segs):
            p0 = wall.lerp(0)
            wall_has_childs = False
            for child in self.childs:
                if child.wall_idx == wall_idx:
                    c, d = child.get_child(context)
                    if d is not None:

                        # child is either a window or a door
                        wall_has_childs = True
                        dt = 0.5 * d.x / wall.length
                        pt = itM @ c.matrix_world.translation
                        res, y, t = wall.point_sur_segment(pt)
                        # child.pos = (wall.length * t, y, child.pos.z)
                        p1 = wall.lerp(t - dt)

                        if m_idx < n_childs:
                            # dumb size between childs, might be < m_idx when manipulating across segments
                            self.childs_manipulators[m_idx].set_pts([
                                (p0.x, p0.y, 0),
                                (p1.x, p1.y, 0),
                                (manip_side, 0, 0)])

                        m_idx += 1

                        side = self.direction
                        if child.flip:
                            side = -1
                        #
                        x, y = 0.5 * d.x, side * 0.5 * d.y * self.x_offset + self.offset

                        # delta loc
                        child.manipulators[0].set_pts([
                            (-x, y, 0),
                            (x, y, 0),
                            (manip_side * side, 0, 0)])

                        # loc size
                        child.manipulators[1].set_pts([
                            (-x, y, 0),
                            (x, y, 0),
                            (manip_side * side, 0, 0)])

                        p0 = wall.lerp(t + dt)

            p1 = wall.lerp(1)
            if wall_has_childs and m_idx < n_childs:
                # dub size after all childs
                self.childs_manipulators[m_idx].set_pts([
                    (p0.x, p0.y, 0),
                    (p1.x, p1.y, 0),
                    (manip_side, 0, 0)])
                m_idx += 1

    def remove_dimension(self, context, o):
        with stop_auto_manipulate(context):
            o.select_set(state=True)
            if bpy.ops.archipack.dimension_auto.poll():
                bpy.ops.archipack.dimension_auto(mode='DELETE')

    def update_dimension(self, context, o, g, remove_object=None):

        # swap manipulators so they always face outside
        # dims = [child
        #    for child in o.children
        #    if child.data and "archipack_dimension_auto" in child.data
        #    ]

        dims = {child.data.archipack_dimension_auto[0].parent_uid: child
            for child in o.children
            if child.data and "archipack_dimension_auto" in child.data
            }

        has_dims = len(dims) > 0

        if self.dimensions:
            with stop_auto_manipulate(context):
                # pts = [seg.p0 for seg in g.segs]
                # is_cw = self.is_cw(pts)

                _parts = self.parts

                # add right side measure points
                _segs = g.outside.valid_segs

                force_update = False
                for wall_idx, wall in enumerate(_segs):

                    parent_uid = _parts[wall_idx].uid

                    if parent_uid in dims:
                        dim = dims[parent_uid]
                        del dims[parent_uid]

                    elif not has_dims:
                        # only add when wall has not dims
                        ct = ArchipackCreateTool()
                        dim = ARCHIPACK_OT_dimension_auto.create(ct, context, 1, True)
                        dim.parent = o
                        dim.matrix_world = o.matrix_world.copy()

                    else:
                        # skip dim removed by user for this seg
                        continue

                    dim.location = wall.p0

                    rot_z = wall.v_angle

                    dim.rotation_euler.z = rot_z

                    # force dim matrix_world update
                    if parent_uid not in dims:
                        force_update = True

                    dim_d = dim.data.archipack_dimension_auto[0]
                    dim_d.parent_uid = parent_uid

                    # XXX Maybe not so wise
                    # dim_d.clear_sources()
                    dim_d.add_source(o, _parts[wall_idx].uid, 'WALL2')

                    if wall_idx + 1 < self.num_parts:
                        dim_d.add_source(o, _parts[wall_idx + 1].uid, 'WALL2')
                    else:
                        dim_d.add_source(o, _parts[0].uid, 'WALL2')

                    # add window providers
                    for child in self.childs:
                        if child.wall_idx == wall_idx:
                            c, d = child.get_child(context)
                            if d is not None:
                                # child is either a window or a door
                                if "archipack_window" in c.data:
                                    provider_type = 'WINDOW'
                                elif "archipack_door" in c.data:
                                    provider_type = 'DOOR'
                                elif "archipack_custom" in c.data:
                                    provider_type = 'CUSTOM'
                                dim_d.add_source(c, 0, provider_type)
                                dim_d.add_source(c, 1, provider_type)

                    self.select_object(context, dim, True)
                    dim_d.update(context, remove_object=remove_object)
                    self.unselect_object(context, dim)

                self.select_object(context, o, True)

                if force_update:
                    context.view_layer.update()

        for dim in dims.values():
            self.delete_object(context, dim)

    def synch_dimension(self, context, o, remove_object=None):
        """
          Synch dimension when manipulating window or doors
        """
        if self.dimensions:
            # self.update_parts()
            g = self.get_generator()
            # prevent cyclic call
            self.update_dimension(context, o, g, remove_object)

    def manipulate_childs(self, context):
        """
            setup child manipulators
        """
        # print("manipulate_childs")
        n_parts = self.num_parts

        for wall_idx in range(n_parts):
            for child in self.childs:
                if child.wall_idx == wall_idx:
                    c, d = child.get_child(context)
                    if d is not None:
                        # delta loc
                        self.manip_stack.append(child.manipulators[0].setup(context, c, d, self.manipulate_callback,
                                                                            start_callback=self.manipulate_on_start,
                                                                            end_callback=self.manipulate_on_end))
                        # loc size
                        self.manip_stack.append(child.manipulators[1].setup(context, c, d, self.manipulate_callback,
                                                                            start_callback=self.manipulate_on_start,
                                                                            end_callback=self.manipulate_on_end))

    def manipulate_callback(self, context, o=None, manipulator=None, sp=None):
        found = False
        if (archipack_wall2.datablock(o) == self):
            self.select_object(context, o, True)
            found = True
        if not found and o.parent is not None:
            for c in o.parent.children:
                if (archipack_wall2.datablock(c) == self):
                    self.select_object(context, c, True)
                    found = True
                    break
        if found:
            self.manipulable_sp_manipulate(context, manipulator=manipulator, sp=sp)

    # temp store start segment index when manipulating location
    manipulation_data_store = None

    def manipulate_on_end(self, context, event):
        self.manipulation_data_store = None

    def manipulate_on_start(self, context, event):
        # start callback, disable finishings when manipulating openings
        self.manipulation_data_store = None
        if self.finish_enable:
            o = self.find_in_selection(context)
            if o is not None:

                must_update = False
                # print("manipulate_on_start", o.name)
                if not throttle.is_active(o.name):
                    throttle.add(context, o, self)
                    must_update = True
                # restore so auto_manipulate state is the right one
                self.restore_context(context)
                if must_update:
                    self.update(context, update_childs=False)

    def manipulable_sp_manipulate(self, context, event=None, manipulator=None, sp=None):
        type_name = type(manipulator).__name__
        # print("manipulable_manipulate %s" % (type_name))
        if type_name in [
                'DeltaLocationManipulator',
                'SizeLocationManipulator',
                'SnapSizeLocationManipulator'
                ]:
            # update manipulators pos of childs
            o = context.object
            if o.parent is None:
                return

            tim = time.time()

            if self.finish_enable:
                throttle.add(context, o, self)

            g = self.get_generator()

            itM = o.matrix_world.inverted()
            for child in self.childs:
                c, d = child.get_child(context)
                # only update manipulated object
                if d is not None and c.name == manipulator.o.name:
                    # allow to move across segments
                    if type_name == 'DeltaLocationManipulator':
                        pt = itM @ sp.placeloc
                        # store wall_idx of child when starting as sp is relative to this segment
                        if self.manipulation_data_store is None:
                            self.manipulation_data_store = child.wall_idx

                        child.wall_idx = self.manipulation_data_store
                    else:
                        pt = itM @ c.matrix_world.translation

                    wall = g.outside.segs[child.wall_idx]

                    res, dist, t = wall.point_sur_segment(pt)

                    if t > 1:
                        while t > 1:
                            print("t", t, "idx", child.wall_idx)
                            child.wall_idx += 1
                            if child.wall_idx >= g.numsegs:
                                if not g.closed:
                                    child.wall_idx = g.numsegs - 1
                                    t = 1
                                    break
                                child.wall_idx = 0

                            overflow = (t - 1) * wall.length
                            wall = g.outside.segs[child.wall_idx]
                            t = overflow / wall.length

                    else:
                        while t < 0:
                            print("t", t, "idx", child.wall_idx)
                            child.wall_idx -= 1
                            if child.wall_idx < 0:
                                if not g.closed:
                                    child.wall_idx = 0
                                    t = 0
                                    break
                                child.wall_idx = g.numsegs - 1
                            overflow = t * wall.length
                            wall = g.outside.segs[child.wall_idx]
                            t = 1 + (overflow / wall.length)

                    child.pos = (t * wall.length, dist, child.pos.z)

            # NOTE: object doesnt update itself
            # explicit relocate
            self.relocate_childs(context, o, g)
            # update childs manipulators
            self.update_childs(context, o, g)
            self.update_dimensions(context, o)
            logger.debug("manipulable_sp_manipulate %.4f" % (time.time() - tim))

    def manipulable_move_t_part(self, context, o=None, manipulator=None):
        """
            Callback for t_parts childs
        """
        type_name = type(manipulator).__name__
        # print("manipulable_manipulate %s" % (type_name))
        if type_name in [
                'DeltaLocationManipulator'
                ]:
            # update manipulators pos of childs
            if archipack_wall2.datablock(o) != self:
                return
            # update childs
            self.relocate_childs(context, o)

    def manipulable_release(self, context):
        """
            Override with action to do on mouse release
            eg: big update
        """
        return

    def manipulable_setup(self, context, o):

        # setup childs
        g = self.get_generator()
        # we do need childs when manipulating
        # and for dimensions
        self.setup_childs(context, o, g=g, update_all=True)
        # store gl points
        self.update_childs(context, o, g)
        # setup childs manipulators
        self.manipulate_childs(context)
        self.setup_manipulators()
        self.manipulable_setup_parts(context, o)

        # width + counter
        for i, m in enumerate(self.manipulators):
            # skip counter on closed walls
            if i == 1 and self.is_closed:
                continue
            elif i == 3:
                # t child location along parent
                self.manip_stack.append(m.setup(context, o, self, self.manipulable_move_t_part))
            else:
                self.manip_stack.append(m.setup(context, o, self))

        # dumb between childs
        for m in self.childs_manipulators:
            self.manip_stack.append(m.setup(context, o, self))

    def manipulable_exit(self, o):
        """
            Override with action to do when modal exit
        """
        # self.childs.clear()
        return

    def find_roof(self, o):
        p = self.get_topmost_parent(o)
        x0, y0, z0, x1, y1, z1 = ArchipackBoolManager._world_bounding_box(o)
        center = 0.5 * Vector((x0 + x1, y0 + y1, z0, z1))
        for c in p.children:
            x0, y0, z0, x1, y1, z1 = ArchipackBoolManager._world_bounding_box(c)
            if x0 < center.x < x1 and y0 < center.y < y1:
                if c.data is not None and "archipack_roof" in c.data:
                    return c, c.data.archipack_roof[0]
        return None, None

    def find_roofs(self, context, o):
        p = self.get_topmost_parent(o)
        xmin, ymin, z0, xmax, ymax, z1 = ArchipackBoolManager._world_bounding_box(o)

        roofs = []
        for c in p.children:
            if c.data is not None and "archipack_roof" in c.data:
                x0, y0, z0, x1, y1, z1 = ArchipackBoolManager._world_bounding_box(c)
                if x0 > xmax or x1 < xmin or y0 > ymax or y1 < ymin:
                    continue
                roofs.append(c)
                """
                # find skylight too, skip shrinkwrap targets
                for s in c.children:
                    if s.data is not None and "archipack_roof" in s.data and \
                            not s.data.archipack_roof[0].schrinkwrap_target:
                        x0, y0, z0, x1, y1, z1 = ArchipackBoolManager._world_bounding_box(s)
                        if x0 > xmax or x1 < xmin or y0 > ymax or y1 < ymin:
                            continue
                        roofs.append(s)
                """
        # fallback to scene objects, slow but might help
        if len(roofs) == 0:
            for c in self.get_scene_objects(context):
                if c.data is not None and "archipack_roof" in c.data:
                    x0, y0, z0, x1, y1, z1 = ArchipackBoolManager._world_bounding_box(c)
                    if x0 > xmax or x1 < xmin or y0 > ymax or y1 < ymin:
                        continue
                    roofs.append(c)

        return roofs

    def _snap_wall_ext(self, s0, tree, maxdist, near='END', whitelist={'inside', 'outside', 'axis'}, exclude=""):

        closest = -1

        if near == 'END':
            pt = s0.p1
            ref_t = 100
            t = 1
        else:
            pt = s0.p0
            ref_t = -100
            t = 0
        p0 = pt
        count, selection = tree.intersects_pt(pt, maxdist)
        # for parent_idx, seg in enumerate(segs):
        for i in selection:
            name, idx, seg, _type = tree._geoms[i]
            if _type in whitelist and name != exclude:
                res, p, u, v = s0.intersect_ext(seg)

                # Collinear segments not crossing (d > 0.001)
                if res is None:
                    continue

                # allow 1 mm to fix precision issues -> disallow extend !!
                dv = 0.001 / seg.length
                # allow extend when angle > 0
                # a = s0.delta_angle(seg)
                # if a > 0:
                #    dv = maxdist / seg.length
                if 1 + dv > v > -dv and (p0 - p).length < maxdist:
                    # intersects into seg interval
                    if near == 'END':
                        # u minimum in interval [0:100]
                        if ref_t > u > 0:
                            ref_t = u
                            t = u
                            pt = p
                            closest = i
                    else:
                        # u maximum in interval [-100:1]
                        # print(name, " idx:", idx, " type:", _type, " u:", u, " ref_t:",ref_t)
                        if 1 > u > ref_t:
                            ref_t = u
                            t = u
                            pt = p
                            closest = i

        return closest > -1, t, pt, closest

    def find_walls(self, o):
        res = {}
        if o.parent:
            res = {c: archipack_wall2.datablock(c)
                    for c in o.parent.children
                    if archipack_wall2.filter(c) and c.name != o.name
                   }
        return res
    
    def snap_baseline(self, o, g):
        # Find intersections of this wall ends and other ones
        # setup base line start and end t for segs 0 and -1
        # g in wall coordsys
        if self.is_closed or not (self.snap_wall_end or self.snap_wall_start):
            return

        logger.debug("archipack_wall2.snap_baseline() %s", o.name)

        maxdist = 2.0 * self.width

        walls = self.find_walls(o)
        itM = o.matrix_world.inverted()

        if len(g.segs) > 0 and len(walls) > 0:
            coordsys = CoordSys(walls.keys(), itM=itM)
            tree = Q_tree(coordsys, max_depth=8)
            # build neighboors tree
            for w, d in walls.items():
                name = w.name
                cg = d.get_generator(itM @ w.matrix_world)
                for i, seg in enumerate(cg.outside.valid_segs):
                    tree.insert_seg(seg, (name, i, seg, "outside"))
                for i, seg in enumerate(cg.inside.valid_segs):
                    tree.insert_seg(seg, (name, i, seg, "inside"))

            if self.snap_wall_start:
                s0 = g.segs[0]
                it, t, p, idx = self._snap_wall_ext(s0, tree, maxdist, near='START', whitelist={'outside', 'inside'})
                if it:
                    s0.p0 = p

            if self.snap_wall_end:
                s0 = g.segs[-2]
                it, t, p, idx = self._snap_wall_ext(s0, tree, maxdist, near='END', whitelist={'outside', 'inside'})
                if it:
                    s0.p1 = p

    def snap_wall_sides(self, o):
        # Find intersections of this wall ends and other ones
        # setup axis, left and right start and end t for segs 0 and -1
        # Use a standalone generator in world coordsys

        self.extremes[0:6] = (0.0, 1.0, 0.0, 1.0, 0.0, 1.0)

        if self.is_closed or not (self.snap_wall_start or self.snap_wall_end):
            return

        maxdist = 2.0 * self.width

        # get all but this wall
        walls = self.find_walls(o)
        tim = time.time()
        if len(self.parts) > 0 and len(walls) > 0:

            coordsys = CoordSys(walls.keys())
            g = self.get_generator(coordsys.invert @ o.matrix_world)
            t = time.time()
            tree = Q_tree(coordsys, max_depth=8)

            # build tree
            for w, d in walls.items():
                name = w.name
                cg = d.get_generator(coordsys.invert @ w.matrix_world)
                for i, seg in enumerate(cg.axis.valid_segs):
                    tree.insert_seg(seg, (name, i, seg, "axis"))
                for i, seg in enumerate(cg.outside.valid_segs):
                    tree.insert_seg(seg, (name, i, seg, "outside"))
                for i, seg in enumerate(cg.inside.valid_segs):
                    tree.insert_seg(seg, (name, i, seg, "inside"))

            logger.debug("snap_wall build tree %.2f", (time.time() - t))
            t = time.time()
            if self.snap_wall_start:
                s0 = g.axis.segs[0]
                it, maxi0, p, idx = self._snap_wall_ext(s0, tree, maxdist, near='START', whitelist={'axis'})
                if it:
                    s0 = g.outside.segs[0]
                    it, maxi1, p, idx = self._snap_wall_ext(s0, tree, maxdist, near='START', whitelist={'outside', 'inside'})
                    if it:
                        s0 = g.inside.segs[0]
                        it, maxi2, p, idx = self._snap_wall_ext(s0, tree, maxdist, near='START', whitelist={'outside', 'inside'})
                        if it:
                            self.extremes[0] = maxi0
                            self.extremes[2] = maxi1
                            self.extremes[4] = maxi2
            logger.debug("snap_wall start %.2f", (time.time() - t))
            t = time.time()
            if self.snap_wall_end:
                s0 = g.axis.segs[-2]
                it, mini0, p, idx = self._snap_wall_ext(s0, tree, maxdist, near='END', whitelist={'axis'})
                if it:
                    s0 = g.outside.segs[-2]
                    it, mini1, p, idx = self._snap_wall_ext(s0, tree, maxdist, near='END', whitelist={'outside', 'inside'})
                    if it:
                        s0 = g.inside.segs[-2]
                        it, mini2, p, idx = self._snap_wall_ext(s0, tree, maxdist, near='END', whitelist={'outside', 'inside'})
                        if it:
                            self.extremes[1] = mini0
                            self.extremes[3] = mini1
                            self.extremes[5] = mini2
            logger.debug("snap_wall end %.2f", (time.time() - t))

        logger.debug("archipack_wall2.snap_wall_sides() %s %s %.2f", o.name, self.extremes[:], time.time() - tim)

    def filter_geom_using_mod2_rule(self, _geoms):
        """
        Filter nested geoms using a mod2 rule
        :param _geoms:
        :return:
        """
        _geoms.sort(key=lambda x: x.exterior_area)

        n_polys = len(_geoms)
        parents = [-1] * n_polys
        to_remove = []
        # filter polygon when inside another one
        for i, poly in enumerate(_geoms):
            for j in range(i + 1, n_polys):
                _g = _geoms[j]
                if _g.contains(poly):
                    parents[i] = j
                    _g.interiors.append(poly.exterior)
                    to_remove.append(i)
                    break

        # use a mod 2 rule to remove holes
        for i in reversed(to_remove):
            depth = 0
            j = i
            while parents[j] > -1:
                depth += 1
                j = parents[j]
            print(i, depth)
            if depth % 2 == 1:
                _geoms.pop(i)

    def as_geom(self, context, o, mode, inter, doors, windows, io=None):
        """
         Build 2d symbol of walls as pygeos entity for further processing
         cut windows and doors
        """

        # MUST disable manipulators as setup_childs update
        # data structure and lead in crash
        bpy.ops.archipack.disable_manipulate()

        # TODO: handle user defined holes

        # setup childs is recursive
        childs = self.setup_childs(context, o, update_all=True)

        if mode == 'BOUNDARY':
            # single wall boundary for auto-boolean
            walls = [o]

        elif "archipack_wall2" in childs:
            # childs[k][c] = d
            walls = childs['archipack_wall2'].keys()
            # may filter out by altitude difference ?

        else:
            # when not child use context object
            walls = [o]

        coordsys, geom = Polygonizer.polygonize_walls(context, walls, extend=0.0001, all_segs=True)
        io = Io(scene=context.scene, coordsys=coordsys)

        _factory = geom._factory

        if geom.type_id == 6:
            geoms = geom.geoms
        else:
            geoms = [geom]

        # filter / convert walls geometry
        if mode == 'FLOORS':
            # extract inside as polygons for floors
            _geoms = []
            # sort by area
            for p in geoms:
                _geoms.extend(p.interiors)

            _geoms = [_factory.createPolygon(exterior=interior) for interior in _geoms]
            self.filter_geom_using_mod2_rule(_geoms)

        elif mode == 'MOLDINGS':
            # extract inside as linestring for moldings
            # side for moldings
            moldings_outside = False
            _geoms = []
            for p in geoms:
                _geoms.extend(p.interiors)

            # no interior found, use exteriors
            if len(_geoms) == 0:
                for p in geoms:
                    _geoms.append(p.exterior)

            # keep floors polygons to check for side
            interiors = [_factory.createPolygon(exterior=interior) for interior in _geoms]

            # filter out poly with too small areas (mostly wall snap errors)
            to_add = set([i for i, poly in enumerate(interiors) if poly.area > 0.1])
            _geoms = [_factory.createLineString(interior.coords) for i, interior in enumerate(_geoms) if i in to_add]

            # filter true floors out of interiors
            self.filter_geom_using_mod2_rule(interiors)

            geom = _factory.buildGeometry(_geoms)

        elif mode == 'SLAB':
            # extract outside as polygons for slabs
            _geoms = [_factory.createPolygon(exterior=p.exterior) for p in geoms]

            _geoms.sort(key=lambda x: x.exterior_area)
            n_polys = len(_geoms)
            to_remove = []
            # mod 2 rule doesnt apply here !!!
            # filter polygon when inside another one
            for i, poly in enumerate(_geoms):
                for j in range(i + 1, n_polys):
                    if _geoms[j].contains(poly):
                        to_remove.append(i)
                        break
            for i in reversed(to_remove):
                _geoms.pop(i)

            geom = _factory.buildGeometry(_geoms)

        elif mode in {'SYMBOL', 'BOUNDARY'}:
            # extract outside as polygons for symbols
            _geoms = [_factory.createPolygon(exterior=p.exterior) for p in geoms]
            self.filter_geom_using_mod2_rule(_geoms)

            # io.to_curve(context.scene, coordsys, _geoms, name="outsides")

        elif mode in {'AREA', 'CEILING'}:
            # extract inside as polygons
            _geoms = []
            for p in geoms:
                _geoms.extend(p.interiors)
            # ensure all lines are cw
            for i, interior in enumerate(_geoms):
                if interior.is_ccw:
                    co = list(reversed(interior.coords))
                    _geoms[i] = _factory.createLinearRing(co)
            _geoms = [_factory.createPolygon(exterior=interior) for interior in _geoms]
            self.filter_geom_using_mod2_rule(_geoms)
            geom = _factory.buildGeometry(_geoms)

        # process openings
        if mode in {'SYMBOL', 'MOLDINGS', 'FLOORS'}:

            # collect child holes as polygons
            if "archipack_window" in childs:

                _windows = []
                for c, d in childs["archipack_window"].items():
                    if d.altitude > 0 and mode != 'SYMBOL':
                        continue
                    h = d.hole_2d(mode)
                    hole = io.coords_to_polygon(c.matrix_world, h)
                    _windows.append(hole)
                    windows.append(c)

                if len(_windows) > 0:

                    if mode == 'MOLDINGS':
                        for poly in _windows:
                            geom = geom.difference(poly)

                    elif mode == 'SYMBOL':

                        # step 1 intersections
                        for i, poly in enumerate(_geoms):

                            for k, basis in enumerate(_windows):
                                diff = poly.intersection(basis)
                                if diff.type_id == 3:
                                    inter.append(diff)

                        # step 2 sub
                        for i, poly in enumerate(_geoms):
                        
                            for k, basis in enumerate(_windows):
                                poly = poly.difference(basis)

                            _geoms[i] = poly

                    elif mode == 'FLOORS':
                        # identify wich polygon the window belong to
                        for i, poly in enumerate(_geoms):
                            for window in _windows:
                                if poly.intersects(window):
                                    # poly intersects
                                    if not poly.contains(window):
                                        # if window not contained into poly
                                        # grow surface inside under window
                                        poly = poly.union(window)
                                        _geoms[i] = poly

            if "archipack_door" in childs:
                _doors = []
                if mode == 'FLOORS':
                    for c, d in childs["archipack_door"].items():
                        # identify door side to get right
                        # hole for this wall
                        h = d.hole_2d('INSIDE')
                        hole = io.coords_to_polygon(c.matrix_world, h)
                        pt = _factory.createPoint(hole.coords[3])
                        _doors.append((pt, hole))
                        h = d.hole_2d('OUTSIDE')
                        hole = io.coords_to_polygon(c.matrix_world, h)
                        pt = _factory.createPoint(hole.coords[0])
                        _doors.append((pt, hole))
                else:
                    for c, d in childs["archipack_door"].items():
                        h = d.hole_2d(mode)
                        hole = io.coords_to_polygon(c.matrix_world, h)
                        _doors.append(hole)
                        doors.append(c)

                if len(_doors) > 0:
                    if mode == 'MOLDINGS':
                        for poly in _doors:
                            geom = geom.difference(poly)

                    elif mode == 'SYMBOL':

                        for i, poly in enumerate(_geoms):
                            for basis in _doors:
                                poly = poly.difference(basis)
                            _geoms[i] = poly

                    elif mode == 'FLOORS':

                        # identify wich polygon the door belong to
                        for i, poly in enumerate(_geoms):
                            # @TODO:
                            # Doors inside on T child may touch 2 polygons (straight unclosed wall)
                            # in such case make an union of door inside and poly
                            for pt, door in _doors:
                                if poly.intersects(door):
                                    # poly intersects
                                    # if point inside poly then add to poly
                                    if not poly.contains(door):
                                        if poly.contains(pt):
                                            poly = poly.union(door)
                                        else:
                                            poly = poly.difference(door)
                                        _geoms[i] = poly

        if mode == 'FLOORS':
            # filter out poly with too low area (mostly wall snap issues)
            _geoms = [poly for poly in _geoms if poly.area > 0.1]
            geom = _factory.buildGeometry(_geoms)

        elif mode in {'SYMBOL', 'BOUNDARY'}:
            geom = _factory.buildGeometry(_geoms)

        elif mode == 'MOLDINGS':
            # Boolean result might not always preserve wall direction
            # so ensure lines are in the right direction
            merged = geom.line_merge()
            # merged is an array of geoms
            # switch direction of line so molding are always outside of wall
            for i, line in enumerate(merged):
                co = line.coords
                # a point in the middle of first segment at 0.01 * width on right side of line
                n = 0.05 * self.width * Vector((co[1].y - co[0].y, co[0].x - co[1].x, 0)).normalized()
                pos = Vector((0.5 * (co[0].x + co[1].x), 0.5 * (co[0].y + co[1].y), 0)) + n
                pt = _factory.createPoint(_factory.createCoordinate(pos))
                swap = True
                for poly in interiors:
                    if poly.contains(pt):
                        swap = poly.area < 0.1
                        break

                if moldings_outside:
                    swap = not swap

                if swap:
                    line.coords = _factory.coordinateSequenceFactory.create(line.coords[::-1])

            geom = _factory.buildGeometry(merged)

        # output only geometry from there
        # for further processing
        return io, geom, []

    def on_delete(self, context, o):
        for child in self.childs:
            c, d = child.get_child(context)
            if c is None:
                continue
            # delete windows and doors
            self.delete_object(context, c)

        self.childs.clear()

        for m in o.modifiers:
            # delete Autoboolean hole object
            if m.type == 'BOOLEAN':
                if m.object is not None:
                    self.delete_object(context, m.object)


class ARCHIPACK_PT_wall2(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_wall2"
    bl_label = "Wall"

    def draw(self, context):
        o = context.active_object
        d = archipack_wall2.datablock(o)
        if d is None:
            return

        icons = icon_man["main"]
        layout = self.layout
        # layout.use_property_split = True

        self.draw_common(context, layout)

        self.draw_prop(context, layout, layout, d, 'auto_synch', icon="AUTO", emboss=True)

        box = layout.box()
        # self.draw_label(context, layout, box, "Styles")
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.wall2_preset_menu",
                     text=bpy.types.ARCHIPACK_OT_wall2_preset_menu.bl_label, icon="PRESET")
        self.draw_op(context, layout, row, "archipack.wall2_preset", icon='ADD', text="")
        self.draw_op(context, layout, row, "archipack.wall2_preset", icon='REMOVE', text="").remove_active = True
        # row = layout.row(align=True)
        # row.d(d, 'realtime')
        # layout.label(text="Manip mode:{}".format(d.manipulate_mode))

        # box = layout.box()
        # box.prop_search(d, "t_part", context.scene, "objects", text="T parent", icon='OBJECT_DATAMODE')
        self.draw_prop(context, layout, layout, d, "tabs", expand=True)

        box = layout.box()

        if d.tabs == 'MAIN':

            self.draw_prop(context, layout, box, d, 'width_ui')

            self.draw_prop(context, layout, box, d, 'z')

            self.draw_prop(context, layout, box, d, 'z_offset')
            box = layout.box()
            self.draw_label(context, layout, box, "Base line")
            row = box.row()
            # XXX ui crash with drop-down (expand=False)
            self.draw_prop(context, layout, row, d, 'base_line_ui', expand=True)
            row = box.row()
            self.draw_op(context, layout, row, "archipack.path_reverse", icon='FILE_REFRESH', text="Flip in/out")
            self.draw_prop(context, layout, row, d, "closed")
            box = layout.box()
            self.draw_prop(context, layout, box, d, "dimensions", icon_value=icons["dimension_auto"].icon_id)
            box = layout.box()
            row = box.row()
            self.draw_op(context, layout, row, "archipack.wall2_fit_roof")
            self.draw_prop(context, layout, row, d, "fit_roof", icon="AUTO", emboss=True, text="Auto fit")

            if not d.closed:
                box = layout.box()
                self.draw_label(context, layout, box, "Auto snap wall")
                row = box.row(align=True)
                icon = 'SNAP_OFF'
                if d.snap_wall_start:
                    icon = 'SNAP_ON'
                self.draw_prop(context, layout, row, d, 'snap_wall_start', icon=icon, emboss=True)
                icon = 'SNAP_OFF'
                if d.snap_wall_end:
                    icon = 'SNAP_ON'
                self.draw_prop(context, layout, row, d, 'snap_wall_end', icon=icon, emboss=True)

        elif d.tabs == 'MATERIALS':
            if "archipack_material" in o:
                o.archipack_material[0].draw(context, box)

            box = layout.box()
            self.draw_label(context, layout, box, "Apply materials")
            self.draw_prop(context, layout, box, d, "material_inside")
            self.draw_prop(context, layout, box, d, "material_outside")
            self.draw_prop(context, layout, box, d, "material_top")
            self.draw_prop(context, layout, box, d, "material_bottom")
            self.draw_prop(context, layout, box, d, "material_start")
            self.draw_prop(context, layout, box, d, "material_end")
            for index, part in enumerate(d.valid_parts):
                part.draw_material(context, layout, index)

        elif d.tabs == 'PARTS':
            d.template_user_path(context, box, focus=False)
            self.draw_op(context, layout, layout, "archipack.wall2_clear_slices")
            d.template_parts(context, layout, draw_type=True, draw_reverse=False)

        elif d.tabs == 'FINISHINGS':
            row = box.row()
            self.draw_prop(context, layout, row, d, 'finish_enable')

            if len(d.finish) < 1:
                self.draw_op(context, layout, row, "archipack.wall2_add_finish2", icon='ADD', text="")
                self.draw_op(context, layout, row, "archipack.wall2_paste_finish2", icon='PASTEDOWN')

                for index, finish2 in enumerate(d.finish2):
                    finish2.draw(context, layout, index)
            else:
                self.draw_op(context, layout, row, "archipack.wall2_add_finish", icon='ADD', text="")
                self.draw_op(context, layout, row, "archipack.wall2_paste_finish", icon='PASTEDOWN')

                for index, finish in enumerate(d.finish):
                    box = layout.box()
                    finish.draw(context, box, index)

            box = layout.box()
            self.draw_label(context, layout, box, "Apply finishing")
            if len(d.finish) < 1:
                row = box.row()
                self.draw_prop(context, layout, row, d, "finish2_inside")
                self.draw_prop(context, layout, row, d, "finish_inside_auto", icon="AUTO", text="")
                row = box.row()
                self.draw_prop(context, layout, row, d, "finish2_outside")
                self.draw_prop(context, layout, row, d, "finish_outside_auto", icon="AUTO", text="")

            else:
                self.draw_prop(context, layout, box, d, "finish_inside")
                self.draw_prop(context, layout, box, d, "finish_outside")

            for index, part in enumerate(d.valid_parts):
                part.draw_finishes(context, layout, index, len(d.finish) < 1)

        elif d.tabs == 'TOOLS':
            # self.draw_prop(context, layout, box, d, "dimensions")
            self.draw_label(context, layout, box, "Create objects")
            row = box.row(align=True)
            self.draw_op(context, layout, row, "archipack.slab_from_wall", text="Slab",
                         icon_value=icons["slab_from_wall"].icon_id).ceiling = False
            self.draw_op(context, layout, row, "archipack.slab_from_wall", text="Ceiling",
                         icon_value=icons["ceiling_from_wall"].icon_id).ceiling = True

            self.draw_op(context, layout, box, "archipack.floor_preset_from_wall", text="Floors",
                         icon_value=icons["floor_from_wall"].icon_id)
            row = box.row(align=True)
            self.draw_op(context, layout, row, "archipack.molding_preset_from_wall", text="Floor Moldings",
                         icon_value=icons["molding_from_wall"].icon_id).mode = 'MOLDINGS'
            self.draw_op(context, layout, row, "archipack.molding_preset_from_wall", text="Ceiling Moldings",
                         icon_value=icons["molding_ceiling_from_wall"].icon_id).mode = 'CEILING'
            self.draw_op(context, layout, box, "archipack.roof_preset_menu", text="Roof",
                         icon_value=icons["roof_from_wall"].icon_id).preset_operator= "archipack.roof_from_wall"

            row = box.row(align=True)
            self.draw_op(context, layout, row, "archipack.window_preset_draw",
                         text="Window",
                         icon_value=icons["window"].icon_id
                         ).preset_operator = "archipack.window_draw"

            self.draw_op(context, layout, row, "archipack.door_preset_draw", text="Door",
                         icon_value=icons["door"].icon_id).preset_operator = "archipack.door_draw"

            self.draw_op(context, layout, box, "archipack.area_from_wall", icon="MOD_EDGESPLIT", text="Area / Volume")

            box = layout.box()
            self.draw_label(context, layout, box, "Auto Boolean")
            self.draw_op(context, layout, box, "archipack.auto_boolean", icon='FILE_REFRESH', text="Auto Boolean")
            box = layout.box()
            self.draw_label(context, layout, box, "Create curves")
            row = box.row(align=True)
            self.draw_op(context, layout, row, "archipack.wall2_to_curve", text="Symbol").mode = 'SYMBOL'
            self.draw_op(context, layout, row, "archipack.wall2_to_curve", text="Bounds").mode = 'BOUNDARY'
            self.draw_op(context, layout, row, "archipack.wall2_to_curve", text="Ceiling").mode = 'CEILING'

            row = box.row(align=True)
            self.draw_op(context, layout, row, "archipack.wall2_to_curve", text="Floor").mode = 'FLOORS'
            self.draw_op(context, layout, row, "archipack.wall2_to_curve", text="Molding").mode = 'MOLDINGS'
            self.draw_op(context, layout, row, "archipack.wall2_to_curve", text="Slab").mode = 'SLAB'

            box = layout.box()
            self.draw_label(context, layout, box, "Indirect lighting")
            self.draw_op(context, layout, box, "archipack.wall2_irradiance_volume", icon='LIGHTPROBE_GRID',
                         text="Irradiance")
            self.draw_op(context, layout, box, "archipack.wall2_cubemaps", icon='LIGHTPROBE_CUBEMAP', text="Cubemaps")

        # self.draw_op(context, layout, row, "archipack.wall2_fit_roof", text="Inside").inside = True

    @classmethod
    def poll(cls, context):
        return archipack_wall2.poll(context.active_object)


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


class ARCHIPACK_OT_wall2(ArchipackCreateTool, Operator):
    bl_idname = "archipack.wall2"
    bl_label = "Wall"
    bl_description = "Create wall"

    def create(self, context):
        m = bpy.data.meshes.new("Wall")
        o = bpy.data.objects.new("Wall", m)
        d = m.archipack_wall2.add()
        d.manipulable_selectable = True
        d.set_parts(1)
        prefs = get_prefs(context)
        d.draw_direction = prefs.draw_wall_tool_direction
        # Link object into scene
        self.link_object_to_scene(context, o)
        # select and make active
        self.select_object(context, o, True)
        self.add_material(context, o)
        self.load_preset(d)
        o.color = (0, 0.5, 1, 1)
        return o

    def execute(self, context):
        if context.mode == "OBJECT":

            bpy.ops.object.select_all(action="DESELECT")
            o = self.create(context)
            o.location = self.get_cursor_location(context)
            # select and make active

            self.select_object(context, o, True)
            self.add_to_reference(context, o)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_wall2_from_curve(ArchipackCreateTool, Operator):
    bl_idname = "archipack.wall2_from_curve"
    bl_label = "Wall from curve"
    bl_description = "Create wall(s) from a curve"

    @classmethod
    def poll(self, context):
        return context.object is not None and context.object.type == 'CURVE'

    def create(self, context):
        curve = context.object
        self.set_cursor_location(context, curve.matrix_world @ Vector(curve.bound_box[0]))
        for i, spline in enumerate(curve.data.splines):
            bpy.ops.archipack.wall2(filepath=self.filepath)
            o = context.active_object
            d = archipack_wall2.datablock(o)
            d.user_defined_spline = i
            d.from_spline(context, o, curve)

        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            o = self.create(context)
            # select and make active
            self.select_object(context, o, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_wall2_from_slab(ArchipackCreateTool, Operator):
    bl_idname = "archipack.wall2_from_slab"
    bl_label = "Wall from slab"
    bl_description = "Create a wall from a slab"

    @classmethod
    def poll(self, context):
        o = context.object
        return o is not None and o.data is not None and 'archipack_slab' in o.data

    def create(self, context):
        slab = context.object
        sd = slab.data.archipack_slab[0]
        bpy.ops.archipack.wall2(filepath=self.filepath)
        o = context.active_object
        d = archipack_wall2.datablock(o)
        d.auto_update = False
        d.draw_direction = "CCW"
        d.closed = True
        d.set_parts(sd.num_parts)
        for dst, part in zip(d.parts, sd.parts):
            dst.type = part.type
            dst.length = part.length
            dst.radius = part.radius
            dst.da = part.da
            dst.a0 = part.a0
        # select and make active
        self.select_object(context, o, True)

        d.auto_update = True

        # pretranslate
        o.matrix_world = Matrix.Translation(Vector((0, 0, d.z_offset))) @ slab.matrix_world

        ref = self.get_reference_point(o)

        if ref is not None:
            # use _delete_object to keep childs
            self._delete_object(context, ref)

        with ensure_select_and_restore(context, o, [o, slab]):
            bpy.ops.archipack.add_reference_point()

        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            o = self.create(context)

            # select and make active
            self.select_object(context, o, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_wall2_fit_roof(Operator):
    bl_idname = "archipack.wall2_fit_roof"
    bl_label = "Fit roof"
    bl_description = "Fit roof"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL', 'UNDO'}

    inside: BoolProperty(default=False)
    skip_z: BoolProperty(default=False)
    auto_update: BoolProperty(default=True)

    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_wall2.datablock(o)
        roofs = d.find_roofs(context, o)
        d.auto_update = False
        if len(roofs) > 0:
            d.clear_slices()
            for roof in roofs:
                rd = roof.data.archipack_roof[0]
                rd.make_wall_fit(context, roof, o, clear_slices=False)

        d.auto_update = True
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_to_curve(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.wall2_to_curve"
    bl_label = "To curve"
    bl_description = "Create curve from wall"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL', 'UNDO'}
    mode: EnumProperty(
        items=(
            ('SYMBOL', 'Symbol', 'Wall 2d Symbol'),
            ('BOUNDARY', 'Both sides', 'Wall boundary'),
            ('FLOORS', 'Floors', 'Floors'),
            ('SLAB', 'Slab', 'Slab'),
            ('MOLDINGS', 'Floor moldings', 'Floor moldings'),
            ('CEILING', 'Ceiling moldings', 'Ceiling moldings'),
            ('AREA', 'Areas dimension', 'Create area measures')
        ),
        default='SYMBOL'
    )

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            d = archipack_wall2.datablock(o)
            if d is None:
                return {'CANCELLED'}
            sel = []
            inter = []
            # doors holes
            doors = []
            windows = []

            try:
                io, wall, t_childs = d.as_geom(context, o, self.mode, inter, doors, windows)
            except:
                print("Archipack 2d export error")
                print("Most of the time this error means there are topology errors like self intersecting walls")
                import traceback
                traceback.print_exc()
                self.report({'ERROR'}, "Archipack: error while building 2d, see console for informations")
                return {'CANCELLED'}

            if self.mode == 'CONVERT':
                # NOTE: DEPRICATED was done to handle vi suite volumes creation

                # build walls as separated geometry and merge using a boolean
                roof, rd = d.find_roof(o)
                m = bpy.data.meshes.new("Wall")
                new_o = bpy.data.objects.new("Wall", m)
                new_o.color = (0, 1, 0, 1)
                new_o.matrix_world = io.coordsys.world.copy()
                # Link object into scene
                self.link_object_to_scene(context, new_o)
                if wall.geom_type == 'MultiPolygon':
                    polys = wall.geoms
                else:
                    polys = [wall]

                for i, poly in enumerate(polys):
                    # setup a random z diff for childs walls so boolean doesnt fails
                    c, cd = t_childs[i]
                    dz = c.matrix_world.translation.z - o.matrix_world.translation.z - cd.z_offset

                    walls = Io.to_wall(context, io.coordsys, poly, cd.z, name="Wall", walls=[], clean=False)
                    for w in walls:
                        w.location.z += dz
                        # select and make active
                        self.select_object(context, w, True)
                        Io.assign_matindex_to_wall(w)

                    if cd.fit_roof:
                        for w in walls:
                            cd.fit_roof_modifier(w, roof, rd, apply=True)

                    self.select_object(context, new_o, True)

                    for w in walls:
                        modif = new_o.modifiers.new('AutoMerge', 'BOOLEAN')
                        modif.operation = 'UNION'
                        if hasattr(modif, 'solver'):
                            modif.solver = 'CARVE'
                        modif.object = w
                        d.apply_modifier(new_o, modif)
                        self.delete_object(context, w)

                self.select_object(context, new_o, True)
                bpy.ops.archipack.wall(z=d.z)

                self.select_object(context, new_o, True)
                bpy.ops.object.mode_set(mode='EDIT')
                bpy.ops.mesh.select_all(action='SELECT')
                bpy.ops.mesh.dissolve_limited(angle_limit=0.0174533, delimit={'MATERIAL'})
                bpy.ops.object.mode_set(mode='OBJECT')

                if bpy.ops.archipack.auto_boolean.poll():
                    bpy.ops.archipack.auto_boolean()

            elif self.mode == 'AREA':

                res = io._to_curve(wall, "{}-{}-2d".format(o.name, self.mode.lower()), '2D')
                sel.append(res)

                if wall.type_id == 6:
                    polys = wall.geoms
                else:
                    polys = [wall]

                for poly in polys:
                    print(poly.area)

            else:
                res = io._to_curve(wall, "{}-{}-2d".format(o.name, self.mode.lower()), '2D')
                sel.append(res)

                # windows openings for symbols
                if len(inter) > 0:
                    res = io._to_curve(inter, "{}-{}-w-2d".format(o.name, self.mode.lower()), '2D')
                    sel.append(res)

                bpy.ops.object.select_all(action="DESELECT")
                for o in sel:
                    self.select_object(context, o)
                self.select_object(context, res, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_wall2_add_finish(Operator):
    bl_idname = "archipack.wall2_add_finish"
    bl_label = "Add finishings"
    bl_description = "Add finishing"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}

    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_wall2.datablock(o)
        d.finish.add()
        d.update(context)
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_remove_finish(Operator):
    bl_idname = "archipack.wall2_remove_finish"
    bl_label = "Remove finishings"
    bl_description = "Remove finishing"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}
    index : IntProperty(default=0)

    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_wall2.datablock(o)
        d.finish.remove(self.index)
        d.update(context)
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_add_finish2(Operator):
    bl_idname = "archipack.wall2_add_finish2"
    bl_label = "Add finishings"
    bl_description = "Add finishing"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}

    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_wall2.datablock(o)
        d.finish2.add()
        d.update(context)
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_remove_finish2(Operator):
    bl_idname = "archipack.wall2_remove_finish2"
    bl_label = "Remove finishings"
    bl_description = "Remove finishing"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}
    index : IntProperty(default=0)

    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_wall2.datablock(o)
        d.finish2.remove(self.index)
        d.update(context)
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_add_slice(Operator):
    bl_idname = "archipack.wall2_add_slice"
    bl_label = "Add slice"
    bl_description = "Add slice"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}
    part: IntProperty(default=0)
    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_wall2.datablock(o)
        d.parts[self.part].slices.add()
        d.update(context)
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_remove_slice(Operator):
    bl_idname = "archipack.wall2_remove_slice"
    bl_label = "Remove slice"
    bl_description = "Remove slice"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}
    part: IntProperty(default=0)
    index: IntProperty(default=0)

    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_wall2.datablock(o)
        d.parts[self.part].slices.remove(self.index)
        d.update(context)
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_clear_slices(Operator):
    bl_idname = "archipack.wall2_clear_slices"
    bl_label = "Clear slices"
    bl_description = "Remove all segments slices"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}

    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_wall2.datablock(o)
        d.clear_slices()
        d.update(context)
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_copy_finish(Operator):
    bl_idname = "archipack.wall2_copy_finish"
    bl_label = "Copy finish"
    bl_description = "Copy finish to clipboard"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL'}
    finish_name: StringProperty(default="")

    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_wall2.datablock(o)
        if d is not None:
            bl = set(['name'])
            clipboard[self.finish_name] = []
            for f in d.finish:
                if f.finish_name == self.finish_name:
                    clipboard[self.finish_name].append(to_dict(f, bl))
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_paste_finish(Operator):
    bl_idname = "archipack.wall2_paste_finish"
    bl_label = "Paste finish"
    bl_description = "Paste finish from clipboard"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL'}

    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object) and len(clipboard) > 0

    def execute(self, context):
        sel = context.selected_objects
        for o in sel:
            d = archipack_wall2.datablock(o)
            if d is not None:
                d.auto_update = False
                for finish_name, finishes in clipboard.items():
                    for finish in finishes:
                        f = d.finish.add()
                        for attr, val in finish.items():
                            setattr(f, attr, val)
                d.auto_update = True
        clipboard.clear()
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_copy_finish2(Operator):
    bl_idname = "archipack.wall2_copy_finish2"
    bl_label = "Copy finish"
    bl_description = "Copy finish to clipboard"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL'}
    finish_name: StringProperty(default="")

    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_wall2.datablock(o)
        if d is not None:
            bl = set(['name'])
            clipboard[self.finish_name] = []
            for f in d.finish2:
                if f.finish_name == self.finish_name:
                    clipboard[self.finish_name].append(to_dict(f, bl))
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_paste_finish2(Operator):
    bl_idname = "archipack.wall2_paste_finish2"
    bl_label = "Paste finish"
    bl_description = "Paste finish from clipboard"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL'}

    @classmethod
    def poll(self, context):
        return archipack_wall2.poll(context.active_object) and len(clipboard) > 0

    def execute(self, context):
        sel = context.selected_objects
        for o in sel:
            d = archipack_wall2.datablock(o)
            if d is not None:
                d.auto_update = False
                for finish_name, finishes in clipboard.items():
                    for finish in finishes:
                        f = d.finish2.add()
                        for attr, val in finish.items():
                            setattr(f, attr, val)
                d.auto_update = True
        clipboard.clear()
        return {'FINISHED'}


class ARCHIPACK_OT_wall2_irradiance_volume(ArchipackCreateTool, Operator):
    bl_idname = "archipack.wall2_irradiance_volume"
    bl_label = "Irradiance volume"
    bl_description = "Create Irradiance volume"
    bl_options = {'INTERNAL', 'REGISTER', 'UNDO'}
    density: IntProperty(
        min=1,
        default=2,
        name="Density",
        description="Probes density per volume unit (size ^ density)"
    )
    all_walls: BoolProperty(
        name="Use all walls",
        default=True,
        description="Use volume of all walls of this level when not active, use volume of selected objects"
    )
    mode: EnumProperty(
        description="Creation method",
        name='Mode',
        items=(
            ('SINGLE', 'Single', 'Create a single irradiance volume using bounding boxes', 'NONE', 0),
            ('FLOORS', 'Multiple', 'Create many irradiance volumes using floors bounding rectangles', 'NONE', 1)
        ),
        default='SINGLE',
    )
    @classmethod
    def poll(self, context):
        return context.active_object is not None and len(context.selected_objects) > 0

    def execute(self, context):
        o = context.active_object
        ref = self.get_reference_point(o)

        if self.mode == 'FLOORS':
            from .archipack_polylines import ShapelyOps

            d = archipack_wall2.datablock(o)
            if d is None:
                return {'CANCELLED'}

            bpy.ops.object.select_all(action="DESELECT")

            inter = []
            # doors holes
            doors = []
            windows = []
            try:
                io, wall, t_childs = d.as_geom(context, o, 'CEILING', inter, doors, windows)
            except:
                print("Archipack 2d export error")
                print("Most of the time this error means there are topology errors like self intersecting walls")
                import traceback
                traceback.print_exc()
                self.report({'ERROR'}, "Archipack: error while building 2d, see console for informations")
                return {'CANCELLED'}

            z = d.z

            # MultiPolygons
            if wall.type_id == 6:
                geoms = wall.geoms
            else:
                geoms = [wall]

            probes = []

            for poly in geoms:
                tM, x, y, poly, w_pts = ShapelyOps.min_bounding_rect(poly)
                # center = io.coordsys.world @ tM @ Matrix.Translation((0, 0, 0.5 * z))
                bpy.ops.object.lightprobe_add(type='GRID', radius=1, align='WORLD', enter_editmode=False,
                                              location=Vector())

                probe = context.object
                probe.matrix_world = io.coordsys.world @ tM @ Matrix.Translation((0, 0, 0.5 * z))
                scale = 0.5 * Vector((x, y, z))
                probe.scale = scale
                density = self.density * scale
                probe.data.grid_resolution_x = min(20, int(density.x))
                probe.data.grid_resolution_y = min(20, int(density.y))
                probe.data.grid_resolution_z = min(20, int(density.z))
                self.unlink_object_from_scene(context, probe)
                self.link_object_to_scene(context, probe, layer_name="Lights")

                if ref is not None:
                    self.add_to_reference(context, probe)

                probes.append(probe)

            for probe in probes:
                self.select_object(context, probe, True)

        else:
            mini = 1e32
            maxi = -1e32
            min_x, min_y, min_z, max_x, max_y, max_z = mini, mini, mini, maxi, maxi, maxi


            if self.all_walls:
                if ref is None:
                    self.report({'WARNING'}, "Unable to find reference point")
                    return {'CANCELLED'}
                sel = ref.children
            else:
                sel = context.selected_objects
            boolman = ArchipackBoolManager()
            for c in sel:
                if archipack_wall2.filter(c):
                    minx, miny, minz, maxx, maxy, maxz = boolman._world_bounding_box(c)
                    min_x = min(min_x, minx)
                    min_y = min(min_y, miny)
                    min_z = min(min_z, minz)
                    max_x = max(max_x, maxx)
                    max_y = max(max_y, maxy)
                    max_z = max(max_z, maxz)
            scale = 0.5 * Vector((max_x - min_x, max_y - min_y, max_z - min_z))
            center = Vector((min_x, min_y, min_z)) + scale

            bpy.ops.object.lightprobe_add(type='GRID', radius=1, align='WORLD', enter_editmode=False,
                                          location=center)

            probe = context.object
            self.unlink_object_from_scene(context, probe)
            self.link_object_to_scene(context, probe, layer_name="Lights")

            if ref is not None:
                self.add_to_reference(context, probe)

            probe.scale = scale
            density = self.density * scale
            probe.data.grid_resolution_x = min(20, int(density.x))
            probe.data.grid_resolution_y = min(20, int(density.y))
            probe.data.grid_resolution_z = min(20, int(density.z))
            self.select_object(context, probe, True)
        return {'FINISHED'}

    def draw(self, context):
        layout = self.layout
        layout.prop(self, 'mode')
        layout.prop(self, 'density')
        if self.mode == 'SINGLE':
            layout.prop(self, 'all_walls')

    def invoke(self, context, event):
        o = context.active_object
        if o is None:
            # fallback to context.object
            o = context.object
        if self.archipack_filter(o):
            self.act = o
        return context.window_manager.invoke_props_dialog(self)


class ARCHIPACK_OT_wall2_cubemaps(ArchipackCreateTool, Operator):
    bl_idname = "archipack.wall2_cubemaps"
    bl_label = "Reflection Cubemap"
    bl_description = "Create Reflection Cubemaps"

    def execute(self, context):
        if context.mode == "OBJECT":

            from .archipack_polylines import ShapelyOps

            o = context.active_object
            d = archipack_wall2.datablock(o)
            if d is None:
                return {'CANCELLED'}

            bpy.ops.object.select_all(action="DESELECT")

            inter = []
            # doors holes
            doors = []
            windows = []
            try:
                io, wall, t_childs = d.as_geom(context, o, 'CEILING', inter, doors, windows)
            except:
                print("Archipack 2d export error")
                print("Most of the time this error means there are topology errors like self intersecting walls")
                import traceback
                traceback.print_exc()
                self.report({'ERROR'}, "Archipack: error while building 2d, see console for informations")
                return {'CANCELLED'}

            z = d.z
            # Multipolygon
            if wall.type_id == 6:
                geoms = wall.geoms
            else:
                geoms = [wall]

            probes = []

            for poly in geoms:
                tM, x, y, poly, w_pts = ShapelyOps.min_bounding_rect(poly)
                center = io.coordsys.world @ tM @ Vector((0, 0, 0.5 * z))
                bpy.ops.object.lightprobe_add(type='CUBEMAP', radius=1, align='WORLD', enter_editmode=False,
                                              location=center)

                probe = context.object
                probe.data.influence_distance = 0.25 * min([x, y, z])
                self.unlink_object_from_scene(context, probe)
                self.link_object_to_scene(context, probe, layer_name="Lights")
                self.add_to_reference(context, probe)
                probes.append(probe)

            for probe in probes:
                self.select_object(context, probe, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


# ------------------------------------------------------------------
# Define operator class to draw a wall
# ------------------------------------------------------------------


class ARCHIPACK_OT_wall2_draw(ArchipackDrawTool, Operator):
    bl_idname = "archipack.wall2_draw"
    bl_label = "Draw a Wall"
    bl_description = "Create a wall by drawing its baseline in 3D view"

    filepath: StringProperty(default="")

    o = None
    state = 'RUNNING'
    flag_create = False
    flag_next = False
    wall_part1 = None
    wall_line1 = None
    line = None
    label = None
    feedback = None
    takeloc = Vector((0, 0, 0))
    sel = []
    act = None

    # auto_manipulate = False
    # constraint to other wall and make a T child
    parent = None
    takemat = None

    @classmethod
    def poll(cls, context):
        return True

    def draw_callback(self, _self, context):
        # logger.debug("ARCHIPACK_OT_wall2_draw.draw_callback")
        self.feedback.draw(context)

    def sp_draw(self, sp, context):
        z = 2.7
        if self.state == 'CREATE':
            p0 = self.takeloc
        else:
            p0 = sp.takeloc

        p1 = sp.placeloc
        delta = p1 - p0
        # print("sp_draw state:%s delta:%s p0:%s p1:%s" % (self.state, delta.length, p0, p1))
        if delta.length == 0:
            return
        logger.debug("ARCHIPACK_OT_wall2_draw.sp_draw")
        self.wall_part1.set_pos([p0, p1, Vector((p1.x, p1.y, p1.z + z)), Vector((p0.x, p0.y, p0.z + z))])
        self.wall_line1.set_pos([p0, p1, Vector((p1.x, p1.y, p1.z + z)), Vector((p0.x, p0.y, p0.z + z))])
        self.wall_part1.draw(context)
        self.wall_line1.draw(context)
        self.line.p = p0
        self.line.v = delta
        self.label.set_pos(context, self.line.length, self.line.lerp(0.5), self.line.v, normal=Vector((0, 0, 1)))
        self.label.draw(context)
        self.line.draw(context)

    def sp_callback(self, context, event, state, sp):
        # logger.debug("ARCHIPACK_OT_wall2_draw.sp_callback event %s %s state:%s", event.type, event.value, state)

        if state == 'SUCCESS':

            if self.state == 'CREATE':
                takeloc = self.takeloc
                delta = sp.placeloc - self.takeloc
            else:
                takeloc = sp.takeloc
                delta = sp.delta

            # old = context.object
            if self.o is None:
                # self.select_object(context, self.act, True)
                self.set_cursor_location(context, takeloc)
                bpy.ops.archipack.wall2('INVOKE_DEFAULT', filepath=self.filepath)
                # self.unselect_object(context, self.act)

                # context.window_manager.archipack.auto_manipulate = False

                o = context.object
                o.location = takeloc
                self.o = o
                d = archipack_wall2.datablock(o)
                # d.manipulable_selectable = False
                part = d.parts[0]
                part.length = delta.length
                state = "CALL_MANIPULATE"
            else:
                o = self.o
                # select and make active
                # self.select_object(context, o, True)
                d = archipack_wall2.datablock(o)
                # Check for end close to start and close when applicable
                dp = sp.placeloc - o.location
                if dp.length < 0.01:
                    d.closed = True
                    self.state = 'CANCEL'
                    return

                part = d.add_part(o, delta.length)

            # print("self.o :%s" % o.name)
            rM = o.matrix_world.inverted().to_3x3()
            g = d.get_generator()
            w = g.segs[-2]
            dp = rM @ delta
            da = w.signed_angle(w.v, dp)
            # atan2(dp.y, dp.x) - w.straight(1).angle
            a0 = w.limit_angle(part.a0 + da)
            part.a0 = a0
            d.update(context)

            # self.select_object(context, old, True)
            # self.select_object(context, o, True)

            self.flag_next = True
            context.area.tag_redraw()
            # print("feedback.on:%s" % self.feedback.on)

        self.state = state

    def sp_init(self, context, event, state, sp):
        logger.debug("ARCHIPACK_OT_wall2_draw.sp_init event %s %s state:%s" % (event.type, event.value, state))
        if state == 'SUCCESS':
            # point placed, check if a wall was under mouse
            logger.debug("self.mouse_hover_wall(context, event)")
            res, tM, wall, width, y, z_offset = self.mouse_hover_wall(context, event)
            logger.debug("self.mouse_hover_wall done")

            if res:
                d = archipack_wall2.datablock(wall)
                if self.is_snapping(context, event, sp.placeloc):
                    # user snap, use direction as constraint
                    tM.translation = sp.placeloc.copy()
                else:
                    # without snap, use wall's bottom
                    tM.translation -= y.normalized() * (0.5 * d.width)
                self.takeloc = tM.translation
                # self.parent = wall.name
                self.act = wall
                self.takemat = tM
            else:
                self.takeloc = sp.placeloc.copy()

            self.state = 'RUNNING'
            # print("feedback.on:%s" % self.feedback.on)
        elif state == 'CANCEL':
            self.state = state
            return

        logger.debug("sp_init() done %s", self.state)

    def exit(self, context):
        # d = archipack_wall2.datablock(self.o)
        # if d is not None:
        #    d.manipulable_selectable = True
        self.o = None
        self.feedback.disable()
        bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')

    def modal(self, context, event):

        if event.type in {'NONE', 'EVT_TWEAK_L', 'WINDOW_DEACTIVATE'} or "TIMER" in event.type:
            return {'PASS_THROUGH'}

        context.area.tag_redraw()

        # logger.debug("ARCHIPACK_OT_wall2_draw.modal(%s) type:%s  value:%s", self.state, event.type, event.value)

        if self.keymap.check(event, self.keymap.delete):
            self.exit(context)
            return {'FINISHED', 'PASS_THROUGH'}

        if self.state == 'STARTING' and event.type not in {'ESC', 'RIGHTMOUSE'}:
            # wait for takeloc being visible when button is over horizon
            takeloc = self.mouse_to_plane(context, event)
            if takeloc is not None:
                logger.debug("ARCHIPACK_OT_wall2_draw.modal(STARTING) location:%s", takeloc)
                snap_point(takeloc=takeloc,
                    callback=self.sp_init,
                    constraint_axis=(True, True, False),
                    release_confirm=True)
            return {'RUNNING_MODAL'}

        elif self.state == 'RUNNING':
            # print("RUNNING")
            # logger.debug("ARCHIPACK_OT_wall2_draw.modal(RUNNING) location:%s", self.takeloc)
            self.state = 'CREATE'
            snap_point(takeloc=self.takeloc,
                draw=self.sp_draw,
                takemat=self.takemat,
                transform_orientation=self.get_transform_orientation(context),
                callback=self.sp_callback,
                constraint_axis=(True, True, False),
                release_confirm=self.max_style_draw_tool)
            return {'RUNNING_MODAL'}

        elif self.state == 'CALL_MANIPULATE':
            o = self.o
            # re-select to enable manipulate
            self.select_object(context, o, True)
            self.state = 'SUCCESS'
            return {'RUNNING_MODAL'}

        elif self.state != 'CANCEL':

            if event.type in {'D', 'd'}:
                logger.debug("ARCHIPACK_OT_wall2_draw.modal(%s) D pressed", self.state)
                self.exit(context)
                bpy.ops.archipack.wall2_preset_menu('INVOKE_DEFAULT', preset_operator="archipack.wall2_draw")
                return {'FINISHED'}

            elif event.type in {'C', 'c'}:

                logger.debug("ARCHIPACK_OT_wall2_draw.modal(%s) C pressed", self.state)
                d = archipack_wall2.datablock(self.o)
                if d is not None:
                    d.closed = True
                self.exit(context)
                return {'FINISHED'}

            elif event.type in {'LEFTMOUSE', 'RET', 'NUMPAD_ENTER', 'SPACE'}:

                # print('LEFTMOUSE %s' % (event.value))
                self.feedback.instructions(context, "Draw a wall", "Click & Drag to add a segment", [
                    ('ENTER', 'Add part'),
                    ('BACK_SPACE', 'Remove part'),
                    ('CTRL', 'Snap'),
                    ('C', 'Close wall and exit'),
                    ('D', 'Draw another wall'),
                    ('MMBTN', 'Constraint to axis'),
                    ('X Y', 'Constraint to axis'),
                    ('RIGHTCLICK or ESC', 'exit')
                    ])

                # press with max mode release with blender mode
                if self.max_style_draw_tool:
                    evt_value = 'PRESS'
                else:
                    evt_value = 'RELEASE'

                if event.value == evt_value:

                    if self.flag_next:
                        self.flag_next = False
                        o = self.o

                        # select and make active
                        self.select_object(context, o, True)

                        d = archipack_wall2.datablock(o)
                        g = d.get_generator()
                        p0 = g.segs[-2].p0
                        p1 = g.segs[-2].p1
                        dp = p1 - p0
                        takemat = o.matrix_world @ Matrix([
                            [dp.x, dp.y, 0, p1.x],
                            [dp.y, -dp.x, 0, p1.y],
                            [0, 0, 1, 0],
                            [0, 0, 0, 1]
                        ])
                        takeloc = o.matrix_world @ p1
                        # self.unselect_object(context, o)
                    else:
                        takemat = None
                        takeloc = self.mouse_to_plane(context, event)

                    if takeloc is not None:

                        logger.debug("ARCHIPACK_OT_wall2_draw.modal(CREATE) location:%s", takeloc)

                        snap_point(takeloc=takeloc,
                            takemat=takemat,
                            draw=self.sp_draw,
                            callback=self.sp_callback,
                            constraint_axis=(True, True, False),
                            release_confirm=self.max_style_draw_tool)

                return {'RUNNING_MODAL'}

        if self.keymap.check(event, self.keymap.undo) or (
                event.type in {'BACK_SPACE'} and event.value == 'RELEASE'
                ):
            if self.o is not None:
                o = self.o

                # select and make active
                self.select_object(context, o, True)
                d = archipack_wall2.datablock(o)
                if d.num_parts > 1:
                    d.n_parts -= 1
            return {'RUNNING_MODAL'}

        if self.state == 'CANCEL' or (event.type in {'ESC', 'RIGHTMOUSE'}):

            logger.debug("ARCHIPACK_OT_wall2_draw.modal(CANCEL) %s", event.type)
            if self.o is None:

                # select and make active
                self.select_object(context, self.act, True)

                for o in self.sel:
                    self.select_object(context, o)
            else:
                self.select_object(context, self.o, True)

                # remove last segment with blender mode
                """
                d = archipack_wall2.datablock(self.o)
                if not self.max_style_draw_tool:
                    if not d.is_closed and d.num_parts > 1:
                        d.n_parts -= 1
                """

            self.exit(context)
            return {'FINISHED'}

        return {'PASS_THROUGH'}

    def add_to_reference(self, context, o):
        ref = self.get_reference_point(self.act)
        if ref is None:
            # print("ref is None")
            ref = o
        # print("ref name:", ref.name)
        with ensure_select_and_restore(context, ref, [o]):
            bpy.ops.archipack.add_reference_point()

    def invoke(self, context, event):

        if context.mode == "OBJECT":

            bpy.ops.archipack.disable_manipulate()

            # self.auto_manipulate = context.window_manager.archipack.auto_manipulate
            prefs = get_prefs(context)
            self.max_style_draw_tool = prefs.max_style_draw_tool
            self.keymap = Keymaps(context)
            self.wall_part1 = GlPolygon((0.5, 0, 0, 0.2))
            self.wall_line1 = GlPolyline((0.5, 0, 0, 0.8))
            self.line = GlLine()
            self.label = GlText()
            self.feedback = FeedbackPanel()
            self.feedback.instructions(context, "Draw a wall", "Click & Drag to start", [
                ('CTRL', 'Snap'),
                ('MMBTN', 'Constraint to axis'),
                ('X Y', 'Constraint to axis'),
                ('SHIFT+CTRL+TAB', 'Switch snap mode'),
                ('RIGHTCLICK or ESC', 'exit without change')
                ])
            self.feedback.enable()
            args = (self, context)

            self.sel = context.selected_objects[:]
            self.act = context.active_object

            bpy.ops.object.select_all(action="DESELECT")
            
            self.state = 'STARTING'

            self._handle = bpy.types.SpaceView3D.draw_handler_add(self.draw_callback, args, 'WINDOW', 'POST_PIXEL')
            context.window_manager.modal_handler_add(self)
            return {'RUNNING_MODAL'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


# ------------------------------------------------------------------
# Define operator class to load / save presets
# ------------------------------------------------------------------


class ARCHIPACK_OT_wall2_preset_draw(PresetMenuOperator, Operator):
    bl_description = "Choose a preset and draw wall"
    bl_idname = "archipack.wall2_preset_draw"
    bl_label = "Wall preset"
    preset_subdir = "archipack_wall2"


class ARCHIPACK_OT_wall2_preset_menu(PresetMenuOperator, Operator):
    bl_description = "Show Wall presets"
    bl_idname = "archipack.wall2_preset_menu"
    bl_label = "Wall preset"
    preset_subdir = "archipack_wall2"


class ARCHIPACK_OT_wall2_preset(ArchipackPreset, Operator):
    bl_description = "Add / remove a Wall Preset"
    bl_idname = "archipack.wall2_preset"
    bl_label = "Wall preset"
    preset_menu = "ARCHIPACK_OT_wall2_preset_menu"

    @property
    def blacklist(self):
        return ["parts", "n_parts",
                'childs', "dimensions",
                'childs_manipulators']


class ARCHIPACK_OT_wall2_preset_from_curve(PresetMenuOperator, Operator):
    bl_description = "Create a Wall from curve"
    bl_idname = "archipack.wall2_preset_from_curve"
    bl_label = "Wall"
    preset_subdir = "archipack_wall2"
    preset_operator : StringProperty(
        options={'SKIP_SAVE'},
        default="archipack.wall2_from_curve"
    )

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o and o.type == 'CURVE'


def register():
    bpy.utils.register_class(archipack_wall2_slice)
    bpy.utils.register_class(archipack_wall2_part)
    bpy.utils.register_class(archipack_wall2_child)
    bpy.utils.register_class(archipack_wall2_finish)

    bpy.utils.register_class(archipack_wall2_finish2)

    bpy.utils.register_class(archipack_wall2_relocate_child)
    bpy.utils.register_class(archipack_wall2)
    Mesh.archipack_wall2 = CollectionProperty(type=archipack_wall2)
    bpy.utils.register_class(ARCHIPACK_PT_wall2)
    bpy.utils.register_class(ARCHIPACK_OT_wall2)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_add_finish)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_remove_finish)

    bpy.utils.register_class(ARCHIPACK_OT_wall2_add_finish2)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_remove_finish2)

    bpy.utils.register_class(ARCHIPACK_OT_wall2_add_slice)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_remove_slice)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_clear_slices)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_draw)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_from_curve)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_from_slab)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_fit_roof)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_to_curve)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_preset)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_preset_from_curve)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_preset_draw)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_preset_menu)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_copy_finish)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_paste_finish)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_copy_finish2)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_paste_finish2)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_irradiance_volume)
    bpy.utils.register_class(ARCHIPACK_OT_wall2_cubemaps)


def unregister():
    bpy.utils.unregister_class(archipack_wall2_part)
    bpy.utils.unregister_class(archipack_wall2_slice)
    bpy.utils.unregister_class(archipack_wall2_child)
    bpy.utils.unregister_class(archipack_wall2_finish)
    bpy.utils.unregister_class(archipack_wall2_finish2)

    bpy.utils.unregister_class(archipack_wall2_relocate_child)
    bpy.utils.unregister_class(archipack_wall2)
    del Mesh.archipack_wall2
    bpy.utils.unregister_class(ARCHIPACK_PT_wall2)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_add_finish)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_remove_finish)

    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_add_finish2)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_remove_finish2)

    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_add_slice)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_clear_slices)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_remove_slice)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_draw)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_from_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_from_slab)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_fit_roof)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_to_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_preset_from_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_preset)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_preset_draw)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_preset_menu)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_copy_finish)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_paste_finish)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_copy_finish2)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_paste_finish2)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_irradiance_volume)
    bpy.utils.unregister_class(ARCHIPACK_OT_wall2_cubemaps)
