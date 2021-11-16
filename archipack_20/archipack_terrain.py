# -*- coding:utf-8 -*-

# #
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
# 
# <pep8 compliant>

# ----------------------------------------------------------
# Author: Stephen Leger (s-leger)
# Inspired from code of Domlysz, Oscurart
# ----------------------------------------------------------

import bpy
from bpy.types import Operator, PropertyGroup, Mesh, Panel
from bpy.props import (
    FloatProperty, IntProperty, BoolProperty,
    CollectionProperty, EnumProperty, StringProperty,
    IntVectorProperty
)
from math import sin, cos, pi
import bmesh
import json
from mathutils.geometry import intersect_ray_tri
from .bmesh_utils import BmeshEdit as bmed
from random import uniform
from .archipack_abstraction import ensure_select_and_restore
from mathutils import Vector, Matrix
from .archipack_i18n import Archipacki18n
from .archipack_polylines import CoordSys, Qtree
from .archipack_object import (
    ArchipackCreateTool,
    ArchipackObject,
    ArchipackPanel,
    ArchipackObjectsManager,
    stop_auto_manipulate
)
from .archipack_generator import Line, Generator
from .archipack_manipulator import Manipulable, archipack_manipulator
from .DelaunayVoronoi import (
    computeDelaunayTriangulation
)
from .archipack_cutter import (
    CutAblePolygon, CutAbleGenerator, CutterGenerator,
    ArchipackCutter,
    ArchipackCutterPart,
    update_operation
    )
from .archipack_dimension import DimensionProvider
from .archipack_curveman import ArchipackUserDefinedPath
from .archipack_segments2 import ArchipackSegment
from .archipack_throttle import throttle
from .archipack_material import build_mat_enum
from mathutils.kdtree import KDTree
import logging
import bmesh

logger = logging.getLogger("archipack")

import time


FACE_PRECISION = 0.02

MAT_TERRAIN = 0
MAT_ROAD = 1
MAT_EARTHWORK = 2
MAT_AREA = 3
MAT_USER = 0

material_enums = []
mat_enum, mat_index_getter, mat_index_setter= build_mat_enum('idmat', material_enums)
c_mat_enum, mat_index_getter, mat_index_setter = build_mat_enum('idmat', material_enums, parent=True)


class Q_tree(Qtree):
    """
     A quadtree to minimize relocate intersections
    """
    def _rec_str(self, child, depth):
        if len(child.children) > 0:
            _s = "\n".join([self._rec_str(c, depth+1) for c in child.children])
        else:
            _s = "\n".join(["depth:{} i:{}".format(
                depth,
                n.item
                ) for i, n in enumerate(child.nodes)])
        return _s

    def __str__(self):
        return "Q_tree\n{}".format(self._rec_str(self, 0))

    def getbounds_pt(self, pt, extend=0):
        x, y = pt.x, pt.y
        return (x - extend,
                  y - extend,
                  x + extend,
                  y + extend)

    def intersects_pt(self, pt, bounds):
        selection = list(self._intersect(bounds))
        count = len(selection)
        return count, sorted(selection)

    def insert_point(self, bounds, pt):
        idx = self.ngeoms
        self._geoms.append(pt)
        self._insert(idx, bounds)

    def new_point(self, pt, min_dist):
        bounds = self.getbounds_pt(pt, min_dist)
        found = self._intersect(bounds)
        if len(found) > 0:
            # use highest pt
            # for idx in found:
            #    self._geoms[idx].z = max(pt.z, self._geoms[idx].z)
            return False
        self.insert_point(bounds, pt)
        return True

    def new_point_maxz(self, pt, min_dist):
        bounds = self.getbounds_pt(pt, min_dist)
        found = self._intersect(bounds)
        if len(found) > 0:
            z = pt.z
            for idx in found:
                z = max(z, self._geoms[idx].z)
            pt.z = z
            for idx in found:
                self._geoms[idx].z = z
            return False
        self.insert_point(bounds, pt)

    # segment intersect mesh edges for roads
    def getbounds_verts(self, verts, extend=0):
        return self.getbounds_pts([v.co for v in verts], extend)

    def getbounds_pts(self, pts, extend=0):
        x, y, z = zip(*pts)
        return (min(x) - extend,
                min(y) - extend,
                max(x) + extend,
                max(y) + extend)

    def intersects_seg(self, seg, extend=0):
        bounds = self.getbounds_pts([seg.p0, seg.p1], extend)
        selection = list(self._intersect(bounds))
        count = len(selection)
        return count, sorted(selection)

    def insert_edge(self, edge, data, extend=0):
        idx = self.ngeoms
        self._geoms.append(data)
        bounds = self.getbounds_verts(edge.verts, extend)
        self._insert(idx, bounds)

    def insert_seg(self, seg, data, extend=0):
        idx = self.ngeoms
        self._geoms.append(data)
        bounds = self.getbounds_pts([seg.p0, seg.p1], extend)
        self._insert(idx, bounds)

    def insert_face(self, face, data, extend=0):
        idx = self.ngeoms
        self._geoms.append(data)
        bounds = self.getbounds_verts(face.verts, extend)
        self._insert(idx, bounds)

    def insert_quad(self, pts, data, extend=0):
        idx = self.ngeoms
        self._geoms.append(data)
        bounds = self.getbounds_pts(pts, extend)
        self._insert(idx, bounds)


def unique(L):
    """Return a list of unhashable elements in s, but without duplicates.
    [[1, 2], [2, 3], [1, 2]] >>> [[1, 2], [2, 3]]"""
    # For unhashable objects, you can sort the sequence and
    # then scan from the end of the list, deleting duplicates as you go
    nDupli = 0
    nZcolinear = 0
    # sort() brings the equal elements together; then duplicates
    # are easy to weed out in a single pass
    L.sort()
    last = L[-1]
    for i in range(len(L) - 2, -1, -1):
        if last[:2] == L[i][:2]:  # XY coordinates compararison
            if last[2] == L[i][2]:  # Z coordinates compararison
                nDupli += 1  # duplicates vertices
            else:  # Z colinear
                nZcolinear += 1
            del L[i]
        else:
            last = L[i]
    # list data type is mutable, input list will automatically update
    # and doesn't need to be returned
    return (nDupli, nZcolinear)


def checkEqual(lst):
    return lst[1:] == lst[:-1]


def random_color():
    return Vector((uniform(0.0, 1.0), uniform(0.0, 1.0), uniform(0.0, 1.0), 1))


def update_falloff(self, context):
    self.update(context, simple=True)


def update(self, context):
    self.update(context)


def update_path(self, context):
    self.update_path(context)


class TerrainGenerator(ArchipackObjectsManager, CutAblePolygon, CutAbleGenerator):

    def __init__(self, o=None):
        CutAbleGenerator.__init__(self, o)
        self.line = None
        self.rotation_matrix = None
        self.operations = []
        self.use_fast = []

    def cut(self, context, o, d, realtime):
        """
            either external or holes cuts
        """
        self.xsize = 1000
        if not realtime:
            self.operations = []
            itM = o.matrix_world.inverted()
            for child in d.childs:
                if child.active:
                    c = self.get_scene_object(context, child.src)
                    _d = archipack_terrain_cutter.datablock(c)
                    if _d is not None:
                        tM = itM @ c.matrix_world
                        g = _d.ensure_direction(tM)
                        self.holes.append(g)
                        self.operations.append(_d.operation)
                        self.use_fast.append(_d.use_fast_boolean)

    def cut_hole(self, o, hole, cleanup=True, delete=True):
        """ Provide alternative method to cut holes when bool fast cut fails
        :param o:
        :param hole:
        :param cleanup:
        :param delete:
        :return:
        """
        bm = bmed._start(o)
        for s in hole.segs:
            if s.length > 0:
                bmed.bisect(bm, s.p0, s.v_normal)

        # use hole boundary
        segs = hole.segs

        if len(segs) > 0 and delete:
            # when hole segs are found clear parts inside hole
            f_geom = [f for f in bm.faces
                if self.inside(
                    f.calc_center_median(),
                    segs=segs)]
            if len(f_geom) > 0:
                bmesh.ops.delete(bm, geom=f_geom, context='FACES')   # 5

        if cleanup:
            self.dissolve_limit(bm)
        bmed._end(bm, o)


class RoadGenerator(Generator):

    def __init__(self, o=None):

        # https://github.com/mozman/dxfwrite/blob/master/dxfwrite/algebra/clothoid.py
        # https://code.google.com/archive/p/cornucopia-lib/source/default/source

        Generator.__init__(self, o)
        self.border = None
        self.transition = None

class EarthworkGenerator(CutterGenerator):

    def __init__(self, o=None):
        CutAbleGenerator.__init__(self, o)
        self.border = None
        self.transition = None


class archipack_terrain_source(Archipacki18n, PropertyGroup):

    src: StringProperty(name="Source point cloud", default="", update=update)

    @property
    def parent_data(self):
        return self.id_data.archipack_terrain[0]

    def update(self, context):
        self.parent_data.update(context)

    def draw(self, context, layout, i):
        row = layout.row(align=True)
        row.prop_search(self, "src", context.scene, "objects", text="", icon='OBJECT_DATA')
        self.draw_op(context, layout, row, "archipack.terrain_remove_source", icon="REMOVE", text="").index = i


class archipack_terrain_child(Archipacki18n, PropertyGroup):

    src: StringProperty(name="Terrain geometry modifier", default="")
    active: BoolProperty(
        name="Active",
        description="Use modifier",
        default=True,
        update=update
    )
    @property
    def parent_data(self):
        return self.id_data.archipack_terrain[0]

    def update(self, context):
        self.parent_data.update(context)

    def draw(self, context, layout, box, i):
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.select", icon="RESTRICT_SELECT_OFF", text=self.src).name = self.src
        if i > 0:
            self.draw_op(context, layout, row, "archipack.terrain_child_up", icon="SORT_DESC", text="").index = i
        self.draw_op(context, layout, row, "archipack.terrain_child_down", icon="SORT_ASC", text="").index = i
        icon = "VISIBLE_IPO_OFF"
        if self.active:
            icon = "VISIBLE_IPO_ON"
        self.draw_prop(context, layout, row, self, "active", icon=icon, text="")
        self.draw_op(context, layout, row, "archipack.terrain_child_remove", icon="TRASH", text="").index = i


class archipack_terrain_cutter_segment(ArchipackCutterPart, PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_terrain_cutter[0]


class archipack_terrain_cutter(ArchipackObject, ArchipackCutter, Manipulable, DimensionProvider, PropertyGroup):
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
    parts: CollectionProperty(type=archipack_terrain_cutter_segment)

    use_fast_boolean: BoolProperty(
        name="Fast boolean",
        description="Use fast boolean to cut boundary and holes, might not be as safe as slow one",
        default=True,
        update=update
    )

    def update_parent(self, context, o):
        if o is not None:
            d = archipack_terrain.datablock(o.parent)
            if d is not None:
                cutables = [o.parent]
                self.filter_cutables(context, o, cutables)
                for c in cutables:
                    with ensure_select_and_restore(context, c, [c]) as (ctx, act, sel):
                        d.update(ctx)
                self.store_cutables(o, cutables)


class archipack_terrain_earthwork_segment(ArchipackCutterPart, PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_terrain_earthwork[0]


class archipack_terrain_earthwork(ArchipackObject, ArchipackCutter, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display earthwork settings', 'NONE', 0),
            ('PARTS', 'Shape', 'Display earthwork segments settings', 'NONE', 1),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 2)
        ),
        default='MAIN',
    )
    parts: CollectionProperty(type=archipack_terrain_earthwork_segment)
    border: FloatProperty(
        name="Border",
        description="Width of the borders",
        min=0.01,
        default=3,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    transition: FloatProperty(
        name="Transition",
        description="Soft transition size",
        min=0.01,
        default=0.2,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    # Note no translation needed
    border_falloff_type: EnumProperty(
        name="Falloff Type",
        items=(
            ('LINEAR', "Linear", "How weights are mapped to their new values", 'LINCURVE', 0),
            ('SHARP', "Sharp", "How weights are mapped to their new values", 'SHARPCURVE', 1),
            ('SMOOTH', "Smooth", "How weights are mapped to their new values", 'SMOOTHCURVE', 2),
            ('ROOT', "Root", "How weights are mapped to their new values", 'ROOTCURVE', 3),
            ('ICON_SPHERECURVE', "Sphere", "How weights are mapped to their new values", 'SPHERECURVE', 4),
            ('RANDOM', "Random", "How weights are mapped to their new values", 'RNDCURVE', 5),
            ('STEP', "Median Step", "How weights are mapped to their new values", 'NOCURVE', 6)
        ),
        default="LINEAR",
        update=update_falloff
    )

    idmat: IntVectorProperty(
        default=[2],
        size=1
    )
    material_user: EnumProperty(
        options={'SKIP_SAVE'},
        name="Road",
        items=c_mat_enum,
        get=mat_index_getter(MAT_USER),
        set=mat_index_setter(MAT_USER),
        update=update
    )
    material_override: BoolProperty(
        name="Override material",
        default=False,
        update=update
    )

    def get_generator(self, o=None):
        g = EarthworkGenerator(o)
        g.operation = self.operation
        g.add_parts(self)
        g.line = g.make_offset(-self.offset)
        g.border = g.line.make_offset(-self.border)
        g.transition = g.line.make_offset(-self.transition)

        return g

    def update_parent(self, context, o, simple=False):
        if o is not None:
            d = archipack_terrain.datablock(o.parent)
            if d is not None:
                cutables = [o.parent]
                self.filter_cutables(context, o, cutables)
                for c in cutables:
                    with ensure_select_and_restore(context, c, [c]) as (ctx, act, sel):
                        d.update(ctx, simple=simple)
                self.store_cutables(o, cutables)

    def get_child_type(self, o, typ):
        """ Return child tagged with archipack_typ
        :param o:
        :param typ:
        :return:
        """
        res = None
        for c in o.children:
            if typ in c:
                res = c
                break
        return res

    def make_border(self, context, o, g):
        # o.parent is Terrain, o is earthwork
        p = o.parent

        c = self.get_child_type(o, "archipack_earthwork_border")

        if c is None:
            m = bpy.data.meshes.new("Earthwork Border")
            c = bpy.data.objects.new("Earthwork Border", m)
            c.parent = o
            self.link_object_to_scene(context, c, layer_name="Terrain")

        c["archipack_earthwork_border"] = True

        c.matrix_world = o.matrix_world.copy()
        itM = o.matrix_world.inverted()
        pz0 = itM @ (p.matrix_world @ Vector(p.bound_box[0]))
        pz1 = itM @ (p.matrix_world @ Vector(p.bound_box[1]))
        z0 = pz0.z - 5.0
        z1 = pz1.z + 5.0

        c.hide_set(True)
        c.hide_render = True
        c.display.show_shadows = False

        # build in local space
        pts = []
        # closest part
        g.line.get_pts(pts)

        verts = []
        # vertical surface
        for v in pts:
            x, y, z = v
            verts.extend([Vector((x, y, z0)), Vector((x, y, z1))])

        nv = int(len(verts) / 2 - 1)
        faces = [(2 * i, 2 * i + 1, 2 * i + 3, 2 * i + 2) for i in range(nv)]
        faces.append((-2, -1, 1, 0))
        bmed.buildmesh(c, verts, faces)
        return c

    def make_wrap(self, context, o, g):
        # o.parent is Terrain, o is road
        c = self.get_child_type(o, "archipack_earthwork_wrap")

        if c is None:
            m = bpy.data.meshes.new("Earthwork Wrap")
            c = bpy.data.objects.new("Earthwork Wrap", m)
            c.parent = o
            self.link_object_to_scene(context, c, layer_name="Terrain")

        c['archipack_earthwork_wrap'] = True
        c.matrix_world = o.matrix_world.copy()
        verts = []
        edges = []
        # vertex index in order to build axis
        g.border.get_verts(verts, edges)

        bm = bmesh.new()
        for v in verts:
            bm.verts.new(v)
        bm.verts.ensure_lookup_table()
        for i in range(1, len(verts)):
            bm.edges.new((bm.verts[i - 1], bm.verts[i]))
        bm.edges.new((bm.verts[-1], bm.verts[0]))
        bm.edges.ensure_lookup_table()
        bmesh.ops.contextual_create(bm, geom=bm.edges)
        bm.to_mesh(c.data)
        bm.free()

        c.display_type = 'WIRE'
        c.hide_set(True)
        c.hide_render = True
        c.display.show_shadows = False
        return c

    def make_area(self, context, o, g):
        # o.parent is Terrain, o is road
        c = self.get_child_type(o, "archipack_earthwork_area")

        if c is None:
            m = bpy.data.meshes.new("Earthwork Area")
            c = bpy.data.objects.new("Earthwork Area", m)
            c.parent = o
            self.link_object_to_scene(context, c, layer_name="Terrain")

        c['archipack_earthwork_area'] = True
        c.matrix_world = o.matrix_world.copy()
        verts = []
        edges = []
        # vertex index in order to build axis
        g.line.get_verts(verts, edges)

        bm = bmesh.new()
        for v in verts:
            bm.verts.new(v)
        bm.verts.ensure_lookup_table()
        for i in range(1, len(verts)):
            bm.edges.new((bm.verts[i - 1], bm.verts[i]))
        bm.edges.new((bm.verts[-1], bm.verts[0]))
        bm.edges.ensure_lookup_table()
        bmesh.ops.contextual_create(bm, geom=bm.edges)
        bm.to_mesh(c.data)
        bm.free()

        c.display_type = 'WIRE'
        c.hide_set(True)
        c.hide_render = True
        c.display.show_shadows = False
        return c

    def update(self, context, manipulable_refresh=False, update_parent=True, simple=False):
        """
         Does update parent make sense at all ?
         as cutter changes must always update parent
        """
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        logger.debug("archipack_road.update() n_parts:%s" % self.n_parts)

        # clean up manipulators before any data model change
        if self.manipulable_refresh:
            self.manipulable_disable(o)

        # self.update_parts()

        verts = []
        edges = []

        self.prepare_update(context, o)

        g = self.get_generator()
        g.locate_manipulators(self)

        # vertex index in order to build axis
        g.line.get_verts(verts, edges)

        if len(verts) > 2:
            self.make_surface(o, verts, edges)
            o.hide_render = True
            o.display.show_shadows = False
            self.make_border(context, o, g)
            self.make_wrap(context, o, g)
            self.make_area(context, o, g)

        # update parent on direct edit
        if self.manipulable_refresh or update_parent:
            self.update_parent(context, o, simple)

        self.update_dimensions(context, o)
        self.restore_context(context)


class archipack_terrain_material_segment(ArchipackCutterPart, PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_terrain_material[0]


class archipack_terrain_material(ArchipackObject, ArchipackCutter, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display earthwork settings', 'NONE', 0),
            ('PARTS', 'Shape', 'Display earthwork segments settings', 'NONE', 1),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 2)
        ),
        default='MAIN',
    )
    parts: CollectionProperty(type=archipack_terrain_earthwork_segment)

    transition: FloatProperty(
        name="Transition",
        description="Soft transition size",
        min=0.01,
        default=0.2,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    idmat: IntVectorProperty(
        default=[3],
        size=1
    )
    material_user: EnumProperty(
        options={'SKIP_SAVE'},
        name="Road",
        items=c_mat_enum,
        get=mat_index_getter(MAT_USER),
        set=mat_index_setter(MAT_USER),
        update=update
    )
    material_override: BoolProperty(
        name="Override material",
        default=False,
        update=update
    )
    def get_generator(self, o=None):
        g = EarthworkGenerator(o)
        g.operation = self.operation
        g.add_parts(self)
        g.line = g.make_offset(-self.offset)
        g.transition = g.line.make_offset(-self.transition)
        return g

    def update_parent(self, context, o, simple=False):
        if o is not None:
            d = archipack_terrain.datablock(o.parent)
            if d is not None:
                cutables = [o.parent]
                self.filter_cutables(context, o, cutables)
                for c in cutables:
                    with ensure_select_and_restore(context, c, [c]) as (ctx, act, sel):
                        d.update(ctx, simple=simple)
                self.store_cutables(o, cutables)

    def update(self, context, manipulable_refresh=False, update_parent=True, simple=False):
        """
         Does update parent make sense at all ?
         as cutter changes must always update parent
        """
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        logger.debug("archipack_road.update() n_parts:%s" % self.n_parts)

        # clean up manipulators before any data model change
        if self.manipulable_refresh:
            self.manipulable_disable(o)

        verts = []
        edges = []

        self.prepare_update(context, o)

        g = self.get_generator()
        g.locate_manipulators(self)

        # vertex index in order to build axis
        g.line.get_verts(verts, edges)

        if len(verts) > 2:
            self.make_surface(o, verts, edges)

        # update parent on direct edit
        if self.manipulable_refresh or update_parent:
            self.update_parent(context, o, simple)

        self.update_dimensions(context, o)
        self.restore_context(context)


class archipack_road_segment(ArchipackCutterPart, PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)
    dz: FloatProperty(
        name="Altitude",
        description="Altitude of the axis point",
        default=0,
        unit='LENGTH', subtype='DISTANCE'
    )
    width_left:  FloatProperty(
        name="Width left",
        description="Tile width",
        min=0.01,
        default=0.3,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    width_right: FloatProperty(
        name="Width right",
        description="Width of right",
        min=0.01,
        default=0.3,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    @property
    def parent_data(self):
        return self.id_data.archipack_road[0]

    def draw(self, context, layout, index, draw_type=False, closed=False):

        icon = "TRIA_RIGHT"
        if self.expand:
            icon = "TRIA_DOWN"

        row = layout.row(align=True)
        self.draw_prop(context, layout, row, self, 'expand', icon=icon, postfix=str(index + 1), emboss=True)

        if draw_type:
            self.draw_prop(context, layout, row, self, "type_ui", text="")
            
        if self.expand:
            self.draw_insert(context, layout, index, closed)
            self.draw_prop(context, layout, layout, self, "dz")
            if self.type == 1:
                self.draw_prop(context, layout, layout, self, "r_ui")
                self.draw_prop(context, layout, layout, self, "da_ui")
            else:
                self.draw_prop(context, layout, layout, self, "l_ui")
            self.draw_prop(context, layout, layout, self, "a_ui")


class archipack_road(ArchipackObject, ArchipackCutter, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display road settings', 'NONE', 0),
            ('PARTS', 'Axis', 'Display road segments settings', 'NONE', 1),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 2)
        ),
        default='MAIN',
    )
    parts: CollectionProperty(type=archipack_road_segment)
    always_closed = False
    y: FloatProperty(
        name="Width",
        description="Width of the road",
        min=0.01,
        default=4,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    border: FloatProperty(
        name="Border",
        description="Width of the road borders",
        min=0.01,
        default=3,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    transition: FloatProperty(
        name="Transition",
        description="Soft transition size",
        min=0.01,
        default=0.2,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    # Note no translation needed
    border_falloff_type: EnumProperty(
        name="Falloff Type",
        items=(
            ('LINEAR', "Linear", "How weights are mapped to their new values", 'LINCURVE', 0),
            ('SHARP', "Sharp", "How weights are mapped to their new values", 'SHARPCURVE', 1),
            ('SMOOTH', "Smooth", "How weights are mapped to their new values", 'SMOOTHCURVE', 2),
            ('ROOT', "Root", "How weights are mapped to their new values", 'ROOTCURVE', 3),
            ('ICON_SPHERECURVE', "Sphere", "How weights are mapped to their new values", 'SPHERECURVE', 4),
            ('RANDOM', "Random", "How weights are mapped to their new values", 'RNDCURVE', 5),
            ('STEP', "Median Step", "How weights are mapped to their new values", 'NOCURVE', 6)
        ),
        default="LINEAR",
        update=update_falloff
    )
    idmat: IntVectorProperty(
        default=[1],
        size=1
    )
    material_user: EnumProperty(
        options={'SKIP_SAVE'},
        name="Road",
        items=c_mat_enum,
        get=mat_index_getter(MAT_USER),
        set=mat_index_setter(MAT_USER),
        update=update
    )
    material_override: BoolProperty(
        name="Override material",
        default=False,
        update=update
    )
    smooth_altitude: BoolProperty(
        name="Smooth altitudes",
        default=False,
        update=update
    )
    smooth_sample_size: FloatProperty(
        name="Sample spacing",
        description="Smooth sample spacing",
        min=0.1,
        default=5.0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    
    def get_generator(self, o=None):
        g = RoadGenerator(o)
        # g.operation = self.operation
        g.add_parts(self)
        z0 = 0
        for p, _k in zip(self.parts, g.segs):
            _k._z = z0
            z0 += p.dz

        y = 0.5 * self.y
        
        g.line = g.make_offset(y)
        right = g.make_offset(-y)
        right.reverse()
        g.line.segs[-1]._next = right.segs[0]
        g.line.segs[0]._last = right.segs[-1]
        right.segs[0]._last = g.line.segs[-1]
        right.segs[-1]._next = g.line.segs[0]
        g.line.segs.extend(right.segs)

        # extended surface for transition vertex colors
        g.transition = g.line.make_offset(-min(max(0.01, self.transition), (y - 0.01)))

        # road surface (as cutter only)
        # extended surface (as cutter and shrinkwrap target)
        g.border = g.line.make_offset(self.border)

        # detect intersections of borders, use a mod 2 rule
        _segs = g.border.segs
        coordsys = CoordSys([], generator=g)
        rep = {}
        tree = Q_tree(coordsys, max_depth=5, max_items=10)
        for i, seg in enumerate(_segs):
            count, selection = tree.intersects_seg(seg)
            for idx in selection:
                _s = tree._geoms[idx]
                res, p, u, v = _s.intersect_ext(seg)
                if res:
                    rep[idx] = i
            tree.insert_seg(seg, seg)

        # linear interpolation between crossing segments
        end = 0
        for i, _s in enumerate(_segs):
            if i < end:
                continue
            if i in rep:
                v = _s.p0
                end = rep[i] + 1
                nv = end - i
                dv = (_segs[end].p0 - v) / nv
                for j in range(1, nv):
                    _segs[i + j].p0 = v + j * dv

        return g

    def update_parts_from_generator(self, g):
        _parts = self.parts
        for _p, _k in zip(_parts, g.segs):
            _p.dz = _k.dz

    def update_parent(self, context, o, simple=False):
        if o is not None:
            d = archipack_terrain.datablock(o.parent)
            if d is not None:
                cutables = [o.parent]
                self.filter_cutables(context, o, cutables)
                for c in cutables:
                    with ensure_select_and_restore(context, c, [c]) as (ctx, act, sel):
                        d.update(ctx, simple=simple)
                self.store_cutables(o, cutables)

    def get_child_type(self, o, typ):
        """ Return child tagged with archipack_typ
        :param o:
        :param typ:
        :return:
        """
        res = None
        for c in o.children:
            if typ in c:
                res = c
                break
        return res

    def ensure_direction(self, o=None):
        """ Override because operation based direction here doesnt make any sense
        :param o:
        :return:
        """
        g = self.get_generator(o)
        return g

    def from_spline(self, context, o, curve, ccw=False, cw=False):
        """ Override because operation based direction here doesnt make any sense
        :param context:
        :param o:
        :param curve:
        :param ccw:
        :param cw:
        :return:
        """
        self.auto_update = False
        g = ArchipackUserDefinedPath.from_spline(self,
                                             context,
                                             o,
                                             curve,
                                             ccw=ccw,
                                             cw=cw
                                             )

        if self.smooth_altitude:
            g.smooth_altitude(sub_samples=self.smooth_sample_size)
            g.update_parts(self)

        self.auto_update = True

        self.update_parent(context, o)

    def make_axis(self, context, o, g):
        # o.parent is Terrain, o is road
        p = o.parent
        c = self.get_child_type(o, "archipack_road_axis")

        if c is None:
            m = bpy.data.meshes.new("Road Axis")
            c = bpy.data.objects.new("Road Axis", m)
            c.parent = o
            self.link_object_to_scene(context, c, layer_name="Terrain")

        c['archipack_road_axis'] = True

        c.matrix_world = o.matrix_world.copy()
        # archipack_terrain.datablock(p)
        # store axis bm
        itM = o.matrix_world.inverted()
        pz0 = itM @ (p.matrix_world @ Vector(p.bound_box[0]))
        pz1 = itM @ (p.matrix_world @ Vector(p.bound_box[1]))
        z0 = pz0.z - 5.0
        z1 = pz1.z + 5.0

        c.hide_set(True)
        c.hide_render = True
        c.display.show_shadows = False

        verts = []
        tmp = []   # [_k.p0 for _k in g.segs]

        g.get_verts(tmp)

        for v in tmp:
            x, y, z = v
            verts.extend([Vector((x, y, z0)), Vector((x, y, z1))])
        nv = int(len(verts) / 2 - 1)
        faces = [(2 * i, 2 * i + 1, 2 * i + 3, 2 * i + 2) for i in range(nv)]
        bmed.buildmesh(c, verts, faces)
        return o

    def make_road_wrap(self, context, o, g):
        # o.parent is Terrain, o is road
        c = self.get_child_type(o, "archipack_road_wrap")

        if c is None:
            m = bpy.data.meshes.new("Road Wrap")
            c = bpy.data.objects.new("Road Wrap", m)
            c.parent = o
            self.link_object_to_scene(context, c, layer_name="Terrain")

        c['archipack_road_wrap'] = True
        c.matrix_world = o.matrix_world.copy()
        verts = []
        verts2 = []
        faces = []

        # vertex index in order to build axis
        g.line.get_verts(verts2)

        # vertex index in order to build axis
        g.border.get_verts(verts)

        z0 = 0
        for i, p in enumerate(self.parts):
            verts[i].z = z0
            verts2[i].z = z0
            verts[-(1 + i)].z = z0
            verts2[-(1 + i)].z = z0
            z0 += p.dz

        verts.extend(verts2)

        nv = len(verts)

        if nv > 2:
            # 0 1    f0 f1
            # 4 5    f2 f3
            # 7 6    f4 f5
            # 3 2    f6 f7
            np = int(nv / 2)
            ns = int(np / 2) - 1

            for i in range(ns):
                f0, f1 = i, i + 1
                f2, f3 = f0 + np, f1 + np
                f4, f5 = nv - i - 1, nv - i - 2
                f6, f7 = f4 - np, f5 - np
                faces.extend([
                    (f0, f1, f3, f2),
                    (f2, f3, f5, f4),
                    (f4, f5, f7, f6)
                ])

            bmed.buildmesh(c, verts, faces)

            c.display_type = 'WIRE'
            c.hide_set(True)
            c.hide_render = True
            c.display.show_shadows = False

    def update(self, context, manipulable_refresh=False, update_parent=True, simple=False):

        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        logger.debug("archipack_road.update() n_parts:%s" % self.n_parts)

        # clean up manipulators before any data model change
        if self.manipulable_refresh:
            self.manipulable_disable(o)

        # self.update_parts()

        verts = []
        edges = []

        self.prepare_update(context, o)

        g = self.get_generator()
        g.locate_manipulators(self, side=(0.5 * self.y))

        # vertex index in order to build axis
        g.line.get_verts(verts)

        edges = [[i, i + 1] for i in range(len(verts) - 1)]

        z0 = 0
        for i, p in enumerate(self.parts):
            verts[i].z = z0
            verts[-(1 + i)].z = z0
            z0 += p.dz

        self.make_surface(o, verts, edges)

        self.make_axis(context, o, g)
        self.make_road_wrap(context, o, g)

        # update parent on direct edit
        if self.manipulable_refresh or update_parent:
            self.update_parent(context, o, simple)

        self.update_dimensions(context, o)
        self.restore_context(context)



# ------------------------------------------------------------------
# Define panel class to show object parameters in ui panel (N)
# ------------------------------------------------------------------


class archipack_terrain(ArchipackObject, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display main settings', 'NONE', 0),
            ('SUBS', 'Modifiers', 'Display road / earthwork settings', 'NONE', 1),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 2)
        ),
        default='MAIN',
    )
    sources: CollectionProperty(type=archipack_terrain_source)
    childs: CollectionProperty(type=archipack_terrain_child)

    slope_max: FloatProperty(
        name='Slope max',
        unit='ROTATION',
        subtype='ANGLE',
        min=0, max=1.5707963267948966,
        default=1.5707963267948966, precision=5,
        description='Maximum slope allowed for face', update=update,
    )
    slope_min: FloatProperty(
        name='Slope min',
        unit='ROTATION',
        subtype='ANGLE',
        min=0, max=1.5707963267948966,
        default=0, precision=5,
        description='Minimum slope allowed for face', update=update,
    )
    max_edge_length: FloatProperty(
        name='Max edge length',
        unit='LENGTH',
        subtype='DISTANCE',
        min=0.001, default=10000.0, precision=5,
        description='Maximum length for edge, delete edges shorter than this value', update=update,
    )
    min_dist: FloatProperty(
        name='Distance min',
        unit='LENGTH',
        subtype='DISTANCE',
        min=0.001, default=5.0, precision=5,
        description='Minimum distance allowed between input points in 2d', update=update,
    )
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update
    )
    use_tree: BoolProperty(
        name="Filter input",
        description="Use a spacial tree to filter out oversampled data / down sample through distance min",
        default=True,
        update=update
    )
    max_items: IntProperty(
        min=5, max=100,
        default=30,
        update=update
    )
    max_depth: IntProperty(
        min=1, max=20,
        default=5,
        update=update
    )
    beautify_fill: BoolProperty(
        name="Beauty",
        description="Beauty faces",
        default=False,
        update=update
    )
    idmat: IntVectorProperty(
        default=[0, 1, 2, 3],
        size=4
    )
    material_terrain: EnumProperty(
        options={'SKIP_SAVE'},
        name="Terrain",
        items=mat_enum,
        get=mat_index_getter(MAT_TERRAIN),
        set=mat_index_setter(MAT_TERRAIN),
        update=update
    )
    material_road: EnumProperty(
        options={'SKIP_SAVE'},
        name="Road",
        items=mat_enum,
        get=mat_index_getter(MAT_ROAD),
        set=mat_index_setter(MAT_ROAD),
        update=update
    )
    material_earthwork: EnumProperty(
        options={'SKIP_SAVE'},
        name="Earthwork",
        items=mat_enum,
        get=mat_index_getter(MAT_EARTHWORK),
        set=mat_index_setter(MAT_EARTHWORK),
        update=update
    )
    material_area: EnumProperty(
        options={'SKIP_SAVE'},
        name="Area",
        items=mat_enum,
        get=mat_index_getter(MAT_AREA),
        set=mat_index_setter(MAT_AREA),
        update=update
    )
    use_transition: BoolProperty(
        name="Soft transition",
        description="Use soft transition",
        default=False,
        update=update
    )
    method: EnumProperty(
        items=(
            ("SINGLE", "Process each child alone", "Process each child alone", 0),
            ("GROUP", "Process all childs at once", "Process all childs at once", 1)
        ),
        default="SINGLE"
    )
    use_fast_boolean: BoolProperty(
        name="Fast boolean",
        description="Use fast boolean to cut boundary and holes, might not be as safe as slow one",
        default=True,
        update=update
    )

    def setup_manipulators(self):
        n_manips = len(self.manipulators)
        if n_manips < 1:
            s = self.manipulators.add()
            s.prop1_name = "z"
            s.type_key = 'SIZE'
            s.normal = Vector((0, 1, 0))
        if n_manips < 2:
            s = self.manipulators.add()
            s.prop1_name = "x"
            s.prop2_name = "x"
            s.type_key = "SNAP_SIZE_LOC"
        if n_manips < 3:
            s = self.manipulators.add()
            s.prop1_name = "y"
            s.prop2_name = "y"
            s.type_key = "SNAP_SIZE_LOC"

    def get_generator(self, o=None):
        g = TerrainGenerator(o)
        return g

    def buildmesh(self, context, o):

        HAS_SCIPY = False
        try:
            from scipy.spatial import Delaunay
            import numpy as np
            HAS_SCIPY = True
        except ImportError:
            pass

        itM = o.matrix_world.inverted()
        verts = []
        sources = []

        logger.debug("**********************")
        logger.debug("buildmesh() %s depth:%s items:%s" % (
            self.method,
            self.max_depth,
            self.max_items
        ))

        for source in self.sources:

            src = self.get_scene_object(context, source.src)

            if src is None:
                continue

            tM = itM @ src.matrix_world

            if src.type == 'MESH':
                pts = src.data.vertices
                verts.extend([tM @ p.co for p in pts])

            elif src.type == 'CURVE':
                for spline in src.data.splines:
                    if spline.type == 'POLY':
                        pts = spline.points
                    elif spline.type == 'BEZIER':
                        pts = spline.bezier_points
                    else:
                        continue
                    verts.extend([tM @ p.co.to_3d() for p in pts])
            else:
                continue

            sources.append(src)

        n_verts = len(verts)

        if n_verts < 3:
            bmed.emptymesh(o)
            return

        t = time.time()

        coordsys = CoordSys(sources, itM=itM)

        faces = []

        if self.use_tree:
            """
            if HAS_SCIPY:

                points_3d = np.array(verts)
                tree = cKDTree(points_3d, leafsize=10)
                found = set()
                keep = []
                for i, p in enumerate(points_3d):
                    if i not in found:
                        res = tree.query_ball_point(p, self.min_dist)
                        found.update(res)
                        keep.append(verts[i])

                verts = keep

                logger.debug("buildmesh() build tree  %s/%s %.2f %.2f" % (
                    len(verts),
                    n_verts,
                    time.time() - t,
                    (time.time() - t) / n_verts
                ))
                n_verts = len(verts)

            else:
            """
            # Use a spacial tree to ensure items are uniques on xy plane
            # filter out oversampled data / down sample through min dist

            # NOTE: good values for max_items is around 50
            # half_dist = 0.5 * self.min_dist

            # Use blender's kd tree even 4x faster than scipy one
            tree = KDTree(n_verts)
            for i, v in enumerate(verts):
                tree.insert(v, i)
            tree.balance()

            found = set()
            keep = []
            for i, p in enumerate(verts):
                if i not in found:
                    for (co, index, dist) in tree.find_range(p, self.min_dist):
                        found.add(index)
                    keep.append(p)

            verts = keep

            logger.debug("buildmesh() build tree  %s/%s %.2f sec" % (
                len(verts),
                n_verts,
                time.time() - t
            ))
            n_verts = len(verts)
        else:

            nDupli, nZcolinear = unique(verts)

            # Check colinear
            x, y, z = list(zip(*verts))
            if checkEqual(x) or checkEqual(y):
                return
            logger.debug("buildmesh() filtering %.2f sec" % (time.time() - t))

        if len(verts) < 3:
            return

        t = time.time()

        if HAS_SCIPY:

            points_2D = np.array([[v.x, v.y] for v in verts])
            logger.debug("buildmesh() Triangulate using scipy %s points ... %.2f sec" % (n_verts, time.time() - t))
            # Triangulate
            tri = Delaunay(points_2D)
            faces = tri.simplices.tolist()

        else:
            try:
                faces = computeDelaunayTriangulation(verts)
                # reverse point order --> if all triangles are specified anticlockwise then all faces up
                faces = [tuple(reversed(tri)) for tri in faces]
            except:
                import traceback
                traceback.print_exc()
                return
            logger.debug("buildmesh() Triangulate using fallback %s points ... %.2f sec" % (n_verts, time.time() - t))

        n_faces = len(faces)

        mat = self.id_mat(MAT_TERRAIN)
        matids = [mat] * n_faces
        vcolors = [(0, 0, 0, 1)] * n_faces
        bmed.buildmesh(o, verts, faces, matids, vcolors=vcolors)

        # cleanup
        bm = bmed._start(o)

        t = time.time()

        geom = [ed for ed in bm.edges if ed.calc_length() > self.max_edge_length]
        if len(geom) > 0:
            bmesh.ops.delete(bm, geom=geom, context='EDGES')

        logger.debug("buildmesh() cleanup %.2f sec" % (time.time() - t))
        bmed._end(bm, o)

        # cutters
        t = time.time()

        g = self.get_generator()
        g.cut(context, o, self, False)

        if o.visible_get():

            for hole, operation, fast in zip(g.holes, g.operations, g.use_fast):

                if self.use_fast_boolean and fast:
                    self.bool_fast_cut(context, o, [hole], mode=operation)
                else:
                    # fallback to slow but safe method
                    g.cut_hole(o, hole, cleanup=True, delete=True)
        else:
            # fallback to slow method but working when object is hidden
            bm = bmed._start(o)
            g.cut_holes(bm, g)
            bmed._end(bm, o)

        logger.debug("buildmesh() cut holes %.2f sec" % (time.time() - t))

        return coordsys

    def get_child_type(self, o, typ):
        """ Return child tagged with archipack_typ
        :param o:
        :param typ:
        :return:
        """
        res = None
        for c in o.children:
            if typ in c:
                res = c
                break
        return res

    def bool_fast_cut(self, context, o, lines, mode="SLICE"):
        """Fast and reliable way to slice / cut mesh  object must be visible !
        """
        # keep inside = intersect and swap
        # keep outside = difference
        # vertical surface
        z0 = o.bound_box[0][2] - 5
        z1 = o.bound_box[1][2] + 5

        to_join = []

        for hole in lines:
            pts = []
            # closest part
            hole.get_pts(pts)

            verts = []

            for v in pts:
                x, y, z = v
                verts.extend([Vector((x, y, z0)), Vector((x, y, z1))])

            nv = int(len(verts) / 2 - 1)
            faces = [(2 * i, 2 * i + 1, 2 * i + 3, 2 * i + 2) for i in range(nv)]
            faces.append((-2, -1, 1, 0))

            bm = bmed.buildmesh(None, verts, faces, temporary=True)
            bmed.select(bm, True, True, True)

            to_join.append(bm)

        bmed.bmesh_join(o, to_join, normal_update=False)

        # fast, but require visible object !!!
        with ensure_select_and_restore(context, o, [o], object_mode="EDIT"):
            if mode == 'SLICE':
                bpy.ops.mesh.intersect(mode='SELECT_UNSELECT', separate_mode="CUT", threshold=1e-06)
            else:
                # keep inside = INTERSECT and swap
                # keep outside = DIFFERENCE
                if mode == "INTERSECTION":
                    mode = "INTERSECT"
                bpy.ops.mesh.intersect_boolean(operation=mode, use_swap=mode=='INTERSECT')

         # remove cutter temp mesh
        bm = bmed._start(o)
        faces = [f for f in bm.faces if abs(f.normal.z) < 0.001]
        if len(faces) > 0:
            bmesh.ops.delete(bm, geom=faces, context="FACES")

        verts = [v for v in bm.verts if v.co.z == z1 or v.co.z == z0]
        if len(verts) > 0:
            bmesh.ops.delete(bm, geom=verts, context="VERTS")

        if mode == 'SLICE':
            bmesh.ops.dissolve_degenerate(bm, edges=bm.edges, dist=0.01)
            bmesh.ops.remove_doubles(bm, verts=bm.verts, dist=0.01)
            bmesh.ops.triangulate(bm, faces=bm.faces, quad_method='BEAUTY', ngon_method='BEAUTY')
        # tag new edges
        bmed._end(bm, o)

    def prepare_uvs_quads(self, cg, coordsys):
        # segment du bord
        #       p3  u  p2 left
        # start v      v    end
        #       p0  u  p1 right
        # where v_norm points inside
        tree = Q_tree(coordsys, max_items=self.max_items, max_depth=self.max_depth)
        _segs = cg.line.segs
        # !! début et fin sont perpendiculaires
        u0 = 0
        n_segs = int(len(_segs) / 2) - 1
        for i in range(n_segs):
            right = _segs[i].copy()
            left  = _segs[-2 - i].copy()
            start = Line(left.p1, last=left, after=right)
            end = Line(right.p1, last=right, after=left)
            mats = []
            _ks =[start, left, end, right]
            # print(["(%.2f %.2f)" % (_k.p0.x, _k.p0.y) for _k in _ks])
            for _k in _ks:
                x, y, z = _k._p0
                x0, x1, x2 = _k.v_normalized
                y0, y1, y2 = -_k.v_normal
                mats.append(
                    Matrix([
                    [x0, y0, 0, x],
                    [x1, y1, 0, y],
                    [0, 0, 1, 0],
                    [0, 0, 0, 1]
                    ]).inverted()
                )

            du = 0.5 * (left.length + right.length)
            dat = (mats, u0, du)
            u0 += du
            tree.insert_quad([_k._p0 for _k in _ks], dat, extend=0.01)

        return tree

    def uv_unwrap_road(self, tree, p):
        count, selection = tree.intersects_co(p)
        # print(p, count)
        for idx in selection:
            mats, u0, du = tree._geoms[idx]
            d = [(m @ p).y for m in mats]
            # only check for start / end as faces are already inside
            outside = any([_d < -0.05 for _d in [d[0], d[2]]])
            if not outside:
                d1, d2, d3, d4 = d
                u = d1 / (d1 + d3)
                v = d2 / (d2 + d4)
                return (u0 + du * u, v)
        return (0, 0)

    def ensure_vcolors(self, bm):
        vcolor_layer = bm.loops.layers.color.get("Archipack")
        if vcolor_layer is None:
            vcolor_layer = bm.loops.layers.color.new("Archipack")
            logger.debug("ensure_vcolors() layer not found")
            for f in bm.faces:
                for loop in f.loops:
                    loop[vcolor_layer] = (0, 0, 0, 1)
        return vcolor_layer

    def update_road(self, context, o, c, cur, coordsys):

        d = archipack_road.datablock(c)

        if coordsys is not None:

            itM = o.matrix_world.inverted()
            g = self.get_generator()

            cg = d.get_generator(itM @ c.matrix_world)

            # road transition (inside road itself
            if self.use_transition:
                g.holes.append(cg.transition)

            # road itself
            g.holes.append(cg.line)

            # road external borders
            g.holes.append(cg.border)

            self.bool_fast_cut(context, o, g.holes, mode="SLICE")

            if d.material_override:
                mat_road = self.id_mat(MAT_USER, source=d)
            else:
                mat_road = self.id_mat(MAT_ROAD)

            t = time.time()
            tree = self.prepare_uvs_quads(cg, coordsys)

            logger.debug("update_road() unwrap build tree (%s) %.2f" % (len(tree._geoms), time.time() - t))

            bm = bmed._start(o)
            bmed.ensure_bmesh(bm)

            t = time.time()
            # create uv_layers for world and normalized
            uv_layer = bm.loops.layers.uv.get("Road_normalized")
            if uv_layer is None:
                uv_layer = bm.loops.layers.uv.new("Road_normalized")

            uv2_layer = bm.loops.layers.uv.get("Road_world")
            if uv2_layer is None:
                uv2_layer = bm.loops.layers.uv.new("Road_world")

            vcolor_layer = self.ensure_vcolors(bm)

            # multiply u coord by this to get normalized u coord
            # without, u and v are metric
            w = d.y
            transition = w / d.transition
            cleanup_verts = set()
            for f in bm.faces:
                center = f.calc_center_median()
                count, selection = tree.intersects_co(center)

                for idx in selection:
                    mats, u0, du = tree._geoms[idx]
                    # use 0.005 to fix precision issues
                    outside = any([(m @ center).y < -0.005 for m in mats])
                    if not outside:

                        for j, loop in enumerate(f.loops):
                            u, v = self.uv_unwrap_road(tree, loop.vert.co)
                            loop[uv_layer].uv = (u / w, v)
                            loop[uv2_layer].uv = (u, v * w)
                            if self.use_transition:
                                # use smallest possible
                                lr = loop[vcolor_layer][0]
                                r = max(lr, min(1.0, transition * (0.5 - abs(v - 0.5))))
                                # r = min(lr, 1.0 - min(1.0, transition * (0.5 - abs(v - 0.5))))
                                dt = (d.transition + 0.005) / w
                            else:
                                r = 1.0
                                dt = 0.005 / w

                            # remove inside verts
                            if r == 1.0 and 1 - dt > v > dt:
                                cleanup_verts.add(loop.vert.index)

                            loop[vcolor_layer] = (r, 0, 0, 1)
                        f.material_index = mat_road
                        # tag faces for post processing
                        f.tag = True

            # remove inside verts
            verts = [bm.verts[i] for i in cleanup_verts]
            if len(verts) > 0:
                bmesh.ops.dissolve_verts(bm, verts=verts, use_face_split=False, use_boundary_tear=False)
                bmed.ensure_bmesh(bm)
                faces = [f for f in bm.faces if f.tag]
                bmesh.ops.triangulate(bm, faces=faces, quad_method='BEAUTY', ngon_method='BEAUTY')
                bmed.ensure_bmesh(bm)
                faces = [f for f in bm.faces if f.tag]

                sides = set()
                middle = set()
                if self.use_transition:
                    for f in faces:
                        center = f.calc_center_median()
                        count, selection = tree.intersects_co(center)
                        f.tag = False

                        for idx in selection:
                            mats, u0, du = tree._geoms[idx]
                            if any([(m @ center).y < d.transition for m in [mats[1], mats[3]]]):
                                sides.add(f.index)
                            else:
                                middle.add(f.index)
                else:
                    middle = set([f.index for f in faces])

                for faces_set in [sides, middle]:
                    faces = []
                    edges = set()
                    for f_id in faces_set:
                        f = bm.faces[f_id]
                        faces.append(f)
                        for ed in f.edges:
                            edges.add(ed.index)

                    edges = [bm.edges[i] for i in edges]
                    bmesh.ops.beautify_fill(bm, faces=faces, edges=edges, use_restrict_tag=False, method='AREA') # , method='AREA'
                    bmed.ensure_bmesh(bm)

            logger.debug("update_road() unwrap %.2f" % (time.time() - t))

            bmed._end(bm, o)

        if len(c.children) > 0:
            axis = self.get_child_type(c, "archipack_road_axis")
            if axis is not None:
                y0 = 0.5 * d.y
                y1 = y0 + d.border
                self.add_vgroup_mod(o, cur, axis, d.border_falloff_type, y0, y1, "Road")

            wrap = self.get_child_type(c, "archipack_road_wrap")
            if wrap is not None:
                self.add_shrinkwrap_mod(o, cur, wrap, "Road")

    def get_slices(self, itM, c, d, slices):

        g = d.get_generator(itM @ c.matrix_world)

        # transition
        if self.use_transition:
            slices.append(g.transition)

        # basis
        slices.append(g.line)

        if hasattr(g, 'border') and g.border is not None:
            # external borders
            slices.append(g.border)

        return g

    def build_faces_tree(self, bm, coordsys):
        t = time.time()
        faces_tree = Q_tree(coordsys, max_depth=self.max_depth, max_items=self.max_items)

        for f in bm.faces:
            faces_tree.insert_face(f, f, extend=0)

        logger.debug("build_faces_tree (%s) %.2f" % (len(faces_tree._geoms), time.time() - t))
        return faces_tree

    def update_earthwork(self, context, o, c, cur, coordsys):

        d = archipack_terrain_earthwork.datablock(c)

        if coordsys is not None:

            itM = o.matrix_world.inverted()
            g = self.get_generator()

            if d.material_override:
                mat = self.id_mat(MAT_USER, source=d)
            else:
                mat = self.id_mat(MAT_EARTHWORK)

            cg = d.get_generator(itM @ c.matrix_world)
            g.holes.append(cg.line)
            g.holes.append(cg.border)

            if self.use_transition and d.border != d.transition:
                g.holes.append(cg.transition)

            self.bool_fast_cut(context, o, g.holes, mode="SLICE")

            # transition does not realy make sense
            # unless we also set vertex colors
            bm = bmed._start(o)

            vcolor_layer = self.ensure_vcolors(bm)

            # build faces tree
            t = time.time()
            tree = Q_tree(coordsys, max_depth=self.max_depth, max_items=self.max_items)

            for f in bm.faces:
                tree.insert_face(f, f, extend=0)

            logger.debug("update_earthwork() build faces tree (%s) %.2f" % (len(tree._geoms), time.time() - t))

            t = time.time()
            # next: get faces in boundary
            self.process_inside_faces(g, cg, tree, vcolor_layer, (0, 1, 0, 1), mat)

            # same as with roads:
            # use a tree made of quads from parallel segs to setup transition
            if self.use_transition:
                t = time.time()
                tree = self.prepare_vcolor_quads(cg, coordsys)
                logger.debug("update_earthwork() material_prepare_quads (%s) %.2f" % (len(tree._geoms), time.time() - t))

                t = time.time()

                for f in bm.faces:
                    center = f.calc_center_median()
                    count, selection = tree.intersects_co(center)

                    for idx in selection:
                        mats = tree._geoms[idx]
                        outside = any([(m @ center).y < -FACE_PRECISION for m in mats])
                        if not outside:
                            for j, loop in enumerate(f.loops):
                                col = self.soft_border_vcolors(tree, loop.vert.co)
                                # use smallest possible
                                r, _g, b = loop[vcolor_layer][0:3]
                                loop[vcolor_layer] = (r, col, b, 1)
                            f.material_index = mat

                logger.debug("update_earthwork() vcolors %.2f" % (time.time() - t))

            bmed._end(bm, o)

        if len(c.children) > 0:
            border = self.get_child_type(c, "archipack_earthwork_border")
            if border is not None:
                self.add_vgroup_mod(o, cur, border, d.border_falloff_type, 0, d.border, "Earthwork")
            wrap = self.get_child_type(c, "archipack_earthwork_wrap")
            if wrap is not None:
                self.add_shrinkwrap_mod(o, cur, wrap, "Earthwork")
            area = self.get_child_type(c, "archipack_earthwork_area")
            if area is not None:
                self.add_shrinkwrap_mod(o, cur, area, "Earthwork", vgroup=False)

    def process_road(self, bm, g, d, cg, coordsys, faces_tree, vcolor_layer):
        bmed.ensure_bmesh(bm)
        if d.material_override:
            mat_road = self.id_mat(MAT_USER, source=d)
        else:
            mat_road = self.id_mat(MAT_ROAD)

        t = time.time()
        tree = self.prepare_uvs_quads(cg, coordsys)
        logger.debug("process_road() prepare_uvs_quads (%s) %.2f" % (len(tree._geoms), time.time() - t))

        t = time.time()
        # create uv_layers for world and normalized
        uv_layer = bm.loops.layers.uv.get("Road_normalized")
        if uv_layer is None:
            uv_layer = bm.loops.layers.uv.new("Road_normalized")

        uv2_layer = bm.loops.layers.uv.get("Road_world")
        if uv2_layer is None:
            uv2_layer = bm.loops.layers.uv.new("Road_world")

        # multiply u coord by this to get normalized u coord
        # without, u and v are metric
        w = d.y
        transition = w / d.transition
        cleanup_verts = set()
        for f in bm.faces:
            center = f.calc_center_median()
            count, selection = tree.intersects_co(center)

            for idx in selection:
                mats, u0, du = tree._geoms[idx]
                # use 0.005 to fix precision issues
                outside = any([(m @ center).y < -FACE_PRECISION for m in mats])
                if not outside:
                    for j, loop in enumerate(f.loops):
                        u, v = self.uv_unwrap_road(tree, loop.vert.co)
                        loop[uv_layer].uv = (u / w, v)
                        loop[uv2_layer].uv = (u, v * w)
                        if self.use_transition:
                            # use smallest possible
                            lr = loop[vcolor_layer][0]
                            r = max(lr, min(1.0, transition * (0.5 - abs(v - 0.5))))
                            # r = min(lr, 1.0 - min(1.0, transition * (0.5 - abs(v - 0.5))))
                            dt = transition
                        else:
                            r = 1.0
                            dt = 0.01 / w
                        # remove inside verts
                        if r == 1.0 and 1 - dt > v > dt:
                            cleanup_verts.add(loop.vert.index)

                        loop[vcolor_layer] = (r, 0, 0, 1)
                    f.material_index = mat_road

        # remove inside verts
        verts = [bm.verts[i] for i in cleanup_verts]
        if len(verts) > 0:
            bmesh.ops.dissolve_verts(bm, verts=verts, use_face_split=False, use_boundary_tear=False)
            bmesh.ops.triangulate(bm, faces=bm.faces, quad_method='BEAUTY', ngon_method='BEAUTY')
        logger.debug("process_road() unwrap %.2f" % (time.time() - t))

    def process_earthwork(self, bm, g, d, cg, coordsys, faces_tree, vcolor_layer):

        if d.material_override:
            mat = self.id_mat(MAT_USER, source=d)
        else:
            mat = self.id_mat(MAT_EARTHWORK)

        self.process_inside_faces(g, cg, faces_tree, vcolor_layer, (0, 1, 0, 1), mat)

        # same as with roads:
        # use a tree made of quads from parallel segs to setup transition
        if self.use_transition:
            t = time.time()
            tree = self.prepare_vcolor_quads(cg, coordsys)
            logger.debug(
                "process_earthwork() prepare_vcolor_quads (%s) %.2f" % (len(tree._geoms), time.time() - t))

            t = time.time()

            for f in bm.faces:
                center = f.calc_center_median()
                count, selection = tree.intersects_co(center)

                for idx in selection:
                    mats = tree._geoms[idx]
                    outside = any([(m @ center).y < -FACE_PRECISION for m in mats])
                    if not outside:
                        for j, loop in enumerate(f.loops):
                            col = self.soft_border_vcolors(tree, loop.vert.co)
                            # use smallest possible
                            r, _g, b = loop[vcolor_layer][0:3]
                            loop[vcolor_layer] = (r, col, b, 1)
                        f.material_index = mat

            logger.debug("process_earthwork() vcolors %.2f" % (time.time() - t))

    def process_inside_faces(self, g, cg, faces_tree, vcolor_layer, vcolor, idmat):
        t = time.time()
        x0, y0, x1, y1 = cg.line.bounding_rect()
        g.xsize = x1 - x0
        selection = list(faces_tree._intersect((x0, y0, x1, y1)))
        selection.sort()
        faces = [faces_tree._geoms[idx] for idx in selection]
        logger.debug("process_inside_faces() select faces (%s) %.2f" % (len(faces), time.time() - t))
        # set material index and kill vcolor
        t = time.time()
        for f in faces:
            if g.inside(
                    f.calc_center_median(),
                    segs=cg.line.segs):
                f.material_index = idmat
                for loop in f.loops:
                    # kill others inside
                    loop[vcolor_layer] = vcolor

        logger.debug("process_inside_faces() assign faces %.2f" % (time.time() - t))

    def process_material(self, bm, g, d, cg, coordsys, faces_tree, vcolor_layer):

        if d.material_override:
            mat = self.id_mat(MAT_USER, source=d)
        else:
            mat = self.id_mat(MAT_AREA)

        self.process_inside_faces(g, cg, faces_tree, vcolor_layer, (0, 0, 1, 1), mat)

        # same as with roads:
        # use a tree made of quads from parallel segs to setup transition
        if self.use_transition:
            t = time.time()
            tree = self.prepare_vcolor_quads(cg, coordsys)
            logger.debug("process_material() material_prepare_quads (%s) %.2f" % (len(tree._geoms), time.time() - t))

            t = time.time()

            for f in bm.faces:
                center = f.calc_center_median()
                count, selection = tree.intersects_co(center)

                for idx in selection:
                    mats = tree._geoms[idx]
                    # use 0.005 to fix precision issues
                    outside = any([(m @ center).y < -FACE_PRECISION for m in mats])
                    if not outside:
                        for j, loop in enumerate(f.loops):
                            col = self.soft_border_vcolors(tree, loop.vert.co)
                            # preserve others on transition
                            r, _g, b = loop[vcolor_layer][0:3]
                            loop[vcolor_layer] = Vector((r, _g, col, 1))
                        f.material_index = mat

            logger.debug("process_material() vcolors %.2f" % (time.time() - t))

    def prepare_vcolor_quads(self, cg, coordsys):
        # segment du bord
        #       p3  u  p2 left
        # start v      v    end
        #       p0  u  p1 right
        # where v_norm points inside
        tree = Q_tree(coordsys, max_items=self.max_items, max_depth=self.max_depth)

        for _right, _left,  in zip(cg.line.segs, cg.transition.segs):
            left = _left.opposite
            right = _right.copy()
            start = Line(left.p1, last=left, after=right)
            end = Line(right.p1, last=right, after=left)
            mats = []
            _ks =[start, left, end, right]
            # print(["(%.2f %.2f)" % (_k.p0.x, _k.p0.y) for _k in _ks])
            for _k in _ks:
                x, y, z = _k._p0
                x0, x1, x2 = _k.v_normalized
                y0, y1, y2 = -_k.v_normal
                mats.append(
                    Matrix([
                    [x0, y0, 0, x],
                    [x1, y1, 0, y],
                    [0, 0, 1, 0],
                    [0, 0, 0, 1]
                    ]).inverted()
                )
            tree.insert_quad([_k._p0 for _k in _ks], mats, extend=0.01)

        return tree

    def soft_border_vcolors(self, tree, p):
        count, selection = tree.intersects_co(p)
        # print(p, count)
        for idx in selection:
            mats = tree._geoms[idx]
            d = [(m @ p).y for m in mats]
            # only check for start / end as faces are already inside
            outside = any([_d < -0.05 for _d in [d[0], d[2]]])
            if not outside:
                d1, d2, d3, d4 = d
                v = d2 / (d2 + d4)
                return v
        return 0

    def update_material(self, context, o, c, cur, coordsys):

        d = archipack_terrain_material.datablock(c)

        if coordsys is not None:

            itM = o.matrix_world.inverted()
            g = self.get_generator()

            if d.material_override:
                mat = self.id_mat(MAT_USER, source=d)
            else:
                mat = self.id_mat(MAT_AREA)

            cg = d.get_generator(itM @ c.matrix_world)
            g.holes.append(cg.line)

            if self.use_transition:
                g.holes.append(cg.transition)

            self.bool_fast_cut(context, o, g.holes, mode="SLICE")

            # transition does not really make sense
            # unless we also set vertex colors
            bm = bmed._start(o)

            vcolor_layer = self.ensure_vcolors(bm)

            t = time.time()
            tree = Q_tree(coordsys, max_depth=self.max_depth, max_items=self.max_items)
            for f in bm.faces:
                tree.insert_face(f, f, extend=0)

            self.process_inside_faces(g, cg, tree, vcolor_layer, (0, 0, 1, 1), mat)

            # same as with roads:
            # use a tree made of quads from parallel segs to setup transition
            if self.use_transition:
                t = time.time()
                tree = self.prepare_vcolor_quads(cg, coordsys)
                logger.debug("update_material() material_prepare_quads (%s) %.2f" % (len(tree._geoms), time.time() - t))
                # for geom in tree._geoms:
                #    print(geom[0:4])

                t = time.time()

                for f in bm.faces:
                    center = f.calc_center_median()
                    count, selection = tree.intersects_co(center)

                    for idx in selection:
                        mats = tree._geoms[idx]
                        # use 0.005 to fix precision issues
                        outside = any([(m @ center).y < -FACE_PRECISION for m in mats])
                        if not outside:
                            for j, loop in enumerate(f.loops):
                                col = self.soft_border_vcolors(tree, loop.vert.co)
                                # preserve others on transition
                                r, _g, b = loop[vcolor_layer][0:3]
                                loop[vcolor_layer] = Vector((r, _g, col, 1))
                            f.material_index = mat

                logger.debug("update_material() vcolors %.2f" % (time.time() - t))

            bmed._end(bm, o)

    def add_shrinkwrap_mod(self, o, cur, target, name, vgroup=True):
        mod = o.modifiers.new("{}_{}-wrap".format(name, cur), 'SHRINKWRAP')
        if vgroup:
            mod.vertex_group = name
        mod.target = target
        mod.invert_vertex_group = vgroup
        mod.wrap_method = 'PROJECT'
        mod.use_project_x = False
        mod.use_project_y = False
        mod.use_project_z = True
        mod.use_negative_direction = True
        mod.use_positive_direction = True

    def add_vgroup_mod(self, o, cur, target, falloff_type, min_dist, max_dist, name):
        mod = o.modifiers.new("{}_{}-weight".format(name, cur), 'VERTEX_WEIGHT_PROXIMITY')
        mod.vertex_group = name
        mod.target = target
        mod.min_dist = min_dist
        mod.max_dist = max_dist
        mod.falloff_type = falloff_type
        mod.proximity_mode = 'GEOMETRY'
        mod.proximity_geometry = {'FACE'}

    def add_weighted_normal_mod(self, o, name):
        mod = o.modifiers.new(name, 'WEIGHTED_NORMAL')
        mod.vertex_group = name

    def remove_wrap(self, o):
        # replace modifiers
        for mod in reversed(o.modifiers):
            if mod.type in {'VERTEX_WEIGHT_PROXIMITY', 'SHRINKWRAP', 'WEIGHTED_NORMAL'}:
                o.modifiers.remove(mod)

    def update(self, context, simple=False):

        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        # do not update if invisible
        if not o.visible_get():
            self.restore_context(context)
            return

        to_remove = []
        t = time.time()
        found_childs = set()

        # add vertex groups
        vg_road = o.vertex_groups.get("Road")
        if vg_road is None:
            vg_road = o.vertex_groups.new(name="Road")

        vg_earth = o.vertex_groups.get("Earthwork")
        if vg_earth is None:
            vg_earth = o.vertex_groups.new(name="Earthwork")

        cur = 0
        if simple:
            self.remove_wrap(o)
            for i, child in enumerate(self.childs):
                c = self.get_scene_object(context, child.src)
                if c is None:
                    to_remove.append(i)
                elif child.active:
                    if archipack_road.filter(c):
                        cur += 1
                        self.update_road(context, o, c, cur, None)
                    elif archipack_terrain_earthwork.filter(c):
                        cur += 1
                        self.update_earthwork(context, o, c, cur, None)
                    elif archipack_terrain_material.filter(c):
                        cur += 1
                        self.update_material(context, o, c, cur, None)

        elif self.method == "SINGLE":

            throttle.add(context, o, self)
            if not throttle.is_active(o.name):
                self.remove_wrap(o)
                coordsys = self.buildmesh(context, o)
                for i, child in enumerate(self.childs):

                    c = self.get_scene_object(context, child.src)
                    if c is None or child.src in found_childs:
                        to_remove.append(i)

                    elif child.active:

                        if archipack_road.filter(c):
                            cur += 1
                            self.update_road(context, o, c, cur, coordsys)
                        elif archipack_terrain_earthwork.filter(c):
                            cur += 1
                            self.update_earthwork(context, o, c, cur, coordsys)
                        elif archipack_terrain_material.filter(c):
                            cur += 1
                            self.update_material(context, o, c, cur, coordsys)
                    found_childs.add(child.src)
            
                # setup vertex groups
                verts = [v.index for v in o.data.vertices]
                vg_road.add(verts, 1, 'REPLACE')
                vg_earth.add(verts, 1, 'REPLACE')

        else:
            throttle.add(context, o, self)
            if not throttle.is_active(o.name):
                self.remove_wrap(o)
                coordsys = self.buildmesh(context, o)
                # method 2 slice before analysis
                active_childs = []
                itM = o.matrix_world.inverted()

                for i, child in enumerate(self.childs):
                    if child.active:
                        c = self.get_scene_object(context, child.src)
                        if c is not None:
                            for key in {"archipack_road",
                                        "archipack_terrain_earthwork",
                                        "archipack_terrain_material"}:
                                if key in c.data:
                                    d = getattr(c.data, key)[0]
                                    slices = []
                                    g = self.get_slices(itM, c, d, slices)
                                    active_childs.append((c, d, g, key))
                                    self.bool_fast_cut(context, o, slices, mode="SLICE")

                # for slice in slices:
                #    self.bool_fast_cut(context, o, slices, mode="SLICE")

                bm = bmed._start(o)

                g = self.get_generator()
                faces_tree = self.build_faces_tree(bm, coordsys)
                vcolor_layer = self.ensure_vcolors(bm)

                for c, d, cg, key in active_childs:
                    if key == "archipack_road":
                        self.process_road(bm, g, d, cg, coordsys, faces_tree, vcolor_layer)
                    elif key == "archipack_terrain_earthwork":
                        self.process_earthwork(bm, g, d, cg, coordsys, faces_tree, vcolor_layer)
                    elif key == "archipack_terrain_material":
                        self.process_material(bm, g, d, cg, coordsys, faces_tree, vcolor_layer)

                bmed._end(bm, o)

                # setup vertex groups
                verts = [v.index for v in o.data.vertices]
                vg_road.add(verts, 1, 'REPLACE')
                vg_earth.add(verts, 1, 'REPLACE')

                # modifiers
                cur = 0
                for c, d, cg, key in active_childs:
                    if len(c.children) > 0:
                        if key == "archipack_road":
                            cur += 1
                            axis = self.get_child_type(c, "archipack_road_axis")
                            if axis is not None:
                                y0 = 0.5 * d.y
                                y1 = y0 + d.border
                                self.add_vgroup_mod(o, cur, axis, d.border_falloff_type, y0, y1, "Road")

                            wrap = self.get_child_type(c, "archipack_road_wrap")
                            if wrap is not None:
                                self.add_shrinkwrap_mod(o, cur, wrap, "Road")

                        elif key == "archipack_terrain_earthwork":
                            cur += 1
                            border = self.get_child_type(c, "archipack_earthwork_border")
                            if border is not None:
                                self.add_vgroup_mod(o, cur, border, d.border_falloff_type, 0, d.border, "Earthwork")
                            wrap = self.get_child_type(c, "archipack_earthwork_wrap")
                            if wrap is not None:
                                self.add_shrinkwrap_mod(o, cur, wrap, "Earthwork")
                            area = self.get_child_type(c, "archipack_earthwork_area")
                            if area is not None:
                                self.add_shrinkwrap_mod(o, cur, area, "Earthwork", vgroup=False)

        self.shade_smooth(context, o, 1.3089969389957472)

        for i in reversed(to_remove):
            self.childs.remove(i)

        self.add_weighted_normal_mod(o, "Earthwork")

        logger.debug("buildmesh() update: %.2f" % (time.time() - t))

        self.restore_context(context)


class ARCHIPACK_PT_terrain(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_terrain"
    bl_label = "Terrain"
    bl_description = "Archipack Terrain"

    @classmethod
    def poll(cls, context):
        return archipack_terrain.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_terrain.datablock(o)

        if d is None:
            return

        layout = self.layout

        self.draw_common(context, layout)
        box=layout.box()

        self.draw_prop(context, layout, box, d, 'auto_update', icon="AUTO")

        # grouped method has proven not to be as fast as expected
        # self.draw_prop(context, layout, layout, d, 'method')

        self.draw_prop(context, layout, layout, d, 'tabs', expand=True)

        box = layout.box()
        if d.tabs == 'MAIN':

            self.draw_op(context, layout, box, "archipack.terrain_add_source", icon="ADD")
            for i, src in enumerate(d.sources):
                src.draw(context, box, i)

            box = layout.box()
            # self.draw_prop(context, layout, box, d, 'beautify_fill', icon="MOD_SMOOTH")
            self.draw_prop(context, layout, box, d, 'use_tree', icon="FILTER")
            if d.use_tree:
                self.draw_label(context, layout, box, "Filter input")
                self.draw_prop(context, layout, box, d, 'min_dist')
                # self.draw_prop(context, layout, box, d, 'max_items')
                # self.draw_prop(context, layout, box, d, 'max_depth')

            box = layout.box()
            self.draw_label(context, layout, box, "Clean up")
            # self.draw_prop(context, layout, box, d, 'slope_max')
            # self.draw_prop(context, layout, box, d, 'slope_min')
            self.draw_prop(context, layout, box, d, 'max_edge_length')
            self.draw_prop(context, layout, layout, d, 'use_fast_boolean')

        elif d.tabs == 'SUBS':
            curve = ""
            for c in context.selected_objects:
                if c.type == "CURVE":
                    curve = c.name

            self.draw_label(context, layout, box, "Add Terrain Modifiers")
            op = self.draw_op(context, layout, box, "archipack.terrain_cutter", icon="MOD_BOOLEAN")
            op.parent = o.name
            op.curve = curve
            op = self.draw_op(context, layout, box, "archipack.road", icon="IPO_BACK")
            op.parent = o.name
            op.curve = curve
            op = self.draw_op(context, layout, box, "archipack.terrain_earthwork", icon="STYLUS_PRESSURE")
            op.parent = o.name
            op.curve = curve
            op = self.draw_op(context, layout, box, "archipack.terrain_material", icon="MATERIAL")
            op.parent = o.name
            op.curve = curve

            box = layout.box()
            self.draw_label(context, layout, box, "Terrain Modifiers")
            self.draw_prop(context, layout, box, d, 'use_transition')
            for i, child in enumerate(d.childs):
                child.draw(context, layout, box, i)

        elif d.tabs == 'MATERIALS':
            if "archipack_material" in o:
                o.archipack_material[0].draw(context, box)
            box = layout.box()
            self.draw_prop(context, layout, box, d, 'material_terrain')
            self.draw_prop(context, layout, box, d, 'material_road')
            self.draw_prop(context, layout, box, d, 'material_earthwork')
            self.draw_prop(context, layout, box, d, 'material_area')


class ARCHIPACK_OT_terrain(ArchipackCreateTool, Operator):
    bl_idname = "archipack.terrain"
    bl_label = "Terrain"
    bl_description = "Create Terrain at cursor location"

    def create(self, context, src):
        m = bpy.data.meshes.new(self.bl_label)
        o = bpy.data.objects.new(self.bl_label, m)
        d = m.archipack_terrain.add()

        for c in src:
            if c.type in {'MESH', 'CURVE'}:
                p = d.sources.add()
                p.src = c.name

        self.link_object_to_scene(context, o, layer_name="Terrain")

        if len(src) > 0:
            o.matrix_world = Matrix.Translation(src[0].matrix_world.translation)

        o.color = (0, 1, 0, 1)

        # select and make active
        self.select_object(context, o, True)

        self.add_material(context, o, material="DEFAULT")
        self.load_preset(d)

        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":

            src = context.selected_objects

            bpy.ops.object.select_all(action="DESELECT")
            o = self.create(context, src)
            # select and make active
            self.add_to_reference(context, o)
            self.select_object(context, o, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_terrain_add_source(Operator):
    bl_idname = "archipack.terrain_add_source"
    bl_label = "Add point cloud"
    bl_description = "Add a source point cloud or curve"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}

    @classmethod
    def poll(self, context):
        return archipack_terrain.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_terrain.datablock(o)
        d.sources.add()
        return {'FINISHED'}


class ARCHIPACK_OT_terrain_remove_source(Operator):
    bl_idname = "archipack.terrain_remove_source"
    bl_label = "Remove point cloud"
    bl_description = "Remove a source point cloud or curve"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}

    index: IntProperty(default=0)

    @classmethod
    def poll(self, context):
        return archipack_terrain.poll(context.active_object)

    def execute(self, context):
        o = context.active_object
        d = archipack_terrain.datablock(o)
        d.sources.remove(self.index)
        d.update(context)
        return {'FINISHED'}


class ARCHIPACK_PT_terrain_cutter(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_terrain_cutter"
    bl_label = "Terrain Cutter"

    @classmethod
    def poll(cls, context):
        return archipack_terrain_cutter.poll(context.active_object)

    def draw(self, context):
        d = archipack_terrain_cutter.datablock(context.active_object)
        if d is None:
            return
        layout = self.layout

        self.draw_common(context, layout)
        self.draw_op(context, layout, layout, "archipack.select_parent", icon="RESTRICT_SELECT_OFF", text="Select Terrain")
        d.draw(context, layout, draw_offset=True)
        self.draw_prop(context, layout, layout, d, 'use_fast_boolean')


class ARCHIPACK_OT_terrain_cutter(ArchipackCreateTool, Operator):
    bl_idname = "archipack.terrain_cutter"
    bl_label = "Terrain Cutter"
    bl_description = "Terrain Cutter"

    parent: StringProperty("")
    curve: StringProperty("")

    def create(self, context, parent):
        m = bpy.data.meshes.new("Terrain Cutter")
        o = bpy.data.objects.new("Terrain Cutter", m)
        d = m.archipack_terrain_cutter.add()
        d.manipulable_selectable = True

        logger.debug("ARCHIPACK_OT_terrain_cutter.create() %s  %s", self.parent, parent)

        if parent is not None:
            o.parent = parent
            td = archipack_terrain.datablock(parent)
            p = td.childs.add()
            p.src = o.name
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
        self.link_object_to_scene(context, o, layer_name="Terrain")
        o.color = (1, 0, 0, 1)

        # select and make active
        self.select_object(context, o, True)
        self.load_preset(d)
        update_operation(d, context)
        return o, d

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            with stop_auto_manipulate(context):
                parent = self.get_scene_object(context, self.parent)
                curve = self.get_scene_object(context, self.curve)

                if curve is not None:
                    for i, spline in enumerate(curve.data.splines):
                        o, d = self.create(context, parent)
                        d.user_defined_spline = i
                        d.user_defined_path = curve.name
                else:
                    o, d = self.create(context, parent)
                    d.update(context)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_PT_road(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_road"
    bl_label = "Road"

    @classmethod
    def poll(cls, context):
        return archipack_road.poll(context.active_object)

    def draw(self, context):
        d = archipack_road.datablock(context.active_object)
        if d is None:
            return
        layout = self.layout

        self.draw_common(context, layout)
        self.draw_op(context, layout, layout, "archipack.select_parent", icon="RESTRICT_SELECT_OFF", text="Select Terrain")
        self.draw_prop(context, layout, layout, d, "tabs", expand=True)

        box = layout.box()
        if d.tabs == 'MAIN':
            self.draw_prop(context, layout, box, d, 'y')
            self.draw_prop(context, layout, box, d, 'border')
            self.draw_prop(context, layout, box, d, 'border_falloff_type')
            self.draw_prop(context, layout, box, d, 'transition')

        elif d.tabs == 'PARTS':
            d.template_user_path(context, box, focus=False)
            self.draw_prop(context, layout, box, d, 'smooth_altitude')
            self.draw_prop(context, layout, box, d, 'smooth_sample_size')
            # while support for arc is available should remains disabled
            d.template_parts(context, layout, draw_type=False)

        elif d.tabs == 'MATERIALS':
            self.draw_prop(context, layout, box, d, 'material_override')
            self.draw_prop(context, layout, box, d, 'material_user')


class ARCHIPACK_OT_road(ArchipackCreateTool, Operator):
    bl_idname = "archipack.road"
    bl_label = "Road"
    bl_description = "Road"

    parent: StringProperty("")
    curve: StringProperty("")

    def create(self, context, parent):
        m = bpy.data.meshes.new(self.bl_label)
        o = bpy.data.objects.new(self.bl_label, m)
        d = m.archipack_road.add()
        d.manipulable_selectable = True
        logger.debug("ARCHIPACK_OT_terrain_cutter.create() %s  %s", self.parent, parent)

        if parent is not None:
            o.parent = parent

            td = archipack_terrain.datablock(parent)
            p = td.childs.add()
            p.src = o.name

            bbox = parent.bound_box
            angle_90 = pi / 2
            x0, y0, z = bbox[0]
            x1, y1, z = bbox[6]
            x = max(1, 0.2 * (x1 - x0))

            o.matrix_world = parent.matrix_world @ Matrix.Translation(Vector((-3 * x, 0, 0)))
            # d.auto_update = False
            d.set_parts(1)
            for i, p in enumerate(d.parts):
                p.a0 = angle_90
                p.length = x
            d.parts[0].a0 = - angle_90
            # d.auto_update = True
        else:
            o.location = self.get_cursor_location(context)

        # Link object into scene
        self.link_object_to_scene(context, o, layer_name="Terrain")
        o.color = (1, 0, 0, 1)

        # select and make active
        self.select_object(context, o, True)
        self.load_preset(d)
        update_operation(d, context)

        return o, d

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            with stop_auto_manipulate(context):
                parent = self.get_scene_object(context, self.parent)
                curve = self.get_scene_object(context, self.curve)

                if curve is not None:
                    for i, spline in enumerate(curve.data.splines):
                        o, d = self.create(context, parent)
                        d.user_defined_spline = i
                        d.user_defined_path = curve.name
                else:
                    o, d = self.create(context, parent)
                    d.update(context)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_PT_terrain_earthwork(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_terrain_earthwork"
    bl_label = "Earthwork"

    @classmethod
    def poll(cls, context):
        return archipack_terrain_earthwork.poll(context.active_object)

    def draw(self, context):
        d = archipack_terrain_earthwork.datablock(context.active_object)
        if d is None:
            return
        layout = self.layout

        self.draw_common(context, layout)
        self.draw_op(context, layout, layout, "archipack.select_parent", icon="RESTRICT_SELECT_OFF", text="Select Terrain")
        self.draw_prop(context, layout, layout, d, "tabs", expand=True)

        box = layout.box()
        if d.tabs == 'MAIN':
            self.draw_prop(context, layout, box, d, 'offset')
            self.draw_prop(context, layout, box, d, 'border')
            self.draw_prop(context, layout, box, d, 'border_falloff_type')
            self.draw_prop(context, layout, box, d, 'transition')

        elif d.tabs == 'PARTS':
            d.template_user_path(context, box, focus=False)
            d.template_parts(context, layout, draw_type=False)

        elif d.tabs == 'MATERIALS':
            self.draw_prop(context, layout, box, d, 'material_override')
            self.draw_prop(context, layout, box, d, 'material_user')


class ARCHIPACK_OT_terrain_earthwork(ArchipackCreateTool, Operator):
    bl_idname = "archipack.terrain_earthwork"
    bl_label = "Earthwork"
    bl_description = "Create Earthwork"

    parent: StringProperty("")
    curve: StringProperty("")

    def create(self, context, parent):
        m = bpy.data.meshes.new(self.bl_label)
        o = bpy.data.objects.new(self.bl_label, m)
        d = m.archipack_terrain_earthwork.add()
        d.manipulable_selectable = True

        logger.debug("ARCHIPACK_OT_terrain_earthwork.create() %s  %s", self.parent, parent)

        if parent is not None:

            td = archipack_terrain.datablock(parent)
            p = td.childs.add()
            p.src = o.name

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
        self.link_object_to_scene(context, o, layer_name="Terrain")
        o.color = (1, 0, 0, 1)

        # select and make active
        self.select_object(context, o, True)
        self.load_preset(d)
        update_operation(d, context)

        return o, d

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            with stop_auto_manipulate(context):
                parent = self.get_scene_object(context, self.parent)
                curve = self.get_scene_object(context, self.curve)

                if curve is not None:
                    for i, spline in enumerate(curve.data.splines):
                        o, d = self.create(context, parent)
                        d.user_defined_spline = i
                        d.user_defined_path = curve.name
                else:
                    o, d = self.create(context, parent)
                    d.update(context)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_PT_terrain_material(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_terrain_material"
    bl_label = "Material"

    @classmethod
    def poll(cls, context):
        return archipack_terrain_material.poll(context.active_object)

    def draw(self, context):
        d = archipack_terrain_material.datablock(context.active_object)
        if d is None:
            return
        layout = self.layout

        self.draw_common(context, layout)
        self.draw_op(context, layout, layout, "archipack.select_parent", icon="RESTRICT_SELECT_OFF", text="Select Terrain")
        self.draw_prop(context, layout, layout, d, "tabs", expand=True)

        box = layout.box()
        if d.tabs == 'MAIN':
            self.draw_prop(context, layout, box, d, 'offset')
            self.draw_prop(context, layout, box, d, 'transition')

        elif d.tabs == 'PARTS':
            d.template_user_path(context, box, focus=False)
            d.template_parts(context, layout, draw_type=False)

        elif d.tabs == 'MATERIALS':
            self.draw_prop(context, layout, box, d, 'material_override')
            self.draw_prop(context, layout, box, d, 'material_user')


class ARCHIPACK_OT_terrain_material(ArchipackCreateTool, Operator):
    bl_idname = "archipack.terrain_material"
    bl_label = "Material"
    bl_description = "Create Material variation"

    parent: StringProperty("")
    curve: StringProperty("")

    def create(self, context, parent):
        m = bpy.data.meshes.new(self.bl_label)
        o = bpy.data.objects.new(self.bl_label, m)
        d = m.archipack_terrain_material.add()
        d.manipulable_selectable = True

        logger.debug("ARCHIPACK_OT_terrain_material.create() %s  %s", self.parent, parent)

        if parent is not None:

            td = archipack_terrain.datablock(parent)
            p = td.childs.add()
            p.src = o.name

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
        self.link_object_to_scene(context, o, layer_name="Terrain")
        o.color = (1, 0, 0, 1)

        # select and make active
        self.select_object(context, o, True)
        self.load_preset(d)
        update_operation(d, context)

        return o, d

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            with stop_auto_manipulate(context):
                parent = self.get_scene_object(context, self.parent)
                curve = self.get_scene_object(context, self.curve)

                if curve is not None:
                    for i, spline in enumerate(curve.data.splines):
                        o, d = self.create(context, parent)
                        d.user_defined_spline = i
                        d.user_defined_path = curve.name
                else:
                    o, d = self.create(context, parent)
                    d.update(context)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_terrain_child_up(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.terrain_child_up"
    bl_label = "Up"
    bl_description = "Move child up"

    index: IntProperty(default=-1)

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            d = archipack_terrain.datablock(o)

            c_names = [p.src for p in d.childs if self.get_scene_object(context, p.src) is not None]
            cur = c_names.pop(self.index)
            c_names.insert(self.index - 1, cur)

            d.childs.clear()
            for src in c_names:
                p = d.childs.add()
                p.src = src

            d.update(context)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_terrain_child_remove(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.terrain_child_remove"
    bl_label = "Remove"
    bl_description = "Remove child"

    index: IntProperty(default=-1)

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            d = archipack_terrain.datablock(o)
            c_name = d.childs[self.index].src

            c = self.get_scene_object(context, c_name)

            if c is not None:
                self.delete_object(context, c)

            d.childs.remove(self.index)
            d.update(context)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_terrain_child_down(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.terrain_child_down"
    bl_label = "Down"
    bl_description = "Move child down"

    index: IntProperty(default=-1)

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            d = archipack_terrain.datablock(o)

            c_names = [p.src for p in d.childs if self.get_scene_object(context, p.src) is not None]
            cur = c_names.pop(self.index)
            c_names.insert(self.index + 1, cur)

            d.childs.clear()
            for src in c_names:
                p = d.childs.add()
                p.src = src

            d.update(context)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


def register():
    bpy.utils.register_class(archipack_terrain_earthwork_segment)
    bpy.utils.register_class(archipack_terrain_earthwork)
    Mesh.archipack_terrain_earthwork = CollectionProperty(type=archipack_terrain_earthwork)
    bpy.utils.register_class(ARCHIPACK_PT_terrain_earthwork)
    bpy.utils.register_class(ARCHIPACK_OT_terrain_earthwork)

    bpy.utils.register_class(archipack_terrain_material_segment)
    bpy.utils.register_class(archipack_terrain_material)
    Mesh.archipack_terrain_material = CollectionProperty(type=archipack_terrain_material)
    bpy.utils.register_class(ARCHIPACK_PT_terrain_material)
    bpy.utils.register_class(ARCHIPACK_OT_terrain_material)

    bpy.utils.register_class(archipack_terrain_cutter_segment)
    bpy.utils.register_class(archipack_terrain_cutter)
    Mesh.archipack_terrain_cutter = CollectionProperty(type=archipack_terrain_cutter)
    bpy.utils.register_class(ARCHIPACK_PT_terrain_cutter)
    bpy.utils.register_class(ARCHIPACK_OT_terrain_cutter)

    bpy.utils.register_class(archipack_road_segment)
    bpy.utils.register_class(archipack_road)
    Mesh.archipack_road = CollectionProperty(type=archipack_road)
    bpy.utils.register_class(ARCHIPACK_PT_road)
    bpy.utils.register_class(ARCHIPACK_OT_road)
    bpy.utils.register_class(archipack_terrain_child)
    bpy.utils.register_class(archipack_terrain_source)
    bpy.utils.register_class(archipack_terrain)
    Mesh.archipack_terrain = CollectionProperty(type=archipack_terrain)
    bpy.utils.register_class(ARCHIPACK_PT_terrain)
    bpy.utils.register_class(ARCHIPACK_OT_terrain)
    bpy.utils.register_class(ARCHIPACK_OT_terrain_add_source)
    bpy.utils.register_class(ARCHIPACK_OT_terrain_remove_source)
    bpy.utils.register_class(ARCHIPACK_OT_terrain_child_down)
    bpy.utils.register_class(ARCHIPACK_OT_terrain_child_up)
    bpy.utils.register_class(ARCHIPACK_OT_terrain_child_remove)


def unregister():
    bpy.utils.unregister_class(ARCHIPACK_OT_terrain_earthwork)
    bpy.utils.unregister_class(ARCHIPACK_PT_terrain_earthwork)
    del Mesh.archipack_terrain_earthwork
    bpy.utils.unregister_class(archipack_terrain_earthwork)
    bpy.utils.unregister_class(archipack_terrain_earthwork_segment)

    bpy.utils.unregister_class(ARCHIPACK_OT_terrain_material)
    bpy.utils.unregister_class(ARCHIPACK_PT_terrain_material)
    del Mesh.archipack_terrain_material
    bpy.utils.unregister_class(archipack_terrain_material)
    bpy.utils.unregister_class(archipack_terrain_material_segment)

    bpy.utils.unregister_class(ARCHIPACK_OT_terrain_cutter)
    bpy.utils.unregister_class(ARCHIPACK_PT_terrain_cutter)
    del Mesh.archipack_terrain_cutter
    bpy.utils.unregister_class(archipack_terrain_cutter)
    bpy.utils.unregister_class(archipack_terrain_cutter_segment)

    bpy.utils.unregister_class(ARCHIPACK_OT_road)
    bpy.utils.unregister_class(ARCHIPACK_PT_road)

    bpy.utils.unregister_class(archipack_road)
    bpy.utils.unregister_class(archipack_road_segment)
    del Mesh.archipack_road

    bpy.utils.unregister_class(ARCHIPACK_OT_terrain_child_remove)
    bpy.utils.unregister_class(ARCHIPACK_OT_terrain_child_down)
    bpy.utils.unregister_class(ARCHIPACK_OT_terrain_child_up)
    bpy.utils.unregister_class(ARCHIPACK_OT_terrain_add_source)
    bpy.utils.unregister_class(ARCHIPACK_OT_terrain_remove_source)
    bpy.utils.unregister_class(ARCHIPACK_PT_terrain)
    bpy.utils.unregister_class(ARCHIPACK_OT_terrain)
    bpy.utils.unregister_class(archipack_terrain)
    bpy.utils.unregister_class(archipack_terrain_source)
    bpy.utils.unregister_class(archipack_terrain_child)
    del Mesh.archipack_terrain

