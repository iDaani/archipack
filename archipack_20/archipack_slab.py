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
# noinspection PyUnresolvedReferences
import bpy
# noinspection PyUnresolvedReferences
from bpy.types import Operator, PropertyGroup, Mesh, Panel
from bpy.props import (
    FloatProperty, BoolProperty, IntProperty, IntVectorProperty,
    StringProperty, EnumProperty,
    CollectionProperty
    )
import bmesh
from .bmesh_utils import BmeshEdit as bmed
from mathutils import Vector, Matrix
from math import pi
from .archipack_manipulator import Manipulable, archipack_manipulator
from .archipack_abstraction import ensure_select_and_restore
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackPanel,
    ArchipackCreateTool,
    ArchipackObject,
    ArchipackObjectsManager,
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
from .archipack_polylines import CoordSys
from .archipack_wall2 import Q_tree
from .archipack_material import build_mat_enum
from .archipack_iconmanager import icons as icon_man
from .archipack_throttle import throttle
from .archipack_prefs import get_prefs


import logging
logger = logging.getLogger("archipack")


X_AXIS = Vector((1, 0, 0))
Z_AXIS = Vector((0, 0, 1))


class SlabGenerator(CutAblePolygon, CutAbleGenerator):

    def __init__(self, o=None):
        CutAbleGenerator.__init__(self, o)

    def get_verts(self, verts):
        verts.extend([s.p0.to_3d() for s in self.segs])

    def cut(self, o, realtime):
        """
            either external or holes cuts
        """
        # use offset segs (self.segs = self.line.segs) as base
        self.as_lines(step_angle=0.0502)

        self.limits()
        if not realtime:
            itM = o.matrix_world.inverted()
            for b in o.children:
                d = archipack_slab_cutter.datablock(b)
                if d is not None:
                    tM = itM @ b.matrix_world
                    g = d.ensure_direction(tM)
                    self.slice(g)

    def slab(self, o, d):

        verts = []
        self.get_verts(verts)
        if len(verts) > 2:

            # init a Qtree with segs data (material indexes)
            itM = o.matrix_world.inverted()
            # wrong coordsys ! must use generator coords instead as center and limits
            coordsys = CoordSys([o], itM=itM)
            tree = Q_tree(coordsys, max_depth=8)

            for seg in self.segs:
                tree.insert_seg(seg, seg)

            # print("SlabGenerator.slab self.segs:", [s.idx for s in self.segs])

            for hole in self.holes:
                for seg in hole.segs:
                    tree.insert_seg(seg, seg)

            # ensure verts are CCW
            if self.is_cw:
                verts = list(reversed(verts))

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
            self.dissolve_limit(bm)
            geom = bm.faces[:]
            bmesh.ops.solidify(bm, geom=geom, thickness=d.z)
            bmesh.ops.translate(bm, vec=Vector((0,0,-d.z)), space=o.matrix_world, verts=bm.verts[:])

            # setup material indexes, this sucks, but there is no way to handle properly with solidify
            # also unwrap uvs
            _polys = bm.faces
            _top = d.id_mat(MAT_TOP)
            _bot = d.id_mat(MAT_BOTTOM)

            layer = bm.loops.layers.uv.verify()
            for poly in _polys:
                vz = poly.normal

                if vz.z > 0.5:
                    vx = X_AXIS
                    poly.material_index = _top
                elif vz.z < -0.5:
                    vx = X_AXIS
                    poly.material_index = _bot
                else:
                    vx = vz.cross(Z_AXIS)
                    found = False
                    for edge in poly.edges:
                        i0, i1 = edge.verts
                        _p0, _p1 = i0.co, i1.co
                        if abs(_p0.z - _p1.z) < 0.01:
                            # find associated edge in tree
                            count, selection = tree.intersects_pts(_p0, _p1, extend=0.01)
                            for idx in selection:
                                # may have sub segments so use approximate
                                seg = tree._geoms[idx]
                                dt = 0.01 / seg.length
                                res, _d, t = seg.point_sur_segment(_p0)
                                if 1 + dt > t > -dt and abs(_d) < 0.01:
                                    res, _d, t = seg.point_sur_segment(_p1)
                                    if 1 + dt > t > -dt  and abs(_d) < 0.01:
                                        found = True
                                if found:
                                    # print("normal", vz, "idmat", seg.idx, _p0, _p1)
                                    poly.material_index = seg.idx
                                    break
                        if found:
                            break

                p = poly.verts[0].co
                if vz.length < 0.5:
                    # fallback for faces with null normal
                    tM = Matrix.Translation(p)
                else:
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

            bmed._end(bm, o)


def update(self, context):
    self.update(context)


def update_manipulators(self, context):
    self.manipulable_refresh = True
    self.update(context, manipulable_refresh=True)


def update_path(self, context):
    self.update_path(context)


# Material indexes slots in idmat array
MAT_PART = 0
MAT_BOTTOM = 0
MAT_TOP = 1
MAT_SIDE = 2


material_enums = []
mat_enum, mat_index_getter, mat_index_setter= build_mat_enum('idmat', material_enums)


class archipack_slab_relocate_child(PropertyGroup):
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
    d0: FloatProperty(
        description="Distance from parent segment 0 the point is bound to"
    )
    d1: FloatProperty(
        description="Distance from parent segment 1 the point is bound to"
    )
    t: FloatProperty(
        description="Distance from start of closest segment normalized"
    )

    def filter_child(self, o):
        d = None
        if o and o.data and "archipack_fence" in o.data:
                d = o.data.archipack_fence[0]
        return d

    def get_child(self, context):
        d = None
        o = context.scene.objects.get(self.child_name)
        d = self.filter_child(o)
        return o, d


class archipack_slab_part(Archipacki18n, ArchipackSegment, PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_slab[0]

    def draw_insert(self, context, layout, index, closed):
        row = layout.row(align=True)
        self.draw_op(context, layout, row, "archipack.segment_insert", icon="ADD", text="").index = index
        self.draw_op(context, layout, row, "archipack.segment_remove", icon="REMOVE", text="").index = index
        self.draw_op(context, layout, row, "archipack.segment_make_first", text="Make 1").index = index
        self.draw_op(context, layout, row, "archipack.slab_balcony", text="Balcony").index = index


class archipack_slab(Archipacki18n, ArchipackUserDefinedPath, ArchipackObject, Manipulable, DimensionProvider, PropertyGroup):

    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        name='Ui tabs',
        description="Display settings",
        items=(
            ('MAIN', 'Main', 'Display main settings', 'NONE', 0),
            ('PARTS', 'Shape', 'Display slab segments settings', 'NONE', 1),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 2),
            ('TOOLS', '', 'Display tools', 'MODIFIER', 3)
        ),
        default='MAIN',
    )

    # boundary
    parts: CollectionProperty(type=archipack_slab_part)

    # UI layout related
    x_offset: FloatProperty(
        name="Offset",
        default=0.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    z: FloatProperty(
        name="Thickness",
        default=0.3, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    idmat: IntVectorProperty(
        default=[0, 1, 2],
        size=3
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
    material_sides: EnumProperty(
        options={'SKIP_SAVE'},
        name="Sides",
        items=mat_enum,
        get=mat_index_getter(MAT_SIDE),
        set=mat_index_setter( MAT_SIDE),
        update=update
    )
    # store childs with parts points locations to relocate
    reloc_childs: CollectionProperty(type=archipack_slab_relocate_child)

    # Flag to prevent mesh update while making bulk changes over variables
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update_manipulators
    )
    always_closed = True

    def get_generator(self, o=None):
        g = SlabGenerator(o)
        g.add_parts(self)
        g.line = g.make_offset(self.x_offset)

        _mat = self.id_mat(MAT_SIDE)
        for _k, _p in zip(g.line.segs, self.parts):
            if _p.material_override:
                _k.idx = _p.id_mat(MAT_PART)
            else:
                _k.idx = _mat

        # print("archipack_slab.get_generator", [_k.idx for _k in g.line.segs])

        return g

    def new_part(self, where, type, length, radius, offset, a0, da):
        idx = len(self.parts)
        p = self.parts.add()
        p.type = type
        p.length = length
        p.offset = offset
        p.da = da
        p.a0 = a0
        # self.n_parts += 1
        self.parts.move(idx, where + 1)
        return p

    def after_insert_part(self, context, o, where, distance):
        for c in self.reloc_childs:
            if c.wall_id0 > where:
                c.wall_id0 += 1
            if c.wall_id1 > where:
                c.wall_id1 += 1

    def insert_balcony(self, context, where):
        o = context.active_object
        self.manipulable_disable(o)
        self.auto_update = False

        # the part we do split
        part_0 = self.parts[where]
        part_0.length /= 3
        part_0.da /= 3

        # store here to keep a valid ref
        type = part_0.type
        length = part_0.length
        radius = part_0.radius
        offset = part_0.offset
        da = part_0.da

        # 1st part 90deg
        self.new_part(where, 0, 1.5, 0, 0, -pi / 2, da)

        # 2nd part -90deg
        self.new_part(where + 1, type, length, radius + 1.5, 0, pi / 2, da)

        # 3th part -90deg
        self.new_part(where + 2, 0, 1.5, 0, 0, pi / 2, da)

        # 4th part -90deg
        self.new_part(where + 3, type, length, radius, offset, -pi / 2, da)

        self.setup_manipulators()

        for c in self.reloc_childs:
            if c.wall_id0 > where:
                c.wall_id0 += 4
            if c.wall_id1 > where:
                c.wall_id1 += 4

        self.auto_update = True

        g = self.get_generator()
        _segs = g.line.segs

        bpy.ops.archipack.fence()
        c = context.active_object
        c.archipack_material[0].material = "DEFAULT"

        d = c.data.archipack_fence[0]
        d.set_parts(3)

        self.unselect_object(context, c)

        # link to o
        c.parent = o
        c.matrix_world = o.matrix_world.copy()
        c.location = _segs[where + 1].p0

        maxdist = 1e32
        coordsys = CoordSys([o])
        tree = Q_tree(coordsys, max_depth=8)

        # collect own walls segs
        # Init Tree for openings
        for i, seg in enumerate(_segs):
            tree.insert_seg(seg, seg)

        self.add_relocate_child(tree, c.name, 0, _segs[where + 1].p0, _segs, maxdist)
        self.add_relocate_child(tree, c.name, 1, _segs[where + 2].p0, _segs, maxdist)
        self.add_relocate_child(tree, c.name, 2, _segs[where + 3].p0, _segs, maxdist)
        self.add_relocate_child(tree, c.name, 3, _segs[where + 4].p0, _segs, maxdist)

        self.select_object(context, o, True)
        self.relocate_childs(context, o)

    def _find_relocate_childs(self, o, childs):
        d = archipack_slab_relocate_child.filter_child(self, o)
        if d:
            childs[o.name] = (o, d)

    def find_relocate_childs(self, o):
        # find childs and sort by kind
        childs = {}
        for c in o.children:
            self._find_relocate_childs(c, childs)
        return childs

    def add_generator(self, name, c, d, generators, tM=None, force=False):
        if name != "" and c and (name not in generators or force):
            if tM is None:
                tM = c.matrix_world

            g = d.get_generator(tM)

            generators[name] = [
                c,
                d,
                g,
                tM.inverted(),
                False
                ]

    def setup_childs(self, context, o):
        """
            Store childs
            create manipulators
            call after a boolean oop

            Unlike other routines,
            generators use world coordsys
        """

        # logger.debug("Setup_childs %s", o.name)

        tim = time.time()
        self.reloc_childs.clear()

        if o.parent is None:
            return 0

        childs = self.find_relocate_childs(o)

        # retrieve objects to init quadtree
        objs = [o]
        for name, child in childs.items():
            c, cd = child
            objs.append(c)

        # init a quadtree to minimize intersections tests on setup
        coordsys = CoordSys(objs)
        tree = Q_tree(coordsys, max_depth=8)

        g = self.get_generator(o)

        segs = g.segs # g.line.segs
        # [s for s in g.line.segs]

        # Init Tree for openings
        for i, seg in enumerate(segs):
            tree.insert_seg(seg, seg)

        # curved segments are not supported
        for p in self.parts:
            if p.type == 1:
                return

        maxdist = 1.0
        if len(childs) > 0:
            # Explicit rule for slab, using wall segs only with larger dist
            # might use same rule for roof boundary cutters
            for name, child in childs.items():
                c, cd = child
                cd.setup_manipulators()
                cg = cd.get_generator(c)
                for c_idx, seg in enumerate(cg.segs):
                    # point in world coordsys
                    p = seg.p0
                    self.add_relocate_child(
                            tree,
                            c.name,
                            c_idx,
                            p,
                            segs,
                            maxdist)

        logger.debug("Setup childs end %s %.4f seconds\n", o.name, (time.time() - tim))

    def add_relocate_child(
            self,
            tree,
            name,
            c_idx,
            pt,
            all_segs,
            maxdist):
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

        # logger.debug("tree.intersects(%s) :%.4f seconds",  count, (time.time() - tim))

        for parent_idx in selection:
            seg = tree._geoms[parent_idx]
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
                closest.append((d2, -dist, parent_idx, t))

        # get 2 closest segments
        # childs walls use projection of themself through seg_idx

        if len(closest) > 1:
            closest.sort(key=lambda s: s[0])
            c = self.reloc_childs.add()
            c.child_name = name
            c.child_idx = c_idx
            d, d1, i1, t1 = closest[0][0:4]
            # try to exclude colinear segments
            i = 1
            s0 = all_segs[i1]
            d2, i2 = closest[1][1:3]
            n_closest = len(closest) - 1
            a = abs(all_segs[i2].delta_angle(s0))
            while i < n_closest and (a > 3.1405 or a < 0.001):
                if closest[i][0] < d:
                    d, d1, i1, t1 = closest[i][0:4]
                    s0 = all_segs[i1]
                i += 1
                d2, i2 = closest[i][1:3]
                a = abs(all_segs[i2].delta_angle(s0))

            c.d0, c.seg0, c.t = d1, i1, t1
            c.d1, c.seg1 = d2, i2

    def post_relocate(self, context):
        o = self.find_in_selection(context)
        if o is not None:
            logger.debug("post_relocate %s", o.name)
            self.relocate_childs(context, o)
            self.restore_auto_manipulate(context)

    def relocate_childs(self, context, o):
        """
            Move and resize childs after wall edition
            childs here are either doors or windows
            Unlike other routines,
            generators use world coordsys
            T childs walls only update doors and windows
        """
        tim = time.time()

        logger.debug("Relocate_childs %s", o.name)
        g = self.get_generator(o)

        tM = o.matrix_world
        loc, rot, scale = tM.decompose()

        # Generators: object, datablock, generator, mat world inverse, dirty flag
        generators = {}

        # build a dict for each child
        soft = {}

        # process childs in the right order
        child_names = []
        for child in self.reloc_childs:
            child_name = child.child_name
            if child_name not in soft:
                c, d = child.get_child(context)
                if c is None:
                    continue
                child_names.append(child_name)
                soft[child_name] = []
                self.add_generator(child_name, c, d, generators)

            soft[child_name].append(child)

        logger.debug("Relocate_childs generators() :%.4f seconds", time.time() - tim)

        n_changes = 0

        for name in child_names:
            child = soft[name]
            # logger.debug("Relocate_childs start :%.2f seconds", time.time() - tim)
            # logger.debug("Relocate_childs %s child:%s", o.name, name)
            c, d, cg, itM, dirty = generators[name]

            # apply changes to generator
            n_segs = len(cg.segs)
            changed = False
            for cd in child:

                # find closest segments: use offset of main wall for t childs
                s0 = g.line.segs[cd.seg0].offset(cd.d0)
                s1 = g.line.segs[cd.seg1].offset(cd.d1)
                # p in world coordsys
                res, p, u, v = s0.intersect_ext(s1)

                # XXX p never is 0 !!
                if res is not None:
                    if cd.child_idx < n_segs:
                        if (cg.segs[cd.child_idx].p0 - p).length > 0.001:
                            n_changes += 1
                            changed = True
                        cg.segs[cd.child_idx].p0 = p
                        if cd.child_idx > 0:
                            cg.segs[cd.child_idx - 1].p1 = p
                        else:
                            d.move_object(c, p.to_3d())
                            # flag generator as dirty
                            generators[name][4] = True
                    else:
                        if (cg.segs[cd.child_idx - 1].p1 - p).length > 0.001:
                            n_changes += 1
                            changed = True
                        cg.segs[cd.child_idx - 1].p1 = p

            if changed:
                # update data from generator
                last = None
                cg.update_parts(d)

                # logger.debug("Relocate_childs change :%.2f seconds", time.time() - tim)
                self.select_object(context, c, True)
                d.update(context)
                # logger.debug("Relocate_childs update :%.2f seconds", time.time() - tim)
                self.unselect_object(context, c)

        logger.debug("Relocate_childs(%s of %s) :%.4f seconds", n_changes, len(self.reloc_childs), time.time() - tim)

        self.select_object(context, o, True)

    def after_remove_part(self, context, o, where):
        """
         Rebuild childs index and location
         When removing a part
        """
        for c in self.reloc_childs:
            if c.wall_id0 >= where:
                c.wall_id0 -= 1
            if c.wall_id1 >= where:
                c.wall_id1 -= 1

    def setup_manipulators(self):

        if len(self.manipulators) < 1:
            s = self.manipulators.add()
            s.type_key = "SIZE"
            s.prop1_name = "z"
            s.normal = Vector((0, 1, 0))

        self.setup_parts_manipulators('z')

    def from_spline(self, context, o, curve, ccw=False, cw=False):
        ArchipackUserDefinedPath.from_spline(self, context, o, curve, ccw=True)

    def make_surface(self, o, verts):
        bm = bmesh.new()
        for v in verts:
            bm.verts.new(v)
        bm.verts.ensure_lookup_table()
        for i in range(1, len(verts)):
            bm.edges.new((bm.verts[i - 1], bm.verts[i]))
        bm.edges.new((bm.verts[-1], bm.verts[0]))
        bm.edges.ensure_lookup_table()
        bmesh.ops.contextual_create(bm, geom=bm.edges)
        bm.to_mesh(o.data)
        bm.free()

    def update(self, context, manipulable_refresh=False, update_childs=False):

        o = self.find_in_selection(context, self.auto_update)
        
        if o is None:
            return

        tim = time.time()
        # clean up manipulators before any data model change
        if self.manipulable_refresh:
            self.manipulable_disable(o)

        # changed = self.update_parts()
        g = self.get_generator()
        g.locate_manipulators(self)

        if self.manipulable_refresh or update_childs:
            self.setup_childs(context, o)

        # relocate before cutting segs
        self.relocate_childs(context, o)

        self.select_object(context, o, True)

        # cut transfer .line with offset into g.segs

        throttle.add(context, o, self)
        realtime = throttle.is_active(o.name)

        g.cut(o, realtime)
        g.slab(o, self)

        self.shade_smooth(context, o, 0.20944)

        # Height
        self.manipulators[0].set_pts([
            Vector((0, 0, 0)),
            Vector((0, 0, -self.z)),
            (-1, 0, 0)
            ], normal=g.segs[0].straight(-1, 0).v.to_3d())

        self.update_dimensions(context, o)

        # enable manipulators rebuild
        # if manipulable_refresh:
        #    self.manipulable_refresh = True

        logger.debug("Slab.update():%.4f seconds", time.time() - tim)
        # restore context
        self.restore_context(context)

    def manipulable_setup(self, context, o):
        """
            NOTE:
            this one assume context.active_object is the instance this
            data belongs to, failing to do so will result in wrong
            manipulators set on active object
        """
         # generator does update manipulators location
        self.get_generator()
        # setup childs manipulators
        self.setup_childs(context, o)
        self.setup_manipulators()
        self.manipulable_setup_parts(context, o)

        for m in self.manipulators:
            self.manip_stack.append(m.setup(context, o, self))

    def as_2d(self, context, o, mode="FULL"):
        curves = []
        g = self.get_generator(o)
        curve = g.line.as_curve(name=o.name)
        self.link_object_to_scene(context, curve, layer_name="2d")
        curves.append(curve)
        if mode == "FULL":
            for c in o.children:
                d = archipack_slab_cutter.datablock(c)
                if d is not None:
                    g = d.get_generator(c)
                    curve = g.line.as_curve(name=c.name)
                    self.link_object_to_scene(context, curve, layer_name="2d")
                    curves.append(curve)
        return curves


def update_hole(self, context):
    # update parent only when manipulated
    self.update(context, update_parent=True)


class archipack_slab_cutter_segment(ArchipackCutterPart, PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_slab_cutter[0]


class archipack_slab_cutter(ArchipackCutter, ArchipackObject, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display cutter settings', 'NONE', 0),
            ('PARTS', 'Shape', 'Display cutter segments settings', 'NONE', 1),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 2)

        ),
        default='MAIN',
    )
    parts: CollectionProperty(type=archipack_slab_cutter_segment)

    idmat: IntVectorProperty(
        default=[0],
        size=1
    )
    material_sides: EnumProperty(
        options={'SKIP_SAVE'},
        name="Sides",
        items=mat_enum,
        get=mat_index_getter(MAT_PART),
        set=mat_index_setter( MAT_PART),
        update=update
    )

    def update_parent(self, context, o):
        if o is not None:

            cutables = []
            slab = o.parent

            d = archipack_slab.datablock(slab)
            if d is not None:
                cutables.append(slab)

            if slab is not None and slab.parent is not None:
                cutables.extend([c for c in slab.parent.children
                                 if c.data is not None and "archipack_floor" in c.data
                                 ])
            # filter cutables in bounding box, and add old ones so we update cutables when cutter goes out
            self.filter_cutables(context, o, cutables)
            store = []
            for c in cutables:
                d = archipack_slab.datablock(c)
                if d is None and "archipack_floor" in c.data:
                    d = c.data.archipack_floor[0]
                if d is not None:
                    store.append(c)
                    with ensure_select_and_restore(context, c, [c]):
                        d.update(context)

            # store cutables found in bound box (realy cut)
            self.store_cutables(o, store)

    def prepare_update(self, context, o):
        if o.parent:
            self.link_materials(context, o.parent, o)

    def get_generator(self, o=None):
        g = ArchipackCutter.get_generator(self, o)

        _mat = self.id_mat(MAT_PART)

        for _k, _p in zip(g.line.segs, self.parts):
            if _p.material_override:
                _k.idx = _p.id_mat(MAT_PART)
            else:
                _k.idx = _mat

        # print("archipack_slab_cutter.get_generator()", [_k.idx for _k in g.line.segs])

        return g

    def draw(self, context, layout, draw_offset=False, draw_type=False):

        self.draw_prop(context, layout, layout, self, "tabs", expand=True)

        box = layout.box()
        if self.tabs == 'MAIN':
            self.draw_prop(context, layout, box, self, 'operation', text="")
            if draw_offset:
                self.draw_prop(context, layout, box, self, 'offset')

        elif self.tabs == 'PARTS':
            self.template_user_path(context, box, focus=False)
            self.template_parts(context, layout, draw_type=draw_type)

        elif self.tabs == 'MATERIALS':

            self.draw_label(context, layout, box, "Apply materials")
            self.draw_prop(context, layout, box, self, 'material_sides')

            for index, part in enumerate(self.valid_parts):
                part.draw_material(context, layout, index)


class ARCHIPACK_PT_slab(ArchipackPanel, Archipacki18n, Panel):
    """Archipack Slab"""
    bl_idname = "ARCHIPACK_PT_slab"
    bl_label = "Slab"

    @classmethod
    def poll(cls, context):
        return archipack_slab.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_slab.datablock(o)
        if d is None:
            return
        layout = self.layout
        icons = icon_man["main"]
        # row = layout.row(align=True)
        self.draw_common(context, layout)
        # self.draw_op(context, layout, "archipack.slab", text="Delete", icon='ERROR').mode = 'DELETE'
        self.draw_op(context, layout, layout, 'archipack.slab_cutter', icon="MOD_BOOLEAN").parent = o.name

        self.draw_prop(context, layout, layout, d, "tabs", expand=True)
        box = layout.box()

        if d.tabs == 'PARTS':
            d.template_user_path(context, box, focus=False)
            d.template_parts(context, layout, draw_type=True)

        elif d.tabs == 'MAIN':
            self.draw_prop(context, layout, box, d, 'z')
            self.draw_prop(context, layout, box, d, 'x_offset')

        elif d.tabs == 'MATERIALS':

            if "archipack_material" in o:
                o.archipack_material[0].draw(context, box)

            box = layout.box()
            self.draw_label(context, layout, box, "Apply materials")
            self.draw_prop(context, layout, box, d, 'material_top')
            self.draw_prop(context, layout, box, d, 'material_bottom')
            self.draw_prop(context, layout, box, d, 'material_sides')
            for index, part in enumerate(d.valid_parts):
                part.draw_material(context, layout, index)

        elif d.tabs == 'TOOLS':
            # self.draw_prop(context, layout, box, d, "dimensions")
            self.draw_label(context, layout, box, "Create objects")
            self.draw_op(context, layout, box, "archipack.wall2_preset_menu", text="Wall from slab",
                         icon_value=icons["wall_from_slab"].icon_id).preset_operator= "archipack.wall2_from_slab"
            prefs = get_prefs(context)
            if prefs.experimental_features:
                self.draw_op(context, layout, box, "archipack.slab_to_curve").mode = "BOUNDARY"


class ARCHIPACK_PT_slab_cutter(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_slab_cutter"
    bl_label = "Slab Cutter"

    @classmethod
    def poll(cls, context):
        return archipack_slab_cutter.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_slab_cutter.datablock(o)
        if d is None:
            return
        layout = self.layout

        self.draw_common(context, layout)

        d.draw(context, layout, draw_offset=True)


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


class ARCHIPACK_OT_slab_balcony(Operator):
    bl_idname = "archipack.slab_balcony"
    bl_label = "Insert"
    bl_description = "Insert part"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    index: IntProperty(default=0)

    def execute(self, context):
        if context.mode == "OBJECT":
            d = archipack_slab.datablock(context.active_object)
            if d is None:
                return {'CANCELLED'}
            d.insert_balcony(context, self.index)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


class ARCHIPACK_OT_slab(ArchipackCreateTool, Operator):
    bl_idname = "archipack.slab"
    bl_label = "Slab"
    bl_description = "Slab"

    def create(self, context):
        m = bpy.data.meshes.new("Slab")
        o = bpy.data.objects.new("Slab", m)
        d = m.archipack_slab.add()
        angle_90 = pi / 2
        x = 4
        # make manipulators selectable
        d.manipulable_selectable = True
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
        self.add_material(context, o, material="DEFAULT")
        self.load_preset(d)
        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            with stop_auto_manipulate(context):
                o = self.create(context)
                o.location = self.get_cursor_location(context)

            self.add_to_reference(context, o)

            # select and make active
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_slab_from_curve(Operator):
    bl_idname = "archipack.slab_from_curve"
    bl_label = "Slab curve"
    bl_description = "Create a slab from a curve"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    @classmethod
    def poll(self, context):
        return context.active_object is not None and context.active_object.type == 'CURVE'

    def create(self, context):
        curve = context.active_object
        bpy.ops.archipack.slab()
        o = context.active_object
        d = archipack_slab.datablock(o)
        d.from_spline(context, o, curve)
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


class ARCHIPACK_OT_slab_to_curve(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.slab_to_curve"
    bl_label = "Slab to curve"
    bl_description = "Create a curve from a slab"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    mode: EnumProperty(
        name="Mode",
        items=(
            ('FULL', 'Full', 'Full including holes', 0),
            ('BOUNDARY', 'Boundary', 'Boundary without holes', 1)
        ),
        default='FULL'
    )
    @classmethod
    def poll(self, context):
        o = context.active_object
        return archipack_slab.poll(o)

    def create(self, context):
        o = context.active_object
        d = archipack_slab.datablock(o)

        curves = d.as_2d(context, o, self.mode)
        return curves

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            curves = self.create(context)
            for c in curves:
                self.select_object(context, c, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_slab_from_wall(ArchipackCreateTool, Operator):
    bl_idname = "archipack.slab_from_wall"
    bl_label = "->Slab"
    bl_description = "Create a slab from a wall"

    ceiling: BoolProperty(default=False)

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o is not None and o.data is not None and 'archipack_wall2' in o.data

    def slab_from_wall(self, context, w, wd):
        """
          Create slab from surrounding wall
         """
        tim = time.time()

        # wall is either a single or collection of polygons
        try:
            io, wall, childs = wd.as_geom(context, w, 'SLAB', [], [], [])
        except RecursionError as ex:
            self.report({"ERROR"}, "Recursion error while building geometry: %s" % ex)
            import traceback
            traceback.print_exc()
            return None
        except Exception as ex:
            self.report({"ERROR"}, "Error while building geometry: %s" % ex)
            import traceback
            traceback.print_exc()
            return None

        # find slab holes if any
        o = None
        # Multipolygon
        if wall.type_id == 6:
            polys = wall.geoms
        else:
            polys = [wall]

        sel = []

        logger.debug("slab_from_wall() curves :%.4f seconds", time.time() - tim)

        for poly in polys:
            boundary = io._to_curve(poly.exterior, "{}-boundary".format(w.name), '2D')
            boundary.location.z = w.matrix_world.translation.z - wd.z_offset
            logger.debug("slab_from_wall() boundary :%.4f seconds", time.time() - tim)
            o = self.create(context)
            o.matrix_world = w.matrix_world.copy()
            d = archipack_slab.datablock(o)
            d.auto_update = False
            logger.debug("slab_from_wall() create :%.4f seconds", time.time() - tim)
            d.user_defined_path = boundary.name
            logger.debug("slab_from_wall() user_defined_path :%.4f seconds", time.time() - tim)
            self.delete_object(context, boundary)
            logger.debug("slab_from_wall() delete_object :%.4f seconds", time.time() - tim)
            d.user_defined_path = ""
            logger.debug("slab_from_wall() floor :%.4f seconds", time.time() - tim)
            self.select_object(context, o, True)
            d.auto_update = True
            self.unselect_object(context, o)
            if self.ceiling:
                o.location.z += wd.z + d.z
            else:
                sel.append(o)

        with ensure_select_and_restore(context, w, sel):
            bpy.ops.archipack.add_reference_point()

        return o

    def create(self, context):
        m = bpy.data.meshes.new("Slab")
        o = bpy.data.objects.new("Slab", m)
        d = m.archipack_slab.add()

        d.manipulable_selectable = True
        d.auto_update = False
        self.link_object_to_scene(context, o)
        o.color = (1, 0.25, 0, 1)

        self.select_object(context, o, True)
        self.add_material(context, o, category="slab", material="DEFAULT")
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
                o = self.slab_from_wall(context, wall, wd)
            self.select_object(context, wall, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_slab_cutter(ArchipackCreateTool, Operator):
    bl_idname = "archipack.slab_cutter"
    bl_label = "Slab Cutter"
    bl_description = "Slab Cutter"

    parent: StringProperty("")
    curve: StringProperty("")

    def create(self, context):
        m = bpy.data.meshes.new("Slab Cutter")
        o = bpy.data.objects.new("Slab Cutter", m)
        d = m.archipack_slab_cutter.add()
        parent = self.get_scene_object(context, self.parent)
        curve = self.get_scene_object(context, self.curve)

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

        # make manipulators selectable
        d.manipulable_selectable = True
        self.link_object_to_scene(context, o)
        o.color = (1, 0, 0, 1)

        if parent is not None:
            self.link_materials(context, o.parent, o)
        self.select_object(context, o, True)
        self.load_preset(d)
        update_operation(d, context)

        if curve is not None:
            d.user_defined_path = curve.name

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


def register():
    bpy.utils.register_class(archipack_slab_cutter_segment)
    bpy.utils.register_class(archipack_slab_cutter)
    Mesh.archipack_slab_cutter = CollectionProperty(type=archipack_slab_cutter)
    bpy.utils.register_class(ARCHIPACK_OT_slab_cutter)
    bpy.utils.register_class(ARCHIPACK_PT_slab_cutter)

    bpy.utils.register_class(archipack_slab_relocate_child)
    bpy.utils.register_class(archipack_slab_part)
    bpy.utils.register_class(archipack_slab)
    Mesh.archipack_slab = CollectionProperty(type=archipack_slab)
    bpy.utils.register_class(ARCHIPACK_PT_slab)
    bpy.utils.register_class(ARCHIPACK_OT_slab)
    bpy.utils.register_class(ARCHIPACK_OT_slab_balcony)
    bpy.utils.register_class(ARCHIPACK_OT_slab_from_curve)
    bpy.utils.register_class(ARCHIPACK_OT_slab_to_curve)
    bpy.utils.register_class(ARCHIPACK_OT_slab_from_wall)


def unregister():
    bpy.utils.unregister_class(archipack_slab_relocate_child)
    bpy.utils.unregister_class(archipack_slab_part)
    bpy.utils.unregister_class(archipack_slab)
    del Mesh.archipack_slab
    bpy.utils.unregister_class(ARCHIPACK_PT_slab)
    bpy.utils.unregister_class(ARCHIPACK_OT_slab)
    bpy.utils.unregister_class(ARCHIPACK_OT_slab_balcony)
    bpy.utils.unregister_class(ARCHIPACK_OT_slab_from_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_slab_to_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_slab_from_wall)
    del Mesh.archipack_slab_cutter
    bpy.utils.unregister_class(archipack_slab_cutter_segment)
    bpy.utils.unregister_class(archipack_slab_cutter)
    bpy.utils.unregister_class(ARCHIPACK_OT_slab_cutter)
    bpy.utils.unregister_class(ARCHIPACK_PT_slab_cutter)
