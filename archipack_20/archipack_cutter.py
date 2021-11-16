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
# Cutter / CutAble shared by roof, slab, and floor
# ----------------------------------------------------------
from mathutils import Vector, Matrix
import bmesh
from .bmesh_utils import BmeshEdit as bmed
from random import uniform
import bpy
from bpy.props import (
    FloatProperty, BoolProperty,
    EnumProperty, IntProperty, StringProperty, CollectionProperty
    )
from bpy.types import PropertyGroup
from .archipack_generator import Line, Generator
from .archipack_abstraction import ensure_select_and_restore
from .archipack_curveman import ArchipackUserDefinedPath
from .archipack_segments2 import ArchipackSegment
from .archipack_i18n import Archipacki18n
from .archipack_autoboolean import ArchipackBoolManager
import logging
logger = logging.getLogger("archipack_cutter")


# TODO: always convert internally to straight segments so we are able to support curved segments


class CutterSegment(Line):

    __slots__ = ('side_type', 'is_hole')

    def __init__(self, p, p1=None, last=None, after=None, side_type='DEFAULT'):
        Line.__init__(self, p, p1=p1, last=last, after=after)
        self.side_type = side_type
        self.is_hole = True

    def new_segment(self, k0, k1):
        _k = CutterSegment(k0, p1=k1, side_type=self.side_type)
        _k.idx = self.idx
        _k.is_hole = self.is_hole
        return _k


class CutterGenerator(Generator):
    """
      Generator for cutter objects
    """
    def __init__(self, o=None):
        Generator.__init__(self, o)
        self.line = None

    def create_segment(self, d, part, co: Vector, last, last_type: str, p1=None, next=None):
        """Add a new segment (Knot derived class)
        :param d: archipack_segments derived datablock
        :param part: archipack_segment derived instance
        :param co: Vector location of first knot
        :param last: archipack_segment derived instance, last segment
        :param last_type: last part type
        :return:
        """
        if hasattr(part, "side_type"):
            side_type = part.side_type
        else:
            side_type = 'DEFAULT'
        _k = CutterSegment(co, side_type=side_type, last=last)

        return _k
    
    def locate_manipulators(self, d, side=1):
        if self.operation == 'DIFFERENCE':
            _side = -side
        else:
            _side = side
        Generator.locate_manipulators(self, d, side=_side)

    def get_index(self, index):
        n_segs = len(self.segs)
        if index >= n_segs:
            index -= n_segs
        return index

    def next_seg(self, index):
        idx = self.get_index(index + 1)
        return self.segs[idx]

    def last_seg(self, index):
        return self.segs[index - 1]

    def get_verts(self, verts, edges):

        n_segs = len(self.segs) - 1

        self.get_pts(verts)
        for i in range(n_segs):
            edges.append([i, i + 1])


class CutAblePolygon:
    """
        Simple boolean operations (mod 2 rule based)
        Cutable generator / polygon
        Object MUST have properties
        - segs
        - holes
        - convex
    """
    def as_lines(self, step_angle=0.104):
        """
            Convert curved segments to straight lines
            Use offset ones when set
        """
        _segs = []

        segs = self.segs

        if self.line is not None:
            segs = self.line.segs

        for s in segs:
            if hasattr(s, "_r"):
                steps = max(1, int(s._da / step_angle))
                res = []
                last = s._last
                for step in range(steps):
                    p = s.lerp(step / steps)
                    _s = Line(p, last=last)
                    _s.idx = s.idx
                    last = _s
                    res.append(_s)
                last._next = s._next
                if s._next is not None:
                    s._next._last = last
                _segs.extend(res)
            else:
                _segs.append(s)

        # print("CutAblePolygon.as_lines", [_k.idx for _k in _segs])

        self.segs = _segs


    def ray_tracing_method(self, x, y, _segs):
        inside = False
        xints = 0.0
        p1x, p1y, _ = _segs[0].p0
        for s in _segs:
            p2x, p2y, _ = s.p1
            if min(p1y, p2y) < y <= max(p1y, p2y) and x <= max(p1x, p2x):
                if p1y != p2y:
                    xints = (y - p1y) * (p2x - p1x) / (p2y - p1y) + p1x
                if p1x == p2x or x <= xints:
                    inside = not inside
            p1x, p1y = p2x, p2y

        return inside

    def inside(self, pt, segs=None):
        """
            Point inside poly (ray-cast method)
            support concave polygons
        """
        x, y, z = pt
        _segs = segs
        if segs is None:
            _segs = self.segs

        # ~10x faster
        inside = self.ray_tracing_method(x, y, _segs)
        """
        print("method 1 : %s %.5f" % (inside1, time.time()-t))

        t = time.time()
        s1 = Line(p0=(x, y, 0), p1=(x + min(100000, 100 * self.xsize), y + uniform(-0.5, 0.5), 0))
        counter = 0

        for s in _segs:
            res, p, u, v = s.intersect_ext(s1)
            logger.debug("CutAblePolygon.intersect(%s) p:%s u:%s v:%s" % (res, p, u, v))
            if res:
                counter += 1
        inside = counter % 2 == 1
        """
        logger.debug("CutAblePolygon.inside(%s)" % (inside))
        return inside

    def get_index(self, index):
        return index % len(self.segs)

    def is_convex(self):

        self.convex = True
        sign = False
        s0 = self.segs[-1]
        for i, s1 in enumerate(self.segs):
            if hasattr(s1, "_r"):
                self.convex = False
                return
            x0, y0, z = s0.v
            x1, y1, z = s1.v
            c = x0 * y1 - y0 * x1
            if i == 0:
                sign = (c > 0)
            elif sign != (c > 0):
                self.convex = False
                return
            s0 = s1

    def get_intersections(self, border, cutter, s_start, segs, start_by_hole):
        """
            Detect all intersections
            for boundary: store intersection point, t, idx of segment, idx of cutter
            sort by t
        """
        s_segs = border.segs
        b_segs = cutter.segs
        s_nsegs = len(s_segs)
        b_nsegs = len(b_segs)
        inter = []

        # find all intersections
        for idx in range(s_nsegs):
            s_idx = border.get_index(s_start + idx)
            s = s_segs[s_idx]
            for b_idx, b in enumerate(b_segs):
                res, p, u, v = s.intersect_ext(b)
                if res:
                    inter.append((s_idx, u, b_idx, v, p))

        # print("%s" % (self.side))
        logger.debug("inter=%s" % (inter))

        if len(inter) < 1:
            return True

        # sort by seg and param t of seg
        inter.sort()

        order = 0
        # reorder so we realy start from s_start
        for i, it in enumerate(inter):
            if it[0] >= s_start:
                order = i
                break

        inter = inter[order:] + inter[:order]

        logger.debug("s_start:%s isort=%s", s_start, inter)
        p0 = s_segs[s_start].p0
        last_p = p0
        n_inter = len(inter) - 1
        s = None
        for i in range(n_inter):
            s_end, u, b_start, v, p = inter[i]
            s_idx = border.get_index(s_start)
            s = s_segs[s_idx].copy(last=s)
            s._p0 = last_p
            last_p = p
            s.is_hole = not start_by_hole
            segs.append(s)
            idx = s_idx
            max_iter = s_nsegs
            # walk through s_segs until intersection
            while s_idx != s_end and max_iter > 0:
                idx += 1
                s_idx = border.get_index(idx)
                s = s_segs[s_idx].copy(last=s)
                s.is_hole = not start_by_hole
                segs.append(s)
                max_iter -= 1
            # segs[-1].p1 = p
            s_start, u, b_end, v, p = inter[i + 1]
            b_idx = cutter.get_index(b_start)
            s = b_segs[b_idx].copy(last=s)
            s._p0 = last_p
            last_p = p
            s.is_hole = start_by_hole
            segs.append(s)
            idx = b_idx
            max_iter = b_nsegs
            # walk through b_segs until intersection
            while b_idx != b_end and max_iter > 0:
                idx += 1
                b_idx = cutter.get_index(idx)
                s = b_segs[b_idx].copy(last=s)
                s.is_hole = start_by_hole
                segs.append(s)
                max_iter -= 1
            # segs[-1].p1 = p

        # add part between last intersection and start point
        idx = s_start
        s_idx = border.get_index(s_start)
        s = s_segs[s_idx].copy(last=s)
        s._p0 = last_p
        s.is_hole = not start_by_hole
        segs.append(s)
        max_iter = s_nsegs
        # go until end of segment is near start of first one
        while (s_segs[s_idx].p1 - p0).length > 0.0001 and max_iter > 0:
            idx += 1
            s_idx = border.get_index(idx)
            s = s_segs[s_idx].copy(last=s)
            s.is_hole = not start_by_hole
            segs.append(s)
            max_iter -= 1

        if len(segs) > s_nsegs + b_nsegs + 1:
            logger.debug("slice failed found:%s of:%s" % (len(segs), s_nsegs + b_nsegs))
            return False

        next = segs[0]
        s._next = next
        next._last = s

        # print("CutAblePolygon.get_intersections mat: %s", [s.idx for s in segs])

        return True

    def slice(self, cutter):
        """
            Simple 2d Boolean between boundary and roof part
            doesn't handle slicing roof into multiple parts

            4 cases:
            1 pitch has point in boundary -> start from this point
            2 boundary has point in pitch -> start from this point
            3 no points inside -> find first crossing segment
            4 not points inside and no crossing segments
        """
        logger.debug("****** CutAblePolygon.slice(%s) ******" % cutter.operation)

        # keep inside or cut inside
        # keep inside must be CCW
        # cut inside must be CW
        keep_inside = (cutter.operation == 'INTERSECTION')

        start = -1

        f_segs = self.segs
        c_segs = cutter.line.segs
        store = []

        slice_res = True
        is_inside = False

        # find if either a cutter or
        # cutter intersects
        # (at least one point of any must be inside other one)
        # logger.debug("CutAblePolygon.slice() self.segs")
        # for s in self.segs:
        #    logger.debug("seg %s %s" % (s.p0, s.p1))
        # logger.debug("CutAblePolygon.slice() cutter.segs")
        # for s in cutter.segs:
        #    logger.debug("seg %s %s" % (s.p0, s.p1))

        # find if all points of the object are inside cutter
        # so there is no need to cut when mode is intersection
        if keep_inside:
            logger.debug("find if all points of the boundary are inside cutter")
            one_outside = False
            for i, s in enumerate(f_segs):
                res = self.inside(s.p0, c_segs)
                if not res:
                    one_outside = True
                    break
            if not one_outside:
                return True

        # find a point of this boundary in/outside cutter
        logger.debug("find a point of boundary %sside cutter.segs: %s" % (("out", "in")[int(keep_inside)], len(c_segs)))
        for i, s in enumerate(f_segs):
            res = self.inside(s.p0, c_segs)
            if res:
                is_inside = True
            if res == keep_inside:
                start = i
                logger.debug("boundary f_start:%s found %sside: %s" % (start, ("out", "in")[int(keep_inside)], is_inside))
                slice_res = self.get_intersections(self, cutter.line, start, store, True)
                break

        # seek for point of cutter inside boundary
        logger.debug("find a point of cutter inside boundary.segs: %s" % len(self.segs))
        for i, s in enumerate(c_segs):
            res = self.inside(s.p0)
            if res:
                is_inside = True
            # no boundary point found inside cutter
            if start < 0 and res == keep_inside:
                start = i
                logger.debug("cutter c_start:%s is_inside %s" % (start, is_inside))
                # swap cutter / pitch so we start from cutter
                slice_res = self.get_intersections(cutter.line, self, start, store, False)
                break

        # no points found at all
        if start < 0:
            logger.debug("no pt inside")
            return not keep_inside

        if not slice_res:
            logger.debug("slice fails")
            # found more segments than input
            # cutter made more than one loop
            return True

        if len(store) < 1:
            if is_inside:
                logger.debug("not touching, add as hole")
                if keep_inside:
                    self.segs = cutter.line.segs
                else:
                    self.holes.append(cutter.line)

                logger.debug("mat: %s", [s.idx for s in self.segs])

            return True

        self.segs = store

        # close (should already be done in get_intersections)
        last, next = store[-1], store[0]
        last._next = next
        next._last = last

        # debug
        # self.as_curve()

        self.is_convex()
        logger.debug("convex: %s" % self.convex)
        # print("CuttablePolygon.slice mat: %s", [s.idx for s in self.segs])

        return True


class CutAbleGenerator(Generator):
    """
     Generator for cutable objects
    """
    __slots__ = ('holes', 'convex', 'xsize', 'line')

    def __init__(self, o=None):
        Generator.__init__(self, o)
        self.holes = []
        self.convex = True
        self.xsize = 100000

        self.line = None

    def dissolve_limit(self, bm, angle_limit=0.01):
        bmesh.ops.dissolve_limit(bm,
             angle_limit=angle_limit,
             use_dissolve_boundaries=False,
             verts=bm.verts,
             edges=bm.edges,
             delimit={'MATERIAL'})

    def limits(self):
        """
        Compute limits for intersection tests
        :return:
        """
        x_min, y_min, x_max, y_max = self.bounding_rect()
        self.xsize = x_max - x_min
        logger.debug("CutAbleGenerator.limits xsize: %s" % self.xsize)

    def cut_holes(self, bm, cutable, offset={'DEFAULT': 0}, cleanup=True, delete=True):
        o_keys = offset.keys()
        has_offset = len(o_keys) > 1 or offset['DEFAULT'] != 0
        # cut holes
        for hole in cutable.holes:

            if has_offset:

                segs = []
                for s in hole.segs:
                    if s.length > 0:
                        if s.side_type in o_keys:
                            of = offset[s.side_type]
                        else:
                            of = offset['DEFAULT']
                        n = s.v_normal
                        p0 = s.p0 + n * of
                        bmed.bisect(bm, p0, n)
                        # compute boundary with offset (preserve idx)
                        new_s = s.offset(of)
                        segs.append(new_s)

                # compute intersections
                if len(segs) > 0:
                    last = segs[-1]
                    for s in segs:
                        res, p0, t = last.intersect(s)
                        s._last = last
                        last._next = s
                        s._p0 = p0
                        last = s
            else:
                for s in hole.segs:
                    if s.length > 0:
                        bmed.bisect(bm, s.p0, s.v_normal)
                # use hole boundary
                segs = hole.segs

            if len(segs) > 0 and delete:
                # when hole segs are found clear parts inside hole
                f_geom = [f for f in bm.faces
                    if cutable.inside(
                        f.calc_center_median(),
                        segs=segs)]
                if len(f_geom) > 0:
                    bmesh.ops.delete(bm, geom=f_geom, context='FACES')   # 5

        if len(cutable.holes) > 0 and cleanup:
            self.dissolve_limit(bm)

    def cut_boundary(self, bm, cutable, offset={'DEFAULT': 0}):
        o_keys = offset.keys()
        has_offset = len(o_keys) > 1 or offset['DEFAULT'] != 0

        # logger.debug("CutAbleGenerator.cut_boundary() cutable.segs")
        # for s in cutable.segs:
        #    logger.debug("seg %s %s" % (s.p0, s.p1))

        # cut outside parts
        if has_offset:
            for s in cutable.segs:
                if s.length > 0:
                    if s.side_type in o_keys:
                        of = offset[s.side_type]
                    else:
                        of = offset['DEFAULT']
                    n = s.v_normal
                    p0 = s.p0 + n * of
                    bmed.bisect(bm, p0, n, clear_outer=cutable.convex)
        else:
            for s in cutable.segs:
                if s.length > 0:
                    bmed.bisect(bm, s.p0, s.v_normal, clear_outer=cutable.convex)

        if not cutable.convex:
            # print("CutAbleGenerator.cut_boundary faces:", len(bm.faces))
            f_geom = [f for f in bm.faces
                if not cutable.inside(f.calc_center_median())]

            logger.debug("CutAbleGenerator remove %s faces outside of %s" % (len(f_geom), len(bm.faces)))

            if len(f_geom) > 0:
                bmesh.ops.delete(bm, geom=f_geom, context='FACES')   # 5

        self.dissolve_limit(bm)


class ArchipackCutterPart(Archipacki18n, ArchipackSegment):
    """
        Cutter segment PropertyGroup
        Childs MUST implements
        -parent_data
    """

    def update(self, context, manipulable_refresh=False, update_parent=True):
        self.parent_data.update(context, manipulable_refresh=manipulable_refresh, update_parent=update_parent)


def update_operation(self, context):
    logger.debug("update_operation()")
    o = self.find_in_selection(context)
    if o is None:
        return
    g = self.get_generator()
    if g.is_cw == (self.operation in {'INTERSECTION', 'UNION'}):
        # print("update_operation reverse()")
        self.reverse(context, o)

    # print("update_operation restore_auto_manipulate")
    self.restore_auto_manipulate(context)


def update_path(self, context):
    self.update_path(context)


def update(self, context):
    self.update(context, update_parent=True)


def update_manipulators(self, context):
    self.update(context, manipulable_refresh=True)


class archipack_cutables(PropertyGroup):
    cutable: StringProperty(
        description="store name of cutable objects to update when removing cutter"
    )


class ArchipackCutter(Archipacki18n, ArchipackUserDefinedPath):

    z: FloatProperty(
        name="dumb z",
        description="Dumb z for manipulator placeholder",
        default=0.01,
        options={'SKIP_SAVE'}
    )
    operation: EnumProperty(
        items=(
            ('DIFFERENCE', 'Difference', 'Cut inside part', 0),
            ('INTERSECTION', 'Intersection', 'Keep inside part', 1),
            ('UNION', 'Union', 'Add to boundary', 2)
            ),
        default='DIFFERENCE',
        update=update_operation
    )
    offset: FloatProperty(
        default=0,
        name="Offset",
        description="Lateral offset of cutter",
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update_manipulators
    )
    cutables: CollectionProperty(type=archipack_cutables)
    always_closed = True

    def draw(self, context, layout, draw_offset=False, draw_type=False):
        # self.draw_op(context, layout, layout, "archipack.delete", icon="TRASH")
        self.draw_prop(context, layout, layout, self, "tabs", expand=True)

        box = layout.box()
        if self.tabs == 'MAIN':
            self.draw_prop(context, layout, box, self, 'operation', text="")
            if draw_offset:
                self.draw_prop(context, layout, box, self, 'offset')

        elif self.tabs == 'PARTS':
            self.template_user_path(context, box, focus=False)
            self.template_parts(context, layout, draw_type=draw_type)

    def _filter_inside(self, o, cutables):
        man = ArchipackBoolManager()
        # use generator as object bounding box is not up to date
        g = self.get_generator(o)
        man.minx, man.miny, man.maxx, man.maxy = g.line.bounding_rect()
        man.minz, man.maxz = -1e32, 1e32
        inside = [p for p in cutables if man._contains(p)]
        cutables.clear()
        cutables.extend(inside)

    def store_cutables(self, o, cutables):
        self._filter_inside(o, cutables)
        # print("cutter.store_cutables:", cutables[:])
        for cutable in cutables:
            c = self.cutables.add()
            c.cutable = cutable.name

    def _retrieve_cutables(self, context, cutables):
        names = set([c.name for c in cutables])
        for c in self.cutables:
            o = self.get_scene_object(context, c.cutable)
            if o is not None and o.name not in names:
                cutables.append(o)
        self.cutables.clear()

    def filter_cutables(self, context, o, cutables):
        """ Filter input objects using bounding box
        :param o:
        :param cutables:
        :return:
        """
        # print("cutter.filter_cutables input:", cutables[:])
        self._filter_inside(o, cutables)
        # print("cutter._filter_inside:", cutables[:])
        self._retrieve_cutables(context, cutables)
        # print("cutter._retrieve_cutables:", cutables[:])

    def update_parent(self, context, o):
        raise NotImplementedError

    def setup_manipulators(self):
        self.setup_parts_manipulators('z', flip=self.operation == 'DIFFERENCE')

    def get_generator(self, o=None):
        g = CutterGenerator(o)
        g.operation = self.operation
        g.add_parts(self)
        g.line = g.make_offset(self.offset)
        return g

    def ensure_direction(self, o=None):
        # get segs ensure they are cw or ccw depending on operation
        # whatever the user do with points
        logger.debug("ArchipackCutter.ensure_direction()")
        g = self.get_generator(o)
        if g.is_cw != (self.operation in {'INTERSECTION', 'UNION'}):
            return g
        g.line.reverse()
        # print("ArchipackCutter.ensure_direction()", [_k.idx for _k in g.line.segs])
        return g

    def from_spline(self, context, o, curve, ccw=False, cw=False):
        ArchipackUserDefinedPath.from_spline(self,
                                             context,
                                             o,
                                             curve,
                                             ccw=(self.operation == 'INTERSECTION'),
                                             cw=(self.operation != 'INTERSECTION')
                                             )
        self.update_parent(context, o)
    
    def after_reverse(self, context, o):
        self.auto_update = True
        self.update_parent(context, o)

    def make_surface(self, o, verts, edges):
        bm = bmesh.new()
        for v in verts:
            bm.verts.new(v)
        bm.verts.ensure_lookup_table()
        for ed in edges:
            bm.edges.new((bm.verts[ed[0]], bm.verts[ed[1]]))
        bm.edges.new((bm.verts[-1], bm.verts[0]))
        bm.edges.ensure_lookup_table()
        bm.to_mesh(o.data)
        bm.free()

    def get_coords(self):
        """
         return coordinates in object coordsys
        """
        # self.update_parts()
        verts = []
        g = self.get_generator()
        g.line.get_pts(verts)
        return verts

    def prepare_update(self, context, o):
        return

    def update(self, context, manipulable_refresh=False, update_parent=True):
        """
         Does update parent make sense at all ?
         as cutter changes must always update parent
        """
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        logger.debug("ArchipackCutter.update() n_parts:%s" % self.n_parts)

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

        # enable manipulators rebuild
        # if manipulable_refresh:
        #    self.manipulable_refresh = True

        # update parent on direct edit
        if self.manipulable_refresh or update_parent:
            self.update_parent(context, o)

        self.update_dimensions(context, o)
        self.restore_context(context)

    def on_delete(self, context, o):
        if o is None:
            return
        o.parent = None
        for cutable in self.cutables:
            c = self.get_scene_object(context, cutable.cutable)
            if c is not None:
                d = self.archipack_datablock(c)
                if d is not None:
                    with ensure_select_and_restore(context, c, [c]):
                        d.update(context)


def register():
    bpy.utils.register_class(archipack_cutables)


def unregister():
    bpy.utils.unregister_class(archipack_cutables)