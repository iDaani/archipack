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
# noinspection PyUnresolvedReferences
import bpy
# noinspection PyUnresolvedReferences
from bpy.types import Operator, PropertyGroup, Mesh, Panel
from bpy.props import (
    FloatProperty, BoolProperty, IntProperty, IntVectorProperty,
    StringProperty, EnumProperty,
    CollectionProperty, FloatVectorProperty
    )
from .bmesh_utils import BmeshEdit as bmed
from random import uniform, randint
import bmesh
import json
from mathutils import Vector, Matrix
from math import sin, cos, pi, atan2, sqrt, tan, atan
from .archipack_manipulator import Manipulable, archipack_manipulator
from .archipack_generator import Line, Generator
from .archipack_preset import ArchipackPreset, PresetMenuOperator
from .archipack_abstraction import ensure_select_and_restore
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackCreateTool,
    ArchipackObject,
    ArchipackPanel,
    ArchipackGenericOperator,
    stop_auto_manipulate
)
from .archipack_cutter import (
    CutAblePolygon, CutAbleGenerator,
    ArchipackCutter, CutterGenerator,
    ArchipackCutterPart
    )
from .archipack_polylines import Io, ShapelyOps, CoordSys
from .archipack_curveman import ArchipackUserDefinedPath
from .archipack_dimension import DimensionProvider
from .archipack_material import build_mat_enum
from .archipack_throttle import throttle
from .archipack_terrain import Q_tree
from .polyskel import polyskel
from .archipack_iconmanager import icons as icon_man


import time
X_AXIS = Vector((1, 0, 0))
Z_AXIS = Vector((0, 0, 1))


MAT_COVERING = 0
MAT_HIP = 1
MAT_RIGE_POLE = 2
MAT_RAFTER = 3
MAT_GUTTER = 4
MAT_FASCIA = 5
MAT_BARGEBOARD = 6
MAT_LAMBRIS = 7


class Roof:

    def __init__(self, side_type='SIDE'):
        self.angle_0 = 0
        self.v0_idx = 0
        self.v1_idx = 0
        self.constraint_type = None
        self.slope_left = 1
        self.slope_right = 1
        self.width_left = 1
        self.width_right = 1
        self.auto_left = 'AUTO'
        self.auto_right = 'AUTO'
        self.side_type = side_type
        # force hip or valley
        self.enforce_part = 'AUTO'
        self.triangular_end = False

        self.remove_left = False
        self.remove_right = False

        # seg is part of hole
        self.is_hole = False

    @property
    def t_diff(self):
        return self.t_end - self.t_start


class StraightRoof(Roof, Line):

    __slots__ = ('angle_0',
                 'v0_idx',
                 'v1_idx',
                 'constraint_type',
                 'slope_left',
                 'slope_right',
                 'width_left',
                 'width_right',
                 'auto_left',
                 'auto_right',
                 'side_type',
                 'enforce_part',
                 'triangular_end',
                 'remove_left',
                 'remove_right',
                 'is_hole'
                 )

    def __str__(self):
        return "StraightRoof p0:{} p1:{}".format(self.p0, self.p1)

    def __init__(self, p0, p1=None, side_type='SIDE', last=None, after=None):
        Line.__init__(self, p0, p1=p1, last=last, after=after)
        Roof.__init__(self, side_type=side_type)

    def new_segment(self, k0, k1):
        """
        Create standalone segment from k0 and k1
        :param k0:
        :param k1:
        :return: StraightRoof
        """
        s = StraightRoof(k0, p1=k1)
        for attr in StraightRoof.__slots__:
            setattr(s, attr, getattr(self, attr))
        return s

    def copy(self, last=None):
        _k = self.new_segment(self.p0, self.p1)
        _k._last = last
        if last is not None:
            last._next = _k
        return _k


class RoofSegment:
    """
        Roof part with 2 polygons
        and "axis" StraightRoof segment
    """
    __slots__ = ('seg', 'left', 'right', 'a0', 'reversed')

    def __init__(self, seg, left, right):
        self.seg = seg
        self.left = left
        self.right = right
        self.a0 = 0
        self.reversed = False


class RoofAxisNode:
    __slots__ = ('segs', 'root', 'center', 'n_horizontal', 'n_slope')
    """
        Connection between parts
        for radial analysis
    """
    def __init__(self):
        # axis segments
        self.segs = []
        self.root = None
        self.center = 0
        # store count of horizontal segs
        self.n_horizontal = 0
        # store count of slopes segs
        self.n_slope = 0

    @property
    def count(self):
        return len(self.segs)

    @property
    def last(self):
        """
            last segments in this node
        """
        return self.segs[-1]

    def left(self, index):
        return self.segs[(index + 1) % self.count]

    def right(self, index):
        return self.segs[index - 1]

    def add(self, a0, reversed, seg, left, right):

        if seg.constraint_type == 'HORIZONTAL':
            self.n_horizontal += 1
        elif seg.constraint_type == 'SLOPE':
            self.n_slope += 1

        s = RoofSegment(seg, left, right)
        s.a0 = a0
        s.reversed = reversed
        if reversed:
            self.root = s
        self.segs.append(s)

    def update_center(self):
        for i, s in enumerate(self.segs):
            if s is self.root:
                self.center = i
                return

    # sort tree segments by angle
    def partition(self, array, begin, end):
        pivot = begin
        for i in range(begin + 1, end + 1):
            if array[i].a0 < array[begin].a0:
                pivot += 1
                array[i], array[pivot] = array[pivot], array[i]
        array[pivot], array[begin] = array[begin], array[pivot]
        return pivot

    def sort(self):
        def _quicksort(array, begin, end):
            if begin >= end:
                return
            pivot = self.partition(array, begin, end)
            _quicksort(array, begin, pivot - 1)
            _quicksort(array, pivot + 1, end)

        end = len(self.segs) - 1
        _quicksort(self.segs, 0, end)

        # index of root in segs array
        self.update_center()


class RoofPolygon(CutAblePolygon):
    """
        ccw roof pitch boundary
        closed by explicit segment
        handle triangular shape with zero axis length

        mov  <_________________
            |                  /\
            |                  | rot
            |                  |   left     last <> next
            \/_____axis_______>|

     node   <_____axis________      next
            |                  /\
            |                  | rot
            |                  |   right    last <> next
        mov \/________________>|
                           side angle
    """
    def __init__(self, axis, side, fake_axis=None, remove=False):
        """
            Create a default rectangle
            axis from node to next
            slope float -z for 1 in side direction
            side in ['LEFT', 'RIGHT'] in axis direction

            NOTE:
            when axis length is null (eg: triangular shape)
            use "fake_axis" with a 1 length to handle
            distance from segment
        """
        if side == 'LEFT':
            # slope
            self.slope = axis.slope_left
            # width
            self.width = axis.width_left
            # constraint width
            self.auto_mode = axis.auto_left
            self.axis = axis.copy()
        else:
            # slope
            self.slope = axis.slope_right
            # width
            self.width = axis.width_right
            # constraint width
            self.auto_mode = axis.auto_right
            self.axis = axis.opposite

        self.remove = remove

        self.side = side
        # backward deps
        self.backward = False
        # pointers to neighboors along axis
        self.last = None
        self.next = None
        self.other_side = None

        self.fake_axis = None

        # _axis is either a fake one or real one
        # to prevent further check
        if fake_axis is None:
            self._axis = self.axis
            self.fake_axis = self.axis
            self.next_cross = axis
            self.last_cross = axis
        else:
            if side == 'RIGHT':
                self.fake_axis = fake_axis.opposite
            else:
                self.fake_axis = fake_axis
            self._axis = self.fake_axis

        # unit vector perpendicular to axis
        # looking at outside part
        v = self.fake_axis.normal(0, -1)
        self.cross = v
        self.next_cross = v
        self.last_cross = v

        self.convex = True
        # segments from axis end in ccw order
        # closed by explicit segment
        self.segs = []
        # holes
        self.holes = []

        # Triangular ends
        self.node_tri = False
        self.next_tri = False
        self.is_tri = False

        self.z = 0
        # sizes
        self.tmin = 0
        self.tmax = 1
        self.dt = 1
        self.ysize = 0
        self.xsize = 0
        self.vx = Vector()
        self.vy = Vector()
        self.vz = Vector()

    def move_node(self, p):
        """
            Move slope point in node side
        """
        if self.side == 'LEFT':
            self.segs[-1].p0 = p
            # self.segs[2].p1 = p
        else:
            self.segs[2].p0 = p
            # self.segs[1].p1 = p

    def move_next(self, p):
        """
            Move slope point in next side
        """
        if self.side == 'LEFT':
            self.segs[2].p0 = p
            # self.segs[1].p1 = p
        else:
            self.segs[-1].p0 = p
            # self.segs[2].p1 = p

    def node_link(self, da):
        angle_90 = round(pi / 2, 4)
        if self.side == 'LEFT':
            idx = -1
        else:
            idx = 1
        da = abs(round(da, 4))
        typ = "LINK"
        if da < angle_90:
            typ += "_VALLEY"
        elif da > angle_90:
            typ += "_HIP"
        self.segs[idx].side_type = typ

    def next_link(self, da):
        angle_90 = round(pi / 2, 4)
        if self.side == 'LEFT':
            idx = 1
        else:
            idx = -1
        da = abs(round(da, 4))
        typ = "LINK"
        if da < angle_90:
            typ += "_VALLEY"
        elif da > angle_90:
            typ += "_HIP"
        self.segs[idx].side_type = typ

    def bind(self, last, ccw=False):
        """
            always in axis real direction
        """
        # backward dependancy relative to axis
        if last.backward:
            self.backward = self.side == last.side

        if self.side == last.side:
            last.next_cross = self.cross
        else:
            last.last_cross = self.cross

        self.last_cross = last.cross

        # axis of last / next segments
        if self.backward:
            self.next = last
            last.last = self
        else:
            self.last = last
            last.next = self

        # width auto
        if self.auto_mode == 'AUTO':
            self.width = last.width
            self.slope = last.slope
        elif self.auto_mode == 'WIDTH' and self.width != 0:
            self.slope = last.slope * last.width / self.width
        elif self.auto_mode == 'SLOPE' and self.slope != 0:
            self.width = last.width * last.slope / self.slope

        self.make_segments()
        last.make_segments()

        res, p, t = self.segs[2].intersect(last.segs[2])

        if res:
            # dont move anything when no intersection found
            # aka when delta angle == 0
            self.move_node(p)
            if self.side != last.side:
                last.move_node(p)
            else:
                last.move_next(p)

        # Free mode
        # move border
        # and find intersections
        # with sides
        if self.auto_mode == 'ALL':
            s0 = self._axis.offset(-self.width)
            res, p0, t = self.segs[1].intersect(s0)
            if res:
                self.segs[2].p0 = p0
                # self.segs[1].p1 = p0
            res, p1, t = self.segs[-1].intersect(s0)
            if res:
                # self.segs[2].p1 = p1
                self.segs[-1].p0 = p1

        #   /\
        #   |   angle
        #   |____>
        #
        # v1 node -> next
        if self.side == 'LEFT':
            v1 = self._axis.v_2d
        else:
            v1 = -self._axis.v_2d

        if last.side == self.side:
            # contigous, v0 node <- next

            # half angle between segments
            if self.side == 'LEFT':
                v0 = -last._axis.v_2d
            else:
                v0 = last._axis.v_2d
            da = v0.angle_signed(v1)
            if ccw:
                if da < 0:
                    da = 2 * pi + da
            elif da > 0:
                da = da - 2 * pi
            last.next_link(0.5 * da)

        else:
            # alternate v0 node -> next
            # half angle between segments
            if last.side == 'LEFT':
                v0 = last._axis.v_2d
            else:
                v0 = -last._axis.v_2d
            da = v0.angle_signed(v1)
            # angle always ccw
            if ccw:
                if da < 0:
                    da = 2 * pi + da
            elif da > 0:
                da = da - 2 * pi
            last.node_link(0.5 * da)

        self.node_link(-0.5 * da)

    def next_seg(self, index):
        idx = self.get_index(index + 1)
        return self.segs[idx]

    def last_seg(self, index):
        return self.segs[index - 1]

    def make_segments(self):
        if len(self.segs) < 1:
            s0 = self._axis
            w = self.width
            s = s0.offset(-w)
            p1, p2, p3 = s0.p1, s.p1, s.p0
            s1 = StraightRoof(p1, side_type='SIDE', last=s0)
            s2 = StraightRoof(p2, side_type='BOTTOM', last=s1)
            s3 = StraightRoof(p3, side_type='SIDE', last=s2, after=s0)
            self.segs = [s0, s1, s2, s3]

    def move_side(self, pt):
        """
            offset side to point
        """
        s1, s2, s3 = self.segs[1:4]
        d0, t = self.distance(s2.p0)
        d1, t = self.distance(pt)
        # adjust width and slope according
        self.width = d1
        self.slope = self.slope * d0 / d1
        s2 = s2.offset(d1 - d0)
        self.segs[2] = s2
        s2._next = s3
        s3._last = s2
        s2._last = s1
        s1._next = s2

    def propagate_backward(self, pt):
        """
            Propagate slope, keep 2d angle of slope
            Move first point and border
            keep border parallel
            adjust slope
            and next shape
        """
        # distance of p
        # offset side to point
        self.move_side(pt)

        # move verts on node side
        self.move_next(pt)

        if self.side == 'LEFT':
            # move verts on next side
            res, p, t = self.segs[-1].intersect(self.segs[2])
        else:
            # move verts on next side
            res, p, t = self.segs[1].intersect(self.segs[2])

        if res:
            self.move_node(p)

            if self.next is not None and self.next.auto_mode in {'AUTO'}:
                self.next.propagate_backward(p)

    def propagate_forward(self, pt):
        """
            Propagate slope, keep 2d angle of slope
            Move first point and border
            keep border parallel
            adjust slope
            and next shape
        """
        # offset side to point
        self.move_side(pt)

        # move verts on node side
        self.move_node(pt)
        if self.side == 'LEFT':
            # move verts on next side
            res, p, t = self.segs[1].intersect(self.segs[2])
        else:
            # move verts on next side
            res, p, t = self.segs[-1].intersect(self.segs[2])

        if res:
            self.move_next(p)
            if self.next is not None and self.next.auto_mode in {'AUTO'}:
                self.next.propagate_forward(p)

    def rotate_next_slope(self, a0):
        """
            Rotate next slope part
        """
        if self.side == 'LEFT':
            s0 = self.segs[1].copy().rotate(a0)
            s1 = self.segs[2]
            res, p, t = s1.intersect(s0)
        else:
            s0 = self.segs[2]
            s1 = self.segs[-1]
            res, p, t = s1.opposite.rotate(-a0).intersect(s0)

        if res:
            s1.p0 = p
            # s0.p1 = p

            if self.next is not None:
                if self.next.auto_mode == 'ALL':
                    return
                if self.next.backward:
                    self.next.propagate_backward(p)
                else:
                    self.next.propagate_forward(p)

    def rotate_node_slope(self, a0):
        """
            Rotate node slope part
        """
        if self.side == 'LEFT':
            s0 = self.segs[2]
            s1 = self.segs[-1]
            res, p, t = s1.opposite.rotate(-a0).intersect(s0)
        else:
            s0 = self.segs[1].copy().rotate(a0)
            s1 = self.segs[2]
            res, p, t = s1.intersect(s0)
        if res:
            # will set both
            s1.p0 = p
            # s0.p1 = p

            if self.next is not None:
                if self.next.auto_mode == 'ALL':
                    return
                if self.next.backward:
                    self.next.propagate_backward(p)
                else:
                    self.next.propagate_forward(p)

    def distance(self, pt):
        """
            distance from axis
            always use fake_axis here to
            allow axis being cut and
            still work
        """
        res, d, t = self.fake_axis.point_sur_segment(pt)
        return d, t

    def altitude(self, pt):
        d, t = self.distance(pt)
        return self.z - d * self.slope

    def uv(self, pt):
        d, t = self.distance(pt)
        return (d, (t - self.tmin) * self.xsize)

    def intersect(self, seg):
        """
            compute intersections of a segment with boundaries
            segment must start on axis
            return segments inside
        """
        it = []
        for s in self.segs:
            res, p, t, u = seg.intersect_ext(s)
            if res:
                it.append((t, p))
        return it

    def merge(self, other):

        raise NotImplementedError

    def draw(self, z, verts, edges):
        f = len(verts)
        #
        #   0_______1
        #   |_______|
        #   3       2
        n_segs = len(self.segs) - 1
        if n_segs > 2:
            verts.extend([(s.p0.x, s.p0.y, z + self.altitude(s.p0)) for s in self.segs])
            edges.extend([[f + i, f + i + 1] for i in range(n_segs)])
            edges.append([f + n_segs, f])

        """
        f = len(verts)
        verts.extend([(s.p1.x, s.p1.y, z + self.altitude(s.p1)) for s in self.segs])
        n_segs = len(self.segs) - 1
        edges.extend([[f + i, f + i + 1] for i in range(n_segs)])
        edges.append([f + n_segs, f])
        """
        # holes
        for hole in self.holes:
            f = len(verts)
            #
            #   0_______1
            #   |_______|
            #   3       2
            n_segs = len(hole.segs) - 1
            if n_segs > 2:
                verts.extend([(s.p0.x, s.p0.y, z + self.altitude(s.p0)) for s in hole.segs])
                edges.extend([[f + i, f + i + 1] for i in range(n_segs)])
                edges.append([f + n_segs, f])

        # axis
        """
        f = len(verts)
        verts.extend([self.axis.p0.to_3d(), self.axis.p1.to_3d()])
        edges.append([f, f + 1])

        # cross
        f = len(verts)
        verts.extend([self.axis.lerp(0.5).to_3d(), (self.axis.lerp(0.5) + self.cross.v).to_3d()])
        edges.append([f, f + 1])
        """

        # relationships arrows
        if self.next or self.last:
            w = 0.2
            s0 = self._axis.offset(-0.5 * self.ysize)
            p0 = s0.lerp(0.4)
            p0.z = z
            p1 = s0.lerp(0.6)
            p1.z = z
            if self.side == 'RIGHT':
                p0, p1 = p1, p0
            if self.backward:
                p0, p1 = p1, p0
            s1 = s0.normal(0.5, w)
            s2 = s0.normal(0.5, -w)
            f = len(verts)
            p2 = s1.p1.to_3d()
            p2.z = z
            p3 = s2.p1.to_3d()
            p3.z = z
            verts.extend([p1, p0, p2, p3])
            edges.extend([[f + 1, f], [f + 2, f], [f + 3, f]])

    def as_string(self):
        """
            Print strips relationships
        """
        if self.backward:
            dir = "/\\"
            print("%s next" % (dir))
        else:
            dir = "\\/"
            print("%s node" % (dir))
        print("%s %s" % (dir, self.side))
        if self.backward:
            print("%s node" % (dir))
        else:
            print("%s next" % (dir))
        if self.next:
            print("_________")
            self.next.as_string()
        else:
            print("#########")

    def limits(self):
        dist = []
        param_t = []
        for s in self.segs:
            res, d, t = self.fake_axis.point_sur_segment(s.p0)
            param_t.append(t)
            dist.append(d)
            
        # distance from 0,0, to align covering 
        res, d, t = self.fake_axis.point_sur_segment(Vector())
        self.distance_from_origin = self.fake_axis.length * t
        self.t_from_origin = t
        
        if len(param_t) > 0:
            self.tmin = min(param_t)
            self.tmax = max(param_t)
        else:
            self.tmin = 0
            self.tmax = 1

        self.dt = self.tmax - self.tmin

        if len(dist) > 0:
            self.ysize = max(dist)
        else:
            self.ysize = 0

        self.xsize = self.fake_axis.length * self.dt
        # vectors components of part matrix
        # where x is is axis direction
        # y down
        # z up
        vx = -self.fake_axis.v_normalized
        vy = Vector((-vx.y, vx.x, self.slope)).normalized()
        self.vx = vx
        self.vy = vy
        self.vz = vx.cross(vy)


"""
import bpy
import bmesh

def print_list(name, lst, cols):
    size = len(lst)
    rows = 1 + int(size / cols)
    print("%s" % "{} = [\n    {}\n    ]\n".format(name,
        ",\n    ".join(
            [", ".join([str(lst[r * cols + i]) for i in range(cols) if r * cols + i < size])
            for r in range(rows)
            ])
        ))

def dump_mesh(m, cols, rounding):
    verts = [(round(v.co.x, rounding), round(v.co.y, rounding), round(v.co.z, rounding)) for v in m.vertices]
    faces = [tuple(p.vertices) for p in m.polygons]
    bpy.ops.object.mode_set(mode='EDIT')
    bm = bmesh.from_edit_mesh(m)
    edges = [tuple(i.index for i in edge.verts) for edge in bm.edges]
    uvs = []
    layer = bm.loops.layers.uv.verify()
    for i, face in enumerate(bm.faces):
        uv = []
        for j, loop in enumerate(face.loops):
            co = loop[layer].uv
            uv.append((round(co.x, rounding), round(co.y, rounding)))
        uvs.append(uv)
    matids = [p.material_index for p in m.polygons]
    print_list("verts", verts, cols)
    print_list("faces", faces, cols)
    print_list("matids", matids, cols)
    print_list("uvs", uvs, cols)
    print_list("edges", edges, cols)
    
def dump_curve(m, cols, rounding):
    verts = [(round(v.co.x, rounding), round(v.co.y, rounding), round(v.co.z, rounding)) for v in m.points]
    print_list("verts", verts, cols)


cols = 1
rounding = 3
m = C.object.data
dump_mesh(m, cols, rounding)

for c in m.splines:
    dump_curve(c, cols, rounding)

"""
# keep a reference to material enums
material_enums = []
mat_enum, mat_index_getter, mat_index_setter= build_mat_enum('idmat', material_enums)


class RoofGenerator(CutAbleGenerator):

    def __init__(self, o=None):
        CutAbleGenerator.__init__(self, o)
        self.nodes = []
        self.pans = []
        self.length = 0
        self.z = 0
        self.width_right = 0
        self.width_left = 0
        self.slope_left = 0
        self.slope_right = 0
        self.user_defined_tile = None
        self.user_defined_uvs = None
        self.user_defined_mat = None
        self.is_t_child = False

    @property
    def random_color(self):
        return Vector((uniform(0.0, 1.0), uniform(0.0, 1.0), uniform(0.0, 1.0), 1))

    def create_segment(self, d, part, co: Vector, last, last_type: str, p1=None, next=None):

        if part.bound_idx < 1:
            last = None

        a0 = part.a0

        if part.constraint_type == 'SLOPE' and a0 == 0:
            a0 = 90

        _k = StraightRoof(co, p1=p1, last=None)

        # parent segment (root) index is  v0_idx - 1
        _k.v0_idx = min(len(self.segs), part.bound_idx)
        _k.constraint_type = part.constraint_type

        if part.constraint_type == 'SLOPE':
            _k.enforce_part = part.enforce_part
        else:
            _k.enforce_part = 'AUTO'

        _k.angle_0 = a0
        _k.take_precedence = part.take_precedence
        _k.auto_right = part.auto_right
        _k.auto_left = part.auto_left
        _k.width_left = part.width_left
        _k.width_right = part.width_right
        _k.slope_left = part.slope_left
        _k.slope_right = part.slope_right
        _k.side_type = 'AXIS'
        _k.triangular_end = part.triangular_end

        _k.remove_left = not (part.enable_left and d.enable_left)
        _k.remove_right = not (part.enable_right and d.enable_right)

        return _k

    def _add_part(self, d, part, last, co: Vector, rM: Matrix, last_type: str, next=None):
        """Append a part to end of curve
        :param d: archipack_segments derived datablock
        :param part: archipack_segment derived instance
        :param co: Vector location of first knot
        :param last: archipack_segment derived instance, last segment
        :param last_type: last part type
        :param next: archipack_segment derived instance, next segment
        :return: Knot, Vector, Matrix, str
        """
        _type = part.type

        # compute location of next knot
        if _type == 0:
            _v = Vector((part.length, 0, 0))
        else:
            # vector p0 p1 length
            _v = Vector((part.radius, 0, 0))
            _v += Matrix.Rotation(part.da, 3, 'Z') @ -_v
            _v = Vector((_v.length, 0, 0))

        # set first point and rotation
        v0_idx = part.bound_idx

        if v0_idx < len(self.segs):
            _s = self.segs[v0_idx - 1]
            _rM = Matrix.Rotation(part.a0 + _s.v_angle, 3, 'Z')
            _c0 = _s.p1

        else:
            _rM = Matrix.Rotation(part.a0, 3, 'Z') @ rM
            _c0 = co

        _co = _c0 + _rM @ _v

        _k = self.create_segment(d, part, _c0, last, last_type, p1=_co, next=next)

        _k.idx = self.numknots

        self.segs.append(_k)

        return _k, _co, _rM, _type

    def locate_manipulators(self, d):
        """

        """
        n_parts = len(d.parts)
        for i, f in enumerate(self.segs):
            if i < n_parts:
                part = d.parts[i]
                manipulators = part.manipulators
                p0 = f.p0.to_3d()
                p0.z = self.z
                p1 = f.p1.to_3d()
                p1.z = self.z

                # angle from last to current segment
                if i > 0:
                    dist = 1
                    if abs(part.a0) < 0.4:
                        dist = 0.8
                    manipulators[0].type_key = 'ANGLE'
                    v0 = self.segs[f.v0_idx - 1].straight(-1, 1).v.to_3d()
                    v1 = f.straight(1, 0).v.to_3d()
                    manipulators[0].set_pts([p0, dist * v0, dist * v1])

                # segment length
                manipulators[1].type_key = 'SIZE'
                manipulators[1].prop1_name = "length"
                manipulators[1].set_pts([p0, p1, (1, 0, 0)])

                # dumb segment id
                manipulators[2].set_pts([p0, p1, (1, 0, 0)])

                p0 = f.lerp(0.5).to_3d()
                p0.z = self.z
                # size left
                p1 = f.normal(0.5, -part.width_left).p1.to_3d()
                p1.z = self.z
                manipulators[3].set_pts([p0, p1, (1, 0, 0)])

                # size right
                p1 = f.normal(0.5, part.width_right).p1.to_3d()
                p1.z = self.z
                manipulators[4].set_pts([p0, p1, (-1, 0, 0)])

                # slope left
                n0 = f.normal(0.5, -1)
                p0 = n0.p1.to_3d()
                p0.z = self.z
                p1 = p0.copy()
                p1.z = self.z - part.slope_left
                manipulators[5].set_pts([p0, p1, (-1, 0, 0)], normal=n0.v.to_3d())

                # slope right
                n0 = f.normal(0.5, 1)
                p0 = n0.p1.copy()
                p0.z = self.z
                p1 = p0.copy()
                p1.z = self.z - part.slope_right
                manipulators[6].set_pts([p0, p1, (1, 0, 0)], normal=n0.v.to_3d())

    def make_roof(self, d):
        """
            Init data structure for possibly multi branched nodes
            nodes : radial relationships
            pans : quad strip linear relationships
        """
        self.z = d.origin.z
        self.width_right = d.width_right
        self.width_left = d.width_left
        self.slope_left = d.slope_left
        self.slope_right = d.slope_right
        self.user_defined_tile = None
        self.user_defined_uvs = None
        self.user_defined_mat = None
        self.is_t_child = d.t_parent != ""

        pans = []

        # node are connected segments
        # node
        # (segment idx)
        # (angle from root part > 0 right)
        # (reversed) a seg connected by p1
        #            "root" of node
        nodes = [RoofAxisNode() for s in range(len(self.segs) + 1)]

        # Init width on seg 0
        s0 = self.segs[0]
        if d.parts[0].auto_left in {'AUTO', 'SLOPE'}:
            s0.width_left = self.width_left
        if d.parts[0].auto_right in {'AUTO', 'SLOPE'}:
            s0.width_right = self.width_right
        if d.parts[0].auto_left in {'AUTO', 'WIDTH'}:
            s0.slope_left = self.slope_left
        if d.parts[0].auto_left in {'AUTO', 'WIDTH'}:
            s0.slope_right = self.slope_right

        # make nodes with HORIZONTAL constraints
        for idx, s in enumerate(self.segs):
            s.v1_idx = idx + 1
            if s.constraint_type == 'HORIZONTAL':
                # print("RoofGenerator.make_roof() axis:", s)
                left = RoofPolygon(s, 'LEFT', remove=s.remove_left)
                right = RoofPolygon(s, 'RIGHT', remove=s.remove_right)
                left.other_side = right
                right.other_side = left
                rs = RoofSegment(s, left, right)
                pans.append(rs)
                nodes[s.v0_idx].add(s.angle_0, False, s, left, right)
                nodes[s.v1_idx].add(-pi, True, s, left, right)

        # set first node root
        # so regular sort does work
        nodes[0].root = nodes[0].segs[0]
        self.nodes = nodes
        # Propagate slope and width
        # on node basis along axis
        # bi-direction Radial around node
        # from left and right to center
        # contigous -> same
        # T: and (x % 2 == 1)
        # First one take precedence over others
        # others inherit from side
        #
        #         l / rb    l = left
        #          3        r = right
        #   l _1_ /         b = backward
        #   r     \
        #          2
        #          r\ l
        #
        # X: rigth one r left one l (x % 2 == 0)
        # inherits from side
        #
        #    l 3 lb         l = left
        # l__1_|_2_l        r = right
        # r    |   r        b = backward -> propagate in reverse axis direction
        #    r 4 rb
        #
        # for idx, node in enumerate(nodes):
        #    print("idx:%s node:%s" % (idx, node.root))

        for idx, node in enumerate(nodes):

            node.sort()

            nb_segs = node.count

            if node.root is None:
                continue

            left = node.root.left
            right = node.root.right

            # basic one single node
            if nb_segs < 2:
                left.make_segments()
                right.make_segments()
                continue

            # get "root" slope and width
            l_bind = left
            r_bind = right

            # simple case: 2 contigous segments
            if nb_segs == 2:
                s = node.last
                s.right.bind(r_bind, ccw=False)
                s.left.bind(l_bind, ccw=True)
                continue

            # More than 2 segments, uneven distribution
            if nb_segs % 2 == 1:
                # find wich child does take precedence
                # first one on rootline (arbitrary)
                center = (nb_segs - 1) / 2
            else:
                # even distribution
                center = nb_segs / 2

            # user defined precedence if any
            for i, s in enumerate(node.segs):
                if s.seg.take_precedence:
                    center = i
                    break

            # bind right side to center
            for i, s in enumerate(node.segs):
                # skip axis
                if i > 0:
                    if i < center:
                        # right contigous with last
                        s.right.bind(r_bind, ccw=False)

                        # next bind to left
                        r_bind = s.left

                        # left backward, not bound
                        # so setup width and slope
                        if s.left.auto_mode in {'AUTO', 'WIDTH'}:
                            s.left.slope = right.slope
                        if s.left.auto_mode in {'AUTO', 'SLOPE'}:
                            s.left.width = right.width
                        s.left.backward = True
                    else:
                        # right bound to last
                        s.right.bind(r_bind, ccw=False)
                        break

            # bind left side to center
            for i, s in enumerate(reversed(node.segs)):
                # skip axis
                if i < nb_segs - center - 1:
                    # left contigous with last
                    s.left.bind(l_bind, ccw=True)
                    # next bind to right
                    l_bind = s.right
                    # right backward, not bound
                    # so setup width and slope
                    if s.right.auto_mode in {'AUTO', 'WIDTH'}:
                        s.right.slope = left.slope
                    if s.right.auto_mode in {'AUTO', 'SLOPE'}:
                        s.right.width = left.width
                    s.right.backward = True
                else:
                    # right bound to last
                    s.left.bind(l_bind, ccw=True)
                    break

        # slope constraints allowed between segments
        # multiple (up to 2) on start and end
        # single between others
        #
        #    2 slope            2 slope           2 slope
        #     |                  |                 |
        #     |______section_1___|___section_2_____|
        #     |                  |                 |
        #     |                  |                 |
        #    multiple           single            multiple

        # add slopes constraints to nodes
        for i, s in enumerate(self.segs):
            if s.constraint_type == 'SLOPE':
                nodes[s.v0_idx].add(s.angle_0, False, s, None, None)

        # sort nodes, remove duplicate slopes between
        # horizontal, keeping only first one
        for idx, node in enumerate(nodes):
            to_remove = []
            node.sort()
            # remove dup between all
            # but start / end nodes
            if node.n_horizontal > 1:
                last = None
                for i, s in enumerate(node.segs):
                    if s.seg.constraint_type == last:
                        if s.seg.constraint_type == 'SLOPE':
                            to_remove.append(i)
                    last = s.seg.constraint_type
                for i in reversed(to_remove):
                    node.segs.pop(i)
                node.update_center()

        for idx, node in enumerate(nodes):

            # a node may contain many slopes
            # 2 * (part starting from node - 1)
            #
            #        s0
            # root 0 |_______
            #        |
            #        s1
            #
            #               s1
            # root   _______|
            #               |
            #               s0
            #
            #       s3  3  s2
            #     l   \l|r/ l
            # root  ___\|/___ 2
            #     r    /|\  r
            #         /r|l\
            #       s0  1  s1
            #
            #        s2  s1=slope
            #        |r /
            #        | / l
            #        |/____s
            #
            # root to first child -> equal side
            # any other childs -> opposite sides

            if node.n_horizontal == 1:
                # slopes at start or end of segment
                # segment slope is not affected
                if node.n_slope > 0:
                    # node has user def slope
                    s = node.root
                    s0 = node.left(node.center)
                    # with nested slopes we not always have valid s0
                    if s is not None and s0 is not None:
                        a0 = s0.seg.delta_angle(s.seg)
                        if node.root.reversed:
                            # slope at end of segment
                            # first one is right or left
                            if a0 < 0:
                                # right side
                                res, p, t = s0.seg.intersect(s.right.segs[2])
                                s.right.segs[-1].p0 = p
                                s.right.segs[2].p1 = p
                            else:
                                # left side
                                res, p, t = s0.seg.intersect(s.left.segs[2])
                                s.left.segs[1].p1 = p
                                s.left.segs[2].p0 = p
                            if node.n_slope > 1:
                                # last one must be left
                                s1 = node.right(node.center)
                                a1 = s1.seg.delta_angle(s.seg)
                                # both slopes on same side:
                                # skip this one
                                if a0 > 0 and a1 < 0:
                                    # right side
                                    res, p, t = s1.seg.intersect(s.right.segs[2])
                                    s.right.segs[-1].p0 = p
                                    s.right.segs[2].p1 = p
                                if a0 < 0 and a1 > 0:
                                    # left side
                                    res, p, t = s1.seg.intersect(s.left.segs[2])
                                    s.left.segs[1].p1 = p
                                    s.left.segs[2].p0 = p

                        else:
                            # slope at start of segment
                            if a0 < 0:
                                # right side
                                res, p, t = s0.seg.intersect(s.right.segs[2])
                                s.right.segs[1].p1 = p
                                s.right.segs[2].p0 = p
                            else:
                                # left side
                                res, p, t = s0.seg.intersect(s.left.segs[2])
                                s.left.segs[-1].p0 = p
                                s.left.segs[2].p1 = p
                            if node.n_slope > 1:
                                # last one must be right
                                s1 = node.right(node.center)
                                a1 = s1.seg.delta_angle(s.seg)
                                # both slopes on same side:
                                # skip this one
                                if a0 > 0 and a1 < 0:
                                    # right side
                                    res, p, t = s1.seg.intersect(s.right.segs[2])
                                    s.right.segs[1].p1 = p
                                    s.right.segs[2].p0 = p
                                if a0 < 0 and a1 > 0:
                                    # left side
                                    res, p, t = s1.seg.intersect(s.left.segs[2])
                                    s.left.segs[-1].p0 = p
                                    s.left.segs[2].p1 = p

            else:
                # slopes between segments
                # does change next segment slope
                for i, s0 in enumerate(node.segs):
                    s1 = node.left(i)
                    s2 = node.left(i + 1)

                    if s0.seg.constraint_type == 'SLOPE':
                        continue

                    if s1.seg.constraint_type == 'SLOPE':

                        # 3 cases:
                        # s0 is root contigous -> sides are same
                        # s2 is root contigous -> sides are same
                        # back to back -> sides are not same

                        if s0.reversed:
                            # contigous right / right
                            # 2 cases
                            # right is backward
                            # right is forward
                            if s2.right.backward:
                                # s0 depends on s2
                                main = s2.right
                                v = main.segs[1].v_2d
                            else:
                                # s2 depends on s0
                                main = s0.right
                                v = -main.segs[-1].v_2d
                            res, p, t = s1.seg.intersect(main.segs[2])
                            if res:
                                # slope vector
                                dp = p - s1.seg.p0
                                a0 = dp.to_2d().angle_signed(v)
                                if s2.right.backward:
                                    main.rotate_node_slope(a0)
                                else:
                                    main.rotate_next_slope(-a0)
                        elif s2.reversed:
                            # contigous left / left
                            # 2 cases
                            # left is backward
                            # left is forward
                            if s0.left.backward:
                                # s0 depends on s2
                                main = s0.left
                                v = -main.segs[-1].v_2d
                            else:
                                # s2 depends on s0
                                main = s2.left
                                v = main.segs[1].v_2d
                            res, p, t = s1.seg.intersect(main.segs[2])
                            if res:
                                # slope vector
                                dp = p - s1.seg.p0
                                a0 = dp.to_2d().angle_signed(v)
                                if s0.left.backward:
                                    main.rotate_node_slope(-a0)
                                else:
                                    main.rotate_next_slope(a0)
                        else:
                            # back left / right
                            # 2 cases
                            # left is backward
                            # left is forward
                            if s0.left.backward:
                                # s2 depends on s0
                                main = s0.left
                                v = -main.segs[-1].v_2d
                            else:
                                # s0 depends on s2
                                main = s2.right
                                v = main.segs[1].v_2d

                            res, p, t = s1.seg.intersect(main.segs[2])
                            if res:
                                # slope vector
                                dp = p - s1.seg.p0
                                a0 = dp.to_2d().angle_signed(v)
                                if s0.left.backward:
                                    main.rotate_node_slope(-a0)
                                else:
                                    main.rotate_node_slope(a0)

        self.pans = []

        # triangular ends
        for node in self.nodes:
            if node.root is None:
                continue
            if node.n_horizontal == 1 and node.root.seg.triangular_end:
                if node.root.reversed:
                    # Next side (segment end)
                    left = node.root.left
                    right = node.root.right
                    left.next_tri = True
                    right.next_tri = True

                    s0 = left.segs[1]
                    s1 = left.segs[2]
                    s2 = right.segs[-1]
                    s3 = right.segs[2]
                    p0 = s1.lerp(-left.width / s1.length)
                    p1 = s0.p0
                    p2 = s3.lerp(1 + right.width / s3.length)

                    # compute slope from points
                    p3 = p0.to_3d()
                    p3.z = -left.width * left.slope
                    p4 = p1.to_3d()
                    p5 = p2.to_3d()
                    p5.z = -right.width * right.slope
                    n = (p3 - p4).normalized().cross((p5 - p4).normalized())
                    v = n.cross(Vector((0, 0, 1)))
                    dz = n.cross(v)

                    # compute axis
                    s = StraightRoof(p1, p1 + v)
                    res, d0, t = s.point_sur_segment(p0)
                    res, d1, t = s.point_sur_segment(p2)
                    p = RoofPolygon(s, 'RIGHT')
                    p.make_segments()
                    p.slope = -dz.z / dz.to_2d().length
                    p.is_tri = True

                    p.cross = StraightRoof(p1, p1 + (p2 - p0)).normal(0, -1)
                    p.next_cross = left.cross
                    p.last_cross = right.cross
                    right.next_cross = p.cross
                    left.next_cross = p.cross

                    # remove axis seg of tri
                    p.segs[-1].p0 = p0
                    # p.segs[-1].p1 = p1
                    p.segs[2].p0 = p2
                    # p.segs[2].p1 = p0
                    # p.segs[1].p1 = p2
                    p.segs[1].p0 = p1
                    p.segs[1].side_type = 'LINK_HIP'
                    p.segs[-1].side_type = 'LINK_HIP'
                    p.segs.pop(0)
                    p.segs[0]._last = p.segs[-1]
                    p.segs[-1]._next = p.segs[0]
                    # adjust left and side borders
                    # s0.p1 = p0
                    s1.p0 = p0
                    s2.p0 = p2
                    # s3.p1 = p2
                    s0.side_type = 'LINK_HIP'
                    s2.side_type = 'LINK_HIP'
                    self.pans.append(p)

                elif not self.is_t_child:
                    # no triangular part with t_child
                    # on "node" parent roof side
                    left = node.root.left
                    right = node.root.right
                    left.node_tri = True
                    right.node_tri = True
                    s0 = right.segs[1]
                    s1 = right.segs[2]
                    s2 = left.segs[-1]
                    s3 = left.segs[2]
                    p0 = s1.lerp(-right.width / s1.length)
                    p1 = s0.p0
                    p2 = s3.lerp(1 + left.width / s3.length)

                    # compute axis and slope from points
                    p3 = p0.copy()
                    p3.z = -right.width * right.slope
                    p4 = p1.copy()
                    p5 = p2.copy()
                    p5.z = -left.width * left.slope
                    n = (p3 - p4).normalized().cross((p5 - p4).normalized())
                    v = n.cross(Vector((0, 0, 1)))
                    dz = n.cross(v)

                    s = StraightRoof(p1, p1 + v)
                    p = RoofPolygon(s, 'RIGHT')
                    p.make_segments()
                    p.slope = -dz.z / dz.to_2d().length
                    p.is_tri = True

                    p.cross = StraightRoof(p1, p1 + (p2 - p0)).normal(0, -1)
                    p.next_cross = right.cross
                    p.last_cross = left.cross
                    right.last_cross = p.cross
                    left.last_cross = p.cross

                    # remove axis seg of tri
                    p.segs[-1].p0 = p0
                    # p.segs[-1].p1 = p1
                    p.segs[2].p0 = p2
                    # p.segs[2].p1 = p0
                    # p.segs[1].p1 = p2
                    p.segs[1].p0 = p1
                    p.segs[1].side_type = 'LINK_HIP'
                    p.segs[-1].side_type = 'LINK_HIP'
                    p.segs.pop(0)
                    p.segs[0]._last = p.segs[-1]
                    p.segs[-1]._next = p.segs[0]
                    # adjust left and side borders
                    # s0.p1 = p0
                    s1.p0 = p0
                    s2.p0 = p2
                    # s3.p1 = p2
                    s0.side_type = 'LINK_HIP'
                    s2.side_type = 'LINK_HIP'
                    self.pans.append(p)

        # make flat array
        for pan in pans:
            self.pans.extend([pan.left, pan.right])

        # merge contigous with 0 angle diff
        to_remove = []
        for i, pan in enumerate(self.pans):

            if pan.remove:
                to_remove.append(i)
            else:
                if pan.backward:
                    next = pan.last
                    if next is not None:
                        # same side only can merge
                        if next.side == pan.side and not next.remove:
                            if round(next._axis.delta_angle(pan._axis), 4) == 0:
                                to_remove.append(i)
                                next.next = pan.next
                                next.last_cross = pan.last_cross
                                next.node_tri = pan.node_tri

                                next.slope = pan.slope
                                if pan.side == 'RIGHT':
                                    if next.backward:
                                        next._axis.p1 = pan._axis.p1
                                        next.segs[1] = pan.segs[1]
                                        next.segs[2].p0 = pan.segs[2].p0
                                    else:
                                        next._axis.p0 = pan._axis.p0
                                        next.segs[-1] = pan.segs[-1]
                                        next.segs[2].p1 = pan.segs[2].p1
                                else:
                                    if next.backward:
                                        next._axis.p0 = pan._axis.p0
                                        next.segs[-1] = pan.segs[-1]
                                        next.segs[2].p1 = pan.segs[2].p1
                                    else:
                                        next._axis.p1 = pan._axis.p1
                                        next.segs[1] = pan.segs[1]
                                        next.segs[2].p0 = pan.segs[2].p0
                else:
                    next = pan.next
                    if next is not None:
                        # same side only can merge
                        if next.side == pan.side and not next.remove:
                            if round(next._axis.delta_angle(pan._axis), 4) == 0:
                                to_remove.append(i)
                                next.last = pan.last
                                next.last_cross = pan.last_cross
                                next.node_tri = pan.node_tri

                                next.slope = pan.slope
                                if pan.side == 'LEFT':
                                    if next.backward:
                                        next._axis.p1 = pan._axis.p1
                                        next.segs[1] = pan.segs[1]
                                        next.segs[2].p0 = pan.segs[2].p0
                                    else:
                                        next._axis.p0 = pan._axis.p0
                                        next.segs[-1] = pan.segs[-1]
                                        next.segs[2].p1 = pan.segs[2].p1
                                else:
                                    if next.backward:
                                        next._axis.p0 = pan._axis.p0
                                        next.segs[-1] = pan.segs[-1]
                                        next.segs[2].p1 = pan.segs[2].p1
                                    else:
                                        next._axis.p1 = pan._axis.p1
                                        next.segs[1] = pan.segs[1]
                                        next.segs[2].p0 = pan.segs[2].p0

        for i in reversed(to_remove):
            self.pans.pop(i)

        # compute limits
        for pan in self.pans:
            pan.limits()

        """
        for pan in self.pans:
            if pan.last is None:
                pan.as_string()
        """
        return

    def lambris(self, o, d):

        idmat = d.id_mat(MAT_LAMBRIS)

        alt = self.z
        for pan in self.pans:

            verts = []
            faces = []
            matids = []
            uvs = []

            f = len(verts)
            verts.extend([(s.p0.x, s.p0.y, alt + pan.altitude(s.p0)) for s in pan.segs])
            uvs.append([pan.uv(s.p0) for s in pan.segs])
            n_segs = len(pan.segs)
            face = [f + i for i in range(n_segs)]
            faces.append(face)
            matids.append(idmat)

            bm = bmed.buildmesh(o, verts, faces, matids, uvs,
                weld=False, clean=False,  temporary=True)

            self.cut_holes(bm, pan)

            geom = bm.faces[:]
            verts = bm.verts[:]
            bmesh.ops.solidify(bm, geom=geom, thickness=0.0001)
            bmesh.ops.translate(bm, vec=Vector((0, 0, d.lambris_height)), space=o.matrix_world, verts=verts)

            # merge with object
            bmed.bmesh_join(o, [bm], normal_update=True)

        bpy.ops.object.mode_set(mode='OBJECT')

    def couverture(self, o, d):

        idmat = d.id_mat(MAT_COVERING)

        ttl = len(self.pans)
        if ttl < 1:
            return

        sx, sy, sz = d.tile_size_x, d.tile_size_y, d.tile_size_z
        random_scale = d.tile_random_scale / 100

        """
        /* Bevel offset_type slot values */
        enum {
          BEVEL_AMT_OFFSET,
          BEVEL_AMT_WIDTH,
          BEVEL_AMT_DEPTH,
          BEVEL_AMT_PERCENT
        };
        """
        # offset_type = 3

        offset_type = 'PERCENT'

        if d.tile_offset > 0:
            offset = - d.tile_offset / 100
        else:
            offset = 0

        if d.tile_model == 'BRAAS2':
            t_pts = [Vector(p) for p in [
                (0.06, -1.0, 1.0), (0.19, -1.0, 0.5), (0.31, -1.0, 0.5), (0.44, -1.0, 1.0),
                (0.56, -1.0, 1.0), (0.69, -1.0, 0.5), (0.81, -1.0, 0.5), (0.94, -1.0, 1.0),
                (0.06, 0.0, 0.5), (0.19, 0.0, 0.0), (0.31, 0.0, 0.0), (0.44, 0.0, 0.5),
                (0.56, 0.0, 0.5), (0.69, 0.0, 0.0), (0.81, 0.0, 0.0), (0.94, 0.0, 0.5),
                (-0.0, -1.0, 1.0), (-0.0, 0.0, 0.5), (1.0, -1.0, 1.0), (1.0, 0.0, 0.5)]]
            t_faces = [
                (16, 0, 8, 17), (0, 1, 9, 8), (1, 2, 10, 9), (2, 3, 11, 10),
                (3, 4, 12, 11), (4, 5, 13, 12), (5, 6, 14, 13), (6, 7, 15, 14), (7, 18, 19, 15)]
        elif d.tile_model == 'BRAAS1':
            t_pts = [Vector(p) for p in [
                (0.1, -1.0, 1.0), (0.2, -1.0, 0.5), (0.6, -1.0, 0.5), (0.7, -1.0, 1.0),
                (0.1, 0.0, 0.5), (0.2, 0.0, 0.0), (0.6, 0.0, 0.0), (0.7, 0.0, 0.5),
                (-0.0, -1.0, 1.0), (-0.0, 0.0, 0.5), (1.0, -1.0, 1.0), (1.0, 0.0, 0.5)]]
            t_faces = [(8, 0, 4, 9), (0, 1, 5, 4), (1, 2, 6, 5), (2, 3, 7, 6), (3, 10, 11, 7)]
        elif d.tile_model == 'ETERNIT':
            t_pts = [Vector(p) for p in [
                (0.11, -1.0, 1.0), (0.9, -1.0, 1.0), (0.0, -0.79, 0.79),
                (1.0, -0.79, 0.79), (0.0, 2.0, -2.0), (1.0, 2.0, -2.0)]]
            t_faces = [(0, 1, 3, 5, 4, 2)]
        elif d.tile_model == 'ONDULEE':
            t_pts = [Vector(p) for p in [
                (0.0, -1.0, 0.1), (0.05, -1.0, 1.0), (0.1, -1.0, 0.1),
                (0.15, -1.0, 1.0), (0.2, -1.0, 0.1), (0.25, -1.0, 1.0),
                (0.3, -1.0, 0.1), (0.35, -1.0, 1.0), (0.4, -1.0, 0.1),
                (0.45, -1.0, 1.0), (0.5, -1.0, 0.1), (0.55, -1.0, 1.0),
                (0.6, -1.0, 0.1), (0.65, -1.0, 1.0), (0.7, -1.0, 0.1),
                (0.75, -1.0, 1.0), (0.8, -1.0, 0.1), (0.85, -1.0, 1.0),
                (0.9, -1.0, 0.1), (0.95, -1.0, 1.0), (1.0, -1.0, 0.1),
                (0.0, 0.0, 0.0), (0.05, 0.0, 0.9), (0.1, 0.0, 0.0),
                (0.15, 0.0, 0.9), (0.2, 0.0, 0.0), (0.25, 0.0, 0.9),
                (0.3, 0.0, 0.0), (0.35, 0.0, 0.9), (0.4, 0.0, 0.0),
                (0.45, 0.0, 0.9), (0.5, 0.0, 0.0), (0.55, 0.0, 0.9),
                (0.6, 0.0, 0.0), (0.65, 0.0, 0.9), (0.7, 0.0, 0.0),
                (0.75, 0.0, 0.9), (0.8, 0.0, 0.0), (0.85, 0.0, 0.9),
                (0.9, 0.0, 0.0), (0.95, 0.0, 0.9), (1.0, 0.0, 0.0)]]
            t_faces = [
                (0, 1, 22, 21), (1, 2, 23, 22), (2, 3, 24, 23),
                (3, 4, 25, 24), (4, 5, 26, 25), (5, 6, 27, 26),
                (6, 7, 28, 27), (7, 8, 29, 28), (8, 9, 30, 29),
                (9, 10, 31, 30), (10, 11, 32, 31), (11, 12, 33, 32),
                (12, 13, 34, 33), (13, 14, 35, 34), (14, 15, 36, 35),
                (15, 16, 37, 36), (16, 17, 38, 37), (17, 18, 39, 38),
                (18, 19, 40, 39), (19, 20, 41, 40)]
        elif d.tile_model == 'METAL':
            t_pts = [Vector(p) for p in [
                (0.0, -1.0, 0.0), (0.99, -1.0, 0.0), (1.0, -1.0, 0.0),
                (0.0, 0.0, 0.0), (0.99, 0.0, 0.0), (1.0, 0.0, 0.0),
                (0.99, -1.0, 1.0), (1.0, -1.0, 1.0), (1.0, 0.0, 1.0), (0.99, 0.0, 1.0)]]
            t_faces = [(0, 1, 4, 3), (7, 2, 5, 8), (1, 6, 9, 4), (6, 7, 8, 9)]
        elif d.tile_model == 'LAUZE':

            t_pts = [Vector(p) for p in [
                (0.75, -0.8, 0.8), (0.5, -1.0, 1.0), (0.25, -0.8, 0.8),
                (0.0, -0.5, 0.5), (1.0, -0.5, 0.5), (0.0, 0.5, -0.5), (1.0, 0.5, -0.5)]]
            l_pts = [[p.copy() for p in t_pts] for i in range(10)]
            l_uvs = []
            t_faces = [(1, 0, 4, 6, 5, 3, 2)]
            for t_pts in l_pts:
                for i, p in enumerate(t_pts):
                    if i < 3:
                        p.x += uniform(-0.05, 0.05)
                        p.y += uniform(0, -0.1)
                    elif i < 4 or i > 5:
                        p.y += uniform(0.05, -0.05)
                l_uvs.append([[(t_pts[i].x, t_pts[i].y) for i in f] for f in t_faces])

        elif d.tile_model == 'PLACEHOLDER':
            t_pts = [Vector(p) for p in [(0.0, -1.0, 1.0), (1.0, -1.0, 1.0), (0.0, 0.0, 0.0), (1.0, 0.0, 0.0)]]
            t_faces = [(0, 1, 3, 2)]
        elif d.tile_model == 'ROMAN':
            t_pts = [Vector(p) for p in [
                (0.18, 0.0, 0.3), (0.24, 0.0, 0.58), (0.76, 0.0, 0.58),
                (0.82, 0.0, 0.3), (0.05, -1.0, 0.5), (0.14, -1.0, 0.8),
                (0.86, -1.0, 0.8), (0.95, -1.0, 0.5), (0.45, 0.0, 0.5),
                (0.36, 0.0, 0.2), (-0.36, 0.0, 0.2), (-0.45, -0.0, 0.5),
                (0.32, -1.0, 0.7), (0.26, -1.0, 0.42), (-0.26, -1.0, 0.42),
                (-0.32, -1.0, 0.7), (0.5, 0.0, 0.74), (0.5, -1.0, 1.0),
                (-0.0, -1.0, 0.26), (-0.0, 0.0, 0.0)]
            ]
            t_faces = [
                (0, 4, 5, 1), (16, 17, 6, 2), (2, 6, 7, 3),
                (13, 12, 8, 9), (18, 13, 9, 19), (15, 14, 10, 11),
                (14, 18, 19, 10), (1, 5, 17, 16)
            ]
        elif d.tile_model == 'ROUND':
            t_pts = [Vector(p) for p in [
                (0.0, -0.5, 0.5), (1.0, -0.5, 0.5), (0.0, 0.0, 0.0),
                (1.0, 0.0, 0.0), (0.93, -0.71, 0.71), (0.78, -0.88, 0.88),
                (0.39, -0.97, 0.97), (0.61, -0.97, 0.97), (0.07, -0.71, 0.71),
                (0.22, -0.88, 0.88)]
            ]
            t_faces = [(6, 7, 5, 4, 1, 3, 2, 0, 8, 9)]
        else:
            return

        n_faces = len(t_faces)
        t_uvs = [[(t_pts[i].x, t_pts[i].y) for i in f] for f in t_faces]

        dx, dy = d.tile_space_x, d.tile_space_y

        step = 100 / ttl

        # if d.quick_edit:
        #    context.scene.archipack_progress_text = "Build tiles:"

        for i, pan in enumerate(self.pans):

            seg = pan.fake_axis
            
            # t param so covering align to a grid from origin
            d_abs = (pan.t_from_origin - pan.tmax) * seg.length
            align_t = (d_abs % dx) / seg.length    
            
            # compute base matrix top left of face
            vx = pan.vx
            vy = pan.vy
            vz = pan.vz

            x0, y0, z = seg.lerp(pan.tmax + align_t)
            z0 = self.z + pan.z + d.tile_altitude
            ysize_2d = (d.tile_border + pan.ysize)
            space_x = pan.xsize + 2 * d.tile_side
            space_y = ysize_2d * sqrt(1 + pan.slope * pan.slope)
            n_x = 2 + int(space_x / dx)
            n_y = 1 + int(space_y / dy)

            if d.tile_fit_x:
                dx = space_x / n_x

            if d.tile_fit_y:
                dy = space_y / n_y

            if d.tile_alternate:
                n_y += 1

            tM = Matrix([
                [vx.x, vy.x, vz.x, x0],
                [vx.y, vy.y, vz.y, y0],
                [vx.z, vy.z, vz.z, z0],
                [0, 0, 0, 1]
            ])

            verts = []
            faces = []
            matids = []
            uvs = []
            vcolors = []
            # steps for this pan
            substep = step / n_y
            # print("step:%s sub:%s" % (step, substep))

            for k in range(n_y):

                y = k * dy

                x0 = offset * dx - d.tile_side
                nx = n_x

                if d.tile_alternate and k % 2 == 1:
                    x0 -= 0.5 * dx
                    nx += 1

                if d.tile_offset > 0:
                    nx += 1

                for j in range(nx):
                    x = x0 + j * dx

                    rz = d.tile_random_z * uniform(0.0, 1.0)
                    az = d.tile_random_angle_z * uniform(-1.0, 1.0)
                    ay = d.tile_random_angle_y * uniform(0.0, 1.0)
                    _sy = sy - random_scale * uniform(0.0, 1.0)
                    _sx = sx - random_scale * uniform(0.0, 1.0)

                    lM = tM @ Matrix([
                        [_sx, 0, 0, x],
                        [0, _sy, 0, -y],
                        [0, 0, sz, rz],
                        [0, 0, 0, 1]
                    ]) @ Matrix.Rotation(az, 4, "Z") @ Matrix.Rotation(ay, 4, "Y")

                    v = len(verts)

                    color = self.random_color

                    if d.tile_model == "LAUZE":
                        _r = randint(0, 9)
                        t_pts = l_pts[_r]
                        t_uvs = l_uvs[_r]

                    verts.extend([lM @ p for p in t_pts])
                    faces.extend([tuple(i + v for i in f) for f in t_faces])
                    # mid = randint(idmat, idmat + rand)
                    vcolors.extend([color] * n_faces)
                    matids.extend([idmat] * n_faces)
                    uvs.extend(t_uvs)

            # build temp bmesh and bissect
            bm = bmed.buildmesh(o, verts, faces, matids, uvs, vcolors,
                weld=False, clean=False,  temporary=True)

            # clean outer on convex parts
            # pan.convex = False
            remove = pan.convex

            for s in pan.segs:
                # seg without length lead to invalid normal
                if s.length > 0:
                    if 'AXIS' in s.side_type:
                        x, y, z = s.p1
                        z = pan.altitude(s.p1)
                        bmed.bisect(bm, Vector((x, y, z)), s.v_normal, clear_outer=remove)
                    elif s.side_type == 'BOTTOM':
                        s0 = s.offset(d.tile_border)
                        dz = pan.altitude(s0.p0)
                        vx = s0.v.copy()
                        vx.z = pan.altitude(s0.p1) - dz
                        vy = vz.cross(vx.normalized())
                        x, y, z = s0.p0
                        z = self.z + dz + d.tile_altitude
                        # z = pan.altitude(s0.p0) + d.tile_altitude
                        bmed.bisect(bm, Vector((x, y, z)), -vy, clear_outer=remove)
                    elif s.side_type == 'SIDE':
                        n = s.v_normal
                        p0 = s.p0 + n * d.tile_side
                        bmed.bisect(bm, p0, n, clear_outer=remove)
                    elif s.side_type == 'LINK_VALLEY':
                        n = s.v_normal
                        p0 = s.p0 - n * d.tile_couloir
                        bmed.bisect(bm, p0, n, clear_outer=remove)
                    elif "LINK" in s.side_type:
                        bmed.bisect(bm, s.p0, s.v_normal, clear_outer=remove)

            # when not convex, select and remove outer parts
            if not pan.convex:
                """
                /* del "context" slot values, used for operator too */
                enum {
                    DEL_VERTS = 1,
                    DEL_EDGES,
                    DEL_ONLYFACES,
                    DEL_EDGESFACES,
                    DEL_FACES,
                    /* A version of 'DEL_FACES' that keeps edges on face boundaries,
                     * allowing the surrounding edge-loop to be kept from removed face regions. */
                    DEL_FACES_KEEP_BOUNDARY,
                    DEL_ONLYTAGGED
                };
                """
                # Build boundary including borders and bottom offsets
                # bmed.normal_update(bm)
                # bmed.ensure_bmesh(bm)

                segs = []
                for s in pan.segs:
                    if s.length > 0:
                        if 'VALLEY' in s.side_type:
                            offset = -d.tile_couloir
                        elif s.side_type == 'BOTTOM':
                            offset = d.tile_border
                        elif s.side_type == 'SIDE':
                            offset = d.tile_side
                        else:
                            offset = 0
                        new_s = s.offset(offset)
                        segs.append(new_s)

                if len(segs) > 0:

                    # compute intersections
                    last = segs[-1]
                    for s in segs:
                        res, p0, t = last.intersect(s)
                        s._last = last
                        last._next = s
                        s._p0 = p0
                        last = s

                    # NOTE: there is an issue with bottom faces cuts as cut is not vertical !!
                    # so projection may fall outside
                    # TODO: use a 4 matrix inside test for each bottom segment

                    f_geom = [f for f in bm.faces if not pan.inside(f.calc_center_median(), segs)]

                    if len(f_geom) > 0:
                        bmesh.ops.delete(bm, geom=f_geom, context='FACES')   # 5

            self.cut_holes(bm, pan, offset={'DEFAULT': 0})

            self.dissolve_limit(bm, angle_limit=0.05)

            if d.tile_bevel:
                geom = bm.verts[:]
                geom.extend(bm.edges[:])
                bmesh.ops.bevel(bm,
                    geom=geom,
                    offset=d.tile_bevel_amt,
                    offset_type=offset_type,
                    segments=d.tile_bevel_segs,
                    profile=0.5,
                    vertex_only=False,
                    clamp_overlap=False,
                    material=-1)

            if d.tile_solidify:
                geom = bm.faces[:]
                verts = bm.verts[:]
                bmesh.ops.solidify(bm, geom=geom, thickness=0.0001)
                bmesh.ops.translate(bm, vec=vz * d.tile_height, space=o.matrix_world, verts=verts)

            # merge with object
            bmed.bmesh_join(o, [bm], normal_update=True)

    def _bargeboard(self, s, i, boundary, pan,
            width, height, altitude, offset, idmat,
            verts, faces, edges, matids, uvs, vcolors):

        f = len(verts)

        s0 = s.offset(offset - width)
        s1 = s.offset(offset)

        p0 = s0.p0
        p1 = s1.p0
        p2 = s0.p1
        p3 = s1.p1

        s2 = boundary.last_seg(i)
        s3 = boundary.next_seg(i)

        if s2.side_type == 'SIDE':
            # intersect last seg offset
            s4 = s2.offset(offset - width)
            s5 = s2.offset(offset)
            res, p, t = s4.intersect(s0)
            if res:
                p0 = p
            res, p, t = s5.intersect(s1)
            if res:
                p1 = p

        elif s2.side_type == 'AXIS' or 'LINK' in s2.side_type:
            # intersect axis or link seg
            res, p, t = s2.intersect(s0)
            if res:
                p0 = p
            res, p, t = s2.intersect(s1)
            if res:
                p1 = p

        if s3.side_type == 'SIDE':
            # intersect next seg offset
            s4 = s3.offset(offset - width)
            s5 = s3.offset(offset)
            res, p, t = s4.intersect(s0)
            if res:
                p2 = p
            res, p, t = s5.intersect(s1)
            if res:
                p3 = p

        elif s3.side_type == 'AXIS' or 'LINK' in s3.side_type:
            # intersect axis or link seg
            res, p, t = s3.intersect(s0)
            if res:
                p2 = p
            res, p, t = s3.intersect(s1)
            if res:
                p3 = p

        x0, y0, z = p0
        x1, y1, z = p1
        x2, y2, z = p3
        x3, y3, z = p2

        z0 = self.z + altitude + pan.altitude(p0)
        z1 = self.z + altitude + pan.altitude(p1)
        z2 = self.z + altitude + pan.altitude(p3)
        z3 = self.z + altitude + pan.altitude(p2)

        verts.extend([
            (x0, y0, z0),
            (x1, y1, z1),
            (x2, y2, z2),
            (x3, y3, z3),
        ])
        z0 -= height
        z1 -= height
        z2 -= height
        z3 -= height
        verts.extend([
            (x0, y0, z0),
            (x1, y1, z1),
            (x2, y2, z2),
            (x3, y3, z3),
        ])

        faces.extend([
            # top
            (f, f + 1, f + 2, f + 3),
            # sides
            (f, f + 4, f + 5, f + 1),
            (f + 1, f + 5, f + 6, f + 2),
            (f + 2, f + 6, f + 7, f + 3),
            (f + 3, f + 7, f + 4, f),
            # bottom
            (f + 5, f + 4, f + 7, f + 6)
        ])
        vcolors.extend([self.random_color] * 6)
        edges.append([f, f + 3])
        edges.append([f + 1, f + 2])
        edges.append([f + 4, f + 7])
        edges.append([f + 5, f + 6])

        matids.extend([idmat] * 6)
        l, w, h = (p2 - p0).length, width, height
        uvs.extend([[(0, 0), (w, 0), (w, l), (0, l)],
                    [(0, 0), (h, 0), (h, w), (0, w)],
                    [(0, 0), (h, 0), (h, l), (0, l)],
                    [(0, 0), (h, 0), (h, w), (0, w)],
                    [(0, 0), (h, 0), (h, l), (0, l)],
                    [(0, 0), (w, 0), (w, l), (0, l)]
                    ])

    def bargeboard(self, d, verts, faces, edges, matids, uvs, vcolors):

        #####################
        # Vire-vents
        #####################

        idmat = d.id_mat(MAT_BARGEBOARD)
        for pan in self.pans:

            for hole in pan.holes:
                for i, s in enumerate(hole.segs):
                    if s.side_type == 'SIDE':
                        self._bargeboard(s,
                            i,
                            hole, pan,
                            d.bargeboard_width,
                            d.bargeboard_height,
                            d.bargeboard_altitude,
                            d.bargeboard_offset,
                            idmat,
                            verts,
                            faces,
                            edges,
                            matids,
                            uvs,
                            vcolors)

            for i, s in enumerate(pan.segs):
                if s.side_type == 'SIDE':
                    self._bargeboard(s,
                        i,
                        pan, pan,
                        d.bargeboard_width,
                        d.bargeboard_height,
                        d.bargeboard_altitude,
                        d.bargeboard_offset,
                        idmat,
                        verts,
                        faces,
                        edges,
                        matids,
                        uvs,
                        vcolors)

    def _fascia(self, s, i, boundary, pan, tri_0, tri_1,
            width, height, altitude, offset, idmat,
            verts, faces, edges, matids, uvs, vcolors):

        f = len(verts)
        s0 = s.offset(offset)
        s1 = s.offset(offset + width)

        s2 = boundary.last_seg(i)
        s3 = boundary.next_seg(i)
        s4 = s2
        s5 = s3

        p0 = s0.p0
        p1 = s1.p0
        p2 = s0.p1
        p3 = s1.p1

        # find last neighboor depending on type
        if s2.side_type == 'AXIS' or 'LINK' in s2.side_type:
            # apply only on boundarys
            if not s.is_hole:
                # use last axis
                if pan.side == 'LEFT':
                    s6 = pan.next_cross
                else:
                    s6 = pan.last_cross
                if tri_0:
                    s2 = s.copy()
                else:
                    s2 = s2.opposite
                s2.v = (s.normal(0, 1).v + s6.v).normalized()
                s4 = s2

        elif s2.side_type == 'SIDE':
            s2 = s.normal(0, 1)
            s4 = s2
        else:
            s2 = s2.offset(offset)
            s4 = s2.offset(offset + width)

        # find next neighboor depending on type
        if s3.side_type == 'AXIS' or 'LINK' in s3.side_type:
            if not s.is_hole:
                # use last axis
                if pan.side == 'LEFT':
                    s6 = pan.last_cross
                else:
                    s6 = pan.next_cross
                if tri_1:
                    s3 = s.opposite
                else:
                    s3 = s3.copy()
                s3.v = (s.normal(0, 1).v + s6.v).normalized()
                s5 = s3
        elif s3.side_type == 'SIDE':
            # when next is side, use perpendicular
            s3 = s.normal(1, 1)
            s5 = s3
        else:
            s3 = s3.offset(offset)
            s5 = s3.offset(offset + width)

        # units vectors and scale
        # is unit normal on sides
        # print("s.p:%s, s.v:%s s1.p::%s s1.v::%s" % (s.p, s.v, s1.p, s1.v))
        res, p, t = s0.intersect(s2)
        if res:
            p0 = p
        res, p, t = s0.intersect(s3)
        if res:
            p1 = p
        res, p, t = s1.intersect(s4)
        if res:
            p2 = p
        res, p, t = s1.intersect(s5)
        if res:
            p3 = p

        x0, y0, z = p0
        x1, y1, z = p2
        x2, y2, z = p3
        x3, y3, z = p1

        z0 = self.z + altitude + pan.altitude(p0)
        z1 = self.z + altitude + pan.altitude(p2)
        z2 = self.z + altitude + pan.altitude(p3)
        z3 = self.z + altitude + pan.altitude(p1)

        verts.extend([
            (x0, y0, z0),
            (x1, y1, z1),
            (x2, y2, z2),
            (x3, y3, z3),
        ])

        z0 -= height
        z1 -= height
        z2 -= height
        z3 -= height
        verts.extend([
            (x0, y0, z0),
            (x1, y1, z1),
            (x2, y2, z2),
            (x3, y3, z3),
        ])

        faces.extend([
            # top
            (f, f + 1, f + 2, f + 3),
            # sides
            (f, f + 4, f + 5, f + 1),
            (f + 1, f + 5, f + 6, f + 2),
            (f + 2, f + 6, f + 7, f + 3),
            (f + 3, f + 7, f + 4, f),
            # bottom
            (f + 5, f + 4, f + 7, f + 6)
        ])
        vcolors.extend([self.random_color] * 6)
        edges.append([f, f + 3])
        edges.append([f + 1, f + 2])
        edges.append([f + 4, f + 7])
        edges.append([f + 5, f + 6])
        matids.extend([idmat] * 6)

        l, w, h = (p2 - p1).length, width, height
        uvs.extend([[(0, 0), (w, 0), (w, l), (0, l)],
                    [(0, 0), (h, 0), (h, w), (0, w)],
                    [(0, 0), (h, 0), (h, l), (0, l)],
                    [(0, 0), (h, 0), (h, w), (0, w)],
                    [(0, 0), (h, 0), (h, l), (0, l)],
                    [(0, 0), (w, 0), (w, l), (0, l)]
                    ])

    def fascia(self, d, verts, faces, edges, matids, uvs, vcolors):

        #####################
        # Larmiers
        #####################

        idmat = d.id_mat(MAT_FASCIA)
        for pan in self.pans:

            for hole in pan.holes:
                for i, s in enumerate(hole.segs):
                    if s.side_type == 'BOTTOM':
                        self._fascia(s,
                            i,
                            hole, pan,
                            False, False,
                            d.fascia_width,
                            d.fascia_height,
                            d.fascia_altitude,
                            d.fascia_offset,
                            idmat,
                            verts,
                            faces,
                            edges,
                            matids,
                            uvs,
                            vcolors)

            for i, s in enumerate(pan.segs):
                if s.side_type == 'BOTTOM':

                    tri_0 = pan.node_tri
                    tri_1 = pan.next_tri

                    # triangular ends apply on boundary only
                    # unless cut, boundary is parallel to axis
                    # except for triangular ends
                    if pan.side == 'LEFT':
                        tri_0, tri_1 = tri_1, tri_0

                    self._fascia(s,
                        i,
                        pan, pan,
                        tri_0, tri_1,
                        d.fascia_width,
                        d.fascia_height,
                        d.fascia_altitude,
                        d.fascia_offset,
                        idmat,
                        verts,
                        faces,
                        edges,
                        matids,
                        uvs,
                        vcolors)

    def gutter(self, d, verts, faces, edges, matids, uvs, vcolors):

        #####################
        # Chenaux
        #####################

        idmat = d.id_mat(MAT_GUTTER)

        # caps at start and end
        if d.gutter_segs % 2 == 1:
            n_faces = int((d.gutter_segs - 1) / 2)
        else:
            n_faces = int((d.gutter_segs / 2) - 1)

        df = 2 * d.gutter_segs + 1

        for pan in self.pans:
            color = self.random_color
            for i, s in enumerate(pan.segs):

                if s.side_type == 'BOTTOM':
                    f = len(verts)

                    s0 = s.offset(d.gutter_dist + d.gutter_width)

                    s1 = pan.last_seg(i)
                    s2 = pan.next_seg(i)

                    p0 = s0.p0
                    p1 = s0.p1

                    tri_0 = pan.node_tri or pan.is_tri
                    tri_1 = pan.next_tri or pan.is_tri

                    if pan.side == 'LEFT':
                        tri_0, tri_1 = tri_1, tri_0

                    f = len(verts)

                    # tiangular use segment direction
                    # find last neighboor depending on type
                    side_type = s1.side_type
                    if side_type == 'AXIS' or 'LINK' in side_type:
                        # apply only on boundarys
                        if not s.is_hole:
                            # use last axis
                            if pan.side == 'LEFT':
                                s3 = pan.next_cross
                            else:
                                s3 = pan.last_cross
                            if tri_0:
                                s1 = s.copy()
                            else:
                                s1 = s1.opposite
                            s1.v = (s.normal(0, 1).v + s3.v).normalized()
                    elif side_type == 'SIDE':
                        s1 = s.normal(0, 1)
                    else:
                        s1 = s1.offset(d.gutter_dist + d.gutter_width)
                    s1.side_type = side_type

                    # find next neighboor depending on type
                    side_type = s2.side_type
                    if side_type == 'AXIS' or 'LINK' in side_type:
                        if not s.is_hole:
                            # use last axis
                            if pan.side == 'LEFT':
                                s3 = pan.last_cross
                            else:
                                s3 = pan.next_cross
                            if tri_1:
                                s2 = s.opposite

                            else:
                                s2 = s2.copy()
                            s2.v = (s.normal(0, 1).v + s3.v).normalized()
                    elif side_type == 'SIDE':
                        s2 = s.normal(1, 1)
                        side_type = 'SIDE'
                    else:
                        s2 = s2.offset(d.gutter_dist + d.gutter_width)
                    s2.side_type = side_type
                    # units vectors and scale
                    # is unit normal on sides
                    # print("s.p:%s, s.v:%s s1.p::%s s1.v::%s" % (s.p, s.v, s1.p, s1.v))
                    res, p, t = s0.intersect(s1)
                    if res:
                        p0 = p
                    res, p, t = s0.intersect(s2)
                    if res:
                        p1 = p
                    """
                    f = len(verts)
                    verts.extend([s1.p0.to_3d(), s1.p1.to_3d()])
                    edges.append([f, f + 1])

                    f = len(verts)
                    verts.extend([s2.p0.to_3d(), s2.p1.to_3d()])
                    edges.append([f, f + 1])
                    continue
                    """

                    v0 = p0 - s.p0
                    v1 = p1 - s.p1

                    scale_0 = v0.length / (d.gutter_dist + d.gutter_width)
                    scale_1 = v1.length / (d.gutter_dist + d.gutter_width)

                    s3 = Line(s.p0, s.p0 + v0.normalized())
                    # s4 = Line(s.p1, s.p0 + v1.normalized())
                    s4 = Line(s.p1, s.p1 + v1.normalized())

                    zt = self.z + d.fascia_altitude + pan.altitude(s3.p0)
                    z0 = self.z + d.gutter_alt + pan.altitude(s3.p0)
                    z1 = z0 - 0.5 * d.gutter_width
                    z2 = z1 - 0.5 * d.gutter_width
                    z3 = z1 - 0.5 * d.gutter_boudin
                    dz0 = z2 - z1
                    dz1 = z3 - z1

                    tt = scale_0 * d.fascia_width
                    t0 = scale_0 * d.gutter_dist
                    t1 = t0 + scale_0 * (0.5 * d.gutter_width)
                    t2 = t1 + scale_0 * (0.5 * d.gutter_width)
                    t3 = t2 + scale_0 * (0.5 * d.gutter_boudin)

                    # bord tablette
                    xt, yt, z = s3.lerp(tt)

                    # bord
                    x0, y0, z = s3.lerp(t0)
                    # axe chenaux
                    x1, y1, z = s3.lerp(t1)
                    # bord boudin interieur
                    x2, y2, z = s3.lerp(t2)
                    # axe boudin
                    x3, y3, z = s3.lerp(t3)

                    dx = x0 - x1
                    dy = y0 - y1

                    verts.append((xt, yt, zt))
                    # chenaux
                    da = pi / d.gutter_segs
                    for i in range(d.gutter_segs):
                        sa = sin(i * da)
                        ca = cos(i * da)
                        verts.append((x1 + dx * ca, y1 + dy * ca, z1 + dz0 * sa))

                    dx = x2 - x3
                    dy = y2 - y3

                    # boudin
                    da = -pi / (0.75 * d.gutter_segs)
                    for i in range(d.gutter_segs):
                        sa = sin(i * da)
                        ca = cos(i * da)
                        verts.append((x3 + dx * ca, y3 + dy * ca, z1 + dz1 * sa))

                    zt = self.z + d.fascia_altitude + pan.altitude(s4.p0)
                    z0 = self.z + d.gutter_alt + pan.altitude(s4.p0)
                    z1 = z0 - 0.5 * d.gutter_width
                    z2 = z1 - 0.5 * d.gutter_width
                    z3 = z1 - 0.5 * d.gutter_boudin
                    dz0 = z2 - z1
                    dz1 = z3 - z1
                    tt = scale_1 * d.fascia_width
                    t0 = scale_1 * d.gutter_dist
                    t1 = t0 + scale_1 * (0.5 * d.gutter_width)
                    t2 = t1 + scale_1 * (0.5 * d.gutter_width)
                    t3 = t2 + scale_1 * (0.5 * d.gutter_boudin)

                    # bord tablette
                    xt, yt, z = s4.lerp(tt)

                    # bord
                    x0, y0, z = s4.lerp(t0)
                    # axe chenaux
                    x1, y1, z = s4.lerp(t1)
                    # bord boudin interieur
                    x2, y2, z = s4.lerp(t2)
                    # axe boudin
                    x3, y3, z = s4.lerp(t3)

                    dx = x0 - x1
                    dy = y0 - y1

                    # tablette
                    verts.append((xt, yt, zt))
                    faces.append((f + df, f, f + 1, f + df + 1))
                    vcolors.append(color)
                    uvs.append([(0, 0), (1, 0), (1, 1), (0, 1)])
                    matids.append(idmat)

                    # chenaux
                    da = pi / d.gutter_segs
                    for i in range(d.gutter_segs):
                        sa = sin(i * da)
                        ca = cos(i * da)
                        verts.append((x1 + dx * ca, y1 + dy * ca, z1 + dz0 * sa))

                    dx = x2 - x3
                    dy = y2 - y3

                    # boudin
                    da = -pi / (0.75 * d.gutter_segs)
                    for i in range(d.gutter_segs):
                        sa = sin(i * da)
                        ca = cos(i * da)
                        verts.append((x3 + dx * ca, y3 + dy * ca, z1 + dz1 * sa))

                    df = 2 * d.gutter_segs + 1

                    for i in range(1, 2 * d.gutter_segs):
                        j = i + f
                        faces.append((j, j + df, j + df + 1, j + 1))
                        vcolors.append(color)
                        uvs.append([(0, 0), (1, 0), (1, 1), (0, 1)])
                        matids.append(idmat)

                    """
                            segs = 6

                            n_faces = segs / 2 - 1

                                0           6
                                 1         5
                                   2     4
                                      3
                    """
                    # close start
                    if s1.side_type == 'SIDE':

                        if d.gutter_segs % 2 == 0:
                            faces.append((f + n_faces + 3, f + n_faces + 1, f + n_faces + 2))
                            vcolors.append(color)
                            uvs.append([(0, 0), (1, 0), (0.5, -0.5)])
                            matids.append(idmat)

                        for i in range(n_faces):

                            j = i + f + 1
                            k = f + d.gutter_segs - i
                            faces.append((j + 1, k, k + 1, j))
                            vcolors.append(color)
                            uvs.append([(0, 0), (1, 0), (1, 1), (0, 1)])
                            matids.append(idmat)

                    # close end
                    if s2.side_type == 'SIDE':

                        f += 2 * d.gutter_segs + 1

                        if d.gutter_segs % 2 == 0:
                            faces.append((f + n_faces + 1, f + n_faces + 3, f + n_faces + 2))
                            vcolors.append(color)
                            uvs.append([(0, 0), (1, 0), (0.5, -0.5)])
                            matids.append(idmat)

                        for i in range(n_faces):

                            j = i + f + 1
                            k = f + d.gutter_segs - i
                            faces.append((j, k + 1, k, j + 1))
                            vcolors.append(color)
                            uvs.append([(0, 0), (1, 0), (1, 1), (0, 1)])
                            matids.append(idmat)

    def beam_primary(self, d, verts, faces, edges, matids, uvs, vcolors):

        idmat = d.id_mat(MAT_RIGE_POLE)
        color = self.random_color

        for pan in self.pans:
            for i, s in enumerate(pan.segs):

                if 'AXIS' in s.side_type:

                    ####################
                    # Poutre Faitiere
                    ####################

                    """
                     1___________________2   left
                    0|___________________|3  axis
                     |___________________|   right
                     5                   4
                    """
                    f = len(verts)

                    s2 = s.offset(-0.5 * d.beam_width)

                    # offset from roof border
                    s0 = pan.last_seg(i)
                    s1 = pan.next_seg(i)
                    t0 = 0
                    t1 = 1

                    s0_tri = pan.next_tri
                    s1_tri = pan.node_tri

                    if pan.side == 'LEFT':
                        s0_tri, s1_tri = s1_tri, s0_tri

                    if s0.side_type == 'SIDE' and s.length > 0:
                        s0 = s0.offset(d.beam_offset)
                        t0 = -d.beam_offset / s.length

                    if s0_tri:
                        p0 = s2.p0
                        t0 = 0
                    else:
                        res, p0, t = s2.intersect(s0)
                        if not res:
                            continue

                    if s1.side_type == 'SIDE' and s.length > 0:
                        s1 = s1.offset(d.beam_offset)
                        t1 = 1 + d.beam_offset / s.length

                    if s1_tri:
                        t1 = 1
                        p1 = s2.p1
                    else:
                        res, p1, t = s2.intersect(s1)
                        if not res:
                            continue

                    x0, y0, z = p0
                    x1, y1, z = s.lerp(t0)
                    x2, y2, z = p1
                    x3, y3, z = s.lerp(t1)
                    z0 = self.z + d.beam_alt + pan.altitude(p0)
                    z1 = z0 - d.beam_height
                    z2 = self.z + d.beam_alt + pan.altitude(p1)
                    z3 = z2 - d.beam_height
                    verts.extend([
                        (x0, y0, z0),
                        (x1, y1, z0),
                        (x2, y2, z2),
                        (x3, y3, z2),
                        (x0, y0, z1),
                        (x1, y1, z1),
                        (x2, y2, z3),
                        (x3, y3, z3),
                    ])


                    l, w, h = (p1 - p0).length, d.beam_width, d.beam_height

                    if s0_tri or s0.side_type == 'SIDE':
                        faces.append((f + 4, f + 5, f + 1, f))
                        vcolors.append(color)
                        uvs.append([(0, 0), (1, 0), (1, 1), (0, 1)])
                        matids.append(idmat)
                    if s1_tri or s1.side_type == 'SIDE':
                        faces.append((f + 2, f + 3, f + 7, f + 6))
                        vcolors.append(color)
                        uvs.append([(0, 0), (1, 0), (1, 1), (0, 1)])
                        matids.append(idmat)

                    faces.extend([
                        # internal side
                        # (f + 1, f + 5, f + 7, f + 3),
                        # external side
                        (f + 2, f + 6, f + 4, f),
                        # top
                        (f, f + 1, f + 3, f + 2),
                        # bottom
                        (f + 5, f + 4, f + 6, f + 7)
                    ])
                    vcolors.extend([color] * 3)
                    matids.extend([
                        idmat, idmat, idmat
                    ])

                    uvs.extend([
                        [(0, 0), (h, 0), (h, l), (0, l)],
                        [(0, 0), (w, 0), (w, l), (0, l)],
                        [(0, 0), (w, 0), (w, l), (0, l)]
                    ])
                    #uvs.extend([
                    #    [(0, 0), (1, 0), (1, 1), (0, 1)],
                    #    [(0, 0), (1, 0), (1, 1), (0, 1)],
                    #    [(0, 0), (1, 0), (1, 1), (0, 1)]
                    #])

    def rafter(self, o, d):

        idmat = d.id_mat(MAT_RAFTER)

        # Rafters / Chevrons
        start = max(0.001 + 0.5 * d.rafter_width, d.rafter_start)

        holes_offset = -d.rafter_width

        # build temp bmesh and bissect
        for pan in self.pans:
            tmin, tmax, ysize = pan.tmin, pan.tmax, pan.ysize

            # print("tmin:%s tmax:%s ysize:%s" % (tmin, tmax, ysize))

            f = 0

            verts = []
            faces = []
            matids = []
            uvs = []
            vcolors = []
            alt = d.rafter_alt
            seg = pan.fake_axis

            t0 = tmin + (start - 0.5 * d.rafter_width) / seg.length
            t1 = tmin + (start + 0.5 * d.rafter_width) / seg.length

            tx = start / seg.length
            dt = d.rafter_spacing / seg.length

            n_items = max(1, round((tmax - tmin) / dt, 0))

            dt = ((tmax - tmin) - 2 * tx) / n_items

            for j in range(int(n_items) + 1):
                n0 = seg.normal(t1 + j * dt, - ysize)
                n1 = seg.normal(t0 + j * dt, - ysize)
                f = len(verts)

                z0 = self.z + alt + pan.altitude(n0.p0)
                x0, y0, z = n0.p0
                z1 = self.z + alt + pan.altitude(n0.p1)
                x1, y1, z = n0.p1
                z2 = self.z + alt + pan.altitude(n1.p0)
                x2, y2, z = n1.p0
                z3 = self.z + alt + pan.altitude(n1.p1)
                x3, y3, z = n1.p1

                verts.extend([
                    (x0, y0, z0),
                    (x1, y1, z1),
                    (x2, y2, z2),
                    (x3, y3, z3)
                    ])

                faces.append((f + 1, f, f + 2, f + 3))
                vcolors.append(self.random_color)
                matids.append(idmat)
                uvs.append([(0, 0), (0, 1), (1, 1), (1, 0)])

            bm = bmed.buildmesh(o, verts, faces, matids, uvs, vcolors,
                weld=False, clean=False,  temporary=True)

            self.cut_boundary(bm, pan)
            self.cut_holes(bm, pan, offset={'DEFAULT': holes_offset})

            geom = bm.faces[:]
            verts = bm.verts[:]
            bmesh.ops.solidify(bm, geom=geom, thickness=0.0001)
            bmesh.ops.translate(bm, vec=Vector((0, 0, -d.rafter_height)), space=o.matrix_world, verts=verts)
            # uvs for sides
            # uvs = [(0, 0), (0, 1), (1, 1), (1, 0)]
            uvs = [(0, 1), (1, 0), (0, -1), (0, 0)]
            layer = bm.loops.layers.uv.verify()
            for i, face in enumerate(bm.faces):
                if len(face.loops) == 4:
                    ul, vl = 0, 0
                    for j, loop in enumerate(face.loops):
                        loop[layer].uv = (ul, vl)
                        l = loop.edge.calc_length()
                        u, v = uvs[j]
                        ul += u * l
                        vl += v * l

            # merge with object
            bmed.bmesh_join(o, [bm], normal_update=True)

        # bpy.ops.object.mode_set(mode='OBJECT')

    def hips(self, d, verts, faces, edges, matids, uvs, vcolors):

        idmat_valley = d.id_mat(MAT_GUTTER)

        idmat = d.id_mat(MAT_HIP)

        idmat_poutre = d.id_mat(MAT_RAFTER)

        sx, sy, sz = d.hip_size_x, d.hip_size_y, d.hip_size_z

        if d.hip_model == 'ROUND':

            # round hips
            t_pts = [Vector((sx * x, sy * y, sz * z)) for x, y, z in [
                (-0.5, 0.34, 0.08), (-0.5, 0.32, 0.19), (0.5, -0.4, -0.5),
                (0.5, 0.4, -0.5), (-0.5, 0.26, 0.28), (-0.5, 0.16, 0.34),
                (-0.5, 0.05, 0.37), (-0.5, -0.05, 0.37), (-0.5, -0.16, 0.34),
                (-0.5, -0.26, 0.28), (-0.5, -0.32, 0.19), (-0.5, -0.34, 0.08),
                (-0.5, -0.25, -0.5), (-0.5, 0.25, -0.5), (0.5, -0.08, 0.5),
                (0.5, -0.5, 0.08), (0.5, -0.24, 0.47), (0.5, -0.38, 0.38),
                (0.5, -0.47, 0.24), (0.5, 0.5, 0.08), (0.5, 0.08, 0.5),
                (0.5, 0.47, 0.24), (0.5, 0.38, 0.38), (0.5, 0.24, 0.47)
            ]]
            t_faces = [
                (23, 22, 4, 5), (3, 19, 21, 22, 23, 20, 14, 16, 17, 18, 15, 2), (14, 20, 6, 7),
                (18, 17, 9, 10), (15, 18, 10, 11), (21, 19, 0, 1), (17, 16, 8, 9),
                (13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 1, 0), (19, 3, 13, 0), (20, 23, 5, 6), (22, 21, 1, 4),
                (3, 2, 12, 13), (2, 15, 11, 12), (16, 14, 7, 8)
            ]
            t_uvs = [
                [(0.584, -1.0), (0.643, -1.0), (0.643, 0.0), (0.584, 0.0)],
                [(0.102, -1.0), (0.0, -0.418), (0.031, -0.262), (0.117, -0.117), (0.262, -0.031), (0.418, 0.0),
                 (0.582, 0.0), (0.742, -0.031), (0.883, -0.117), (0.973, -0.262), (1.0, -0.418), (0.898, -1.0)],
                [(0.473, -1.0), (0.527, -1.0), (0.527, 0.0), (0.473, 0.0)],
                [(0.3, -1.0), (0.357, -1.0), (0.357, 0.0), (0.3, 0.0)],
                [(0.243, -1.0), (0.3, -1.0), (0.3, 0.0), (0.243, 0.0)],
                [(0.7, -1.0), (0.757, -1.0), (0.757, 0.0), (0.7, 0.0)],
                [(0.357, -1.0), (0.416, -1.0), (0.416, 0.0), (0.357, 0.0)],
                [(0.25, -1.0), (0.754, -1.0), (0.844, -0.418), (0.82, -0.309), (0.766, -0.219), (0.66, -0.156),
                 (0.547, -0.129), (0.453, -0.129), (0.34, -0.156), (0.242, -0.219), (0.184, -0.309), (0.164, -0.418)],
                [(0.757, -1.0), (1.0, -1.0), (1.0, 0.0), (0.757, 0.0)],
                [(0.527, -1.0), (0.584, -1.0), (0.584, 0.0), (0.527, 0.0)],
                [(0.643, -1.0), (0.7, -1.0), (0.7, 0.0), (0.643, 0.0)],
                [(0.0, -1.0), (1.0, -1.0), (1.0, 0.0), (0.0, 0.0)],
                [(0.0, -1.0), (0.243, -1.0), (0.243, 0.0), (0.0, 0.0)],
                [(0.416, -1.0), (0.473, -1.0), (0.473, 0.0), (0.416, 0.0)]
            ]
            # affect vertex with slope
            t_left = []
            t_right = []

        elif d.hip_model == 'ETERNIT':

            # square hips "eternit like"
            t_pts = [Vector((sx * x, sy * y, sz * z)) for x, y, z in [
                (0.5, 0.5, 0.0), (-0.5, 0.5, -0.5), (0.5, -0.5, 0.0),
                (-0.5, -0.5, -0.5), (0.5, 0.0, 0.0), (-0.5, -0.0, -0.5),
                (0.5, 0.0, 0.5), (0.5, -0.5, 0.5), (-0.5, -0.5, 0.0),
                (-0.5, -0.0, 0.0), (0.5, 0.5, 0.5), (-0.5, 0.5, 0.0)]
            ]
            t_faces = [
                (4, 2, 3, 5), (0, 4, 5, 1), (6, 9, 8, 7),
                (10, 11, 9, 6), (0, 10, 6, 4), (5, 9, 11, 1),
                (2, 7, 8, 3), (1, 11, 10, 0), (4, 6, 7, 2), (3, 8, 9, 5)
                ]
            t_uvs = [
                [(0.0, 0.5), (0.0, 1.0), (1.0, 1.0), (1.0, 0.5)], [(0.0, 0.0), (0.0, 0.5), (1.0, 0.5), (1.0, 0.0)],
                [(0.0, 0.5), (1.0, 0.5), (1.0, 1.0), (0.0, 1.0)], [(0.0, 0.0), (1.0, 0.0), (1.0, 0.5), (0.0, 0.5)],
                [(0.0, 0.5), (0.0, 1.0), (0.5, 1.0), (0.5, 0.5)], [(0.5, 0.5), (0.5, 1.0), (0.0, 1.0), (0.0, 0.5)],
                [(0.0, 0.5), (0.0, 1.0), (1.0, 1.0), (1.0, 0.5)], [(0.0, 0.5), (0.0, 1.0), (-1.0, 1.0), (-1.0, 0.5)],
                [(0.5, 0.5), (0.5, 1.0), (1.0, 1.0), (1.0, 0.5)], [(0.0, 0.5), (0.0, 1.0), (-0.5, 1.0), (-0.5, 0.5)]
            ]
            t_left = [2, 3, 7, 8]
            t_right = [0, 1, 10, 11]

        elif d.hip_model == 'FLAT':
            # square hips "eternit like"
            t_pts = [Vector((sx * x, sy * y, sz * z)) for x, y, z in [
                (-0.5, -0.4, 0.0), (-0.5, -0.4, 0.5), (-0.5, 0.4, 0.0),
                (-0.5, 0.4, 0.5), (0.5, -0.5, 0.5), (0.5, -0.5, 1.0),
                (0.5, 0.5, 0.5), (0.5, 0.5, 1.0), (-0.5, 0.33, 0.0),
                (-0.5, -0.33, 0.0), (0.5, -0.33, 0.5), (0.5, 0.33, 0.5),
                (-0.5, 0.33, -0.5), (-0.5, -0.33, -0.5), (0.5, -0.33, -0.5),
                (0.5, 0.33, -0.5)]
            ]
            t_faces = [
                (0, 1, 3, 2, 8, 9), (2, 3, 7, 6), (6, 7, 5, 4, 10, 11),
                (4, 5, 1, 0), (9, 10, 4, 0), (7, 3, 1, 5),
                (2, 6, 11, 8), (9, 8, 12, 13), (12, 15, 14, 13),
                (8, 11, 15, 12), (10, 9, 13, 14), (11, 10, 14, 15)]
            t_uvs = [
                [(0.5, 1.0), (0.93, 0.75), (0.93, 0.25), (0.5, 0.0), (0.07, 0.25), (0.07, 0.75)],
                [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)],
                [(0.5, 1.0), (0.93, 0.75), (0.93, 0.25), (0.5, 0.0), (0.07, 0.25), (0.07, 0.75)],
                [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)],
                [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)],
                [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)],
                [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)],
                [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)],
                [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)],
                [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)],
                [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)],
                [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)]
            ]
            t_left = []
            t_right = []

        # t_idmats = [idmat for f in t_faces]
        n_faces = len(t_faces)

        for pan in self.pans:
            for i, s in enumerate(pan.segs):
                if ('LINK' in s.side_type and
                        d.beam_sec_enable):
                    ##############
                    # beam inside
                    ##############
                    f = len(verts)

                    s0 = s.offset(-0.5 * d.beam_sec_width)

                    s2 = pan.last_seg(i)
                    s3 = pan.next_seg(i)
                    p0 = s0.p0
                    p1 = s0.p1
                    t0 = 0
                    t1 = 1
                    res, p, t = s0.intersect(s2)
                    if res:
                        t0 = t
                        p0 = p
                    res, p, t = s0.intersect(s3)
                    if res:
                        t1 = t
                        p1 = p

                    p0 = s.lerp(t0)
                    p1 = s.lerp(t1)

                    x0, y0, z = s0.lerp(t0)
                    x1, y1, z = s.p0

                    z0 = self.z + d.beam_sec_alt + pan.altitude(p0)
                    z1 = z0 - d.beam_sec_height
                    z2 = self.z + d.beam_sec_alt + pan.altitude(s.p0)
                    z3 = z2 - d.beam_sec_height

                    verts.extend([
                        (x0, y0, z0),
                        (x0, y0, z1),
                        (x1, y1, z2),
                        (x1, y1, z3)
                    ])

                    x2, y2, z = s0.lerp(t1)
                    x3, y3, z = s.p1

                    z0 = self.z + d.beam_sec_alt + pan.altitude(p1)
                    z1 = z0 - d.beam_sec_height
                    z2 = self.z + d.beam_sec_alt + pan.altitude(s.p1)
                    z3 = z2 - d.beam_sec_height

                    verts.extend([
                        (x2, y2, z0),
                        (x2, y2, z1),
                        (x3, y3, z2),
                        (x3, y3, z3)
                    ])

                    faces.extend([
                        # top
                        (f, f + 4, f + 5, f + 1),
                        # bottom
                        (f + 1, f + 5, f + 7, f + 3),
                        # inside
                        (f + 6, f + 2, f + 3, f + 7),
                        # outside
                        (f + 2, f + 6, f + 4, f),
                        # small ends
                        (f + 2, f, f + 1, f + 3),
                        (f + 4, f + 6, f + 7, f + 5)
                    ])
                    vcolors.extend([self.random_color] * 6)
                    matids.extend([idmat_poutre] * 6)
                    l, w, h = (p1 - p0).length,  d.beam_sec_width, d.beam_sec_height
                    uvs.extend([
                        [(0, 0), (0, l), (w, l), (w, 0)],
                        [(0, 0), (0, l), (w, l), (w, 0)],
                        [(0, 0), (0, l), (h, l), (h, 0)],
                        [(0, 0), (0, l), (h, l), (h, 0)],
                        [(0, 0), (0, w), (h, w), (h, 0)],
                        [(0, 0), (0, w), (h, w), (h, 0)]
                    ])

                if s.side_type == 'LINK_HIP':

                    # TODO:
                    # Slice borders properly

                    if d.hip_enable:

                        s0 = pan.last_seg(i)
                        s1 = pan.next_seg(i)
                        s2 = s
                        p0 = s0.p1
                        p1 = s1.p0
                        z0 = pan.altitude(p0)
                        z1 = pan.altitude(p1)

                        # s0 is top seg
                        if z1 > z0:
                            p0, p1 = p1, p0
                            z0, z1 = z1, z0
                            s2 = s2.opposite

                        dz = pan.altitude(s2.normal(0, 1).p1) - z0

                        if dz < 0:
                            s1 = s1.offset(d.tile_border)
                            # vx from p0 to p1
                            x, y, z = p1 - p0
                            v = Vector((x, y, z1 - z0))
                            vx = v.normalized()
                            vy = vx.cross(Vector((0, 0, 1)))
                            vz = vy.cross(vx)

                            x0, y0, z = p0 + d.hip_alt * vz
                            z2 = z0 + self.z + d.hip_alt * vz.z

                            tM = Matrix([
                                [vx.x, vy.x, vz.x, x0],
                                [vx.y, vy.y, vz.y, y0],
                                [vx.z, vy.z, vz.z, z2],
                                [0, 0, 0, 1]
                            ])
                            space_x = v.length - d.tile_border
                            n_x = 1 + int(space_x / d.hip_space_x)
                            dx = space_x / n_x
                            x0 = 0.5 * dx

                            t_verts = t_pts[:]

                            # apply slope

                            for i in t_left:
                                t_verts[i] = t_verts[i].copy()
                                t_verts[i].z -= dz * t_verts[i].y
                            for i in t_right:
                                t_verts[i] = t_verts[i].copy()
                                t_verts[i].z += dz * t_verts[i].y

                            for k in range(n_x):
                                rz = d.tile_random_z * uniform(0.0, 1.0)
                                az = d.tile_random_angle_z * uniform(-1.0, 1.0)
                                ay = d.tile_random_angle_y * uniform(0.0, 1.0)

                                lM = tM @ Matrix([
                                    [1, 0, 0, x0 + k * dx],
                                    [0, -1, 0, 0],
                                    [0, 0, 1, rz],
                                    [0, 0, 0, 1]
                                ]) @ Matrix.Rotation(az, 4, "Z") @ Matrix.Rotation(ay, 4, "Y")

                                f = len(verts)
                                color = self.random_color
                                verts.extend([lM @ p for p in t_verts])
                                faces.extend([tuple(i + f for i in p) for p in t_faces])
                                vcolors.extend([color] * n_faces)
                                matids.extend([idmat] * n_faces)
                                uvs.extend(t_uvs)

                elif s.side_type == 'LINK_VALLEY':
                    if d.valley_enable:
                        f = len(verts)
                        s0 = s.offset(-2 * d.tile_couloir)
                        s1 = pan.last_seg(i)
                        s2 = pan.next_seg(i)
                        p0 = s0.p0
                        p1 = s0.p1
                        res, p, t = s0.intersect(s1)
                        if res:
                            p0 = p
                        res, p, t = s0.intersect(s2)
                        if res:
                            p1 = p
                        alt = self.z + d.valley_altitude
                        x0, y0, z = s1.p1
                        x1, y1, z = p0
                        x2, y2, z = p1
                        x3, y3, z = s2.p0
                        z0 = alt + pan.altitude(s1.p1)
                        z1 = alt + pan.altitude(p0)
                        z2 = alt + pan.altitude(p1)
                        z3 = alt + pan.altitude(s2.p0)

                        verts.extend([
                            (x0, y0, z0),
                            (x1, y1, z1),
                            (x2, y2, z2),
                            (x3, y3, z3),
                        ])
                        faces.extend([
                            (f, f + 3, f + 2, f + 1)
                        ])
                        vcolors.extend([self.random_color])
                        matids.extend([
                            idmat_valley
                            ])
                        uvs.extend([
                            [(0, 0), (1, 0), (1, 1), (0, 1)]
                        ])

                elif s.side_type == 'AXIS' and d.hip_enable and pan.side == 'LEFT':

                    tmin = 0
                    tmax = 1
                    s0 = pan.last_seg(i)
                    if s0.side_type == 'SIDE' and s.length > 0:
                        tmin = 0 - d.tile_side / s.length
                    s1 = pan.next_seg(i)

                    if s1.side_type == 'SIDE' and s.length > 0:
                        tmax = 1 + d.tile_side / s.length

                    # print("tmin:%s tmax:%s" % (tmin, tmax))
                    ####################
                    # Faitiere
                    ####################

                    f = len(verts)
                    s_len = (tmax - tmin) * s.length
                    n_obj = 1 + int(s_len / d.hip_space_x)
                    dx = s_len / n_obj
                    x0 = 0.5 * dx
                    v = s.v_normalized
                    p0 = s.lerp(tmin)
                    z = self.z + pan.altitude(p0) + d.hip_alt
                    tM = Matrix([
                        [v.x, v.y, 0, p0.x],
                        [v.y, -v.x, 0, p0.y],
                        [0, 0, 1, z],
                        [0, 0, 0, 1]
                    ])
                    t_verts = [p.copy() for p in t_pts]

                    # apply slope
                    for i in t_left:
                        t_verts[i].z += t_verts[i].y * (pan.other_side.slope - d.tile_size_z / d.tile_size_y)
                    for i in t_right:
                        t_verts[i].z -= t_verts[i].y * (pan.slope - d.tile_size_z / d.tile_size_y)

                    for k in range(n_obj):
                        rz = d.tile_random_z * uniform(0.0, 1.0)
                        az = d.tile_random_angle_z * uniform(-1.0, 1.0)
                        ay = d.tile_random_angle_y * uniform(0.0, 1.0)

                        lM = tM @ Matrix([
                            [1, 0, 0, x0 + k * dx],
                            [0, -1, 0, 0],
                            [0, 0, 1, rz],
                            [0, 0, 0, 1]
                        ]) @ Matrix.Rotation(az, 4, "Z") @ Matrix.Rotation(ay, 4, "Y")

                        # mid = idmat + randint(0, rand)
                        # t_mats = [idmat] * n_faces
                        v = len(verts)
                        color = self.random_color
                        verts.extend([lM @ p for p in t_verts])
                        faces.extend([tuple(i + v for i in f) for f in t_faces])
                        vcolors.extend([color] * n_faces)
                        matids.extend([idmat] * n_faces)
                        uvs.extend(t_uvs)

    def make_hole(self, context, hole_obj, o, d, update_parent=False):
        """
            Hole for t child on parent
            create / update a RoofCutter on parent
            assume context object is child roof
            with parent set
        """
        # print("Make hole :%s hole_obj:%s" % (o.name, hole_obj))
        if o.parent is None:
            return
        # root is a RoofSegment
        root = self.nodes[0].root
        r_pan = root.right
        l_pan = root.left

        # merge :
        # 5   ____________ 4
        #    /            |
        #   /     left    |
        #  /_____axis_____|  3 <- kill axis and this one
        # 0\              |
        #   \     right   |
        # 1  \____________| 2
        #
        # degenerate case:
        #
        #  /|
        # / |
        # \ |
        #  \|
        #

        segs = []
        _k = None
        # seg[0] -> axis
        for i, seg in enumerate(r_pan.segs):
            # r_pan start parent roof side
            if seg.side_type != 'AXIS':
                _k = seg.copy(last=_k)
                segs.append(seg)

        for i, seg in enumerate(l_pan.segs):
            # l_pan end parent roof side
            if i > 1 and seg.side_type != 'AXIS':
                _k = seg.copy(last=_k)
                segs.append(seg)
        # close
        segs[0]._last = _k
        _k._next = segs[0]

        # if there is side offset:
        # create an arrow
        #
        # 4   s4
        #    /|
        #   / |___s1_______
        #  / p3            | p2  s3
        # 0\ p0___s0_______| p1
        #   \ |
        # 1  \|

        s0 = root.left._axis.offset(
                max(0.001,
                    min(
                        root.right.ysize - 0.001,
                        root.right.ysize - d.hole_offset_right
                        )
                    ))
        s1 = root.left._axis.offset(
                -max(0.001,
                    min(
                        root.left.ysize - 0.001,
                        root.left.ysize - d.hole_offset_left
                        )
                    ))

        s3 = segs[2].offset(
            -min(root.left.xsize - 0.001, d.hole_offset_front)
            )

        s4 = segs[-1].copy()
        s4._next = segs[1]
        # p1 = s4.p1
        # s4.p1 = segs[-1].p0
        # s4.p0 = p1

        res, p0, t = s4.intersect(s0)
        res, p1, t = s0.intersect(s3)
        res, p2, t = s1.intersect(s3)
        res, p3, t = s4.intersect(s1)
        pts = []
        # pts in cw order for 'DIFFERENCE' mode
        pts.extend([segs[-1].p1, segs[-1].p0])
        if (segs[-1].p0 - p3).length > 0.001:
            pts.append(p3)
        pts.extend([p2, p1])
        if (segs[0].p1 - p0).length > 0.001:
            pts.append(p0)
        pts.append(segs[0].p1)

        if hole_obj is None:
            o.parent.select_set(state=True)
            bpy.ops.archipack.roof_cutter(parent=d.t_parent)
            hole_obj = context.active_object
        else:
            hole_obj.select_set(state=True)

        hole_obj.select_set(state=True)

        if d.parts[0].a0 < 0:
            y = -d.t_dist_y
        else:
            y = d.t_dist_y

        hole_obj.matrix_world = o.matrix_world @ Matrix([
            [1, 0, 0, 0],
            [0, 1, 0, y],
            [0, 0, 1, 0],
            [0, 0, 0, 1]
            ])

        hd = archipack_roof_cutter.datablock(hole_obj)
        hd.boundary = o.name
        hd.update_points(context, hole_obj, pts, update_parent=update_parent)
        hole_obj.select_set(state=True)
        o.select_set(state=True)

    def t_partition(self, array, begin, end):
        pivot = begin
        for i in range(begin + 1, end + 1):
            # wall idx
            if array[i][0] < array[begin][0]:
                pivot += 1
                array[i], array[pivot] = array[pivot], array[i]
        array[pivot], array[begin] = array[begin], array[pivot]
        return pivot

    def sort_t(self, array, begin=0, end=None):
        # print("sort_child")
        if end is None:
            end = len(array) - 1

        def _quicksort(array, begin, end):
            if begin >= end:
                return
            pivot = self.t_partition(array, begin, end)
            _quicksort(array, begin, pivot - 1)
            _quicksort(array, pivot + 1, end)
        return _quicksort(array, begin, end)

    def make_wall_fit(self, wall, dz=0, clear_slices=True):
        """
         Skip_z : doesnt set z for auto-fit roof
        """
        wd = wall.data.archipack_wall2[0]
        wg = wd.get_generator(wall)
        z0 = self.z - (wd.z - wd.z_offset) + dz

        # wg in roof coordsys
        # wg.change_coordsys(wall.matrix_world, o.matrix_world)
        _segs = wg.segs
        _parts = wd.parts

        for part, wseg in zip(_parts, _segs):
            if clear_slices:
                part.slices.clear()
                # add default start part
                _s = part.slices.add()
            ls = wseg.length

            for pan in self.pans:
                # walls segment

                for seg in pan.segs:
                    # intersect with a roof segment
                    # any linked or axis intersection here
                    # will be dup as they are between 2 roof parts
                    res, p, t, v = wseg.intersect_ext(seg)
                    if res:
                        n = wseg.normal(t)
                        _p = part.slices.add()
                        _p.auto_update = False
                        a = seg.delta_angle(n)
                        a1 = seg.opposite.delta_angle(n)
                        if abs(a1) < abs(a):
                            a = a1
                        _p.a = a
                        _p.z = z0 + pan.altitude(p)
                        _p.d = t * ls
                        _p.auto_update = True
                        # print("a:",_p.a, " z:", _p.z, " d:", _p.d)
                        # wall_t[widx].append((t, z, t * ls))

                # lie under roof
                if hasattr(wseg, "_r"):
                    for step in range(12):
                        t = step / 12
                        p = wseg.lerp(t)
                        if pan.inside(p):
                            _p = part.slices.add()
                            _p.auto_update = False
                            _p.a = 0
                            _p.z = z0 + pan.altitude(p)
                            _p.d = t * ls
                            _p.auto_update = True
                            #z = z0 + pan.altitude(p)
                            # wall_t[widx].append((t, z, t * ls))
                else:
                    if pan.inside(wseg.p0):
                        _s = part.slices[0]
                        z = z0 + pan.altitude(wseg.p0)
                        _s.auto_update = False
                        _s.z = z
                        _s.auto_update = True
                        # wall_t[widx].append((0, z, 0))

            part.remove_double_slices()

    def boundary(self, o):
        """
            either external or holes cuts
        """
        to_remove = []

        wd = archipack_roof.datablock(o)
        # schrinkwrap target use parent's holes
        if wd.schrinkwrap_target and o.parent:
            childs = o.parent.children
        else:
            childs = o.children

        for b in childs:
            d = archipack_roof_cutter.datablock(b)
            if d is not None:
                tM = o.matrix_world.inverted() @ b.matrix_world
                g = d.ensure_direction(tM)
                # g.change_coordsys(b.matrix_world, o.matrix_world)
                for i, pan in enumerate(self.pans):
                    keep = pan.slice(g)
                    if not keep:
                        if i not in to_remove:
                            to_remove.append(i)
                    pan.limits()

        to_remove.sort()
        for i in reversed(to_remove):
            self.pans.pop(i)

    def draft(self, verts, edges):
        for pan in self.pans:
            pan.draw(self.z, verts, edges)

        for s in self.segs:
            if s.constraint_type == 'SLOPE':
                f = len(verts)
                p0 = s.p0.to_3d()
                p0.z = self.z
                p1 = s.p1.to_3d()
                p1.z = self.z
                verts.extend([p0, p1])
                edges.append([f, f + 1])


def update(self, context):
    self.update(context)


def update_manipulators(self, context):
    self.manipulable_refresh = True
    self.update(context, manipulable_refresh=True)


def update_path(self, context):
    self.update_path(context)


def update_parent(self, context):

    # update part a0
    o = context.active_object
    p, d = self.find_parent(context)

    if d is not None:

        o.parent = p

        # trigger object update
        # hole creation and parent's update

        self.parts[0].a0 = pi / 2

    elif self.t_parent != "":
        self.t_parent = ""


def update_cutter(self, context):
    self.update(context, update_hole=True)


def update_childs(self, context):
    self.update(context, update_childs=True, update_hole=True)


def update_components(self, context):
    self.update(context, update_parent=False, update_hole=False)


class ArchipackSegment():
    length: FloatProperty(
        unit='LENGTH', subtype='DISTANCE',
        precision=5,
        name="Length",
        min=0.01,
        max=1000.0,
        default=4.0,
        update=update
    )
    a0: FloatProperty(
        name="Angle",
        min=-2 * pi,
        max=2 * pi,
        default=0,
        subtype='ANGLE', unit='ROTATION',
        update=update       # _cutter
    )
    expand: BoolProperty(
        description="Expand Segment settings",
        options={'SKIP_SAVE'},
        default=False
    )
    manipulators: CollectionProperty(type=archipack_manipulator)


class ArchipackLines:
    n_parts: IntProperty(
        name="Segments",
        min=1,
        default=1, update=update_manipulators
    )

    def draw(self, layout, context):
        self.draw_prop(context, layout, self, 'n_parts')
        for i, part in enumerate(self.parts):
            part.draw(layout, context, i)

    def update_parts(self):
        # print("update_parts")
        # remove rows
        # NOTE:
        # n_parts+1
        # as last one is end point of last segment or closing one
        for i in range(len(self.parts), self.n_parts + 1, -1):
            self.parts.remove(i - 1)

        # add rows
        for i in range(len(self.parts), self.n_parts + 1):
            self.parts.add()

        self.setup_manipulators()


def slope_getter(attr):
    def get_slope(self):
        return getattr(self, attr) * 100
    return get_slope


def slope_setter(attr):
    def set_slope(self, value):
        setattr(self, attr, value / 100)
        return None
    return set_slope


def slope_angle_getter(attr):
    def get_slope_angle(self):
        return atan(getattr(self, attr))
    return get_slope_angle


def slope_angle_setter(attr):
    def set_slope_angle(self, value):
        setattr(self, attr, tan(value))
        return None
    return set_slope_angle


class archipack_roof_segment(Archipacki18n, ArchipackSegment, PropertyGroup):
    bound_idx: IntProperty(
        name="Link to",
        default=0,
        min=0,
        update=update_manipulators
    )
    width_left: FloatProperty(
        unit='LENGTH', subtype='DISTANCE',
        precision=5,
        name="L Width",
        min=0.01,
        default=3.0,
        update=update_cutter
    )
    width_right: FloatProperty(
        unit='LENGTH', subtype='DISTANCE',
        precision=5,
        name="R Width",
        min=0.01,
        default=3.0,
        update=update_cutter
    )
    slope_left: FloatProperty(
        precision=5,
        name="L slope",
        min=0.0,
        default=0.3
    )

    slope_left_percent: FloatProperty(
        options={'SKIP_SAVE'},
        subtype="PERCENTAGE",
        precision=2,
        name="L slope",
        min=0.0,
        max=560,
        get=slope_getter('slope_left'),
        set=slope_setter('slope_left'),
        update=update_cutter
    )

    slope_left_angle: FloatProperty(
        options={'SKIP_SAVE'},
        subtype='ANGLE', unit='ROTATION',
        precision=5,
        name="L slope",
        min=0.0,
        max=1.4835298641951802,
        get=slope_angle_getter('slope_left'),
        set=slope_angle_setter('slope_left'),
        update=update_cutter
    )
    slope_right: FloatProperty(
        precision=5,
        name="R slope",
        min=0.0,
        default=0.3
    )
    slope_right_percent: FloatProperty(
        options={'SKIP_SAVE'},
        subtype="PERCENTAGE",
        precision=2,
        name="R slope",
        min=0.0,
        max=560,
        get=slope_getter('slope_right'),
        set=slope_setter('slope_right'),
        update=update_cutter
    )

    slope_right_angle: FloatProperty(
        options={'SKIP_SAVE'},
        subtype='ANGLE', unit='ROTATION',
        precision=5,
        name="R slope",
        min=0.0,
        max=1.4835298641951802,
        get=slope_angle_getter('slope_right'),
        set=slope_angle_setter('slope_right'),
        update=update_cutter
    )
    enable_left: BoolProperty(
        name="Left",
        default=True,
        update=update
    )
    enable_right: BoolProperty(
        name="Right",
        default=True,
        update=update
    )
    auto_left: EnumProperty(
        description="Left mode",
        name="Left",
        items=(
            ('AUTO', 'Auto', '', 0),
            ('WIDTH', 'Width', '', 1),
            ('SLOPE', 'Slope', '', 2),
            ('ALL', 'All', '', 3),
        ),
        default="AUTO",
        update=update_manipulators
    )
    auto_right: EnumProperty(
        description="Right mode",
        name="Right",
        items=(
            ('AUTO', 'Auto', '', 0),
            ('WIDTH', 'Width', '', 1),
            ('SLOPE', 'Slope', '', 2),
            ('ALL', 'All', '', 3),
        ),
        default="AUTO",
        update=update_manipulators
    )
    triangular_end: BoolProperty(
        name="Triangular end",
        default=False,
        update=update
    )
    take_precedence: BoolProperty(
        name="Take precedence",
        description="On T segment take width precedence",
        default=False,
        update=update
    )
    constraint_type: EnumProperty(
        items=(
            ('HORIZONTAL', 'Horizontal', '', 0),
            ('SLOPE', 'Slope', '', 1)
        ),
        default='HORIZONTAL',
        update=update_manipulators
    )
    enforce_part: EnumProperty(
        name="Enforce part",
        items=(
            ('AUTO', 'Auto', '', 0),
            ('VALLEY', 'Valley', '', 1),
            ('HIP', 'Hip', '', 2)
        ),
        default='AUTO',
        update=update
    )
    # DimensionProvider
    uid: IntProperty(default=0)

    type = 0

    @property
    def parent_data(self):
        return self.id_data.archipack_roof[0]

    def draw(self, context, layout, index):
        box = layout.box()

        icon = "TRIA_RIGHT"
        if self.expand:
            icon = "TRIA_DOWN"

        row = box.row(align=True)
        self.draw_prop(context, layout, row, self, 'expand', icon=icon, emboss=True,
                       text="Seg", postfix=str(index + 1))

        if index > 0:
            self.draw_prop(context, layout, row, self, "constraint_type", text="")
            if self.expand and self.constraint_type == 'SLOPE':
                self.draw_prop(context, layout, box, self, "enforce_part", text="")

        if self.expand:

            self.draw_prop(context, layout, box, self, "length")
            self.draw_prop(context, layout, box, self, "a0")

            if index > 0:
                self.draw_prop(context, layout, box, self, 'bound_idx')
                if self.constraint_type == 'HORIZONTAL':

                    self.draw_prop(context, layout, box, self, "triangular_end")

                    # row = box.row(align=True)
                    self.draw_prop(context, layout, box, self, 'enable_left')
                    if self.enable_left:
                        self.draw_prop(context, layout, box, self, "auto_left", text="")

                        if self.auto_left in {'ALL', 'WIDTH'}:
                            self.draw_prop(context, layout, box, self, "width_left")
                        if self.auto_left in {'ALL', 'SLOPE'}:
                            self.draw_prop(context, layout, box, self, "slope_left_percent")
                            self.draw_prop(context, layout, box, self, "slope_left_angle")

                    self.draw_prop(context, layout, box, self, 'enable_right')
                    if self.enable_right:
                        self.draw_prop(context, layout, box, self, "auto_right", text="")

                        if self.auto_right in {'ALL', 'WIDTH'}:
                            self.draw_prop(context, layout, box, self, "width_right")
                        if self.auto_right in {'ALL', 'SLOPE'}:
                            self.draw_prop(context, layout, box, self, "slope_right_percent")
                            self.draw_prop(context, layout, box, self, "slope_right_angle")

            elif self.constraint_type == 'HORIZONTAL':
                row = box.row(align=True)
                self.draw_prop(context, layout, row, self, 'enable_left')
                self.draw_prop(context, layout, row, self, 'enable_right')

                self.draw_prop(context, layout, box, self, "triangular_end")

    def update(self, context, manipulable_refresh=False, update_hole=False):
        self.parent_data.update(context,
                manipulable_refresh,
                update_parent=True,
                update_hole=True,
                update_childs=True)


def get_t_part(self):
    return self.t_part + 1


def set_t_part(self, value):
    self.t_part = value - 1
    return None

def get_z(self):
    return self.z

def set_z(self, value):
    self.z = value
    return None


class archipack_roof(Archipacki18n, ArchipackLines, ArchipackObject, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display main settings', 'NONE', 0),
            ('PARTS', 'Axis', 'Display roof parts settings', 'NONE', 1),
            ('SUB', 'Parts', 'Display components settings', 'NONE', 2),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 3)
        ),
        default='MAIN',
    )
    parts: CollectionProperty(type=archipack_roof_segment)
    origin: FloatVectorProperty(subtype="XYZ")
    z: FloatProperty(
        name="Altitude",
        default=3, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE'
    )
    z_ui: FloatProperty(
        name="Altitude",
        default=3, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        get=get_z,
        set=set_z,
        update=update_childs
    )

    slope_left: FloatProperty(
        name="L slope",
        default=0.5, precision=5, step=1
    )

    slope_left_percent: FloatProperty(
        options={'SKIP_SAVE'},
        subtype="PERCENTAGE",
        precision=2,
        name="L slope",
        min=0.0,
        max=560,
        get=slope_getter('slope_left'),
        set=slope_setter('slope_left'),
        update=update_childs
    )

    slope_left_angle: FloatProperty(
        options={'SKIP_SAVE'},
        subtype='ANGLE', unit='ROTATION',
        precision=5,
        name="L slope",
        min=0.0,
        max=1.4835298641951802,
        get=slope_angle_getter('slope_left'),
        set=slope_angle_setter('slope_left'),
        update=update_childs
    )
    slope_right: FloatProperty(
        name="R slope",
        default=0.5, precision=5, step=1
    )
    slope_right_percent: FloatProperty(
        options={'SKIP_SAVE'},
        subtype="PERCENTAGE",
        precision=2,
        name="R slope",
        min=0.0,
        max=560,
        get=slope_getter('slope_right'),
        set=slope_setter('slope_right'),
        update=update_childs
    )

    slope_right_angle: FloatProperty(
        options={'SKIP_SAVE'},
        subtype='ANGLE', unit='ROTATION',
        precision=5,
        name="R slope",
        min=0.0,
        max=1.4835298641951802,
        get=slope_angle_getter('slope_right'),
        set=slope_angle_setter('slope_right'),
        update=update_childs
    )

    width_left: FloatProperty(
        name="L width",
        default=3, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update_cutter
    )
    width_right: FloatProperty(
        name="R width",
        default=3, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update_cutter
    )
    draft: BoolProperty(
        options={'SKIP_SAVE'},
        name="Draft mode",
        default=False,
        update=update_manipulators
    )
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update_manipulators
    )

    tile_enable: BoolProperty(
        name="Enable",
        default=True,
        update=update_components
    )
    tile_solidify: BoolProperty(
        name="Solidify",
        default=True,
        update=update_components
    )
    tile_height: FloatProperty(
        name="Height",
        description="Amount for solidify",
        min=0,
        default=0.02,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    tile_bevel: BoolProperty(
        name="Bevel",
        default=False,
        update=update_components
    )
    tile_bevel_amt: FloatProperty(
        name="Amount",
        description="Amount for bevel",
        min=0, max=100,
        default=0.02,
        subtype='PERCENTAGE',
        # unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    tile_bevel_segs: IntProperty(
        name="Segs",
        description="Bevel Segs",
        min=1,
        default=2,
        update=update_components
    )
    tile_alternate: BoolProperty(
        name="Alternate",
        default=False,
        update=update_components
    )
    tile_offset: FloatProperty(
        name="Offset",
        description="Offset from start",
        min=0,
        max=100,
        subtype="PERCENTAGE",
        update=update_components
    )
    tile_altitude: FloatProperty(
        name="Altitude",
        description="Altitude from roof",
        default=0.1,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    tile_size_x: FloatProperty(
        name="Width",
        description="Size of tiles on x axis",
        min=0.01,
        default=0.2,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    tile_size_y: FloatProperty(
        name="Length",
        description="Size of tiles on y axis",
        min=0.01,
        default=0.3,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    tile_random_z: FloatProperty(
        name="Altitude",
        description="Random altitude",
        min=0.0,
        default=0.005,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    tile_random_angle_z: FloatProperty(
        name="Angle z",
        description="Random angle on z axis",
        default=0.005,
        min=0, max=1.57,
        unit='ROTATION',
        subtype='ANGLE',
        update=update_components
    )
    tile_random_angle_y: FloatProperty(
        name="Angle y",
        description="Random angle on y axis",
        default=0.005,
        min=0, max=1.57,
        unit='ROTATION',
        subtype='ANGLE',
        update=update_components
    )
    tile_random_scale: FloatProperty(
        name="Scale",
        description="Random scale",
        default=0.0,
        min=0,
        subtype='PERCENTAGE',
        update=update_components
    )
    tile_size_z: FloatProperty(
        name="Thickness",
        description="Size of tiles on z axis",
        min=0.0,
        default=0.02,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    tile_space_x: FloatProperty(
        name="Width",
        description="Space between tiles on x axis",
        min=0.01,
        default=0.2,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )

    tile_space_y: FloatProperty(
        name="Length",
        description="Space between tiles on y axis",
        min=0.01,
        default=0.3,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    tile_fit_x: BoolProperty(
        name="Fit x",
        description="Fit roof on x axis",
        default=True,
        update=update_components
    )
    tile_fit_y: BoolProperty(
        name="Fit y",
        description="Fit roof on y axis",
        default=True,
        update=update_components
    )
    tile_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Tiles",
        description="Expand tiles panel",
        default=False
    )
    tile_model: EnumProperty(
        name="Model",
        items=(
            ('BRAAS1', 'Braas 1', '', 0),
            ('BRAAS2', 'Braas 2', '', 1),
            ('ETERNIT', 'Eternit', '', 2),
            ('LAUZE', 'Lauze', '', 3),
            ('ROMAN', 'Roman', '', 4),
            ('ROUND', 'Round', '', 5),
            ('PLACEHOLDER', 'Square', '', 6),
            ('ONDULEE', 'Ondule', '', 7),
            ('METAL', 'Metal', '', 8),
            # ('USER', 'User defined', '', 7)
        ),
        default="BRAAS2",
        update=update_components
    )
    tile_side: FloatProperty(
        name="Side",
        description="Space on side",
        default=0,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    tile_couloir: FloatProperty(
        name="Valley",
        description="Space between tiles on valley",
        min=0,
        default=0.05,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    tile_border: FloatProperty(
        name="Bottom",
        description="Tiles offset from bottom",
        default=0,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )

    gutter_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Gutter",
        description="Expand gutter panel",
        default=False
    )
    gutter_enable: BoolProperty(
        name="Enable",
        description="Enable gutter",
        default=True,
        update=update_components
    )
    gutter_alt: FloatProperty(
        name="Altitude",
        description="Gutter altitude",
        default=0,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    gutter_width: FloatProperty(
        name="Width",
        description="Gutter width",
        min=0.01,
        default=0.15,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    gutter_dist: FloatProperty(
        name="Spacing",
        description="Gutter spacing",
        min=0,
        default=0.05,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    gutter_boudin: FloatProperty(
        name="Small width",
        description="Small width",
        min=0,
        default=0.015,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    gutter_segs: IntProperty(
        default=6,
        min=1,
        name="Segs",
        update=update_components
    )

    beam_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Beam",
        description="Expand beam panel",
        default=False
    )
    beam_enable: BoolProperty(
        name="Ridge pole",
        default=True,
        update=update_components
    )
    beam_width: FloatProperty(
        name="Width",
        description="Width",
        min=0.01,
        default=0.2,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    beam_height: FloatProperty(
        name="Height",
        description="Height",
        min=0.01,
        default=0.35,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    beam_offset: FloatProperty(
        name="Offset",
        description="Distance from roof border",
        default=0.02,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    beam_alt: FloatProperty(
        name="Altitude",
        description="Altitude from roof",
        default=-0.15,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    beam_sec_enable: BoolProperty(
        name="Hip rafter",
        default=True,
        update=update_components
    )
    beam_sec_width: FloatProperty(
        name="Width",
        description="Width",
        min=0.01,
        default=0.15,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    beam_sec_height: FloatProperty(
        name="Height",
        description="Height",
        min=0.01,
        default=0.2,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    beam_sec_alt: FloatProperty(
        name="Altitude",
        description="Distance from roof",
        default=-0.1,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )

    rafter_enable: BoolProperty(
        name="Rafter",
        default=True,
        update=update_components
    )
    rafter_width: FloatProperty(
        name="Width",
        description="Width",
        min=0.01,
        default=0.1,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    rafter_height: FloatProperty(
        name="Height",
        description="Height",
        min=0.01,
        default=0.2,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    rafter_spacing: FloatProperty(
        name="Spacing",
        description="Spacing",
        min=0.1,
        default=0.7,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    rafter_start: FloatProperty(
        name="Offset",
        description="Spacing from roof border",
        min=0,
        default=0.1,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    rafter_alt: FloatProperty(
        name="Altitude",
        description="Altitude from roof",
        max=-0.0001,
        default=-0.001,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )

    hip_enable: BoolProperty(
        name="Enable",
        default=True,
        update=update_components
    )
    hip_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Hips",
        description="Expand hips panel",
        default=False
    )
    hip_alt: FloatProperty(
        name="Altitude",
        description="Hip altitude from roof",
        default=0.1,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    hip_space_x: FloatProperty(
        name="Spacing",
        description="Space between hips",
        min=0.01,
        default=0.4,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    hip_size_x: FloatProperty(
        name="Length",
        description="Length of hip",
        min=0.01,
        default=0.4,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    hip_size_y: FloatProperty(
        name="Width",
        description="Width of hip",
        min=0.01,
        default=0.15,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    hip_size_z: FloatProperty(
        name="Height",
        description="Height of hip",
        min=0.0,
        default=0.15,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    hip_model: EnumProperty(
        name="Model",
        items=(
            ('ROUND', 'Round', '', 0),
            ('ETERNIT', 'Eternit', '', 1),
            ('FLAT', 'Flat', '', 2)
        ),
        default="ROUND",
        update=update_components
    )
    valley_altitude: FloatProperty(
        name="Altitude",
        description="Valley altitude from roof",
        default=0.1,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    valley_enable: BoolProperty(
        name="Valley",
        default=True,
        update=update_components
    )
    fascia_enable: BoolProperty(
        name="Enable",
        description="Enable Fascia",
        default=True,
        update=update_components
    )
    fascia_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Fascia",
        description="Expand fascia panel",
        default=False
    )
    fascia_height: FloatProperty(
        name="Height",
        description="Height",
        min=0.01,
        default=0.3,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    fascia_width: FloatProperty(
        name="Width",
        description="Width",
        min=0.01,
        default=0.02,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    fascia_offset: FloatProperty(
        name="Offset",
        description="Offset from roof border",
        default=0,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    fascia_altitude: FloatProperty(
        name="Altitude",
        description="Fascia altitude from roof",
        default=0.1,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    bargeboard_enable: BoolProperty(
        name="Enable",
        description="Enable Bargeboard",
        default=True,
        update=update_components
    )
    bargeboard_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Bargeboard",
        description="Expand Bargeboard panel",
        default=False
    )
    bargeboard_height: FloatProperty(
        name="Height",
        description="Height",
        min=0.01,
        default=0.3,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    bargeboard_width: FloatProperty(
        name="Width",
        description="Width",
        min=0.01,
        default=0.02,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    bargeboard_offset: FloatProperty(
        name="Offset",
        description="Offset from roof border",
        default=0.001,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    bargeboard_altitude: FloatProperty(
        name="Altitude",
        description="Fascia altitude from roof",
        default=0.1,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    lambris_height: FloatProperty(
        name="Paneling",
        description="Paneling thickness",
        default=0.02,
        unit='LENGTH', subtype='DISTANCE',
        update=update_components
    )
    enable_left: BoolProperty(
        name="Left",
        default=True,
        update=update
    )
    enable_right: BoolProperty(
        name="Right",
        default=True,
        update=update
    )

    t_parent: StringProperty(
        name="Parent",
        default="",
        update=update_parent
    )
    t_part: IntProperty(
        name="Part",
        description="Parent part index",
        default=0,
        min=0
    )

    t_part_ui: IntProperty(
        options={'SKIP_SAVE'},
        name="Part",
        description="Parent part index",
        default=1,
        min=1,
        get=get_t_part,
        set=set_t_part,
        update=update_cutter
    )
    t_dist_x: FloatProperty(
        name="Dist x",
        description="Location on axis ",
        default=0,
        update=update_cutter
    )
    t_dist_y: FloatProperty(
        name="Dist y",
        description="Lateral distance from axis",
        min=0.0001,
        default=0.0001,
        update=update_cutter
    )

    hole_offset_left: FloatProperty(
        name="Left",
        description="Left distance from border",
        default=0,
        update=update_cutter
    )
    hole_offset_right: FloatProperty(
        name="Right",
        description="Right distance from border",
        default=0,
        update=update_cutter
    )
    hole_offset_front: FloatProperty(
        name="Front",
        description="Front distance from border",
        default=0,
        update=update_cutter
    )
    schrinkwrap_target: BoolProperty(
        name="Schrikwrap target",
        description="Use this part as target for wall fit",
        default=False
    )
    expand_materials: BoolProperty(
        name="Materials",
        default=False,
        options={'SKIP_SAVE'}
    )

    idmat: IntVectorProperty(
        default=[
            2, 2,
            0, 0,
            1,
            0, 0, 0
        ],
        size=8
    )

    idmat_covering: EnumProperty(
        options={'SKIP_SAVE'},
        description="Covering",
        name="Covering",
        items=mat_enum,
        get=mat_index_getter(MAT_COVERING),
        set=mat_index_setter(MAT_COVERING),
        update=update
    )
    idmat_hip: EnumProperty(
        options={'SKIP_SAVE'},
        name="Hip",
        items=mat_enum,
        get=mat_index_getter(MAT_HIP),
        set=mat_index_setter(MAT_HIP),
        update=update
    )
    idmat_ridge_pole: EnumProperty(
        options={'SKIP_SAVE'},
        name="Ridge pole",
        items=mat_enum,
        get=mat_index_getter(MAT_RIGE_POLE),
        set=mat_index_setter(MAT_RIGE_POLE),
        update=update
    )
    idmat_rafter: EnumProperty(
        options={'SKIP_SAVE'},
        name="Rafter",
        items=mat_enum,
        get=mat_index_getter(MAT_RAFTER),
        set=mat_index_setter(MAT_RAFTER),
        update=update
    )
    idmat_gutter: EnumProperty(
        options={'SKIP_SAVE'},
        name="Gutter",
        items=mat_enum,
        get=mat_index_getter(MAT_GUTTER),
        set=mat_index_setter(MAT_GUTTER),
        update=update
    )
    idmat_fascia: EnumProperty(
        options={'SKIP_SAVE'},
        name="Fascia",
        items=mat_enum,
        get=mat_index_getter(MAT_FASCIA),
        set=mat_index_setter(MAT_FASCIA),
        update=update
    )

    idmat_bargeboard: EnumProperty(
        options={'SKIP_SAVE'},
        name="Bargeboard",
        items=mat_enum,
        get=mat_index_getter(MAT_BARGEBOARD),
        set=mat_index_setter(MAT_BARGEBOARD),
        update=update
    )
    idmat_lambris: EnumProperty(
        options={'SKIP_SAVE'},
        name="Bottom",
        items=mat_enum,
        get=mat_index_getter(MAT_LAMBRIS),
        set=mat_index_setter(MAT_LAMBRIS),
        update=update
    )
    user_defined_roof: StringProperty(
        name="User defined",
        default="",
        update=update_manipulators
    )

    @property
    def num_parts(self):
        return len(self.parts)

    is_closed = False
    always_closed = False

    def make_wall_fit(self, context, o, wall, clear_slices=True):
        g = self.get_generator(o)
        dz = o.matrix_world.translation.z - wall.matrix_world.translation.z
        custom = self.get_scene_object(context, self.user_defined_roof)
        if custom is not None:
            # in world coordsys
            g.pans = self.user_roof(o, custom, world=True)
        else:
            g.make_roof(self)
        g.make_wall_fit(wall, dz, clear_slices=clear_slices)

    def find_shrinkwrap(self, o):
        for c in o.children:
            d = archipack_roof.datablock(c)
            if d and d.schrinkwrap_target:
                return c
        return None

    def create_shrinkwrap(self, context, o, target=None):
        """
         Create shrinkwrap target from roof
        """
        m = o.data.copy()
        if target is None:
            new_o = bpy.data.objects.new(o.name, m)

        else:
            old_m = target.data
            target.data = m
            new_o = target
            bpy.data.meshes.remove(old_m)

        d = archipack_roof.datablock(new_o)
        d.schrinkwrap_target = True
        d.idmat[MAT_LAMBRIS] = self.idmat[MAT_LAMBRIS]

        if target is None:
            # Link object into scene
            self.link_object_to_scene(context, new_o)
            new_o.color = (0, 1, 0, 1)
            new_o.parent = o
            new_o.matrix_world = o.matrix_world.copy()
        self.link_materials(context, o, new_o)
        self.select_object(context, new_o)
        d.auto_update = True
        self.unselect_object(context, new_o)
        return new_o

    def update_childrens(self, context):
        self.update(context, update_childs=True, update_hole=True)

    def update_parts(self):
        # NOTE:
        # n_parts+1
        # as last one is end point of last segment or closing one
        for i in range(len(self.parts), self.n_parts, -1):
            self.parts.remove(i - 1)

        # add rows
        for i in range(len(self.parts), self.n_parts):
            bound_idx = len(self.parts)
            self.parts.add()
            self.parts[-1].bound_idx = bound_idx

        for p in self.parts:
            self.create_uid(p)

        self.setup_manipulators()

    def setup_manipulators(self):
        if self.schrinkwrap_target:
            return
        if len(self.manipulators) < 1:
            s = self.manipulators.add()
        else:
            s = self.manipulators[0]
        s.type_key = "SIZE"
        s.prop1_name = "z_ui"
        s.normal = (0, 1, 0)
        if len(self.manipulators) < 2:
            s = self.manipulators.add()
            s.type_key = "SIZE"
            s.prop1_name = "width_left"
        if len(self.manipulators) < 3:
            s = self.manipulators.add()
            s.type_key = "SIZE"
            s.prop1_name = "width_right"
        if len(self.manipulators) < 4:
            s = self.manipulators.add()
            s.prop1_name = "n_parts"
            s.type_key = 'COUNTER'

        for i, p in enumerate(self.parts):
            n_manips = len(p.manipulators)
            if n_manips < 1:
                s = p.manipulators.add()
                s.type_key = "ANGLE"
                s.prop1_name = "a0"
            if n_manips < 2:
                s = p.manipulators.add()
                s.type_key = "SIZE"
                s.prop1_name = "length"
            if n_manips < 3:
                s = p.manipulators.add()
                s.type_key = 'DUMB_STRING'
                s.prop1_name = str(i + 1)
            p.manipulators[2].prop1_name = str(i + 1)
            if n_manips < 4:
                s = p.manipulators.add()
                s.type_key = 'SIZE'
                s.prop1_name = "width_left"
            if n_manips < 5:
                s = p.manipulators.add()
                s.type_key = 'SIZE'
                s.prop1_name = "width_right"
            if n_manips < 6:
                s = p.manipulators.add()
                s.type_key = 'SIZE'
                s.prop1_name = "slope_left_percent"
                s.prop2_name = json.dumps({'factor': 100})
            if n_manips < 7:
                s = p.manipulators.add()
                s.type_key = 'SIZE'
                s.prop1_name = "slope_right_percent"
                s.prop2_name = json.dumps({'factor': 100})

    def get_generator(self, o=None):

        g = RoofGenerator(o)

        if self.t_parent != "":
            y = self.t_dist_y
            if self.parts[0].a0 < 0:
                y = -y
            g.location = Vector((0, y, 0))

        # TODO: sort part by bound idx so deps always find parent
        g.add_parts(self)

        return g

    def make_surface(self, o, verts, edges):
        bm = bmesh.new()
        for v in verts:
            bm.verts.new(v)
        bm.verts.ensure_lookup_table()
        for ed in edges:
            bm.edges.new((bm.verts[ed[0]], bm.verts[ed[1]]))
        bm.edges.ensure_lookup_table()
        # bmesh.ops.contextual_create(bm, geom=bm.edges)
        bm.to_mesh(o.data)
        bm.free()

    def find_parent(self, context):
        o = self.get_scene_object(context, self.t_parent)
        return o, archipack_roof.datablock(o)

    def intersection_angle(self, t_slope, t_width, p_slope, angle):
        # 2d intersection angle between two roofs parts
        dy = abs(t_slope * t_width / p_slope)
        ca = cos(angle)
        ta = tan(angle)
        if ta == 0:
            w0 = 0
        else:
            w0 = dy * ta
        if ca == 0:
            w1 = 0
        else:
            w1 = t_width / ca
        dx = w1 - w0
        return atan2(dy, dx)

    def relocate_child(self, context, o, g, child):

        d = archipack_roof.datablock(child)
        if d is not None:

            if self.user_defined_roof != "":

                if d.t_part < len(g.pans):
                    pan = g.pans[d.t_part]
                    t = d.t_dist_x / pan.fake_axis.length
                    x, y, z = pan.fake_axis.lerp(t)

                    dy = pan.fake_axis.v_normalized
                    child.matrix_world = o.matrix_world @ Matrix([
                        [dy.x, -dy.y, 0, x],
                        [dy.y, dy.x, 0, y],
                        [0, 0, 1, z],
                        [0, 0, 0, 1]
                    ])

            elif d.t_part < len(g.segs):
                    # print("relocate_child(%s)" % (child.name))

                    seg = g.segs[d.t_part]
                    # adjust T part matrix_world from parent
                    # T part origin located on parent axis
                    # with y in parent direction
                    t = max(0, d.t_dist_x) / seg.length
                    x, y, z = seg.lerp(t)
                    dy = -seg.v_normalized
                    child.matrix_world = o.matrix_world @ Matrix([
                        [dy.x, -dy.y, 0, x],
                        [dy.y, dy.x, 0, y],
                        [0, 0, 1, z],
                        [0, 0, 0, 1]
                    ])

    def relocate_childs(self, context, o, g):
        for child in o.children:
            d = archipack_roof.datablock(child)
            if d is not None and d.t_parent == o.name:
                self.relocate_child(context, o, g, child)

    def update_childs(self, context, o, g):
        for child in o.children:
            d = archipack_roof.datablock(child)
            if d is not None:
                if d.t_parent.strip() == o.name and not d.schrinkwrap_target:
                    self.select_object(context, child, True)
                    # regenerate hole
                    d.update(context, update_hole=True, update_parent=False)
                    self.unselect_object(context, child)

        self.select_object(context, o, True)

    def polygon_matrix_2d(self, poly):
        """Matrix at polygon center with z_axis in normal direction
        :param poly:
        :return:
        """
        vx, vy = self.polygon_vector(poly)
        p = poly.calc_center_median()

        return Matrix([
            [vx.x, vy.x, 0, p.x],
            [vx.y, vy.y, 0, p.y],
            [vx.z, vy.z, 1, p.z],
            [0, 0, 0, 1]
        ])

    def poly_bounds(self, poly, tM):
        itM = tM.inverted()
        return [itM @ loop.vert.co for loop in poly.loops]

    def polygon_vector(self, poly):
        """Horizontal vectors vy in normal direction, vx parallel to face
        :param poly:
        :return:
        """
        vz = poly.normal
        if abs(vz.z) > 0.1:
            vx = vz.cross(Z_AXIS)
            vy = vx.cross(Z_AXIS)
        else:
            vx = vz.cross(X_AXIS)
            vy = vx.cross(Z_AXIS)
        return vx, vy

    def user_roof(self, o, custom, world=False):
        bm = bmed._start(custom)

        pans = []

        # top bottom left
        cust_loc = Vector(custom.bound_box[1])
        cust_pos0 = cust_loc.copy()
        cust_pos0.z = 0

        # When moving object, must relocate cutters
        # Keep track of child location in world coordsys
        _cp = [child.matrix_world.translation.to_3d() for child in o.children if archipack_roof_cutter.filter(child)]

        o.matrix_world = custom.matrix_world @ Matrix.Translation(cust_pos0)

        # Update child location so absolute location wont change
        if len(_cp) > 0:
            itM = o.matrix_world.inverted()
            for _p, child in zip(_cp, o.children):
                child.location = itM @ _p
                child.matrix_world.translation = _p

        self.z = cust_loc.z
        # Matrix in the custom left bottom coordsys
        # for fit roof we need a world location
        if world:
            itM = custom.matrix_world
        else:
            itM = Matrix.Translation(cust_loc).inverted()

        z_top = self.z

        polys = [poly for poly in bm.faces if poly.normal.z > 0.01]
        # Normal vector on xy plane
        normals = [self.polygon_vector(poly)[1].normalized() for poly in bm.faces]

        top_edges = set()
        link_edges = set()

        for poly in polys:
            tM = self.polygon_matrix_2d(poly)
            # fake axis
            v_axis, v_slope = self.polygon_vector(poly)
            bounds = self.poly_bounds(poly, tM)
            bx, by, bz = zip(*bounds)

            minx, miny, minz, maxx, maxy, maxz = min(bx), min(by), min(bz), max(bx), max(by), max(bz)
            width_left = maxy - miny
            slope_left = poly.normal.to_2d().length / poly.normal.z

            # identify a top left vertex
            i = 0
            cur_x = 1e32
            cur_id = 0
            for x, z, v in zip(bx, bz, poly.verts):
                if z == maxz:
                    if x < cur_x:
                        cur_x = x
                        cur_id = i
                i += 1

            co = itM @ poly.verts[cur_id].co
            pan_z = co.z
            loops = poly.loops[cur_id:] + poly.loops[:cur_id]
            bz = bz[cur_id:] + bz[:cur_id]

            s = StraightRoof(co, p1=co + v_axis, last=None)
            # parent segment (root) index is  v0_idx - 1
            s.v0_idx = -1
            s.constraint_type = 'HORIZONTAL'
            s.enforce_part = 'AUTO'
            s.angle_0 = 0
            s.take_precedence = False
            s.auto_right = True
            s.auto_left = True
            s.width_left = width_left
            s.width_right = width_left
            s.slope_left = slope_left
            s.slope_right = slope_left
            s.side_type = 'AXIS'
            s.triangular_end = False

            left = RoofPolygon(s, 'LEFT', fake_axis=s, remove=False)
            right = RoofPolygon(s, 'RIGHT', fake_axis=s, remove=True)
            left.segs = []
            left.next_tri = True
            left.node_tri = True

            last = None

            # setup roof polygons segments (left only)
            for z, loop in zip(bz, loops):
                x, y, _ = itM @ loop.vert.co
                co = Vector((x, y, 0))
                last = StraightRoof(co, last=last)
                v0, v1 = loop.edge.verts
                id0, id1 = v0.index, v1.index

                if id0 > id1:
                    id0, id1 = id1, id0

                edge_idx = (id0, id1)

                if loop.edge.is_manifold:
                    if abs(v0.co.z - v1.co.z) < 0.01:
                        side_type = "AXIS_NO_HIP"
                        if edge_idx not in top_edges and abs(z - maxz) < 0.01:
                            top_edges.add(edge_idx)
                            side_type = "AXIS"
                    else:
                        a = loop.edge.calc_face_angle_signed()

                        if a is None or a < 0:
                            side_type = "LINK_VALLEY"
                        else:
                            side_type = "LINK_HIP"

                else:
                    if abs(v0.co.z - v1.co.z) < 0.01:
                        side_type = "BOTTOM"
                        # get face normals linked to this edge
                        faces = [other.face.index for other in loop.link_loop_next.link_loops if
                                 other.face.index != poly.index]
                        if len(faces) > 0:
                            vy = normals[faces[0]]
                            p0 = itM @ v0.co
                            left.last_cross = StraightRoof(p0, p1=p0 - vy)
                        faces = [other.face.index for other in loop.link_loop_prev.link_loops if
                                 other.face.index != poly.index]
                        if len(faces) > 0:
                            vy = normals[faces[0]]
                            p1 = itM @ v1.co
                            left.next_cross = StraightRoof(p1, p1=p1 - vy)
                    else:
                        side_type = "SIDE"

                last.side_type = side_type
                left.segs.append(last)

            last._next = left.segs[0]
            left.segs[0]._last = last

            left.other_side = right
            right.other_side = left

            left.convex = all([loop.is_convex for loop in poly.loops])
            left.z = pan_z + z_top

            pans.append(left)

        # set last_cross and next_cross
        # associate edges with cross

        for pan in pans:
            pan.limits()

        bm.free()
        return pans

    def update(self,
                context,
                manipulable_refresh=False,
                update_childs=False,
                update_parent=True,
                update_hole=False,
                force_update=False):
        """
            update_hole: on t_child must update parent
            update_childs: force childs update
            force_update: skip throttle
        """

        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        if not (self.draft or self.schrinkwrap_target):
            throttle.add(context, o, self)

        # clean up manipulators before any data model change
        if manipulable_refresh and not self.schrinkwrap_target:
            self.manipulable_disable(o)

        self.update_parts()

        verts, edges, faces, matids, uvs, vcolors = [], [], [], [], [], []

        # me = o.data
        # vlay = me.vertex_colors.get("Archipack")
        # if vlay is not None:
        #    me.vertex_colors.remove(vlay)

        y = 0
        z = self.z
        p, d = self.find_parent(context)
        g = None

        # t childs: use parent to relocate
        # setup slopes into generator
        if d is not None:
            pg = d.get_generator()

            custom = self.get_scene_object(context, d.user_defined_roof)
            if custom is not None:
                pg.pans = d.user_roof(p, custom)
                max_tpart = len(pg.pans)
            else:
                pg.make_roof(d)
                max_tpart = len(pg.nodes)

            if self.t_part < max_tpart:

                d.relocate_child(context, p, pg, o)

                a0 = self.parts[0].a0
                a_axis = a0 - pi / 2
                a_offset = 0
                s_left = self.slope_left
                w_left = -self.width_left
                s_right = self.slope_right
                w_right = self.width_right

                if custom is None:

                    seg = pg.nodes[self.t_part].root
                    if a0 > 0:
                        # a_axis est mesure depuis la perpendiculaire à l'axe
                        slope = seg.right.slope
                        y = self.t_dist_y
                    else:
                        a_offset = pi
                        slope = seg.left.slope
                        y = -self.t_dist_y
                        s_left, s_right = s_right, s_left
                        w_left, w_right = -w_right, -w_left

                    if slope == 0:
                        slope = 0.0001

                    # print("slope: %s" % (slope))

                    z = d.z - self.t_dist_y * slope

                else:
                    pan = pg.pans[self.t_part]
                    slope = pan.slope
                    y = self.t_dist_y
                    z = pan.z - self.t_dist_y * slope

                # a_right from axis cross z

                b_right = self.intersection_angle(
                    s_left,
                    w_left,
                    slope,
                    a_axis)

                a_right = b_right + a_offset

                b_left = self.intersection_angle(
                    s_right,
                    w_right,
                    slope,
                    a_axis)

                a_left = b_left + a_offset
                self.origin = Vector((0, y, z))
                g = self.get_generator()

                # override by user defined slope if any
                make_right = True
                make_left = True
                for s in g.segs:
                    if (s.constraint_type == 'SLOPE' and
                            s.v0_idx == 0):
                        da = g.segs[0].v_2d.angle_signed(s.v_2d)
                        if da > 0:
                            make_left = False
                        else:
                            make_right = False

                if make_left:
                    # Add 'SLOPE' constraints for segment 0
                    v = g.origin + Vector((cos(a_left), sin(a_left), 0))
                    s = StraightRoof(g.origin, p1=v)
                    s.v0_idx = 0
                    s.constraint_type = 'SLOPE'
                    # s.enforce_part = 'VALLEY'
                    s.angle_0 = a_left
                    s.take_precedence = False
                    g.segs.append(s)

                if make_right:
                    v = g.origin + Vector((cos(a_right), sin(a_right), 0))
                    s = StraightRoof(g.origin, p1=v)
                    s.v0_idx = 0
                    s.constraint_type = 'SLOPE'
                    # s.enforce_part = 'VALLEY'
                    s.angle_0 = a_right
                    s.take_precedence = False
                    g.segs.append(s)

        if g is None:
            self.origin = Vector((0, y, z))
            g = self.get_generator()

        # setup per segment manipulators
        if len(g.segs) > 0 and not self.schrinkwrap_target:
            f = g.segs[0]
            # z
            n = f.straight(-1, 0).v.to_3d()
            self.manipulators[0].set_pts([(0, 0, 0), (0, 0, self.z), (1, 0, 0)], normal=n)
            # left width
            n = f.normal(0, -self.width_left)
            self.manipulators[1].set_pts([n.p0.to_3d(), n.p1.to_3d(), (-1, 0, 0)])
            # right width
            n = f.normal(0, self.width_right)
            self.manipulators[2].set_pts([n.p0.to_3d(), n.p1.to_3d(), (1, 0, 0)])
            # Parts COUNTER
            f = g.segs[self.num_parts - 1]
            self.manipulators[3].set_pts([f.lerp(1.1),
                  f.lerp(1.1 + 0.5 / f.length),
                  (-1, 0, 0)
                  ])

        custom = self.get_scene_object(context, self.user_defined_roof)

        if custom is not None:
            g.pans = self.user_roof(o, custom)
        else:
            g.make_roof(self)

        if not self.schrinkwrap_target:
            g.locate_manipulators(self)

        # update childs here so parent may use
        # new holes when parent shape does change
        if update_childs:
            self.update_childs(context, o, g)

        # on t_child
        if d is not None and update_hole:
            hole_obj = self.find_hole(context, o)
            g.make_hole(context, hole_obj, o, self, update_parent)
            # print("make_hole")

        # add cutters
        g.boundary(o)

        if self.draft and not self.schrinkwrap_target:

            g.draft(verts, edges)

            self.make_surface(o, verts, edges)

        else:
            if not self.schrinkwrap_target:

                if self.bargeboard_enable:
                    g.bargeboard(self, verts, faces, edges, matids, uvs, vcolors)

                if self.fascia_enable:
                    g.fascia(self, verts, faces, edges, matids, uvs, vcolors)

                if self.beam_enable:
                    g.beam_primary(self, verts, faces, edges, matids, uvs, vcolors)

                if not throttle.is_active(o.name) and self.hip_enable:
                    g.hips(self, verts, faces, edges, matids, uvs, vcolors)

                if self.gutter_enable:
                    g.gutter(self, verts, faces, edges, matids, uvs, vcolors)

            bmed.buildmesh(o, verts, faces, matids, uvs, vcolors,
                weld=False, clean=False,  temporary=False)

            if self.schrinkwrap_target:
                g.lambris(o, self)

            else:
                if self.rafter_enable:
                    # bpy.ops.object.mode_set(mode='EDIT')
                    g.rafter(o, self)
                    # print("rafter")
                """
                if self.quick_edit and:
                    if self.tile_enable:
                        bpy.ops.archipack.roof_throttle_update(name=o.name)
                else:
                """
                if not throttle.is_active(o.name):
                    # throttle here
                    if self.tile_enable:

                        g.couverture(o, self)

            self.shade_smooth(context, o, 0.558505)

        if not self.schrinkwrap_target:

            target = self.find_shrinkwrap(o)
            if self.draft:
                if target is not None:
                    self.delete_object(context, target)
            else:
                self.create_shrinkwrap(context, o, target)

        # enable manipulators rebuild
        if manipulable_refresh and not self.schrinkwrap_target:
            self.manipulable_refresh = True
        # print("rafter")
        # restore context
        if not self.schrinkwrap_target and not self.draft:

            ref = self.get_reference_point(o)
            if ref is not None:
                for c in ref.children:
                    cd = c.data
                    if cd and "archipack_wall2" in cd and cd.archipack_wall2[0].fit_roof:
                        with ensure_select_and_restore(context, c, [c]):
                            cd.archipack_wall2[0].update(context)

        self.restore_context(context)

    def find_hole(self, context, o):
        p, d = self.find_parent(context)
        if d is not None:
            for child in p.children:
                cd = archipack_roof_cutter.datablock(child)
                if cd is not None and cd.boundary == o.name:
                    return child
        return None

    def manipulable_setup(self, context, o):
        """
            NOTE:
            this one assume context.active_object is the instance this
            data belongs to, failing to do so will result in wrong
            manipulators set on active object
        """
        if self.schrinkwrap_target:
            return

        self.setup_manipulators()

        if self.get_scene_object(context, self.user_defined_roof) is None:
            for i, part in enumerate(self.parts):

                if i > 0:
                    # start angle
                    self.manip_stack.append(part.manipulators[0].setup(context, o, part))

                if part.constraint_type == 'HORIZONTAL':
                    # length / radius + angle
                    self.manip_stack.append(part.manipulators[1].setup(context, o, part))

                # index
                self.manip_stack.append(part.manipulators[2].setup(context, o, self))

                # size left
                if part.auto_left in {'WIDTH', 'ALL'}:
                    self.manip_stack.append(part.manipulators[3].setup(context, o, part))
                # size right
                if part.auto_right in {'WIDTH', 'ALL'}:
                    self.manip_stack.append(part.manipulators[4].setup(context, o, part))
                # slope left
                if part.auto_left in {'SLOPE', 'ALL'}:
                    self.manip_stack.append(part.manipulators[5].setup(context, o, part))
                # slope right
                if part.auto_right in {'SLOPE', 'ALL'}:
                    self.manip_stack.append(part.manipulators[6].setup(context, o, part))

            for m in self.manipulators:
                self.manip_stack.append(m.setup(context, o, self))
        else:
            self.manip_stack.append(self.manipulators[0].setup(context, o, self))

    def draw(self, context, layout):
        box = layout.box()
        self.draw_prop(context, layout, box, self, 'n_parts')
        for i, part in enumerate(self.parts):
            part.draw(context, layout, i)


def update_hole(self, context):
    # update parent's roof only when manipulated
    self.update(context, update_parent=True)


def update_operation(self, context):
    o = self.find_in_selection(context, self.auto_update)
    if o is None:
        return
    g = self.get_generator()
    if g.is_cw != (self.operation == 'INTERSECTION'):
        return
    self.reverse(context, o)


class archipack_roof_cutter_segment(ArchipackCutterPart, PropertyGroup):
    side_type: EnumProperty(
        name="Type",
        items=(
            ('SIDE', 'Side', 'Side with bargeboard', 0),
            ('BOTTOM', 'Bottom', 'Bottom with gutter', 1),
            ('LINK', 'Side link', 'Side witout decoration', 2),
            ('AXIS', 'Top', 'Top part with hip and beam', 3)
        ),
        default='SIDE',
        update=update_hole
    )
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_roof_cutter[0]

    def draw(self, context, layout, index, draw_type=True, closed=False):
        icon = "TRIA_RIGHT"
        if self.expand:
            icon = "TRIA_DOWN"
        box = layout.box()
        row = box.row(align=True)
        self.draw_prop(context, layout, row, self, 'expand', icon=icon, emboss=True,
                       text="Seg", postfix=str(index + 1))
        self.draw_prop(context, layout, row, self, "side_type", text="")
        if self.expand:
            self.draw_insert(context, layout, index, closed=True)
            self.draw_prop(context, layout, box, self, "length")
            self.draw_prop(context, layout, box, self, "a0")


class archipack_roof_cutter(ArchipackObject, ArchipackCutter, Manipulable, DimensionProvider, PropertyGroup):
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
    parts: CollectionProperty(type=archipack_roof_cutter_segment)
    boundary: StringProperty(
        default="",
        name="Boundary",
        description="Boundary of t child to cut parent"
    )

    def from_points(self, o, pts):
        g = Generator()
        g.from_points(pts, False, True, False, True)
        self.n_parts = len(g.segs)
        # self.update_parts()
        g.update_parts(self)

    def update_points(self, context, o, pts, update_parent=False):
        """
            Create boundary from roof
        """
        self.auto_update = False
        self.manipulable_disable(o)
        self.from_points(o, pts)
        self.manipulable_refresh = True
        self.auto_update = True
        if update_parent:
            self.update_parent(context, o)

    def update_parent(self, context, o):
        if o is not None:
            d = archipack_roof.datablock(o.parent)
            if d is not None:
                cutables = [o.parent]
                self.filter_cutables(context, o, cutables)
                for c in cutables:
                    with ensure_select_and_restore(context, c, [c]) as (ctx, act, sel):
                        d.update(ctx, update_childs=False, update_hole=False)
                self.store_cutables(o, cutables)


class archipack_roof_draft_segment(ArchipackCutterPart, PropertyGroup):
    side_type: EnumProperty(
        name="Type",
        items=(
            ('SIDE', 'Side', 'Side with bargeboard', 0),
            ('BOTTOM', 'Bottom', 'Bottom with gutter', 1),
            ('LINK', 'Side link', 'Side witout decoration', 2),
            ('AXIS', 'Top', 'Top part with hip and beam', 3)
        ),
        default='SIDE',
        update=update
    )
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_roof_draft[0]

    def draw(self, context, layout, index, draw_type=True, closed=False):
        icon = "TRIA_RIGHT"
        if self.expand:
            icon = "TRIA_DOWN"
        box = layout.box()
        row = box.row(align=True)
        self.draw_prop(context, layout, row, self, 'expand', icon=icon, emboss=True,
                       text="Seg", postfix=str(index + 1))

        if self.expand:
            self.draw_insert(context, layout, index, closed=True)
            self.draw_prop(context, layout, box, self, "length")
            self.draw_prop(context, layout, box, self, "a0")


class archipack_roof_draft(ArchipackObject, ArchipackCutter, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display draft settings', 'NONE', 0),
            ('PARTS', 'Shape', 'Display draft segments settings', 'NONE', 1),
            ('TOOLS', '', 'Display tools', 'MODIFIER', 2)
        ),
        default='MAIN',
    )
    parts: CollectionProperty(type=archipack_roof_draft_segment)
    slope: FloatProperty(
        name="Slope",
        default=0.5, precision=5, step=1
    )
    slope_percent: FloatProperty(
        options={'SKIP_SAVE'},
        subtype="PERCENTAGE",
        precision=2,
        name="Slope",
        min=0.0,
        max=560,
        get=slope_getter('slope'),
        set=slope_setter('slope'),
        update=update_hole
    )
    slope_angle: FloatProperty(
        options={'SKIP_SAVE'},
        subtype='ANGLE', unit='ROTATION',
        precision=5,
        name="Slope",
        min=0.0,
        max=1.4835298641951802,
        get=slope_angle_getter('slope'),
        set=slope_angle_setter('slope'),
        update=update_hole
    )
    altitude: FloatProperty(
        name="Altitude",
        default=0,
        update=update_hole
    )

    def draw(self, context, layout, draw_offset=False, draw_type=False):
        self.draw_prop(context, layout, layout, self, "tabs", expand=True)
        icons = icon_man["main"]
        box = layout.box()
        if self.tabs == 'MAIN':
            self.draw_prop(context, layout, box, self, 'altitude')
            self.draw_prop(context, layout, box, self, 'slope_percent')
            self.draw_prop(context, layout, box, self, 'slope_angle')
            self.draw_prop(context, layout, box, self, 'offset')
            box.prop(context.object, "display_type")

        elif self.tabs == 'PARTS':
            self.template_user_path(context, box, focus=False)
            self.template_parts(context, layout, draw_type=False)

        elif self.tabs == 'TOOLS':
            self.draw_op(context, layout, box, "archipack.roof_preset_draft",
                         text="Roof",
                         icon_value=icons["roof"].icon_id
                         ).preset_operator = "archipack.roof_from_draft"

    def ensure_direction(self, o=None):
        """ Override because operation based direction here doesnt make any sense
        :param o:
        :return:
        """
        g = self.get_generator(o)
        return g

    def from_spline(self, context, o, curve, ccw=False, cw=False):
        ArchipackUserDefinedPath.from_spline(self, context, o, curve, cw=True)

    def from_points(self, o, pts):
        g = Generator()
        g.from_points(pts, False, True, False, True)
        self.n_parts = len(g.segs)
        # self.update_parts()
        g.update_parts(self)

    def get_generator(self, o=None):
        g = CutterGenerator(o)
        g.operation = 'DIFFERENCE'
        g.add_parts(self)
        g.line = g.make_offset(-self.offset)
        return g

    def update_points(self, context, o, pts, update_parent=False):
        """
            Create boundary from roof
        """
        self.auto_update = False
        self.manipulable_disable(o)
        self.from_points(o, pts)
        self.manipulable_refresh = True
        self.auto_update = True

    def update_parent(self, context, o):
        ref = self.get_reference_point(o)
        if ref is not None:
            for c in ref.children:
                d = archipack_roof.datablock(c)
                if d and d.user_defined_roof == o.name and not d.schrinkwrap_target:
                    with ensure_select_and_restore(context, c, [c]):
                        d.update(context)
        return

    def update(self, context, manipulable_refresh=False, update_parent=True):

        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        # clean up manipulators before any data model change
        if self.manipulable_refresh:
            self.manipulable_disable(o)

        # changed = self.update_parts()
        g = self.get_generator()
        g.locate_manipulators(self)

        pts = []
        g.line.get_pts(pts)
        polygon = [p.to_2d() for p in pts]
        coordsys = CoordSys([o], itM=g.itM)
        tree = Q_tree(coordsys, max_items=8, max_depth=8)

        res = polyskel.skeletonize(polygon, [])
        slope = self.slope
        z = self.altitude - slope * max([t.height for t in res])

        pts = []
        for t in res:
            sub = []
            sub.append(Vector((t.source.x, t.source.y, z + slope * t.height)))
            sub.extend([Vector((p.x, p.y, z)) for p in t.sinks])
            pts.append(sub)

        edges = []
        verts = []
        # g.line.get_verts(verts, edges)

        i = 0  #len(verts)
        for sub in pts:
            p0 = sub.pop(0)
            tree.new_point_maxz(p0, 0.01)
            i0 = i
            verts.append(p0)
            for p1 in sub:
                i += 1
                edges.append([i0, i])
                verts.append(p1)
                tree.new_point_maxz(p1, 0.01)

            i += 1

        boundary = []
        g.line.get_pts(boundary)
        for p in boundary:
            p.z = z
            verts.append(p)

        for p in range(g.numsegs - 1):
            edges.append([i, i + 1])
            i += 1
        edges.append([i, i + 1 - g.numsegs])

        import bmesh
        bm = bmesh.new()
        for v in verts:
            bm.verts.new(v)

        bm.verts.ensure_lookup_table()
        for i, j in edges:
            bm.edges.new((bm.verts[i], bm.verts[j]))

        bm.edges.ensure_lookup_table()
        bmesh.ops.remove_doubles(bm, verts=bm.verts, dist=0.01)
        bmesh.ops.contextual_create(bm, geom=bm.edges)
        bmesh.ops.recalc_face_normals(bm, faces=bm.faces)

        faces = [f for f in bm.faces if f.normal.z < 0.01]
        bmesh.ops.delete(bm, geom=faces, context="FACES")

        bm.to_mesh(o.data)
        bm.free()

        if self.manipulable_refresh or update_parent:
            self.update_parent(context, o)

        self.restore_context(context)


class ARCHIPACK_PT_roof_draft(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_roof_draft"
    bl_label = "Roof draft"

    @classmethod
    def poll(cls, context):
        return archipack_roof_draft.poll(context.active_object)

    def draw(self, context):
        d = archipack_roof_draft.datablock(context.active_object)
        if d is None:
            return
        layout = self.layout

        self.draw_common(context, layout)
        d.draw(context, layout)


class ARCHIPACK_PT_roof_cutter(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_roof_cutter"
    bl_label = "Roof Cutter"

    @classmethod
    def poll(cls, context):
        return archipack_roof_cutter.poll(context.active_object)

    def draw(self, context):
        d = archipack_roof_cutter.datablock(context.active_object)
        if d is None:
            return
        layout = self.layout

        if d.boundary != "":
            box = layout.box()
            self.draw_label(context, layout, box, "Auto Cutter:")
            self.draw_label(context, layout, box, d.boundary)
        else:

            self.draw_common(context, layout)

            d.draw(context, layout)


class ARCHIPACK_PT_roof(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_roof"
    bl_label = "Roof"

    @classmethod
    def poll(cls, context):
        return archipack_roof.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_roof.datablock(o)
        if d is None:
            return
        scene = context.scene
        layout = self.layout

        if d.schrinkwrap_target:
            self.draw_op(context, layout, layout, "archipack.select_parent", icon="RESTRICT_SELECT_OFF")
            return


        self.draw_common(context, layout)

        box = layout.box()
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.roof_preset_menu",
                     text=bpy.types.ARCHIPACK_OT_roof_preset_menu.bl_label, icon="PRESET")
        self.draw_op(context, layout, row, "archipack.roof_preset", icon='ADD', text="")
        self.draw_op(context, layout, row, "archipack.roof_preset", icon='REMOVE', text="").remove_active = True

        self.draw_op(context, layout, layout, 'archipack.roof_cutter', icon="MOD_BOOLEAN").parent = o.name
        self.draw_prop(context, layout, layout, d, "tabs", expand=True)

        is_user = d.user_defined_roof != ""

        if d.tabs == 'MAIN':
            box = layout.box()
            self.draw_label(context, layout, box, "User defined")
            row = box.row(align=True)
            if is_user:
                op = self.draw_op(context, layout, row, "archipack.object_edit", text="", icon="EDITMODE_HLT")
                op.focus = False
                op.object_name = d.user_defined_roof
                op.update_func_name = "update_childrens"
                self.draw_op(context, layout, row,
                             "archipack.select",
                             text="",
                             icon="RESTRICT_SELECT_OFF").name = d.user_defined_roof

            row.prop_search(d, "user_defined_roof", scene, "objects", text="", icon='OBJECT_DATA')

            if is_user:
                op = self.draw_op(context, layout, row, "archipack.object_update", text="", icon="FILE_REFRESH")
                op.update_func_name = "update_childrens"

            p, _d = d.find_parent(context)

            if not is_user:
                box = layout.box()
                box.prop_search(d, "t_parent", scene, "objects", text="Parent", icon='OBJECT_DATA')
                if _d is not None:
                    self.draw_prop(context, layout, box, d, 't_part_ui')
                    self.draw_prop(context, layout, box, d, 't_dist_x')
                    self.draw_prop(context, layout, box, d, 't_dist_y')
                    self.draw_label(context, layout, box, "Hole")
                    self.draw_prop(context, layout, box, d, 'hole_offset_front')
                    self.draw_prop(context, layout, box, d, 'hole_offset_left')
                    self.draw_prop(context, layout, box, d, 'hole_offset_right')

            box = layout.box()
            # self.draw_prop(context, layout, box, d, 'quick_edit')
            self.draw_prop(context, layout, box, d, 'draft', icon="MOD_MULTIRES")
            if _d is None:
                self.draw_prop(context, layout, box, d, 'z_ui')

            if not is_user:
                self.draw_prop(context, layout, box, d, 'enable_left')
                if d.enable_left:
                    self.draw_prop(context, layout, box, d, 'slope_left_angle')
                    self.draw_prop(context, layout, box, d, 'slope_left_percent')
                    self.draw_prop(context, layout, box, d, 'width_left')

                self.draw_prop(context, layout, box, d, 'enable_right')
                if d.enable_right:
                    self.draw_prop(context, layout, box, d, 'slope_right_angle')
                    self.draw_prop(context, layout, box, d, 'slope_right_percent')
                    self.draw_prop(context, layout, box, d, 'width_right')

        # parts
        elif d.tabs == 'PARTS':
            if not is_user:
                d.draw(context, layout)

        elif d.tabs == 'SUB':
            # tiles
            box = layout.box()
            row = box.row(align=True)

            icon = "TRIA_RIGHT"
            if d.tile_expand:
                icon="TRIA_DOWN"
            self.draw_prop(context, layout, row, d, 'tile_expand', icon=icon, text="Covering")
            self.draw_prop(context, layout, row, d, 'tile_enable')
            if d.tile_expand:
                self.draw_prop(context, layout, box, d, 'tile_model', text="")

                self.draw_prop(context, layout, box, d, 'tile_solidify', icon='MOD_SOLIDIFY')
                if d.tile_solidify:
                    self.draw_prop(context, layout, box, d, 'tile_height')
                    box.separator()
                self.draw_prop(context, layout, box, d, 'tile_bevel', icon='MOD_BEVEL')
                if d.tile_bevel:
                    self.draw_prop(context, layout, box, d, 'tile_bevel_amt')
                    self.draw_prop(context, layout, box, d, 'tile_bevel_segs')
                    box.separator()
                self.draw_label(context, layout, box, "Tile size")
                self.draw_prop(context, layout, box, d, 'tile_size_x')
                self.draw_prop(context, layout, box, d, 'tile_size_y')
                self.draw_prop(context, layout, box, d, 'tile_size_z')
                self.draw_prop(context, layout, box, d, 'tile_altitude')

                self.draw_label(context, layout, box, "Random")
                self.draw_prop(context, layout, box, d, 'tile_random_z')
                self.draw_prop(context, layout, box, d, 'tile_random_angle_z')
                self.draw_prop(context, layout, box, d, 'tile_random_angle_y')
                self.draw_prop(context, layout, box, d, 'tile_random_scale')

                box.separator()
                self.draw_label(context, layout, box, "Distribution")
                self.draw_prop(context, layout, box, d, 'tile_alternate', icon='NLA')
                row = box.row(align=True)
                self.draw_prop(context, layout, row, d, 'tile_fit_x')  #, icon='ALIGN')
                self.draw_prop(context, layout, row, d, 'tile_fit_y')  #, icon='ALIGN')
                self.draw_prop(context, layout, box, d, 'tile_offset')

                self.draw_label(context, layout, box, "Spacing")
                self.draw_prop(context, layout, box, d, 'tile_space_x')
                self.draw_prop(context, layout, box, d, 'tile_space_y')

                box.separator()     # hip
                self.draw_label(context, layout, box, "Border")
                self.draw_prop(context, layout, box, d, 'tile_side')
                self.draw_prop(context, layout, box, d, 'tile_couloir')
                self.draw_prop(context, layout, box, d, 'tile_border')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.hip_expand:
                icon = "TRIA_DOWN"
            self.draw_prop(context, layout, row, d, 'hip_expand', icon=icon, text="Hip")
            self.draw_prop(context, layout, row, d, 'hip_enable')
            if d.hip_expand:
                self.draw_prop(context, layout, box, d, 'hip_model', text="")
                self.draw_prop(context, layout, box, d, 'hip_size_x')
                self.draw_prop(context, layout, box, d, 'hip_size_y')
                self.draw_prop(context, layout, box, d, 'hip_size_z')
                self.draw_prop(context, layout, box, d, 'hip_alt')
                self.draw_prop(context, layout, box, d, 'hip_space_x')
                box.separator()
                self.draw_prop(context, layout, box, d, 'valley_enable')
                self.draw_prop(context, layout, box, d, 'valley_altitude')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.beam_expand:
                icon = "TRIA_DOWN"
            self.draw_prop(context, layout, row, d, 'beam_expand', icon=icon, text="Beam")
            if d.beam_expand:
                self.draw_prop(context, layout, box, d, 'beam_enable')
                if d.beam_enable:
                    self.draw_prop(context, layout, box, d, 'beam_width')
                    self.draw_prop(context, layout, box, d, 'beam_height')
                    self.draw_prop(context, layout, box, d, 'beam_offset')
                    self.draw_prop(context, layout, box, d, 'beam_alt')
                box.separator()
                self.draw_prop(context, layout, box, d, 'beam_sec_enable')
                if d.beam_sec_enable:
                    self.draw_prop(context, layout, box, d, 'beam_sec_width')
                    self.draw_prop(context, layout, box, d, 'beam_sec_height')
                    self.draw_prop(context, layout, box, d, 'beam_sec_alt')
                box.separator()
                self.draw_prop(context, layout, box, d, 'rafter_enable')
                if d.rafter_enable:
                    self.draw_prop(context, layout, box, d, 'rafter_height')
                    self.draw_prop(context, layout, box, d, 'rafter_width')
                    self.draw_prop(context, layout, box, d, 'rafter_spacing')
                    self.draw_prop(context, layout, box, d, 'rafter_start')
                    self.draw_prop(context, layout, box, d, 'rafter_alt')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.gutter_expand:
                icon = "TRIA_DOWN"
            self.draw_prop(context, layout, row, d, 'gutter_expand', icon=icon, text="Gutter")
            self.draw_prop(context, layout, row, d, 'gutter_enable')
            if d.gutter_expand:
                self.draw_prop(context, layout, box, d, 'gutter_alt')
                self.draw_prop(context, layout, box, d, 'gutter_width')
                self.draw_prop(context, layout, box, d, 'gutter_dist')
                self.draw_prop(context, layout, box, d, 'gutter_boudin')
                self.draw_prop(context, layout, box, d, 'gutter_segs')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.fascia_expand:
                icon = "TRIA_DOWN"
            self.draw_prop(context, layout, row, d, 'fascia_expand', icon=icon, text="Fascia")
            self.draw_prop(context, layout, row, d, 'fascia_enable')
            if d.fascia_expand:
                self.draw_prop(context, layout, box, d, 'fascia_altitude')
                self.draw_prop(context, layout, box, d, 'fascia_width')
                self.draw_prop(context, layout, box, d, 'fascia_height')
                self.draw_prop(context, layout, box, d, 'fascia_offset')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.bargeboard_expand:
                icon = "TRIA_DOWN"
            self.draw_prop(context, layout, row, d, 'bargeboard_expand', icon=icon, text="Bargeboard")
            self.draw_prop(context, layout, row, d, 'bargeboard_enable')
            if d.bargeboard_expand:
                self.draw_prop(context, layout, box, d, 'bargeboard_altitude')
                self.draw_prop(context, layout, box, d, 'bargeboard_width')
                self.draw_prop(context, layout, box, d, 'bargeboard_height')
                self.draw_prop(context, layout, box, d, 'bargeboard_offset')
            box = layout.box()
            self.draw_prop(context, layout, box, d, 'lambris_height')

        elif d.tabs == 'MATERIALS':
            box = layout.box()
            if "archipack_material" in o:
                o.archipack_material[0].draw(context, box)

            box = layout.box()
            self.draw_label(context, layout, box, "Apply materials")
            self.draw_prop(context, layout, box, d, 'idmat_covering')
            self.draw_prop(context, layout, box, d, 'idmat_hip')
            self.draw_prop(context, layout, box, d, 'idmat_ridge_pole')
            self.draw_prop(context, layout, box, d, 'idmat_rafter')
            self.draw_prop(context, layout, box, d, 'idmat_gutter')
            self.draw_prop(context, layout, box, d, 'idmat_fascia')
            self.draw_prop(context, layout, box, d, 'idmat_bargeboard')
            self.draw_prop(context, layout, box, d, 'idmat_lambris')


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


class ARCHIPACK_OT_roof(ArchipackCreateTool, Operator):
    bl_idname = "archipack.roof"
    bl_label = "Roof"
    bl_description = "Roof"
    user_defined_roof: StringProperty()

    def create(self, context):
        m = bpy.data.meshes.new("Roof")
        o = bpy.data.objects.new("Roof", m)
        d = m.archipack_roof.add()
        # make manipulators selectable
        d.manipulable_selectable = True
        d.user_defined_roof = self.user_defined_roof

        # disable quick edit when
        # background render thumbs
        if bpy.app.background:
            d.quick_edit = False

        # Link object into scene
        self.link_object_to_scene(context, o)
        o.color = (0, 1, 0, 1)

        # select and make active
        self.select_object(context, o, True)
        self.add_material(context, o)
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
            # select and make active
            self.add_to_reference(context, o)
            self.select_object(context, o, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_roof_cutter(ArchipackCreateTool, Operator):
    bl_idname = "archipack.roof_cutter"
    bl_label = "Roof Cutter"
    bl_description = "Roof Cutter"

    parent: StringProperty("")
    curve: StringProperty("")

    def create(self, context):
        m = bpy.data.meshes.new("Roof Cutter")
        o = bpy.data.objects.new("Roof Cutter", m)
        d = m.archipack_roof_cutter.add()

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
            d.set_parts(4)
            for i, p in enumerate(d.parts):
                p.a0 = angle_90
                p.length = x
            d.parts[0].a0 = - angle_90

            # This part differs from regular cutters
            pd = archipack_roof.datablock(parent)
            pd.boundary = o.name

        else:

            o.location = self.get_cursor_location(context)

        # make manipulators selectable
        d.manipulable_selectable = True
        # Link object into scene
        self.link_object_to_scene(context, o)

        # select and make active
        self.select_object(context, o, True)
        o.color = (1, 0, 0, 1)

        self.add_material(context, o)
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
            # select and make active
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_roof_draft(ArchipackCreateTool, Operator):
    bl_idname = "archipack.roof_draft"
    bl_label = "Roof Draft"
    bl_description = "Roof Draft"

    parent: StringProperty("")
    curve: StringProperty("")

    def create(self, context):
        m = bpy.data.meshes.new("Roof Draft")
        o = bpy.data.objects.new("Roof Draft", m)
        d = m.archipack_roof_draft.add()

        curve = self.get_scene_object(context, self.curve)
        d.manipulable_selectable = True
        x = 4
        angle_90 = pi / 2
        d.set_parts(4)
        for i, p in enumerate(d.parts):
            p.a0 = angle_90
            p.length = x
        d.parts[0].a0 = - angle_90

        o.location = self.get_cursor_location(context)

        # make manipulators selectable
        d.manipulable_selectable = True
        # Link object into scene
        self.link_object_to_scene(context, o, layer_name="Roofs")

        # select and make active
        self.select_object(context, o, True)
        o.display_type = 'WIRE'
        o.hide_render = True
        o.show_all_edges = True
        o.display.show_shadows = False
        o.color = (1, 0, 0, 1)

        # self.add_material(context, o)
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
            # select and make active
            self.add_to_reference(context, o)
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


class ARCHIPACK_OT_roof_from_curve(ArchipackCreateTool, Operator):
    bl_idname = "archipack.roof_from_curve"
    bl_label = "Roof curve"
    bl_description = "Create a roof from a curve"

    @classmethod
    def poll(self, context):
        return context.active_object is not None and context.active_object.type == 'CURVE'

    def create(self, context):
        curve = context.active_object
        m = bpy.data.meshes.new("Roof")
        o = bpy.data.objects.new("Roof", m)
        d = m.archipack_roof.add()

        # make manipulators selectable
        d.manipulable_selectable = True
        d.user_defined_path = curve.name
        # Link object into scene
        self.link_object_to_scene(context, o)
        o.color = (0, 1, 0, 1)

        self.add_material(context, o, category="roof")
        # select and make active
        self.select_object(context, o, True)
        d.update_path(context)

        spline = curve.data.splines[0]
        if spline.type == 'POLY':
            pt = spline.points[0].co
        elif spline.type == 'BEZIER':
            pt = spline.bezier_points[0].co
        else:
            pt = Vector((0, 0, 0))
        # pretranslate
        o.matrix_world = curve.matrix_world @ Matrix.Translation(pt)
        # select and make active

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


class ARCHIPACK_OT_roof_from_wall(Archipacki18n, ArchipackCreateTool, Operator):
    bl_idname = "archipack.roof_from_wall"
    bl_label = "Roof"
    bl_description = "Create a roof from a wall"

    roof_overflow: FloatProperty(
        unit='LENGTH', subtype='DISTANCE',
        precision=5,
        name="Overflow",
        default=1.0,
        min=0
    )
    use_small_as_axis: BoolProperty(
        name="Use small side as axis",
        default=False
    )
    cut_borders: BoolProperty(
        name="Cut borders",
        default=False
    )

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o and o.data is not None and "archipack_wall2" in o.data

    def draw(self, context):
        layout = self.layout
        layout.prop(self, 'roof_overflow', text_ctxt = self.translation_context)
        layout.prop(self, 'use_small_as_axis', text_ctxt = self.translation_context)
        layout.prop(self, 'cut_borders', text_ctxt = self.translation_context)

    def create(self, context, wall):

        wd = wall.data.archipack_wall2[0]
        try:
            io, exterior, childs = wd.as_geom(context, wall, 'SLAB', [], [], [])
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

        if self.cut_borders:
            buffer = exterior.buffer(self.roof_overflow,
                                    resolution=12,
                                    join_style=2,
                                    cap_style=3,
                                    mitre_limit=10 * self.roof_overflow,
                                    single_sided=False
                                    )
            tM, w, h, poly, w_pts = ShapelyOps.min_bounding_rect(buffer)
        else:
            tM, w, h, poly, w_pts = ShapelyOps.min_bounding_rect(exterior)

        # compute height from w / h
        if self.use_small_as_axis:
            height = wd.z + 0.25 * max(w, h)
            h, w = w, h
            rM = Matrix([
                [0, -1, 0, 0],
                [1, 0, 0, 0],
                [0, 0, 1, 0],
                [0, 0, 0, 1],
                ])
        else:
            height = wd.z + 0.25 * min(w, h)
            rM = Matrix()

        bpy.ops.archipack.roof('INVOKE_DEFAULT', filepath=self.filepath)

        o = context.active_object
        # o.archipack_material[0].material = "DEFAULT"

        # with ensure_select_and_restore(context, wall, [o]):
        #    bpy.ops.archipack.add_reference_point()

        # select and make active
        z = wall.matrix_world.translation.z
        o.matrix_world = io.coordsys.world @ tM @ rM @ Matrix.Translation(
            Vector((-(self.roof_overflow + 0.5 * w), 0, z)))

        d = o.data.archipack_roof[0]
        d.auto_update = False
        d.z = height
        d.width_left = self.roof_overflow + (h / 2)
        d.width_right = self.roof_overflow + (h / 2)
        d.parts[0].length = w + 2 * self.roof_overflow

        if self.cut_borders:
            # output geom as curve
            result = Io.to_curve(context.scene, io.coordsys, buffer, 'buffer')
            bpy.ops.archipack.roof_cutter(parent=o.name)
            cutter = context.active_object
            cutter.data.archipack_roof_cutter[0].operation = 'INTERSECTION'
            cutter.data.archipack_roof_cutter[0].user_defined_path = result.name
            self.delete_object(context, result)

        # select and make active
        self.select_object(context, o, True)

        # Is this realy needed ?
        # context.view_layer.update()

        d.auto_update = True

        with ensure_select_and_restore(context, wall, [wall]):
            wall.data.archipack_wall2[0].fit_roof = True

        # not certain it is desirable
        """
        if not self.schrinkwrap_target:
            ref = self.get_reference_point(o)
            if ref is not None:
                for c in ref.children:
                    cd = c.data
                    if cd and "archipack_wall2" in cd:
                        with ensure_select_and_restore(context, c, [c]):
                            c.data.archipack_wall2[0].fit_roof = True
        """
        return o

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            wall = context.active_object
            bpy.ops.object.select_all(action="DESELECT")
            with stop_auto_manipulate(context):
                o = self.create(context, wall)
            # select and make active
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_roof_from_draft(Archipacki18n, ArchipackCreateTool, Operator):
    bl_idname = "archipack.roof_from_draft"
    bl_label = "Roof"
    bl_description = "Create a roof from a draft"

    @classmethod
    def poll(self, context):
        o = context.active_object
        return archipack_roof_draft.filter(o)

    def create(self, context, draft):

        bpy.ops.archipack.roof('INVOKE_DEFAULT', filepath=self.filepath, user_defined_roof=draft.name)
        o = context.active_object
        # select and make active
        self.select_object(context, o, True)
        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            draft = context.active_object
            bpy.ops.object.select_all(action="DESELECT")
            with stop_auto_manipulate(context):
                o = self.create(context, draft)
            # select and make active
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


# ------------------------------------------------------------------
# Define operator class to load / save presets
# ------------------------------------------------------------------


class ARCHIPACK_OT_roof_preset_create(PresetMenuOperator, Operator):
    bl_description = "Show Roof presets and create object at cursor location"
    bl_idname = "archipack.roof_preset_create"
    bl_label = "Roof Styles"
    preset_subdir = "archipack_roof"


class ARCHIPACK_OT_roof_preset_draft(PresetMenuOperator, Operator):
    bl_description = "Show Roof presets and create object from draft"
    bl_idname = "archipack.roof_preset_draft"
    bl_label = "Roof Styles"
    preset_subdir = "archipack_roof"


class ARCHIPACK_OT_roof_preset_menu(PresetMenuOperator, Operator):
    bl_description = "Show Roof presets"
    bl_idname = "archipack.roof_preset_menu"
    bl_label = "Roof Styles"
    preset_subdir = "archipack_roof"


class ARCHIPACK_OT_roof_preset(ArchipackPreset, Operator):
    bl_description = "Add / remove a Roof Preset"
    bl_idname = "archipack.roof_preset"
    bl_label = "Add Roof Style"
    preset_menu = "ARCHIPACK_OT_roof_preset_menu"

    @property
    def blacklist(self):
        return ['draft', 'z', 'slope_left', 'slope_right', 'width_left', 'width_right', 'parts', 'n_parts',
                't_parent', 't_part', 't_dist_x', 't_dist_y',
                'hole_offset_front', 'hole_offset_left', 'hole_offset_right', 'user_defined_roof']


def register():
    # bpy.utils.register_class(archipack_roof_material)
    bpy.utils.register_class(archipack_roof_cutter_segment)
    bpy.utils.register_class(archipack_roof_cutter)
    bpy.utils.register_class(ARCHIPACK_PT_roof_cutter)
    bpy.utils.register_class(ARCHIPACK_OT_roof_cutter)
    Mesh.archipack_roof_cutter = CollectionProperty(type=archipack_roof_cutter)
    bpy.utils.register_class(archipack_roof_draft_segment)
    bpy.utils.register_class(archipack_roof_draft)
    bpy.utils.register_class(ARCHIPACK_PT_roof_draft)
    bpy.utils.register_class(ARCHIPACK_OT_roof_draft)
    Mesh.archipack_roof_draft = CollectionProperty(type=archipack_roof_draft)

    bpy.utils.register_class(archipack_roof_segment)
    bpy.utils.register_class(archipack_roof)
    Mesh.archipack_roof = CollectionProperty(type=archipack_roof)
    bpy.utils.register_class(ARCHIPACK_OT_roof_preset_menu)
    bpy.utils.register_class(ARCHIPACK_OT_roof_preset_create)
    bpy.utils.register_class(ARCHIPACK_OT_roof_preset_draft)

    bpy.utils.register_class(ARCHIPACK_PT_roof)
    bpy.utils.register_class(ARCHIPACK_OT_roof)
    bpy.utils.register_class(ARCHIPACK_OT_roof_preset)
    bpy.utils.register_class(ARCHIPACK_OT_roof_from_curve)
    bpy.utils.register_class(ARCHIPACK_OT_roof_from_wall)
    bpy.utils.register_class(ARCHIPACK_OT_roof_from_draft)


def unregister():
    # bpy.utils.unregister_class(archipack_roof_material)
    bpy.utils.unregister_class(archipack_roof_cutter_segment)
    bpy.utils.unregister_class(archipack_roof_cutter)
    bpy.utils.unregister_class(ARCHIPACK_PT_roof_cutter)
    bpy.utils.unregister_class(ARCHIPACK_OT_roof_cutter)
    del Mesh.archipack_roof_cutter
    bpy.utils.unregister_class(archipack_roof_draft_segment)
    bpy.utils.unregister_class(archipack_roof_draft)
    bpy.utils.unregister_class(ARCHIPACK_PT_roof_draft)
    bpy.utils.unregister_class(ARCHIPACK_OT_roof_draft)
    del Mesh.archipack_roof_draft

    bpy.utils.unregister_class(archipack_roof_segment)
    bpy.utils.unregister_class(archipack_roof)
    del Mesh.archipack_roof
    bpy.utils.unregister_class(ARCHIPACK_OT_roof_preset_menu)
    bpy.utils.unregister_class(ARCHIPACK_OT_roof_preset_create)
    bpy.utils.unregister_class(ARCHIPACK_OT_roof_preset_draft)
    bpy.utils.unregister_class(ARCHIPACK_PT_roof)
    bpy.utils.unregister_class(ARCHIPACK_OT_roof)
    bpy.utils.unregister_class(ARCHIPACK_OT_roof_preset)
    bpy.utils.unregister_class(ARCHIPACK_OT_roof_from_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_roof_from_wall)
    bpy.utils.unregister_class(ARCHIPACK_OT_roof_from_draft)
