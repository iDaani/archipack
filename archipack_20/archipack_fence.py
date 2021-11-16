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
    FloatProperty, BoolProperty, IntProperty, CollectionProperty,
    StringProperty, EnumProperty, FloatVectorProperty, IntVectorProperty
    )
from .bmesh_utils import BmeshEdit as bmed
from .panel import Panel as Lofter
from mathutils import Vector, Matrix
from math import sin, cos, pi, acos
from random import uniform
from .archipack_snap import snap_point
from .archipack_manipulator import (
    Manipulable, archipack_manipulator,
    GlPolygon, GlPolyline,
    GlLine, GlText, FeedbackPanel
    )
from .archipack_generator import Generator, Line, Arc
from .archipack_preset import ArchipackPreset, PresetMenuOperator
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackCreateTool,
    ArchipackObject,
    ArchipackPanel,
    ArchipackDrawTool
)
from .archipack_dimension import DimensionProvider
from .archipack_curveman import ArchipackProfile, ArchipackUserDefinedPath
from .archipack_segments2 import ArchipackSegment
from .archipack_material import build_mat_enum
from .archipack_prefs import get_prefs
from .archipack_keymaps import Keymaps

import logging
logger = logging.getLogger("archipack")


class Fence:

    def __init__(self):
        # total distance from start
        self.line = None
        self.dist = 0
        self.t_start = 0
        self.t_end = 0

    @property
    def t_diff(self):
        return self.t_end - self.t_start


class StraightSegment(Fence, Line):
    def __str__(self):
        return "t_start:{} t_end:{} dist:{}".format(self.t_start, self.t_end, self.dist)

    def __init__(self, p, last=None):
        Fence.__init__(self)
        Line.__init__(self, p, last=last)


class CurvedSegment(Fence, Arc):
    def __str__(self):
        return "t_start:{} t_end:{} dist:{}".format(self.t_start, self.t_end, self.dist)

    def __init__(self, p, radius, da, last=None):
        Fence.__init__(self)
        Arc.__init__(self, p, radius, da, last=last)


class FenceSegment:
    def __str__(self):
        return "t_start:{} t_end:{} n_step:{}  t_step:{} i_start:{} i_end:{}".format(
            self.t_start, self.t_end, self.n_step, self.t_step, self.i_start, self.i_end)

    def __init__(self, t_start, t_end, n_step, t_step, i_start, i_end):
        self.t_start = t_start
        self.t_end = t_end
        self.n_step = n_step
        self.t_step = t_step
        self.i_start = i_start
        self.i_end = i_end


class FenceGenerator(Generator):

    def __init__(self, o=None):
        Generator.__init__(self, o)
        self.length = 0
        self.user_defined_post = None
        self.user_defined_uvs = None
        self.user_defined_mat = None

    @property
    def random_color(self):
        return Vector((uniform(0.0, 1.0), uniform(0.0, 1.0), uniform(0.0, 1.0), 1))

    @property
    def n_parts(self):
        return len(self.parts)

    def init_sides(self, d):
        self.line = self.make_offset(d.x_offset)

    def create_segment(self, d, part, co: Vector, last, last_type: str, p1=None, next=None):
        # start a new fence
        if part.type == 0:
            _k = StraightSegment(co, last=last)
        else:
            _k = CurvedSegment(co, part.radius, part.da, last=last)

        return _k

    def param_t(self, d, angle_limit, post_spacing):
        """
            setup corners and fences dz
            compute index of fences wich belong to each group of fences between corners
            compute t of each fence
        """


        # segments are group of parts separated by limit angle
        self.segments = []
        i_start = 0
        t_start = 0
        dist_0 = 0
        z = 0
        self.length = 0

        _segs = self.segs
        _line = self.line.segs

        n_parts = self.numsegs

        i = 0
        for f, line in zip(_segs, _line):
            f.dist = self.length
            if i < n_parts:
                self.length += line.length
            i += 1

        vz0 = Vector((1, 0))
        angle_z = 0
        i = 0
        for part, f, line in zip(d.parts, _segs, _line):
            dz = part.dz
            if f.dist > 0:
                f.t_start = f.dist / self.length
            else:
                f.t_start = 0

            f.t_end = (f.dist + line.length) / self.length
            f._z = z
            # f.dz = dz
            z += dz

            if i < n_parts - 1:

                vz1 = Vector((self.segs[i + 1].length, d.parts[i + 1].dz))
                angle_z = abs(vz0.angle_signed(vz1))
                vz0 = vz1

                if (abs(d.parts[i + 1].a0) >= angle_limit or angle_z >= angle_limit):
                    l_seg = f.dist + line.length - dist_0
                    t_seg = f.t_end - t_start
                    n_fences = max(1, int(l_seg / post_spacing))
                    t_fence = t_seg / n_fences
                    segment = FenceSegment(t_start, f.t_end, n_fences, t_fence, i_start, i)
                    dist_0 = f.dist + line.length
                    t_start = f.t_end
                    i_start = i
                    self.segments.append(segment)
            i += 1

        f = _segs[n_parts - 1]
        l_seg = f.dist + _line[n_parts - 1].length - dist_0
        t_seg = f.t_end - t_start
        n_fences = max(1, int(l_seg / post_spacing))
        t_fence = t_seg / n_fences
        segment = FenceSegment(t_start, f.t_end, n_fences, t_fence, i_start, i)
        self.segments.append(segment)

    def setup_user_defined_post(self, o, post_x, post_y, post_z, post_rotation, use_matid, matid):
        self.user_defined_post = o
        x = o.bound_box[6][0] - o.bound_box[0][0]
        y = o.bound_box[6][1] - o.bound_box[0][1]
        z = o.bound_box[6][2] - o.bound_box[0][2]
        # Prevent 0 division error on objects with single vertex
        if x != 0:
            x = post_x / x
        if y != 0:
            y = post_y / y
        if z != 0:
            z = post_z / z
        self.user_defined_post_scale = Vector((x, -y, z))
        m = o.data
        # create vertex group lookup dictionary for names
        vgroup_names = {vgroup.index: vgroup.name for vgroup in o.vertex_groups}
        # create dictionary of vertex group assignments per vertex
        self.vertex_groups = [[vgroup_names[g.group] for g in v.groups] for v in m.vertices]
        # uvs
        uv_act = m.uv_layers.active
        if uv_act is not None:
            uv_layer = uv_act.data
            self.user_defined_uvs = [[uv_layer[li].uv for li in p.loop_indices] for p in m.polygons]
        else:
            self.user_defined_uvs = [[(0, 0) for i in p.vertices] for p in m.polygons]

        # material ids
        if use_matid:
            self.user_defined_mat = [p.material_index for p in m.polygons]
        else:
            self.user_defined_mat = [matid for p in m.polygons]

        ca = cos(post_rotation)
        sa = sin(post_rotation)
        self.user_rM = Matrix([
            [ca, -sa, 0, 0],
            [sa, ca, 0, 0],
            [0, 0, 1, 0],
            [0, 0, 0, 1]
        ])

    def get_user_defined_post(self, tM, z0, z1, z2, slope, post_z, verts, faces, matids, uvs, vcolors):
        f = len(verts)
        m = self.user_defined_post.data

        for i, g in enumerate(self.vertex_groups):
            co = m.vertices[i].co.copy()
            co.x *= self.user_defined_post_scale.x
            co.y *= self.user_defined_post_scale.y
            co.z *= self.user_defined_post_scale.z
            co = self.user_rM @ co
            if 'Slope' in g:
                co.z += co.y * slope
            verts.append(tM @ co)
        color = self.random_color
        matids.extend(self.user_defined_mat)
        faces.extend([tuple([i + f for i in p.vertices]) for p in m.polygons])
        vcolors.extend([color] * len(m.polygons))
        uvs.extend(self.user_defined_uvs)

    def get_post(self, post, post_x, post_y, post_z, post_alt, sub_offset_x,
            id_mat, verts, faces, matids, uvs, vcolors):

        n, dz, zl = post
        slope = dz * post_y

        if self.user_defined_post is not None:
            x, y, z = -n.v.normalized()
            p = n.p0+ sub_offset_x * n.v.normalized()
            tM = Matrix([
                [x, y, 0, p.x],
                [y, -x, 0, p.y],
                [0, 0, 1, zl + post_alt],
                [0, 0, 0, 1]
            ])
            self.get_user_defined_post(tM, zl, 0, 0, dz, post_z, verts, faces, matids, uvs, vcolors)
            return

        z3 = zl + post_z + post_alt - slope
        z4 = zl + post_z + post_alt + slope
        z0 = zl + post_alt - slope
        z1 = zl + post_alt + slope
        vn = n.v.normalized()
        dx = post_x * vn
        dy = post_y * Vector((vn.y, -vn.x, 0))
        oy = sub_offset_x * vn
        x0, y0, z = n.p0 - dx + dy + oy
        x1, y1, z = n.p0 - dx - dy + oy
        x2, y2, z = n.p0 + dx - dy + oy
        x3, y3, z = n.p0 + dx + dy + oy
        f = len(verts)
        verts.extend([(x0, y0, z0), (x0, y0, z3),
                    (x1, y1, z1), (x1, y1, z4),
                    (x2, y2, z1), (x2, y2, z4),
                    (x3, y3, z0), (x3, y3, z3)])
        faces.extend([(f, f + 1, f + 3, f + 2),
                    (f + 2, f + 3, f + 5, f + 4),
                    (f + 4, f + 5, f + 7, f + 6),
                    (f + 6, f + 7, f + 1, f),
                    (f, f + 2, f + 4, f + 6),
                    (f + 7, f + 5, f + 3, f + 1)])
        vcolors.extend([self.random_color, self.random_color,
                        self.random_color, self.random_color,
                        self.random_color, self.random_color
                        ])
        matids.extend([id_mat] * 6)
        x = [(0, 0), (0, post_z), (post_y, post_z), (post_y, 0)]
        y = [(0, 0), (0, post_z), (post_x, post_z), (post_x, 0)]
        z = [(0, 0), (post_y, 0), (post_y, post_x), (0, post_x)]
        uvs.extend([x, y, x, y, z, z])

    def get_panel(self, subs, altitude, panel_x, panel_z, sub_offset_x, idmat, verts, faces, matids, uvs, vcolors):
        n_subs = len(subs)
        if n_subs < 1:
            return
        f = len(verts)
        x0 = sub_offset_x - 0.5 * panel_x
        x1 = sub_offset_x + 0.5 * panel_x
        z0 = 0
        z1 = panel_z
        profile = [Vector((x0, z0)), Vector((x1, z0)), Vector((x1, z1)), Vector((x0, z1))]
        user_path_uv_v = []
        n_sections = n_subs - 1
        n, dz, zl = subs[0]
        p0 = n.p0
        v0 = n.v.normalized()
        for s, section in enumerate(subs):
            n, dz, zl = section
            p1 = n.p0
            if s < n_sections:
                v1 = subs[s + 1][0].v.normalized()
            dir = (v0 + v1).normalized()
            scale = 1 / cos(0.5 * acos(min(1, max(-1, v0 @ v1))))
            for p in profile:
                x, y, z = n.p0 + scale * p.x * dir
                z = zl + p.y + altitude
                verts.append((x, y, z))
            if s > 0:
                user_path_uv_v.append((p1 - p0).length)
            p0 = p1
            v0 = v1

        # build faces using Panel
        lofter = Lofter(
            # closed_shape, index, x, y, idmat
            True,
            [i for i in range(len(profile))],
            [p.x for p in profile],
            [p.y for p in profile],
            [idmat for i in range(len(profile))],
            closed_path=False,
            user_path_uv_v=user_path_uv_v,
            user_path_verts=n_subs
            )
        color = self.random_color
        faces += lofter.faces(16, offset=f, path_type='USER_DEFINED')
        vcolors += lofter.vcolors(16, color, path_type='USER_DEFINED')
        matids += lofter.mat(16, idmat, idmat, path_type='USER_DEFINED')
        v = Vector((0, 0))
        uvs += lofter.uv(16, v, v, v, v, 0, v, 0, 0, path_type='USER_DEFINED')

    def make_subs(self, x, y, z, post_y, altitude,
            sub_spacing, offset_x, sub_offset_x, mat, verts, faces, matids, uvs, vcolors):

        t_post = (0.5 * post_y - y) / self.length
        t_spacing = (sub_spacing + y) / self.length

        _segs = self.segs
        _line = self.line.segs

        for segment in self.segments:
            t_step = segment.t_step
            t_start = segment.t_start + t_post
            s = 0
            s_sub = t_step - 2 * t_post
            n_sub = int(s_sub / t_spacing)
            if n_sub > 0:
                t_sub = s_sub / n_sub
            else:
                t_sub = 1
            i = segment.i_start
            while s < segment.n_step:
                t_cur = t_start + s * t_step
                for j in range(1, n_sub):
                    t_s = t_cur + t_sub * j
                    while _segs[i].t_end < t_s:
                        i += 1
                    f = _segs[i]
                    line = _line[i]
                    t = (t_s - f.t_start) / f.t_diff
                    n = line.normal(t)
                    post = (n, f.dz / f.length, f.lerp_z(t))
                    self.get_post(post, x, y, z, altitude, sub_offset_x, mat, verts, faces, matids, uvs, vcolors)
                s += 1

    def make_post(self, x, y, z, altitude, x_offset, mat, verts, faces, matids, uvs, vcolors):
        # skip first port when closed
        skip_post = self.closed
        _line = self.line.segs
        _segs = self.segs

        for segment in self.segments:

            t_step = segment.t_step
            t_start = segment.t_start
            s = 0
            i = segment.i_start
            while s <= segment.n_step:
                t_cur = t_start + s * t_step
                while _segs[i].t_end < t_cur:
                    i += 1
                f = _segs[i]
                line = _line[i]
                t = (t_cur - f.t_start) / f.t_diff
                n = line.normal(t, 1)
                post = (n, f.dz / line.length, f.lerp_z(t))
                # self.get_post(post, x, y, z, altitude, x_offset, mat, verts, faces, matids, uvs, vcolors)
                if not skip_post:
                    self.get_post(post, x, y, z, altitude, 0, mat, verts, faces, matids, uvs, vcolors)
                skip_post = False
                s += 1
            skip_post = True

    def make_panels(self, x, z, post_y, altitude, panel_dist,
            offset_x, sub_offset_x, idmat, verts, faces, matids, uvs, vcolors):
        
        if self.closed:
            _line = self.line.segs
            _segs = self.segs
        else:
            _line = self.line.segs[:-1]
            _segs = self.segs[:-1]
        
        t_post = (0.5 * post_y + panel_dist) / self.length
        for segment in self.segments:
            t_step = segment.t_step
            t_start = segment.t_start
            s = 0
            i = segment.i_start
            while s < segment.n_step:
                subs = []
                t_cur = t_start + s * t_step + t_post
                t_end = t_start + (s + 1) * t_step - t_post
                # find first section
                while _segs[i].t_end < t_cur and i < segment.i_end:
                    i += 1
                f = _segs[i]
                line = _line[i]
                # 1st section
                t = (t_cur - f.t_start) / f.t_diff
                n = line.normal(t, 1)
                subs.append((n, f.dz / line.length, f.lerp_z(t)))
                # crossing sections -> new segment
                while i < segment.i_end:
                    f = _segs[i]
                    line = _line[i]
                    if f.t_end < t_end:
                        if type(f).__name__ == 'CurvedSegment':
                            # cant end after segment
                            t0 = max(0, (t_cur - f.t_start) / f.t_diff)
                            t1 = min(1, (t_end - f.t_start) / f.t_diff)
                            n_s = int(max(1, abs(f.da) * (5) / pi - 1))
                            dt = (t1 - t0) / n_s
                            for j in range(1, n_s + 1):
                                t = t0 + dt * j
                                n = line.normal(t, 1)
                                # n.p0= f.lerp(x_offset)
                                subs.append((n, f.dz / line.length, f.lerp_z(t)))
                        else:
                            n = line.normal(1, 1)
                            subs.append((n, f.dz / line.length, f.z0 + f.dz))
                    if f.t_end >= t_end:
                        break
                    elif f.t_start < t_end:
                        i += 1

                f = _segs[i]
                line = _line[i]
                # last section
                if type(f).__name__ == 'CurvedSegment':
                    # cant start before segment
                    t0 = max(0, (t_cur - f.t_start) / f.t_diff)
                    t1 = min(1, (t_end - f.t_start) / f.t_diff)
                    n_s = int(max(1, abs(f.da) * (5) / pi - 1))
                    dt = (t1 - t0) / n_s
                    for j in range(1, n_s + 1):
                        t = t0 + dt * j
                        n = line.normal(t, 1)
                        # n.p0= f.lerp(x_offset)
                        subs.append((n, f.dz / line.length, f.lerp_z(t)))
                else:
                    t = (t_end - f.t_start) / f.t_diff
                    n = line.normal(t, 1)
                    subs.append((n, f.dz / line.length, f.lerp_z(t)))

                # self.get_panel(subs, altitude, x, z, 0, idmat, verts, faces, matids, uvs, vcolors)
                self.get_panel(subs, altitude, x, z, sub_offset_x, idmat, verts, faces, matids, uvs, vcolors)
                s += 1

    def make_profile(self, profile, idmat,
            x_offset, z_offset, extend, closed, verts, faces, matids, uvs, vcolors):

        if self.closed:
            extend = 0

        p_line = self.make_offset(x_offset)
        if self.closed:
            _line = p_line.segs
            _segs = self.segs
        else:
            _line = p_line.segs[:-1]
            _segs = self.segs[:-1]

        n_fences = len(_segs) - 1

        if n_fences < 0:
            return

        sections = []

        f = _segs[0]
        line = _line[0]
        # first step
        if extend != 0 and line.length != 0:
            t = -extend / line.length
            n = line.normal(t, 1)
            # n.p0= f.lerp(x_offset)
            sections.append((n, f.dz / line.length, f.lerp_z(t)))

        # add first section
        if self.closed:
            n = _line[-1].normal(1, 1)
        else:
            n = line.normal(0, 1)

        sections.append((n, f.dz / line.length, f.z0))

        for f, line in zip(_segs, _line):
            if line.length == 0:
                continue
            if type(f).__name__ == 'CurvedSegment':
                n_s = int(max(1, abs(f.da) * 30 / pi - 1))
                for i in range(1, n_s + 1):
                    t = i / n_s
                    n = line.normal(t, 1)
                    # n.p0= f.lerp(x_offset)
                    sections.append((n, f.dz / line.length, f.lerp_z(t)))
            else:
                n = line.normal(1, 1)
                # n.p0= f.lerp(x_offset)
                sections.append((n, f.dz / line.length, f.z0 + f.dz))

        f = _segs[-1]
        line = _line[-1]
        if extend != 0 and line.length != 0:
            t = 1 + extend / line.length
            n = line.normal(t, 1)
            sections.append((n, f.dz / line.length, f.lerp_z(t)))

        user_path_verts = len(sections)

        offset = len(verts)
        if user_path_verts > 0:
            user_path_uv_v = []
            # n, dz, z0 = sections[-1]
            # sections[-1] = (n, dz, z0)
            n_sections = user_path_verts - 1

            n, dz, zl = sections[0]
            p0 = n.p0
            v0 = n.v_normalized
            for s, section in enumerate(sections):
                n, dz, zl = section
                p1 = n.p0
                if s < n_sections:
                    v1 = sections[s + 1][0].v_normalized

                if not self.closed or s < n_sections:
                    dir = (v0 + v1).normalized()
                    scale = min(10, 1 / cos(0.5 * acos(min(1, max(-1, v0 @ v1)))))
                    for p in profile:
                        # x, y = n.p0+ scale * (x_offset + p.x) * dir
                        x, y, z = n.p0 + scale * p.x * dir
                        z = zl + p.y + z_offset
                        verts.append((x, y, z))

                if s > 0:
                    user_path_uv_v.append((p1 - p0).length)
                p0 = p1
                v0 = v1

            if self.closed:
                user_path_verts -= 1
                n, dz, zl = sections[0]
                p1 = n.p0
                user_path_uv_v.append((p1 - p0).length)

            # build faces using Panel
            lofter = Lofter(
                # closed_shape, index, x, y, idmat
                closed,
                [i for i in range(len(profile))],
                [p.x for p in profile],
                [p.y for p in profile],
                [idmat for i in range(len(profile))],
                closed_path=self.closed,
                user_path_uv_v=user_path_uv_v,
                user_path_verts=user_path_verts
                )
            color = self.random_color
            faces += lofter.faces(16, offset=offset, path_type='USER_DEFINED')
            matids += lofter.mat(16, idmat, idmat, path_type='USER_DEFINED')
            vcolors += lofter.vcolors(16, color, path_type='USER_DEFINED')
            v = Vector((0, 0))
            uvs += lofter.uv(16, v, v, v, v, 0, v, 0, 0, path_type='USER_DEFINED')


def update(self, context):
    self.update(context)


def update_manipulators(self, context):
    self.update(context, manipulable_refresh=True)


def update_path(self, context):
    self.update_path(context)


MAT_HANDRAIL = 0
MAT_PANEL = 1
MAT_POST = 2
MAT_SUBS = 3
MAT_RAIL = 0


material_enums = []
mat_enum, mat_index_getter, mat_index_setter= build_mat_enum('idmat', material_enums)


class archipack_fence_part(Archipacki18n, ArchipackSegment, PropertyGroup):
    dz: FloatProperty(
        name="delta z",
        default=0,
        unit='LENGTH', subtype='DISTANCE'
    )
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_fence[0]


class archipack_fence_rail(Archipacki18n, ArchipackProfile, PropertyGroup):
    profil_x: FloatProperty(
        name="Width",
        min=0.001,
        default=0.02, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    profil_y: FloatProperty(
        name="Height",
        min=0.001,
        default=0.1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    offset: FloatProperty(
        name="Offset",
        default=0,
        precision=5, step=1,
        unit='LENGTH',
        update=update
    )
    extend: FloatProperty(
        name="Extend",
        default=0,
        precision=5, step=1,
        unit='LENGTH',
        update=update
    )
    alt: FloatProperty(
        name="Altitude",
        default=1.0,
        precision=5, step=1,
        unit='LENGTH',
        update=update
    )
    profil: EnumProperty(
        name="Profil",
        items=(
            ('SQUARE', 'Square', '', 0),
            ('CIRCLE', 'Circle', '', 1),
            ('SAFETY', 'Safety rail', '', 2),
            ('USER', 'User defined', '', 3)
        ),
        default='SQUARE',
        update=update
    )
    idmat: IntVectorProperty(
        default=[0],
        size=1
    )
    mat: EnumProperty(
        options={'SKIP_SAVE'},
        name="Material",
        items=mat_enum,
        get=mat_index_getter(MAT_RAIL),
        set=mat_index_setter(MAT_RAIL),
        update=update
    )
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True
    )

    def id_mat(self, index):
        _idx = self.idmat[index]
        if _idx < len(self.id_data.materials):
            return _idx
        return 0

    @property
    def parent_data(self):
        return self.id_data.archipack_fence[0]

    def refresh_profile_size(self, context, x, y):
        self.profil_x = x
        self.profil_y = y
        self.auto_update = True
        self.update(context)

    def update(self, context, manipulable_refresh=False):
        if self.auto_update:
            self.parent_data.update(context, manipulable_refresh)

    def draw(self, context, layout):
        self.draw_prop(context, layout, layout, self, 'profil')
        if self.profil == 'USER':
            self.draw_user_profile(context, layout)
        self.draw_prop(context, layout, layout, self, 'profil_x')
        self.draw_prop(context, layout, layout, self, 'profil_y')
        self.draw_prop(context, layout, layout, self, 'alt')
        self.draw_prop(context, layout, layout, self, 'offset')
        self.draw_prop(context, layout, layout, self, 'extend')
        self.draw_prop(context, layout, layout, self, 'mat')



def set_rail(self, value):
    # remove rails
    for i in range(len(self.rails), value, -1):
        self.rails.remove(i - 1)

    # add rails
    for i in range(len(self.rails), value):
        self.rails.add()
    return None


def get_rail(self):
    return len(self.rails)


class archipack_fence(Archipacki18n, ArchipackUserDefinedPath, ArchipackProfile,
                      ArchipackObject, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display main settings', 'NONE', 0),
            ('PARTS', 'Axis', 'Display fence segments settings', 'NONE', 1),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 2)
        ),
        default='MAIN',
    )
    parts: CollectionProperty(type=archipack_fence_part)

    x_offset: FloatProperty(
        name="Offset",
        default=0.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    radius: FloatProperty(
        name="Radius",
        min=0.01,
        default=0.7,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    da: FloatProperty(
        name="Angle",
        min=-pi,
        max=pi,
        default=pi / 2,
        subtype='ANGLE', unit='ROTATION',
        update=update
    )
    angle_limit: FloatProperty(
        name="Angle",
        min=0,
        max=2 * pi,
        default=pi / 8,
        subtype='ANGLE', unit='ROTATION',
        update=update_manipulators
    )
    shape: EnumProperty(
        items=(
            ('RECTANGLE', 'Straight', '', 0),
            ('CIRCLE', 'Curved ', '', 1)
        ),
        default='RECTANGLE',
        update=update
    )
    post: BoolProperty(
        name='Enable',
        default=True,
        update=update
    )
    post_spacing: FloatProperty(
        name="Spacing",
        min=0.1,
        default=1.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    post_x: FloatProperty(
        name="Width",
        min=0.001,
        default=0.04, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    post_y: FloatProperty(
        name="Length",
        min=0.001, max=1000,
        default=0.04, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    post_z: FloatProperty(
        name="Height",
        min=0.001,
        default=1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    post_alt: FloatProperty(
        name="Altitude",
        default=0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    post_rotation: FloatProperty(
        name="Rotation",
        min=-pi,
        max=pi,
        default=pi / 2,
        subtype='ANGLE', unit='ROTATION',
        update=update
    )
    user_defined_post_enable: BoolProperty(
        name="User",
        update=update,
        default=True
    )
    user_defined_post: StringProperty(
        name="User defined",
        update=update
    )

    subs: BoolProperty(
        name='Enable',
        default=False,
        update=update
    )
    subs_spacing: FloatProperty(
        name="Spacing",
        min=0.05,
        default=0.10, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    subs_x: FloatProperty(
        name="Width",
        min=0.001,
        default=0.02, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    subs_y: FloatProperty(
        name="Length",
        min=0.001,
        default=0.02, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    subs_z: FloatProperty(
        name="Height",
        min=0.001,
        default=1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    subs_alt: FloatProperty(
        name="Altitude",
        default=0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    subs_offset_x: FloatProperty(
        name="Offset",
        default=0.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    subs_bottom: EnumProperty(
        name="Bottom",
        items=(
            ('STEP', 'Follow step', '', 0),
            ('LINEAR', 'Linear', '', 1),
        ),
        default='STEP',
        update=update
    )
    subs_rotation: FloatProperty(
        name="Rotation",
        min=-pi,
        max=pi,
        default=pi / 2,
        subtype='ANGLE', unit='ROTATION',
        update=update
    )
    user_defined_subs_enable: BoolProperty(
        name="User",
        update=update,
        default=True
    )
    user_defined_subs: StringProperty(
        name="User defined",
        update=update
    )

    panel: BoolProperty(
        name='Enable',
        default=True,
        update=update
    )
    panel_alt: FloatProperty(
        name="Altitude",
        default=0.25, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    panel_x: FloatProperty(
        name="Width",
        min=0.001,
        default=0.01, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    panel_z: FloatProperty(
        name="Height",
        min=0.001,
        default=0.6, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    panel_dist: FloatProperty(
        name="Spacing",
        min=0.001,
        default=0.05, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    panel_offset_x: FloatProperty(
        name="Offset",
        default=0.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    rail: BoolProperty(
        name="Enable",
        update=update,
        default=False
    )
    rail_n: IntProperty(
        options={'SKIP_SAVE'},
        name="#",
        default=0,
        min=0,
        max=255,
        get=get_rail,
        set=set_rail,
        update=update
    )
    rails: CollectionProperty(type=archipack_fence_rail)

    handrail: BoolProperty(
        name="Enable",
        update=update,
        default=True
    )
    handrail_offset: FloatProperty(
        name="Offset",
        default=0.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    handrail_alt: FloatProperty(
        name="Altitude",
        default=1.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    handrail_extend: FloatProperty(
        name="Extend",
        min=0,
        default=0.1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    handrail_slice: BoolProperty(
        name='Slice',
        default=True,
        update=update
    )
    handrail_slice_right: BoolProperty(
        name='Slice',
        default=True,
        update=update
    )
    handrail_profil: EnumProperty(
        name="Profil",
        items=(
            ('SQUARE', 'Square', '', 0),
            ('CIRCLE', 'Circle', '', 1),
            ('COMPLEX', 'Circle over square', '', 2)
        ),
        default='SQUARE',
        update=update
    )
    handrail_x: FloatProperty(
        name="Width",
        min=0.001,
        default=0.04, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    handrail_y: FloatProperty(
        name="Height",
        min=0.001,
        default=0.04, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    handrail_radius: FloatProperty(
        name="Radius",
        min=0.001,
        default=0.02, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    use_subs_material: BoolProperty(
        name="Use material",
        description="Use material indexes of object",
        update=update,
        default=False
    )
    use_post_material: BoolProperty(
        name="Use material",
        description="Use material indexes of object",
        update=update,
        default=False
    )

    idmat: IntVectorProperty(
        default=[
            0, 2, 1, 1
        ],
        size=4
    )

    idmat_handrail: EnumProperty(
        options={'SKIP_SAVE'},
        name="Handrail",
        items=mat_enum,
        get=mat_index_getter(MAT_HANDRAIL),
        set=mat_index_setter(MAT_HANDRAIL),
        update=update
    )

    idmat_subs: EnumProperty(
        options={'SKIP_SAVE'},
        name="Subs",
        items=mat_enum,
        get=mat_index_getter(MAT_SUBS),
        set=mat_index_setter(MAT_SUBS),
        update=update
    )

    idmat_panel: EnumProperty(
        options={'SKIP_SAVE'},
        name="Panels",
        items=mat_enum,
        get=mat_index_getter(MAT_PANEL),
        set=mat_index_setter(MAT_PANEL),
        update=update
    )

    idmat_post: EnumProperty(
        options={'SKIP_SAVE'},
        name="Post",
        items=mat_enum,
        get=mat_index_getter(MAT_POST),
        set=mat_index_setter(MAT_POST),
        update=update
    )

    # UI layout related
    parts_expand: BoolProperty(
        options={'SKIP_SAVE'},
        default=False
    )
    rail_expand: BoolProperty(
        options={'SKIP_SAVE'},
        default=False
    )

    idmats_expand: BoolProperty(
        options={'SKIP_SAVE'},
        default=False
    )
    handrail_expand: BoolProperty(
        options={'SKIP_SAVE'},
        default=False
    )
    post_expand: BoolProperty(
        options={'SKIP_SAVE'},
        default=False
    )
    panel_expand: BoolProperty(
        options={'SKIP_SAVE'},
        default=False
    )
    subs_expand: BoolProperty(
        options={'SKIP_SAVE'},
        default=False
    )
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update_manipulators
    )
    always_closed = False

    def setup_manipulators(self):
        """
        if len(self.manipulators) == 0:
            s = self.manipulators.add()
            s.prop1_name = "width"
            s = self.manipulators.add()
            s.prop1_name = "height"
            s.normal = Vector((0, 1, 0))
        """
        if len(self.manipulators) == 0:
            s = self.manipulators.add()
            s.prop1_name = "n_parts"
            s.type_key = 'COUNTER'

        self.setup_parts_manipulators('dumb_z')

    def update_parts_from_generator(self, g):
        _parts = self.parts
        _z = [s.dz for s in g.segs]
        for _p, z1 in zip(_parts, _z):
            _p.dz = z1

    def get_generator(self, o=None):
        g = FenceGenerator(o)
        g.add_parts(self)
        g.init_sides(self)
        return g

    def update(self, context, manipulable_refresh=False):

        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        # clean up manipulators before any data model change
        if self.manipulable_refresh:
            self.manipulable_disable(o)

        # self.update_parts()

        verts = []
        faces = []
        matids = []
        uvs = []
        vcolors = []

        g = self.get_generator()
        g.locate_manipulators(self)

        # Parts COUNTER
        self.manipulators[0].set_pts([g.segs[-2].lerp(1.1),
              g.segs[-2].lerp(1.1 + 0.5 / g.segs[-2].length),
              (-1, 0, 0)
              ])

        g.param_t(self, self.angle_limit, self.post_spacing)

        # depth at bottom
        # self.manipulators[1].set_pts([(0, 0, 0), (0, 0, self.height), (1, 0, 0)])

        if self.user_defined_post_enable:
            # user defined posts
            user_def_post = self.get_scene_object(context, self.user_defined_post)
            if user_def_post is not None and user_def_post.type == 'MESH':
                g.setup_user_defined_post(user_def_post,
                    self.post_x, self.post_y, self.post_z, self.post_rotation,
                    self.use_post_material, self.id_mat(MAT_POST))

        if self.post:
            g.make_post(0.5 * self.post_x, 0.5 * self.post_y, self.post_z,
                    self.post_alt, self.x_offset,
                    self.id_mat(MAT_POST), verts, faces, matids, uvs, vcolors)

        # reset user def posts
        g.user_defined_post = None

        # user defined subs
        if self.user_defined_subs_enable:
            user_def_subs = self.get_scene_object(context, self.user_defined_subs)
            if user_def_subs is not None and user_def_subs.type == 'MESH':
                g.setup_user_defined_post(user_def_subs,
                    self.subs_x, self.subs_y, self.subs_z, self.subs_rotation,
                    self.use_subs_material, self.id_mat(MAT_SUBS))

        if self.subs:
            g.make_subs(0.5 * self.subs_x, 0.5 * self.subs_y, self.subs_z,
                    self.post_y, self.subs_alt, self.subs_spacing, self.x_offset, self.subs_offset_x,
                    self.id_mat(MAT_SUBS), verts, faces, matids, uvs, vcolors)

        g.user_defined_post = None

        if self.panel:
            g.make_panels(0.5 * self.panel_x, self.panel_z, self.post_y,
                    self.panel_alt, self.panel_dist, self.x_offset, self.panel_offset_x,
                    self.id_mat(MAT_PANEL), verts, faces, matids, uvs, vcolors)

        if self.rail:
            for i in range(self.rail_n):
                rd = self.rails[i]
                x = 0.5 * rd.profil_x
                y = rd.profil_y
                closed = True
                mat = rd.id_mat(MAT_RAIL)
                if rd.profil == 'SQUARE':
                    rail = [Vector((-x, y)), Vector((-x, 0)), Vector((x, 0)), Vector((x, y))]
                elif rd.profil == 'CIRCLE':
                    rail = [Vector((x * sin(0.1 * -a * pi), x * (0.5 + cos(0.1 * -a * pi)))) for a in range(0, 20)]

                elif rd.profil == 'SAFETY':
                    closed = False
                    rail = [Vector((i * x, j * y)) for i, j in [(0, -0.5),
                        (1, -0.35714),
                        (1, -0.21429),
                        (0, -0.07143),
                        (0, 0.07143),
                        (1, 0.21429),
                        (1, 0.35714),
                        (0, 0.5)]]
                elif rd.profil == 'USER':
                    curve = rd.update_profile(context)
                    if curve and curve.type == 'CURVE':
                        sx, sy = 1, 1
                        if rd.user_profile_dimension.x > 0:
                            sx = rd.profil_x / rd.user_profile_dimension.x
                        if rd.user_profile_dimension.y > 0:
                            sy = rd.profil_y / rd.user_profile_dimension.y
                        wM = Matrix([
                            [sx, 0, 0, 0],
                            [0, sy, 0, 0],
                            [0, 0, 1, 0],
                            [0, 0, 0, 1]
                            ])
                        for spline in curve.data.splines:
                            rail = self.coords_from_spline(spline, wM, 12, ccw=True)
                            closed = spline.use_cyclic_u

                            g.make_profile(rail, mat, self.x_offset - rd.offset,
                                rd.alt, rd.extend, closed, verts, faces, matids, uvs, vcolors)
                    else:
                        print("fence.update curve not found")

                    # dont call
                    continue

                g.make_profile(rail, mat, self.x_offset - rd.offset,
                        rd.alt, rd.extend, closed, verts, faces, matids, uvs, vcolors)

        if self.handrail:

            if self.handrail_profil == 'COMPLEX':
                sx = self.handrail_x
                sy = self.handrail_y
                handrail = [Vector((sx * x, sy * y)) for x, y in [
                (-0.28, 1.83), (-0.355, 1.77), (-0.415, 1.695), (-0.46, 1.605), (-0.49, 1.51), (-0.5, 1.415),
                (-0.49, 1.315), (-0.46, 1.225), (-0.415, 1.135), (-0.355, 1.06), (-0.28, 1.0), (-0.255, 0.925),
                (-0.33, 0.855), (-0.5, 0.855), (-0.5, 0.0), (0.5, 0.0), (0.5, 0.855), (0.33, 0.855), (0.255, 0.925),
                (0.28, 1.0), (0.355, 1.06), (0.415, 1.135), (0.46, 1.225), (0.49, 1.315), (0.5, 1.415),
                (0.49, 1.51), (0.46, 1.605), (0.415, 1.695), (0.355, 1.77), (0.28, 1.83), (0.19, 1.875),
                (0.1, 1.905), (0.0, 1.915), (-0.095, 1.905), (-0.19, 1.875)]]

            elif self.handrail_profil == 'SQUARE':
                x = 0.5 * self.handrail_x
                y = self.handrail_y
                handrail = [Vector((-x, y)), Vector((-x, 0)), Vector((x, 0)), Vector((x, y))]

            elif self.handrail_profil == 'CIRCLE':
                r = self.handrail_radius
                handrail = [Vector((r * sin(0.1 * -a * pi), r * (0.5 + cos(0.1 * -a * pi)))) for a in range(0, 20)]

            g.make_profile(handrail, self.id_mat(MAT_HANDRAIL), self.x_offset - self.handrail_offset,
                self.handrail_alt, self.handrail_extend, True, verts, faces, matids, uvs, vcolors)

        bmed.buildmesh(o, verts, faces, matids, uvs, vcolors, weld=True, clean=False)
        self.shade_smooth(context, o, 0.418879)
        self.update_dimensions(context, o)

        # restore context
        self.restore_context(context)

    def manipulable_setup(self, context, o):
        """
            NOTE:
            this one assume context.active_object is the instance this
            data belongs to, failing to do so will result in wrong
            manipulators set on active object
        """
        self.setup_manipulators()
        self.manipulable_setup_parts(context, o)

        if not self.is_closed:
            for m in self.manipulators:
                self.manip_stack.append(m.setup(context, o, self))


class ARCHIPACK_PT_fence(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_fence"
    bl_label = "Fence"

    @classmethod
    def poll(cls, context):
        return archipack_fence.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_fence.datablock(o)
        if d is None:
            return
        scene = context.scene
        layout = self.layout

        self.draw_common(context, layout)

        box = layout.box()
        # self.draw_label(context, layout, box, "Styles")
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.fence_preset_menu",
                     text=bpy.types.ARCHIPACK_OT_fence_preset_menu.bl_label, icon="PRESET")
        self.draw_op(context, layout, row, "archipack.fence_preset", icon='ADD', text="")
        self.draw_op(context, layout, row, "archipack.fence_preset", icon='REMOVE', text="").remove_active = True

        self.draw_prop(context, layout, layout, d, "tabs", expand=True)

        box = layout.box()

        if d.tabs == 'PARTS':
            expand = d.template_user_path(context, box, focus=False)
            if expand:
                if d.user_defined_path is not "":
                    self.draw_prop(context, layout, box, d, 'user_defined_reverse')

                self.draw_prop(context, layout, box, d, 'angle_limit')

            d.template_parts(context, layout, draw_type=True)

        elif d.tabs == 'MAIN':

            self.draw_prop(context, layout, box, d, 'x_offset')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.handrail_expand:
                icon = "TRIA_DOWN"

            self.draw_prop(context, layout, row, d, 'handrail_expand', icon=icon, emboss=True, text="Handrail")
            self.draw_prop(context, layout, row, d, 'handrail')

            if d.handrail_expand:
                self.draw_prop(context, layout, box, d, 'handrail_alt')
                self.draw_prop(context, layout, box, d, 'handrail_offset')
                self.draw_prop(context, layout, box, d, 'handrail_extend')
                self.draw_prop(context, layout, box, d, 'handrail_profil')
                if d.handrail_profil != 'CIRCLE':
                    self.draw_prop(context, layout, box, d, 'handrail_x')
                    self.draw_prop(context, layout, box, d, 'handrail_y')
                else:
                    self.draw_prop(context, layout, box, d, 'handrail_radius')
                row = box.row(align=True)
                self.draw_prop(context, layout, row, d, 'handrail_slice')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.post_expand:
                icon = "TRIA_DOWN"

            self.draw_prop(context, layout, row, d, 'post_expand', icon=icon, emboss=True, text="Post")
            self.draw_prop(context, layout, row, d, 'post')

            if d.post_expand:
                self.draw_prop(context, layout, box, d, 'post_spacing')
                self.draw_prop(context, layout, box, d, 'post_x')
                self.draw_prop(context, layout, box, d, 'post_y')
                self.draw_prop(context, layout, box, d, 'post_z')
                self.draw_prop(context, layout, box, d, 'post_alt')
                row = box.row(align=True)
                self.draw_prop(context, layout, row, d, 'user_defined_post_enable', text="")
                self.draw_op(context, layout, row, "archipack.fence_subpart_dimensions", icon="FULLSCREEN_ENTER",
                             text="").part = "POST"  #, icon='BBOX'
                row.prop_search(d, "user_defined_post", scene, "objects", text="")
                if d.user_defined_post:
                    self.draw_prop(context, layout, box, d, 'post_rotation')
                    self.draw_prop(context, layout, box, d, 'use_post_material')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.subs_expand:
                icon = "TRIA_DOWN"

            self.draw_prop(context, layout, row, d, 'subs_expand', icon=icon, emboss=True, text="Subs")
            self.draw_prop(context, layout, row, d, 'subs')

            if d.subs_expand:
                self.draw_prop(context, layout, box, d, 'subs_spacing')
                self.draw_prop(context, layout, box, d, 'subs_x')
                self.draw_prop(context, layout, box, d, 'subs_y')
                self.draw_prop(context, layout, box, d, 'subs_z')
                self.draw_prop(context, layout, box, d, 'subs_alt')
                self.draw_prop(context, layout, box, d, 'subs_offset_x')
                row = box.row(align=True)
                self.draw_prop(context, layout, row, d, 'user_defined_subs_enable', text="")
                self.draw_op(context, layout, row, "archipack.fence_subpart_dimensions", icon="FULLSCREEN_ENTER",
                             text="").part = "SUB"  #, icon='BBOX'
                row.prop_search(d, "user_defined_subs", scene, "objects", text="")
                if d.user_defined_subs:
                    self.draw_prop(context, layout, box, d, 'subs_rotation')
                    self.draw_prop(context, layout, box, d, 'use_subs_material')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.panel_expand:
                icon = "TRIA_DOWN"

            self.draw_prop(context, layout, row, d, 'panel_expand', icon=icon, emboss=True, text="Panels")
            self.draw_prop(context, layout, row, d, 'panel')

            if d.panel_expand:
                self.draw_prop(context, layout, box, d, 'panel_dist')
                self.draw_prop(context, layout, box, d, 'panel_x')
                self.draw_prop(context, layout, box, d, 'panel_z')
                self.draw_prop(context, layout, box, d, 'panel_alt')
                self.draw_prop(context, layout, box, d, 'panel_offset_x')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.rail_expand:
                icon = "TRIA_DOWN"

            self.draw_prop(context, layout, row, d, 'rail_expand', icon=icon, emboss=True, text="Rails")
            self.draw_prop(context, layout, row, d, 'rail')

            if d.rail_expand:
                self.draw_prop(context, layout, box, d, 'rail_n')
                for i in range(d.rail_n):
                    box = layout.box()
                    self.draw_label(context, layout, box, "Rail", postfix=str(i + 1))
                    d.rails[i].draw(context, box)

        elif d.tabs == 'MATERIALS':
            if "archipack_material" in o:
                o.archipack_material[0].draw(context, box)
            box = layout.box()
            self.draw_label(context, layout, box, "Apply materials")
            self.draw_prop(context, layout, box, d, 'idmat_handrail')
            self.draw_prop(context, layout, box, d, 'idmat_panel')
            self.draw_prop(context, layout, box, d, 'idmat_post')
            self.draw_prop(context, layout, box, d, 'idmat_subs')


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


class ARCHIPACK_OT_fence(ArchipackCreateTool, Operator):
    bl_idname = "archipack.fence"
    bl_label = "Fence"
    bl_description = "Create Fence"

    def create(self, context):
        m = bpy.data.meshes.new("Fence")
        o = bpy.data.objects.new("Fence", m)
        d = m.archipack_fence.add()
        # make manipulators selectable
        d.manipulable_selectable = True
        d.set_parts(1)
        # Link object into scene
        self.link_object_to_scene(context, o)
        o.color = (0, 1, 0, 1)

        # select and make active
        self.select_object(context, o, True)
        self.add_material(context, o)
        self.load_preset(d)
        return o

    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            o = self.create(context)
            o.location = self.get_cursor_location(context)
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


"""
 TODO:
 make fence child of curve
 use relationship to update (add/remove) childs
"""


class ARCHIPACK_OT_fence_from_curve(ArchipackCreateTool, Operator):
    bl_idname = "archipack.fence_from_curve"
    bl_label = "Fence curve"
    bl_description = "Create fence(s) from a curve"

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o is not None and o.type == 'CURVE'

    def create_one(self, context, curve, i):
        bpy.ops.archipack.fence(filepath=self.filepath)
        o = context.active_object
        d = archipack_fence.datablock(o)
        d.auto_update = False
        d.user_defined_spline = i
        d.user_defined_path = curve.name
        d.user_defined_resolution = min(128, curve.data.resolution_u)
        d.auto_update = True
        return o

    def create(self, context):
        o = None
        curve = context.active_object

        sel = []
        for i, spline in enumerate(curve.data.splines):
            o = self.create_one(context, curve, i)
            sel.append(o)
        for obj in sel:
            self.select_object(context, obj, True)
        return o

    def execute(self, context):
        curve = context.active_object
        self.set_cursor_location(context, curve.matrix_world @ Vector(curve.bound_box[0]))

        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            o = self.create(context)
            # select and make active
            self.select_object(context, o, True)
            return {'FINISHED'}

        elif context.mode == 'EDIT_CURVE':
            # Tynkatopi's contrib
            d = curve.data
            spline_index = 0

            if d.splines.active:
                spline_index = d.splines[:].index(d.splines.active)

            rot = curve.rotation_euler.copy()
            curve.rotation_euler = 0, 0, 0
            bpy.ops.object.mode_set(mode='OBJECT')
            o = self.create_one(context, curve, 0)
            if o:
                d = archipack_fence.datablock(o)
                o.rotation_euler = 0, 0, 0
                o.parent = curve
                # update here so fence location match spline vertex 1
                d.user_defined_spline = spline_index
                self.select_object(context, o)

            # select and make active
            self.select_object(context, curve, True)
            curve.rotation_euler = rot
            bpy.ops.object.mode_set(mode='EDIT')
            return {'FINISHED'}

        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object/Edit mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_fence_draw(ArchipackDrawTool, Operator):
    bl_idname = "archipack.fence_draw"
    bl_label = "Draw a fence"
    bl_description = "Create a fence by drawing its baseline in 3D view"

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
        z = 0.1
        if self.state == 'CREATE':
            p0 = self.takeloc
        else:
            p0 = sp.takeloc

        p1 = sp.placeloc
        delta = p1 - p0
        # print("sp_draw state:%s delta:%s p0:%s p1:%s" % (self.state, delta.length, p0, p1))
        if delta.length == 0:
            return
        logger.debug("ARCHIPACK_OT_fence_draw.sp_draw")
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
                bpy.ops.archipack.fence('INVOKE_DEFAULT', filepath=self.filepath)
                # self.unselect_object(context, self.act)

                # context.window_manager.archipack.auto_manipulate = False

                o = context.object
                o.location = takeloc
                self.o = o
                d = archipack_fence.datablock(o)
                # d.manipulable_selectable = False
                part = d.parts[0]
                part.length = delta.length
                state = "CALL_MANIPULATE"
            else:
                o = self.o
                # select and make active
                # self.select_object(context, o, True)
                d = archipack_fence.datablock(o)
                # Check for end close to start and close when applicable
                dp = sp.placeloc - o.location
                if dp.length < 0.01:
                    d.is_closed = True
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
        logger.debug("ARCHIPACK_OT_fence_draw.sp_init event %s %s state:%s" % (event.type, event.value, state))
        if state == 'SUCCESS':
            # point placed, check if object was under mouse
            logger.debug("self.mouse_to_scene_raycast(context, event)")

            res, pt, y, i, o, tM = self.mouse_to_scene_raycast(context, event)
            logger.debug("self.mouse_to_scene_raycast done")

            if res:
                # hover another object
                if self.is_snapping(context, event, sp.placeloc):
                    # user snap, use direction as constraint
                    tM.translation = sp.placeloc.copy()
                else:
                    # without snap must use ray intersection on xy plane
                    # Note: not certain about this behaviour, might use ray cast pt instead ?
                    pt = self.mouse_to_plane(context, event)
                    if pt is not None:
                        tM.translation = pt

                self.takeloc = tM.translation

                self.act = o
                self.takemat = tM
            else:

                self.takeloc = sp.placeloc.copy()
                if not self.is_snapping(context, event, sp.placeloc):
                    # without snap use ray intersection on xy plane
                    pt = self.mouse_to_plane(context, event)
                    if pt is not None:
                        self.takeloc = pt

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
                logger.debug("ARCHIPACK_OT_fence_draw.modal(STARTING) location:%s", takeloc)
                snap_point(takeloc=takeloc,
                           callback=self.sp_init,
                           constraint_axis=(False, False, False),
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
                logger.debug("ARCHIPACK_OT_fence_draw.modal(%s) D pressed", self.state)
                self.exit(context)
                bpy.ops.archipack.fence_preset_menu('INVOKE_DEFAULT', preset_operator="archipack.fence_draw")
                return {'FINISHED'}

            elif event.type in {'C', 'c'}:

                logger.debug("ARCHIPACK_OT_fence_draw.modal(%s) C pressed", self.state)
                d = archipack_fence.datablock(self.o)
                if d is not None:
                    d.closed = True
                self.exit(context)
                return {'FINISHED'}

            elif event.type in {'LEFTMOUSE', 'RET', 'NUMPAD_ENTER', 'SPACE'}:

                # print('LEFTMOUSE %s' % (event.value))
                self.feedback.instructions(context, "Draw a fence", "Click & Drag to add a segment", [
                    ('ENTER', 'Add part'),
                    ('BACK_SPACE', 'Remove part'),
                    ('CTRL', 'Snap'),
                    ('C', 'Close fence and exit'),
                    ('D', 'Draw another fence'),
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

                        d = archipack_fence.datablock(o)
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
                        logger.debug("ARCHIPACK_OT_fence_draw.modal(CREATE) location:%s", takeloc)

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
                d = archipack_fence.datablock(o)
                if d.num_parts > 1:
                    d.n_parts -= 1
            return {'RUNNING_MODAL'}

        if self.state == 'CANCEL' or (event.type in {'ESC', 'RIGHTMOUSE'}):

            logger.debug("ARCHIPACK_OT_fence_draw.modal(CANCEL) %s", event.type)
            if self.o is None:

                # select and make active
                self.select_object(context, self.act, True)

                for o in self.sel:
                    self.select_object(context, o)
            else:
                self.select_object(context, self.o, True)


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
            self.feedback.instructions(context, "Draw a fence", "Click & Drag to start", [
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


class ARCHIPACK_OT_fence_preset_create(PresetMenuOperator, Operator):
    bl_description = "Show Fence presets and create object at cursor location"
    bl_idname = "archipack.fence_preset_create"
    bl_label = "Fence Presets"
    preset_subdir = "archipack_fence"


class ARCHIPACK_OT_fence_preset_draw(PresetMenuOperator, Operator):
    bl_description = "Show Fence presets and draw"
    bl_idname = "archipack.fence_preset_draw"
    bl_label = "Fence Presets"
    preset_subdir = "archipack_fence"


class ARCHIPACK_OT_fence_preset_menu(PresetMenuOperator, Operator):
    bl_description = "Show Fence Presets"
    bl_idname = "archipack.fence_preset_menu"
    bl_label = "Fence preset"
    preset_subdir = "archipack_fence"


class ARCHIPACK_OT_fence_preset(ArchipackPreset, Operator):
    bl_description = "Add / remove a Add a Fence Preset"
    bl_idname = "archipack.fence_preset"
    bl_label = "Fence preset"
    preset_menu = "ARCHIPACK_OT_fence_preset_menu"

    @property
    def blacklist(self):
        return ["parts", "n_parts"]


class ARCHIPACK_OT_fence_preset_from_curve(PresetMenuOperator, Operator):
    bl_description = "Create a Fence from curve"
    bl_idname = "archipack.fence_preset_from_curve"
    bl_label = "Fence"
    preset_subdir = "archipack_fence"
    preset_operator : StringProperty(
        options={'SKIP_SAVE'},
        default="archipack.fence_from_curve"
    )

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o and o.type == 'CURVE'


class ARCHIPACK_OT_fence_subpart_dimensions(Operator):

    bl_idname = "archipack.fence_subpart_dimensions"
    bl_label = "Dimension"
    bl_description = "Use object's dimensions"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    part: StringProperty()

    @classmethod
    def poll(cls, context):
        return archipack_fence.filter(context.active_object)

    def execute(self, context):
        d = archipack_fence.datablock(context.active_object)

        if d is None:
            self.report({'WARNING'}, "Archipack: Operator only valid with fence")
            return {'CANCELLED'}

        if self.part == "SUB":
            part_obj = bpy.data.objects.get(d.user_defined_subs)
            if part_obj is None:
                self.report({'WARNING'}, "Archipack: User defined sub object not found")
                return {'CANCELLED'}
            d.subs_x, d.subs_y, d.subs_z = part_obj.dimensions.x, part_obj.dimensions.y, part_obj.dimensions.z
        else:
            part_obj = bpy.data.objects.get(d.user_defined_post)
            if part_obj is None:
                self.report({'WARNING'}, "Archipack: User defined post object not found")
                return {'CANCELLED'}
            d.post_x, d.post_y, d.post_z = part_obj.dimensions.x, part_obj.dimensions.y, part_obj.dimensions.z

        return {'FINISHED'}


def register():
    bpy.utils.register_class(archipack_fence_rail)
    bpy.utils.register_class(archipack_fence_part)
    bpy.utils.register_class(archipack_fence)
    Mesh.archipack_fence = CollectionProperty(type=archipack_fence)
    bpy.utils.register_class(ARCHIPACK_OT_fence_preset_menu)
    bpy.utils.register_class(ARCHIPACK_PT_fence)
    bpy.utils.register_class(ARCHIPACK_OT_fence)
    bpy.utils.register_class(ARCHIPACK_OT_fence_preset)
    bpy.utils.register_class(ARCHIPACK_OT_fence_preset_create)
    bpy.utils.register_class(ARCHIPACK_OT_fence_preset_draw)
    bpy.utils.register_class(ARCHIPACK_OT_fence_draw)
    bpy.utils.register_class(ARCHIPACK_OT_fence_from_curve)
    bpy.utils.register_class(ARCHIPACK_OT_fence_preset_from_curve)
    bpy.utils.register_class(ARCHIPACK_OT_fence_subpart_dimensions)


def unregister():
    bpy.utils.unregister_class(archipack_fence_rail)
    bpy.utils.unregister_class(archipack_fence_part)
    bpy.utils.unregister_class(archipack_fence)
    del Mesh.archipack_fence
    bpy.utils.unregister_class(ARCHIPACK_OT_fence_preset_menu)
    bpy.utils.unregister_class(ARCHIPACK_PT_fence)
    bpy.utils.unregister_class(ARCHIPACK_OT_fence)
    bpy.utils.unregister_class(ARCHIPACK_OT_fence_preset)
    bpy.utils.unregister_class(ARCHIPACK_OT_fence_preset_create)
    bpy.utils.unregister_class(ARCHIPACK_OT_fence_preset_draw)
    bpy.utils.unregister_class(ARCHIPACK_OT_fence_draw)
    bpy.utils.unregister_class(ARCHIPACK_OT_fence_from_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_fence_preset_from_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_fence_subpart_dimensions)
    global material_enums
    material_enums.clear()