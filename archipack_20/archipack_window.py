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
from bpy.types import Operator, PropertyGroup, Mesh, Panel, Object
from bpy.props import (
    FloatProperty, IntProperty, BoolProperty, BoolVectorProperty, IntVectorProperty,
    CollectionProperty, FloatVectorProperty, EnumProperty, StringProperty, PointerProperty
)
from mathutils import Vector, Matrix
import time
from random import uniform
import bmesh
from math import tan, sqrt, pi, sin, cos, floor
from .bmesh_utils import BmeshEdit as bmed
from .panel import Panel as WindowPanel
from .archipack_handle import create_handle, window_handle_vertical_01, window_handle_vertical_02
from .archipack_manipulator import Manipulable
from .archipack_preset import ArchipackPreset, PresetMenuOperator
from .archipack_gl import FeedbackPanel
from .archipack_abstraction import ensure_select_and_restore
from .archipack_i18n import Archipacki18n
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackPanel,
    ArchipackObject, 
    ArchipackCreateTool, 
    ArchipackDrawTool
    )
from .archipack_segments2 import OpeningGenerator
from .archipack_keymaps import Keymaps
from .archipack_dimension import DimensionProvider
from .archipack_material import build_mat_enum
from .archipack_iconmanager import icons as icon_man
import logging
logger = logging.getLogger("archipack_window")


def random_color():
    return Vector((uniform(0.0, 1.0), uniform(0.0, 1.0), uniform(0.0, 1.0), 1))


def update(self, context):
    self.update(context)


def update_childs(self, context):
    self.update(context, childs_only=True)


def set_cols(self, value):
    if self.n_cols != value:
        self.auto_update = False
        self._set_width(value)
        self.auto_update = True
        self.n_cols = value
    return None


def get_cols(self):
    return self.n_cols


MAT_GLASS = 0
MAT_FRAME_INSIDE = 1
MAT_FRAME_OUTSIDE = 2
MAT_SILL_IN = 3
MAT_SILL_OUT = 4
MAT_SPOILER = 5
MAT_SHUTTER = 6
MAT_HANDLE = 7
MAT_HINGE = 8
MAT_GLASS_FRAME = 9
MAT_JOINT = 10
MAT_SILL_CURTAIN = 11
MAT_ROD_CURTAIN = 12


# keep a reference to material enums
material_enums = []
mat_enum, mat_index_getter, mat_index_setter = build_mat_enum(
    'idmat', material_enums)


class archipack_window_panel(ArchipackObject, PropertyGroup):
    idmat: IntVectorProperty(
        default=[
            2,
            1, 0,
            1, 4,
            3, 7,
            5, 3,
            5, 6,
            1, 3
        ],
        size=13
    )
    mat_frame_inside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Frame in",
        description="Material index of frame inside",
        items=mat_enum,
        get=mat_index_getter(MAT_FRAME_INSIDE),
        set=mat_index_setter(MAT_FRAME_INSIDE),
        update=update
    )
    mat_frame_outside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Frame out",
        description="Material index of frame outside",
        items=mat_enum,
        get=mat_index_getter(MAT_FRAME_OUTSIDE),
        set=mat_index_setter(MAT_FRAME_OUTSIDE),
        update=update
    )
    mat_glass: EnumProperty(
        options={'SKIP_SAVE'},
        description="Material index of glass",
        name="Glass",
        items=mat_enum,
        get=mat_index_getter(MAT_GLASS),
        set=mat_index_setter(MAT_GLASS),
        update=update
    )
    mat_override: BoolProperty(
        name="Override material",
        default=False
    )
    @property
    def parent_data(self):
        return self.id_data.archipack_window[0]

    def update(self, context):
        self.parent_data.update(context)

    def window(self,
               d,
               fixed,
               side_material):

        verre = 0.008
        joint_h = 0.002
        joint_v = 0.004
        chanfer = 0.004
        x0 = 0
        x1 = d.panel_x
        x2 = 0.75 * d.panel_x
        x3 = chanfer
        x4 = x1 + joint_v
        y0 = -d.panel_y
        y1 = 0
        y2 = -0.5 * d.panel_y
        y3 = -chanfer
        y4 = chanfer - d.panel_y
        y5 = y2 - verre
        y6 = y2 + verre
        mi, mo = d.id_mat(MAT_FRAME_INSIDE), d.id_mat(MAT_FRAME_OUTSIDE)
        mc = d.id_mat(MAT_GLASS_FRAME)
        mj = d.id_mat(MAT_JOINT)

        if fixed:
            # profil carre avec support pour verre
            # p ______       y1
            # / |      y3
            # |       |___
            # x       |___   y2  verre
            # |       |      y4
            #  \______|      y0
            # x0 x3   x1
            #
            x1 = 0.5 * d.panel_x
            x4 = x1 + joint_v
            y1 = -0.45 * d.panel_y
            y3 = y1 - chanfer
            y4 = chanfer + y0
            y2 = (y0 + y2) / 2
            y5 = y2 - verre
            y6 = y2 + verre
            side_cap_front = -1
            side_cap_back = -1

            if d.enable_glass:
                side_cap_front = 7
                side_cap_back = 10

            return WindowPanel(
                True,  # closed
                [1, 0, 0, 0, 1, 2, 2, 3, 2, 2, 3, 2, 2],  # x index
                [x0, x3, x1, x4],
                [y0, y4, y2, y3, y1, y1, y6 + joint_h, y6, y6, y5, y5, y5 - joint_h, y0],
                [mo, mo, mi, mi, mi, mi, mj, mj, mc, mj, mj, mo, mo, mo],  # materials
                side_cap_front=side_cap_front,
                side_cap_back=side_cap_back  # cap index
            )
        else:
            # profil avec chanfrein et joint et support pour verre
            # p ____         y1    inside
            # /     |_       y3
            # |       |___y5
            # x       |___   y2  verre
            # |      _|   y6 y4
            #  \____|        y0
            # x0 x3 x2 x1 x4         outside
            # TODO: handle hung window sides materials
            if side_material == 0:
                materials = [mo, mo, mi, mi, mi, mi, mi, mi, mj, mj, mc, mj, mj, mo, mo, mo, mo]
            elif side_material == 1:
                # rail window exterior
                materials = [mo, mo, mo, mi, mi, mi, mi, mi, mj, mj, mc, mj, mj, mo, mo, mo, mo]
            else:
                # rail window interior
                materials = [mo, mi, mi, mi, mi, mi, mi, mi, mj, mj, mc, mj, mj, mo, mo, mo, mo]

            side_cap_front = -1
            side_cap_back = -1

            if d.enable_glass:
                side_cap_front = 9
                side_cap_back = 12

            return WindowPanel(
                True,  # closed shape
                [1, 0, 0, 0, 1, 2, 2, 3, 3, 4, 3, 3, 4, 3, 3, 2, 2],  # x index
                [x0, x3, x2, x1, x4],  # unique x positions
                [y0, y4, y2, y3, y1, y1, y3, y3, y6 + joint_h, y6, y6, y5, y5, y5 - joint_h, y4, y4, y0],
                materials,  # materials
                side_cap_front=side_cap_front,
                side_cap_back=side_cap_back  # cap index
            )

    def find_handle(self, o):
        for child in o.children:
            if 'archipack_handle' in child:
                return child
        return None

    def update_handle(self,
                      context,
                      o,
                      d,
                      handle_model,
                      handle_altitude,
                      pivot,
                      size):
        handle = self.find_handle(o)
        if handle is None:
            m = bpy.data.meshes.new("Handle")
            handle = create_handle(context, o, m)
            # MaterialUtils.add_handle_materials(handle)
        if handle_model == 1:
            verts, faces = window_handle_vertical_01(1)
        else:
            verts, faces = window_handle_vertical_02(1)
        self.link_materials(context, o, handle)
        handle.location = (pivot * (size.x - 0.4 * d.panel_x), 0, handle_altitude)
        mat = d.id_mat(MAT_HANDLE)
        matids = [mat] * len(faces)
        bmed.buildmesh(handle, verts, faces, matids)
        self.shade_smooth(context, handle, 0.20944)

    def remove_handle(self, context, o):
        handle = self.find_handle(o)
        if handle is not None:
            self.delete_object(context, handle)

    def update(self,
               context,
               o,
               d,
               origin,
               center,
               radius,
               size,
               pivot,
               shape,
               fixed,
               handle,
               handle_model,
               handle_altitude,
               side_material):

        if handle == 'NONE':
            self.remove_handle(context, o)
        else:
            self.update_handle(context,
                               o,
                               d,
                               handle_model,
                               handle_altitude,
                               pivot,
                               size)

        mat_glass = d.id_mat(MAT_GLASS)
        window = self.window(d, fixed, side_material)

        offset = Vector((0, 0, 0))
        verts = window.vertices(d.curve_steps, offset, center, origin, size,
                                radius, d.angle_y, pivot, shape_z=None, path_type=shape)

        faces = window.faces(d.curve_steps, path_type=shape)
        matids = window.mat(d.curve_steps, mat_glass, mat_glass, path_type=shape)
        uvs = window.uv(d.curve_steps, center, origin, size,
                        radius, d.angle_y, pivot, 0, d.frame_x, path_type=shape)
        col = random_color()
        vcolors = window.vcolors(d.curve_steps, col, path_type=shape)
        bmed.buildmesh(o, verts, faces, matids, uvs, vcolors=vcolors)
        self.shade_smooth(context, o, 0.20944)

        # self.restore_context(context)


class archipack_window_panelrow(Archipacki18n, PropertyGroup):
    # panels: CollectionProperty(type=archipack_window_panel)

    width: FloatVectorProperty(
        name="Width",
        description="Leaf width in percent of overall width",
        min=0.1,
        max=100.0,
        default=[
            50, 50, 50, 50, 50, 50, 50, 50,
            50, 50, 50, 50, 50, 50, 50, 50,
            50, 50, 50, 50, 50, 50, 50, 50,
            50, 50, 50, 50, 50, 50, 50
        ],
        size=31,
        update=update
    )
    fixed: BoolVectorProperty(
        name="Fixed",
        description="Fixed leaf (generate a smallest frame)",
        default=[
            False, False, False, False, False, False, False, False,
            False, False, False, False, False, False, False, False,
            False, False, False, False, False, False, False, False,
            False, False, False, False, False, False, False, False
        ],
        size=32,
        update=update
    )

    cols: IntProperty(
        name="Window leaf",
        description="Number of leaf on this row (up to 32)",
        min=1,
        max=32,
        default=2,
        get=get_cols, set=set_cols
    )
    n_cols: IntProperty(
        name="Window leaf",
        description="store number of leaf, internal use only to avoid infinite recursion",
        min=1,
        max=32,
        default=2,
        update=update
    )
    height: FloatProperty(
        name="Height",
        min=0.1,
        default=1.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        name="auto_update",
        description="Disable auto update to avoid infinite recursion",
        default=True
    )

    @property
    def parent_data(self):
        return self.id_data.archipack_window[0]

    def get_row(self, x, y):
        size = [Vector((x * self.width[w] / 100, y, 0)) for w in range(self.cols - 1)]
        sum_x = sum([s.x for s in size])
        size.append(Vector((x - sum_x, y, 0)))
        origin = []
        pivot = []
        ttl = 0
        xh = x / 2
        n_center = len(size) / 2
        for i, sx in enumerate(size):
            ttl += sx.x
            if i < n_center:
                # pivot left
                origin.append(Vector((ttl - xh - sx.x, 0, 0)))
                pivot.append(1)
            else:
                # pivot right
                origin.append(Vector((ttl - xh, 0, 0)))
                pivot.append(-1)
        return size, origin, pivot

    def _set_width(self, cols):
        width = 100 / cols
        for i in range(cols - 1):
            self.width[i] = width

    def update(self, context):
        if self.auto_update:
            self.parent_data.update(context, childs_only=False)

    def draw(self, context, layout, box, last_row):
        # store parent at runtime to trigger update on parent
        row = box.row()
        self.draw_prop(context, layout, row, self, "cols")
        row = box.row()
        if not last_row:
            self.draw_prop(context, layout, row, self, "height")
        for i in range(self.cols - 1):
            row = box.row()
            self.draw_prop(context, layout, row, self, "width", text="Col", postfix=str(i + 1), index=i)
            self.draw_prop(context, layout, row, self, "fixed", text="fixed", index=i)
        row = box.row()
        self.draw_label(context, layout, row, "Col", postfix=str(self.cols))
        self.draw_prop(context, layout, row, self, "fixed", text="fixed", index=(self.cols - 1))


class archipack_window_shutter(ArchipackObject, PropertyGroup):

    def shutter(self, d):
        border = d.shutter_border
        chanfer = 0.004
        spacing = 0.75 * border
        x0 = 0
        x1 = border - 0.5 * spacing
        x3 = chanfer
        w = 0.5 * d.shutter_depth
        # offset pivot point on outside part
        y0 = 0
        y1 = y0 + w
        y2 = y1 - 0.5 * w
        y3 = y1 - chanfer
        y4 = y0 + chanfer

        # profil
        # p ______   y1
        #  /         y3
        # |
        # x          y2
        # |          y4
        #  \______   y0
        # x0 x3   x1
        #
        mat = d.id_mat(MAT_SHUTTER)
        side = WindowPanel(
            False,  # closed
            [2, 1, 0, 0, 0, 1, 2],  # x index
            [x0, x3, x1],
            [y0, y0, y4, y2, y3, y1, y1],
            [mat] * 7,  # materials
            closed_path=True,    #
            subdiv_x=0,
            subdiv_y=1
            )

        #     /   y2-y3
        #  __/    y1-y0
        #   x2 x3
        x2 = 0.5 * spacing
        x3 = x2 + chanfer
        y2 = y1 - chanfer
        y3 = y0 + chanfer

        face = WindowPanel(
            False,              # profil closed
            [0, 1, 2],          # x index
            [0, x2, x3],
            [y1, y1, y2],
            [mat] * 3,          # material index
            side_cap_front=2,   # cap index
            closed_path=True
            )

        back = WindowPanel(
            False,              # profil closed
            [0, 1, 2],          # x index
            [x3, x2, 0],
            [y3, y0, y0],
            [mat] * 3,          # material index
            side_cap_back=0,    # cap index
            closed_path=True
            )

        return side, face, back

    def hinge(self, altitude, offset, pivot, hinge_size, verts):

        # panel chanfer
        chanfer = 0.004

        seg = 12
        deg = 2 * pi / seg
        radius = hinge_size / 6
        x = 0
        y = 0
        z = altitude

        d = (offset + pivot * chanfer) / radius
        tM = Matrix([
            [radius, 0, 0, x],
            [0, radius, 0, y],
            [0, 0, hinge_size, z - 0.5 * hinge_size],
            [0, 0, 0, 1]
        ])
        if pivot < 0:
            verts.extend([tM @ Vector((sin(deg * a), cos(deg * a), 0)) for a in range(seg - 2)])
            verts.extend([
                tM @ Vector((d, cos(deg * (seg - 3)), 0)),
                tM @ Vector((d, 1, 0)),
            ])
        else:
            verts.extend([tM @ Vector((sin(deg * a), cos(deg * a), 0)) for a in range(3, seg + 1)])
            verts.extend([
                tM @ Vector((d, 1, 0)),
                tM @ Vector((d, cos(deg * (seg - 3)), 0)),
            ])

        if pivot < 0:
            verts.extend([tM @ Vector((sin(deg * a), cos(deg * a), 1)) for a in range(seg - 2)])
            verts.extend([
                tM @ Vector((d, cos(deg * (seg - 3)), 1)),
                tM @ Vector((d, 1, 1)),
            ])
        else:
            verts.extend([tM @ Vector((sin(deg * a), cos(deg * a), 1)) for a in range(3, seg + 1)])
            verts.extend([
                tM @ Vector((d, 1, 1)),
                tM @ Vector((d, cos(deg * (seg - 3)), 1)),
            ])

    def verts(self, d, shutter, size, center, origin, pivot, radius, offset, hinge_enable, hinge_count, hinge_space):

        side, face, back = shutter
        border = d.shutter_border
        spacing = 0.75 * border

        x1 = border - 0.5 * spacing
        _offset = Vector((offset, 0, 0))
        verts = side.vertices(d.curve_steps, _offset, center, origin, size,
            radius, d.angle_y, pivot, shape_z=None, path_type=d.shape)

        p_radius = radius.copy()
        p_radius.x -= x1
        p_radius.y -= x1

        p_size = Vector((size.x - 2 * x1, (size.y - 2 * x1) / 2, 0))

        for j in range(2):
            if j < 1:
                shape = 'RECTANGLE'
            else:
                shape = d.shape

            _offset = Vector((
                offset + pivot * x1,
                p_size.y * j + x1,
                0))

            _origin = Vector((
                origin.x + pivot * x1,
                p_size.y * j + x1,
                0))

            verts += face.vertices(d.curve_steps, _offset, center, _origin,
                p_size, p_radius, d.angle_y, pivot, shape_z=None, path_type=shape)
            verts += back.vertices(d.curve_steps, _offset, center, _origin,
                p_size, p_radius, d.angle_y, pivot, shape_z=None, path_type=shape)

        if hinge_enable:
            z0 = 0.15
            dz = (hinge_space - 2 * z0) / (hinge_count - 1)
            for j in range(hinge_count):
                self.hinge(z0 + dz * j, offset, pivot, d.shutter_hinge, verts)

        return verts

    def faces(self, d, shutter, hinge_enable, hinge_count):

        side, face, back = shutter

        faces = side.faces(d.curve_steps, path_type=d.shape)
        faces_offset = side.n_verts(d.curve_steps, path_type=d.shape)

        for j in range(2):
            if j < 1:
                shape = 'RECTANGLE'
            else:
                shape = d.shape
            faces += face.faces(d.curve_steps, path_type=shape, offset=faces_offset)
            faces_offset += face.n_verts(d.curve_steps, path_type=shape)
            faces += back.faces(d.curve_steps, path_type=shape, offset=faces_offset)
            faces_offset += back.n_verts(d.curve_steps, path_type=shape)

        if hinge_enable:
            seg = 12
            for j in range(hinge_count):
                faces.append(tuple([faces_offset + i + seg for i in range(seg - 1, -1, -1)]))
                faces.append(tuple([faces_offset + i for i in range(seg)]))
                faces.extend([tuple([faces_offset + i + f for f in (1, 0, seg, seg + 1)]) for i in range(seg - 1)])
                faces.append((
                    faces_offset,
                    faces_offset + seg - 1,
                    faces_offset + 2 * seg - 1,
                    faces_offset + seg
                    ))

                faces_offset += 2 * seg

        return faces

    def uvs(self, d, shutter, size, center, origin, pivot, radius, hinge_enable, hinge_count):

        side, face, back = shutter
        border = d.shutter_border
        spacing = 0.75 * border
        x1 = border - 0.5 * spacing

        uvs = side.uv(d.curve_steps,
            center,
            origin,
            size,
            radius,
            d.angle_y,
            pivot,
            border, 0,
            path_type=d.shape)

        p_radius = radius.copy()
        p_radius.x -= x1
        p_radius.y -= x1
        p_size = Vector((size.x - 2 * x1, (size.y - 2 * x1) / 2, 0))

        for j in range(2):
            if j < 1:
                shape = 'RECTANGLE'
            else:
                shape = d.shape
            _origin = Vector((
                origin.x + pivot * x1,
                p_size.y * j + x1,
                0))
            uvs += face.uv(d.curve_steps, center, _origin, p_size,
                p_radius, d.angle_y, pivot, 0, 0, path_type=shape)
            uvs += back.uv(d.curve_steps, center, _origin, p_size,
                p_radius, d.angle_y, pivot, 0, 0, path_type=shape)

        if hinge_enable:
            seg = 12
            deg = 2 * pi / seg
            _radius = 0.005
            x = 0
            y = 0
            z = 0
            _size = 0.04
            tM = Matrix([
                [_radius, 0, 0, x],
                [0, _radius, 0, y],
                [0, 0, _size, z],
                [0, 0, 0, 1]
            ])

            for j in range(hinge_count):
                uvs.append(tuple([(tM @ Vector((sin(deg * a), cos(deg * a), 0))).to_2d() for a in range(seg)]))
                uvs.append(tuple([(tM @ Vector((sin(deg * a), cos(deg * a), 0))).to_2d() for a in range(seg)]))
                uvs.extend([[(0, 0), (0, 1), (1, 1), (1, 0)] for i in range(seg)])

        return uvs

    def matids(self, d, shutter, hinge_enable, hinge_count):

        side, face, back = shutter
        ms, mh = d.id_mat(MAT_SHUTTER), d.id_mat(MAT_HINGE)
        mat = side.mat(d.curve_steps, ms, ms, path_type=d.shape)
        for j in range(2):
            if j < 1:
                shape = 'RECTANGLE'
            else:
                shape = d.shape
            mat += face.mat(d.curve_steps, ms, ms, path_type=shape)
            mat += back.mat(d.curve_steps, ms, ms, path_type=shape)

        if hinge_enable:
            for j in range(hinge_count):
                seg = 12
                mat.extend([mh] * (seg + 2))
        return mat

    def vcolors(self, d, shutter, hinge_enable, hinge_count):

        side, face, back = shutter
        col = random_color()
        vcolors = side.vcolors(d.curve_steps, col, path_type=d.shape)
        for j in range(2):
            if j < 1:
                shape = 'RECTANGLE'
            else:
                shape = d.shape
            col = random_color()
            vcolors += face.vcolors(d.curve_steps, col, path_type=shape)
            vcolors += back.vcolors(d.curve_steps, col, path_type=shape)

        if hinge_enable:
            for j in range(hinge_count):
                seg = 12
                vcolors.extend([col] * (seg + 2))
        return vcolors

    def update(self,
            context,
            o,
            d,
            origin,
            center,
            radius,
            size,
            pivot,
            offset,
            hinge_enable,
            hinge_count,
            hinge_space):

        shutter = self.shutter(d)
        verts = self.verts(d, shutter, size, center, origin, pivot, radius, offset,
                           hinge_enable, hinge_count, hinge_space)
        faces = self.faces(d, shutter, hinge_enable, hinge_count)
        matids =  self.matids(d, shutter, hinge_enable, hinge_count)
        uvs = self.uvs(d, shutter, size, center, origin, pivot, radius,
                       hinge_enable, hinge_count)
        vcolors = self.vcolors(d, shutter, hinge_enable, hinge_count)
        bmed.buildmesh(o, verts, faces, matids, uvs, vcolors=vcolors)
        self.shade_smooth(context, o, 0.20944)


class archipack_window_bend(PropertyGroup):
    x: FloatProperty()
    a: FloatProperty()


class archipack_window(ArchipackObject, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display main settings', 'NONE', 0),
            ('PARTS', 'Leaf', 'Display leaf settings', 'NONE', 1),
            ('SUB', 'Parts', 'Display components settings', 'NONE', 2),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 3)
        ),
        default='MAIN',
    )
    x: FloatProperty(
        name='Width',
        min=0.1,
        default=100.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Width', update=update
    )
    y: FloatProperty(
        name='Depth',
        min=0.05,
        default=0.20, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Depth', update=update,
    )
    z: FloatProperty(
        name='Height',
        min=0.1,
        default=1.2, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Height', update=update,
    )
    angle_y: FloatProperty(
        name='Angle',
        unit='ROTATION',
        subtype='ANGLE',
        min=-1.5, max=1.5,
        default=0, precision=5,
        description='Angle', update=update,
    )
    radius: FloatProperty(
        name='Radius',
        min=0.1,
        default=2.5, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Radius', update=update,
    )
    elipsis_b: FloatProperty(
        name='Ellipsis',
        min=0.1,
        default=0.5, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Ellipsis vertical size', update=update,
    )
    altitude: FloatProperty(
        name='Altitude',
        default=1.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Altitude', update=update,
    )
    offset: FloatProperty(
        name='Offset',
        default=0.1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Offset', update=update,
    )
    frame_y: FloatProperty(
        name='Depth',
        min=0,
        default=0.06, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Frame depth', update=update,
    )
    frame_x: FloatProperty(
        name='Width',
        min=0,
        default=0.06, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Frame width', update=update,
    )
    frame_overflow: FloatProperty(
        name='Overflow side',
        min=0,
        default=0.06, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Frame overflow', update=update,
    )
    finishing_out: FloatProperty(
        name='Finishing thickness',
        min=0,
        default=0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Thickness of outside wall finishing'
    )
    finishing_int: FloatProperty(
        name='Finishing thickness',
        min=0,
        default=0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Thickness of outside wall finishing'
    )
    panel_x: FloatProperty(
        name='Width',
        min=0,
        default=0.06, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='panel width', update=update,
    )
    panel_y: FloatProperty(
        name='Depth',
        min=0,
        default=0.06, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='panel depth', update=update,
    )
    out_frame: BoolProperty(
        description="Create a frame outside",
        name="Out frame",
        default=False, update=update,
    )
    out_frame_y: FloatProperty(
        name='Side Depth',
        min=0.001,
        default=0.02, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='frame side depth', update=update,
    )
    out_frame_y2: FloatProperty(
        name='Front Depth',
        min=0.001,
        default=0.02, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='frame front depth', update=update,
    )
    out_frame_x: FloatProperty(
        name='Front Width',
        min=0.0,
        default=0.1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='frame width set to 0 disable front frame', update=update,
    )
    out_frame_offset: FloatProperty(
        name='Offset',
        min=0.0,
        default=0.0, precision=3, step=0.1,
        unit='LENGTH', subtype='DISTANCE',
        description='frame offset', update=update,
    )
    out_tablet_enable: BoolProperty(
        description="Create a sill outside",
        name="Sill out",
        default=True, update=update,
    )
    out_tablet_x: FloatProperty(
        name='Width',
        min=0.0,
        default=0.04, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='tablet width', update=update,
    )
    out_tablet_y: FloatProperty(
        name='Depth',
        min=0.001,
        default=0.04, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='tablet depth', update=update,
    )
    out_tablet_z: FloatProperty(
        name='Height',
        min=0.001,
        default=0.03, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='tablet height', update=update,
    )


    in_tablet_enable: BoolProperty(
        description="Create a sill inside",
        name="Sill in",
        default=True, update=update,
    )
    in_tablet_x: FloatProperty(
        name='Width',
        min=0.0,
        default=0.04, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='tablet width', update=update,
    )
    in_tablet_y: FloatProperty(
        name='Depth',
        min=0.001,
        default=0.04, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='tablet depth', update=update,
    )
    in_tablet_z: FloatProperty(
        name='Height',
        min=0.001,
        default=0.03, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='tablet height', update=update,
    )
    blind_inside: BoolProperty(
        default=False,
        name="Blind inside",
        description="Generate a blind inside",
        update=update
    )
    blind_outside: BoolProperty(
        default=False,
        name="Blind outside",
        description="Generate a blind outside",
        update=update
    )

    curtain_sill_enable: BoolProperty(
        description="Create a sill",
        name="Sill",
        default=False, update=update,
    )
    curtain_sill_y: FloatProperty(
        name='Depth',
        min=0.001,
        default=0.2, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Sill curtain depth', update=update,
    )
    curtain_sill_z: FloatProperty(
        name='Height',
        min=0.001,
        default=0.03, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Sill curtain height', update=update,
    )
    curtain_sill_alt: FloatProperty(
        name='Altitude',
        default=0.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Sill curtain altitude', update=update,
    )
    curtain_sill_x: FloatProperty(
        name='Over width',
        default=0.05, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Sill curtain over width', update=update,
    )
    curtain_rod_enable: BoolProperty(
        name="Rod",
        default=False, update=update,
    )
    curtain_rod_y: FloatProperty(
        name='Depth',
        min=0.001,
        default=0.15, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Distance from wall', update=update,
    )
    curtain_rod_radius: FloatProperty(
        name='Radius',
        min=0.001,
        default=0.015, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Rod radius', update=update,
    )
    curtain_rod_alt: FloatProperty(
        name='Altitude',
        default=-0.05, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Rod altitude', update=update,
    )
    curtain_rod_x: FloatProperty(
        name='Over width',
        default=0.05, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Rod over width', update=update,
    )
    rows: CollectionProperty(type=archipack_window_panelrow)
    n_rows: IntProperty(
        name="Number of rows",
        min=1,
        max=32,
        default=1, update=update,
    )
    curve_steps: IntProperty(
        name="Resolution",
        min=6,
        max=128,
        default=16, update=update,
    )
    idmat: IntVectorProperty(
        default=[
            2,
            1, 0,
            1, 4,
            3, 7,
            5, 3,
            5, 6,
            1, 3
        ],
        size=13
    )

    mat_curtain_rod: EnumProperty(
        options={'SKIP_SAVE'},
        name="Rod curtain",
        description="Material index of curtain rod",
        items=mat_enum,
        get=mat_index_getter(MAT_ROD_CURTAIN),
        set=mat_index_setter(MAT_ROD_CURTAIN),
        update=update
    )
    mat_curtain_sill: EnumProperty(
        options={'SKIP_SAVE'},
        name="Sill curtain",
        description="Material index of curtain sill",
        items=mat_enum,
        get=mat_index_getter(MAT_SILL_CURTAIN),
        set=mat_index_setter(MAT_SILL_CURTAIN),
        update=update
    )
    mat_frame_inside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Frame in",
        description="Material index of frame inside",
        items=mat_enum,
        get=mat_index_getter(MAT_FRAME_INSIDE),
        set=mat_index_setter(MAT_FRAME_INSIDE),
        update=update
    )

    mat_frame_outside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Frame out",
        description="Material index of frame outside",
        items=mat_enum,
        get=mat_index_getter(MAT_FRAME_OUTSIDE),
        set=mat_index_setter(MAT_FRAME_OUTSIDE),
        update=update
    )

    mat_sill_in: EnumProperty(
        options={'SKIP_SAVE'},
        name="Sill in",
        description="Material index of sill in",
        items=mat_enum,
        get=mat_index_getter(MAT_SILL_IN),
        set=mat_index_setter(MAT_SILL_IN),
        update=update
    )

    mat_sill_out: EnumProperty(
        options={'SKIP_SAVE'},
        description="Material index of sill out",
        name="Sill out",
        items=mat_enum,
        get=mat_index_getter(MAT_SILL_OUT),
        set=mat_index_setter(MAT_SILL_OUT),
        update=update
    )

    mat_spoiler: EnumProperty(
        options={'SKIP_SAVE'},
        description="Material index of spoiler",
        name="Spoiler",
        items=mat_enum,
        get=mat_index_getter(MAT_SPOILER),
        set=mat_index_setter(MAT_SPOILER),
        update=update
    )

    mat_shutter: EnumProperty(
        options={'SKIP_SAVE'},
        description="Material index of shutters",
        name="Shutter",
        items=mat_enum,
        get=mat_index_getter(MAT_SHUTTER),
        set=mat_index_setter(MAT_SHUTTER),
        update=update
    )

    mat_handle: EnumProperty(
        options={'SKIP_SAVE'},
        description="Material index of handle",
        name="Handle",
        items=mat_enum,
        get=mat_index_getter(MAT_HANDLE),
        set=mat_index_setter(MAT_HANDLE),
        update=update
    )

    mat_hinge: EnumProperty(
        options={'SKIP_SAVE'},
        description="Material index of hinge",
        name="Hinge",
        items=mat_enum,
        get=mat_index_getter(MAT_HINGE),
        set=mat_index_setter(MAT_HINGE),
        update=update
    )

    mat_glass: EnumProperty(
        options={'SKIP_SAVE'},
        description="Material index of glass",
        name="Glass",
        items=mat_enum,
        get=mat_index_getter(MAT_GLASS),
        set=mat_index_setter(MAT_GLASS),
        update=update
    )

    mat_glass_frame: EnumProperty(
        options={'SKIP_SAVE'},
        description="Material index of glass frame",
        name="Glass frame",
        items=mat_enum,
        get=mat_index_getter(MAT_GLASS_FRAME),
        set=mat_index_setter(MAT_GLASS_FRAME),
        update=update
    )

    mat_joint: EnumProperty(
        options={'SKIP_SAVE'},
        description="Material index of glass joints",
        name="Joint",
        items=mat_enum,
        get=mat_index_getter(MAT_JOINT),
        set=mat_index_setter(MAT_JOINT),
        update=update
    )

    # TODO: take a look at this as materials are wall's ones
    mat_hole_outside: IntProperty(
        name="Outside",
        description="Material index of wall for outside part of the hole",
        min=0,
        max=128,
        default=1
    )

    mat_hole_inside: IntProperty(
        name="Inside",
        description="Material index of wall for inside part of the hole",
        min=0,
        max=128,
        default=0
    )
    mat_finish_inside: IntProperty(
        name="Finish Inside",
        description="Material index of wall for inside part of the hole",
        min=0,
        max=128,
        default=0
    )
    mat_finish_outside: IntProperty(
        name="Finish Outside",
        description="Material index of wall for inside part of the hole",
        min=0,
        max=128,
        default=0
    )
    window_shape: EnumProperty(
        name='Shape',
        items=(
            ('RECTANGLE', 'Rectangle', '', 0),
            ('ROUND', 'Top Round', '', 1),
            ('ELLIPSIS', 'Top elliptic', '', 2),
            ('QUADRI', 'Top oblique', '', 3),
            ('CIRCLE', 'Full circle', '', 4)
        ),
        default='RECTANGLE', update=update,
    )
    window_type: EnumProperty(
        name='Type',
        items=(
            ('FLAT', 'Swing window', '', 0),
            ('RAIL', 'Rail window', '', 1),
            ('HUNG', 'Hung window', '', 2)
        ),
        default='FLAT', update=update,
    )
    enable_glass: BoolProperty(
        name="Enable glass",
        default=True,
        update=update
    )
    warning: BoolProperty(
        options={'SKIP_SAVE'},
        name="Warning",
        default=False
    )
    handle_enable: BoolProperty(
        name='Handle',
        default=True, update=update_childs,
    )
    handle_altitude: FloatProperty(
        name="Altitude",
        min=0,
        default=1.5, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='handle altitude', update=update_childs,
    )
    shutter_enable: BoolProperty(
        name="Shutters",
        default=False, update=update,
    )
    shutter_left: IntProperty(
        name="Left",
        description="Number of shutters on left side",
        default=1,
        update=update
    )
    shutter_right: IntProperty(
        name="Right",
        description="Number of shutters on right side",
        default=1,
        update=update
    )
    shutter_border: FloatProperty(
        name='Border',
        min=0,
        default=0.1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Shutter panels borders',
        update=update
    )
    shutter_depth: FloatProperty(
        name='Depth',
        min=0.01,
        default=0.04, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Shutter panels depth',
        update=update
    )
    shutter_hinge: FloatProperty(
        name='Hinge',
        min=0.001,
        default=0.03, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Shutter hinge size',
        update=update
    )
    hole_margin: FloatProperty(
        name='Hole margin',
        min=0.0,
        default=0.1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='how much hole surround wall'
    )

    flip: BoolProperty(
        default=False,
        update=update,
        description='flip outside and outside material of hole'
    )

    auto_mat: BoolProperty(
        name="Auto materials",
        default=True
    )
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update
    )
    portal: BoolProperty(
        default=False,
        name="Portal",
        description="Makes this window a light portal, enabling better photon gathering during render",
        update=update
    )
    portal_energy: FloatProperty(
        default=0.3,
        min=0, step=1, precision=5,
        name="Energy",
        description="Portal lamp energy for eevee",
        update=update
    )
    curtain_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Curtain",
        default=False
    )
    shutter_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Shutter",
        default=False
    )
    in_tablet_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Sill in",
        default=False
    )
    out_tablet_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Sill out",
        default=False
    )
    frame_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Window Frame",
        default=False
    )
    out_frame_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Frame out",
        default=False
    )
    panel_expand: BoolProperty(
        options={'SKIP_SAVE'},
        name="Panel frame",
        default=False
    )
    # not exposed, rail profile rail and front depht
    rail_depth:FloatProperty(
        name='Rail depth',
        min=0.0,
        default=0.005, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Rail window rails depth'
    )

    rail_frame_depth:FloatProperty(
        name='Frame depth',
        min=0.0,
        default=0.01, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Rail window out frame depth before panel'
    )

    bend: CollectionProperty(type=archipack_window_bend)

    @property
    def shape(self):
        if self.window_type in {'RAIL', 'HUNG'}:
            return 'RECTANGLE'
        else:
            return self.window_shape

    @property
    def window(self):
        # Flat window frame profil
        #  ___        y1
        # |   |__
        # |      |    y2
        # |______|    y0
        #
        x0 = 0
        x1 = -x0 - self.frame_x
        x2 = x0 + 0.5 * self.frame_x
        y0 = self.hole_center_y
        y2 = y0 + 0.5 * self.frame_y
        mat_in, mat_out, mat_side = self.id_mat(MAT_FRAME_INSIDE), \
                                    self.id_mat(MAT_FRAME_OUTSIDE), \
                                    self.id_mat(MAT_HINGE)
        if self.window_type == 'FLAT':
            y1 = y0 + self.frame_y
            return WindowPanel(
                True,     # closed
                [0, 0, 1, 1, 2, 2],     # x index
                [x1, x0, x2],
                [y0, y1, y1, y2, y2, y0],
                [mat_in, mat_in, mat_in, mat_in, mat_out, mat_out]  # material index
                )
        else:
            # Rail window frame profil
            #  ________       y1
            # |      __|      y5
            # |     |__       y4
            # |      __|      y3
            # |     |____
            # |          |    y2
            # |__________|    y0
            # -x1   0 x3 x2
            out_depth = self.rail_frame_depth
            rail_depth = self.rail_depth
            # space not available for panels
            rails_depth = out_depth + 2 * rail_depth
            depth = max(self.frame_y, 2 * self.panel_y + rails_depth)

            x2 = x0 + 0.5 * self.frame_x
            x3 = x0 + 0.2 * self.frame_x
            x4 = x0 + 0.1 * self.frame_x
            y1 = y0 + depth
            yc = y0 + out_depth + 0.5 * rail_depth + 0.5 * (depth - rails_depth)
            y2 = y0 + out_depth
            y3 = yc - 0.5 * rail_depth
            y4 = yc + 0.5 * rail_depth
            y5 = y1 - rail_depth

            return WindowPanel(
                True,     # closed
                [0, 0, 2, 2, 1, 1, 2, 2, 1, 1, 3, 3],     # x index
                [x1, x4, x3, x2],
                [y0, y1, y1, y5, y5, y4, y4, y3, y3, y2, y2, y0],
                [mat_in, mat_in, mat_in, mat_side, mat_in, mat_side,
                 mat_side, mat_side, mat_out, mat_side, mat_out, mat_out]  # material index
                )

    @property
    def hole(self):
        # profil percement                          ____
        #   _____  y_inside          vertical ___|      x1
        #  |
        #  |__     y0                outside   ___
        #     |___ y_outside                       |____ x1-shape_z     inside
        # -x1 x0
        y0 = self.hole_center_y
        x1 = self.frame_x     # sur-largeur percement interieur

        # is half of finishing gap
        gap = 0.0001

        y = 0.5 * self.y + gap
        y_inside = y + self.finishing_int + self.hole_margin    # inside wall

        x0 = self._overflow - gap

        if self.out_frame:
            x0 -= min(self.frame_x, self.out_frame_y + self.out_frame_offset)

        outside_mat = self.mat_hole_outside
        inside_mat = self.mat_hole_inside
        in_finish_mat = self.mat_finish_inside
        out_finish_mat = self.mat_finish_outside
        # if self.flip:
        #    outside_mat, inside_mat = inside_mat, outside_mat

        y_outside = -y           # outside wall
        y_outside -= self.finishing_out + self.hole_margin

        if self.frame_overflow > 0 or (
                (self.out_frame and self.out_frame_offset > 0) or
                self.out_tablet_enable):
            return WindowPanel(
                False,     # closed
                [1, 1, 1, 0, 0, 0],     # x index
                [-x1, x0],
                [y_outside, -y, y0, y0, y, y_inside],
                [out_finish_mat, outside_mat, outside_mat, inside_mat, in_finish_mat],     # material index
                side_cap_front=5,     # cap index
                side_cap_back=0
                )
        else:
            # Hole without overflow
            return WindowPanel(
                False,     # closed
                [0, 0, 0, 0, 0],     # x index
                [x0],
                [y_outside, -y, y0, y, y_inside],
                [out_finish_mat, outside_mat, inside_mat, in_finish_mat],     # material index
                side_cap_front=4,     # cap index
                side_cap_back=0
                )

    @property
    def hole_center_y(self):
        """
        Hole center on y axis - this is the location of ground
        :return:
        """
        y = 0.5 * self.y - self.offset
        """
        if self.window_type == 'FLAT':
            y += self.frame_y
        else:
            y += max(self.frame_y, 2.05 * self.panel_y)
        """
        return y

    @property
    def inside_hole(self):
        # inside part of hole to setup line for floor
        # profil percement                          ____
        #   _____  y_inside          vertical ___|      x1
        #  |
        #  |__     y0                outside   ___
        #     |___ y_outside                       |____ x1-shape_z     inside
        # -x1 x0
        y0 = self.hole_center_y
        x1 = self.frame_x     # sur-largeur percement interieur
        y_inside = 0.5 * self.y + 0.01     # outside wall

        inside_mat = self.mat_hole_inside
        # if self.flip:
        #    outside_mat, inside_mat = inside_mat, outside_mat

        return WindowPanel(
            False,     # closed
            [0, 0],     # x index
            [-x1],
            [y0, y_inside],
            [inside_mat, inside_mat],     # material index
            side_cap_front=1,     # cap index
            side_cap_back=0
            )
    
    @property
    def frame(self):
        # profil cadre
        #     ___     y0
        #  __|   |
        # |      |    y2
        # |______|    y1
        # x1 x2  x0
        y2 = -0.5 * self.y
        y0 = self.hole_center_y
        y1 = y2 - self.out_frame_y2 - self.finishing_out
        x0 = 0.001   # -min(self.frame_x - 0.001, self.out_frame_offset)
        x1 = x0 - self.out_frame_x
        x2 = x0 - self.out_frame_y
        # y = depth
        # x = width
        mat = self.id_mat(MAT_FRAME_OUTSIDE)
        if self.out_frame_x <= self.out_frame_y:
            if self.out_frame_x == 0:
                pts_y = [y2, y0, y0, y2]
            else:
                pts_y = [y1, y0, y0, y1]
            return WindowPanel(
                True,     # closed profil
                [0, 0, 1, 1],     # x index
                [x2, x0],
                pts_y,
                [mat] * 4,     # material index
                closed_path=bool(self.shape == 'CIRCLE')  # closed path
                )
        else:
            return WindowPanel(
                True,     # closed profil
                [0, 0, 1, 1, 2, 2],     # x index
                [x1, x2, x0],
                [y1, y2, y2, y0, y0, y1],
                [mat] * 6,     # material index
                closed_path=bool(self.shape == 'CIRCLE')   # closed path
                )

    @property
    def out_tablet(self):
        # profil tablette
        #  __  y0
        # |  | y2
        # | / y3
        # |_| y1
        # x0 x2 x1
        y0 = self.hole_center_y
        y1 = -0.5 * self.y - self.out_tablet_y - self.finishing_out
        y2 = y0 - 0.01
        y3 = y2 - 0.04
        x2 = 0.001
        x0 = x2 - self.out_tablet_z
        x1 = x2 + 0.3 * self.frame_x
        # y = depth
        # x = width1
        ms, mp = self.id_mat(MAT_SILL_OUT), self.id_mat(MAT_SPOILER)
        return WindowPanel(
            True,     # closed profil
            [1, 1, 2, 2, 0, 0],     # x index
            [x0, x2, x1],
            [y1, y3, y2, y0, y0, y1],
            [ms, mp, mp, ms, ms, ms],     # material index
            closed_path=False           # closed path
            )

    @property
    def in_tablet(self):
        # profil tablette
        #  __  y0
        # |  |
        # |  |
        # |__| y1
        # x0  x1
        y0 = self.hole_center_y + self.frame_y
        y1 = 0.5 * self.y + self.in_tablet_y
        if self.window_type == 'RAIL':
            y0 = 0.5 * self.y - self.offset + max(self.frame_y, 2.05 * self.panel_y)
        x0 = -self.frame_x
        x1 = min(x0 + self.in_tablet_z, x0 + self.frame_x - 0.001)
        # y = depth
        # x = width1
        mat = self.id_mat(MAT_SILL_IN)
        return WindowPanel(
            True,     # closed profil
            [0, 0, 1, 1],     # x index
            [x0, x1],
            [y1, y0, y0, y1],
            [mat] * 4,     # material index
            closed_path=False           # closed path
            )

    @property
    def curtain_tablet(self):
        # profil tablette
        #  __  y0
        # |  |
        # |  |
        # |__| y1
        # x0  x1
        y0 = 0.5 * self.y
        y1 = y0 + self.curtain_sill_y

        x0 = self.altitude + self.z + self.curtain_sill_alt + min(self.frame_overflow, self.frame_x)
        x1 = x0 + self.curtain_sill_z
        # y = depth
        # x = width1
        mat = self.id_mat(MAT_SILL_CURTAIN)
        return WindowPanel(
            True,  # closed profil
            [0, 0, 1, 1],  # x index
            [x0, x1],
            [y1, y0, y0, y1],
            [mat] * 4,  # material index
            closed_path=False  # closed path
        )

    @property
    def curtain_rod(self):
        # profil rod
        #  __  y0
        # |  |
        # |  |
        # |__| y1
        # x0  x1
        y0 = 0.5 * self.y
        yc = y0 + self.curtain_rod_y
        xc = self.altitude + self.z + self.curtain_rod_alt + min(self.frame_overflow, self.frame_x)
        r = self.curtain_rod_radius
        da = 2 * pi / self.curve_steps
        mat = self.id_mat(MAT_ROD_CURTAIN)
        rod = WindowPanel(
            True,  # closed profil
            [i for i in range(self.curve_steps)],  # x index
            [xc + r * sin(a * da) for a in range(self.curve_steps)],
            [yc + r * cos(a * da) for a in range(self.curve_steps)],
            [mat] * self.curve_steps,  # material index
            closed_path=False  # closed path
        )
        side = WindowPanel(
            False,
            [0, 0],
            [r],
            [y0, yc],
            [mat],
            closed_path=True
        )
        return rod, side

    @property
    def vertical_space(self):
        """
            avaliable space for hinges
        """
        center, origin, size, radius = self.get_radius(self.x, self.z)
        offset = Vector((0, self.altitude - self._overflow, 0))
        left, right = self.window.avaliable_vertical_space(self.curve_steps, offset, center, origin,
            size, radius, self.angle_y, 0, shape_z=None, path_type=self.shape)
        return left, right

    def find_blind(self, o, inside):
        for child in o.children:
            if child.type == 'MESH' and 'archipack_blind' in child.data:
                loc = o.matrix_world.inverted() @ child.matrix_world.translation
                if inside:
                    if loc.y > 0:
                        return child
                elif loc.y < 0:
                    return child
        return None

    def update_blind(self, context, o, inside):

        blind = self.find_blind(o, inside)
        if inside:
            enabled = self.blind_inside
            overflow = 2 * self.frame_overflow
            style = 'VENITIAN'
            # half width + handle
            y = 0.02 + 0.08
        else:
            enabled = self.blind_outside
            overflow = 0
            style = 'SLAT'
            y = -0.5 * (self.y - self.offset)

        if enabled:

            x = self.x + overflow
            z = self.z + overflow
            a = self.altitude - 0.5 * overflow

            if blind is None:
                # @TODO: contextless create
                with ensure_select_and_restore(context, o, [o]):
                    bpy.ops.archipack.blind('INVOKE_DEFAULT',
                        x=x,
                        z=z,
                        offset_y=y,
                        altitude=a,
                        frame_enable=inside,
                        frame_depth=2 * y,
                        frame_height=0.04,
                        style=style,
                        randomize=True
                        )
                    blind = context.object
                    blind.parent = o

            else:
                self.select_object(context, blind, True)
                d = blind.data.archipack_blind[0]
                if (d.x != x or
                        d.z != z or
                        d.offset_y != y or
                        d.altitude != a):
                    d.auto_update = False
                    d.x = x
                    d.z = z
                    d.offset_y = y
                    d.altitude = a
                    d.auto_update = True

            self.unselect_object(context, blind)

            if inside:
                tM = Matrix([
                    [-1, 0, 0, 0],
                    [0, -1, 0, 0.5 * self.y],
                    [0, 0, 1, 0],
                    [0, 0, 0, 1]
                ])
            else:
                tM = Matrix([
                    [1, 0, 0, 0],
                    [0, 1, 0, -0.5 * self.y],
                    [0, 0, 1, 0],
                    [0, 0, 0, 1]
                ])
            blind.matrix_world = o.matrix_world @ tM
            self.select_object(context, o, True)
        else:
            self.delete_object(context, blind)

    def find_portal(self, o):

        for child in o.children:
            if child.type == 'LIGHT':
                return child
        return None

    def update_portal(self, context, o):

        lamp = self.find_portal(o)
        if self.portal:
            if lamp is None:
                d = bpy.data.lights.new(name="Portal", type='AREA')
                lamp = bpy.data.objects.new("Portal", d)
                self.link_object_to_scene(context, lamp, layer_name="Lights")
                lamp.parent = o

            d = lamp.data
            d.shape = 'RECTANGLE'
            d.size = self.x
            d.size_y = self.z

            # eevee
            d.energy = self.portal_energy
            d.use_contact_shadow = True
            # cycles
            d.cycles.is_portal = True

            tM = Matrix([
                [1, 0, 0, 0],
                [0, 0, -1, -0.5 * self.y],
                [0, 1, 0, 0.5 * self.z + self.altitude],
                [0, 0, 0, 1]
            ])
            lamp.matrix_world = o.matrix_world @ tM

        else:
            self.delete_object(context, lamp)

    def get_generator(self, o=None):
        return OpeningGenerator(self, o, "WINDOW")
        
    def setup_manipulators(self):
        if len(self.manipulators) == 4:
            return
        s = self.manipulators.add()
        s.prop1_name = "x"
        s.prop2_name = "x"
        s.type_key = "SNAP_SIZE_LOC"
        s = self.manipulators.add()
        s.prop1_name = "y"
        s.prop2_name = "y"
        s.type_key = "SNAP_SIZE_LOC"
        s = self.manipulators.add()
        s.prop1_name = "z"
        s.normal = Vector((0, 1, 0))
        s = self.manipulators.add()
        s.prop1_name = "altitude"
        s.normal = Vector((0, 1, 0))

    def remove_childs(self, context, o, to_remove):
        for child in o.children:
            if to_remove < 1:
                return
            if archipack_window_panel.filter(child):
                to_remove -= 1
                self.delete_object(context, child)

    def remove_shutters(self, context, childs, to_remove):
        for child in childs:
            if to_remove < 1:
                return
            to_remove -= 1
            self.delete_object(context, child)

    def update_rows(self, context, o):
        # remove rows
        for i in range(len(self.rows), self.n_rows, -1):
            self.rows.remove(i - 1)

        # add rows
        for i in range(len(self.rows), self.n_rows):
            self.rows.add()

        # wanted childs
        if self.shape == 'CIRCLE':
            w_childs = 1
        elif self.window_type == 'RAIL':
            w_childs = self.rows[0].cols
        else:
            w_childs = sum([row.cols for row in self.rows])

        # real childs
        childs = self.get_childs_panels(context, o)
        n_childs = len(childs)

        # remove child
        if n_childs > w_childs:
            self.remove_childs(context, o, n_childs - w_childs)

    def get_childs_panels(self, context, o):
        return [child for child in o.children if archipack_window_panel.filter(child)]

    def get_childs_shutters(self, context, o, left_side):
        if left_side:
            return [child for child in o.children
                if (archipack_window_shutter.filter(child) and
                    child.location.x < 0)
                ]
        else:
            return [child for child in o.children
                if (archipack_window_shutter.filter(child) and
                    child.location.x > 0)
                ]

    """
    
                out_depth = self.rail_frame_depth
                rail_depth = self.rail_depth
                # space not available for panels
                rails_depth = out_depth + 2 * rail_depth
                depth = max(self.frame_y, 2 * self.panel_y + rails_depth)
    
                x2 = x0 + 0.5 * self.frame_x
                x3 = x0 + 0.2 * self.frame_x
                x4 = x0 + 0.1 * self.frame_x
                y1 = y0 + depth
                yc = y0 + out_depth + 0.5 * rail_depth + 0.5 * (depth - rails_depth)
                y2 = y0 + out_depth
                y3 = yc - 0.5 * rail_depth
                y4 = yc + 0.5 * rail_depth
                y5 = y1 - rail_depth
    """

    def adjust_size_for_rail(self, row, size, origin, pivot, materials):
        # rail window
        if len(size) > 1:
            size[0].x += 0.5 * self.panel_x
            size[-1].x += 0.5 * self.panel_x
        for i in range(1, len(size) - 1):
            size[i].x += 0.5 * self.panel_x
            origin[i].x += -0.25 * self.panel_x * pivot[i]

        out_depth = self.rail_frame_depth
        rail_depth = self.rail_depth
        # space not available for panels
        rails_depth = out_depth + 2 * rail_depth
        frame_y = max(self.frame_y, 2 * self.panel_y + rails_depth)
        # max panel depth
        panel_max = 0.5 * (frame_y - rails_depth)
        # delta frame rail axis / window axis

        for i, o in enumerate(origin):
            panel_y = min(panel_max, self.panel_y)

            if row.fixed[i]:
                panel_y *= 0.5
            frame_axis = - (0.5 * self.frame_y + panel_y) + out_depth + 0.5 * rail_depth + 0.5 * (frame_y - rails_depth)

            o.y = frame_axis + (1 - (i % 2)) * panel_y


        for i, o in enumerate(origin):
            materials[i] = (1 - (i % 2)) + 1
    
    def adjust_size_for_hung(self, n_rows, row_id, row, size, origin, offset, materials):
        # hung window
        out_depth = self.rail_frame_depth
        rail_depth = self.rail_depth
        # space not available for panels
        rails_depth = out_depth + 2 * rail_depth
        frame_y = max(self.frame_y, 2 * self.panel_y + rails_depth)
        # max panel depth
        panel_max = 0.5 * (frame_y - rails_depth)
        # delta frame rail axis / window axis

        # panel_y = min(self.panel_y, 0.5 * self.frame_y)
        if n_rows > 0:
            if row_id == 0 or row_id == n_rows:
                for i, s in enumerate(size):
                    delta = 0.5
                    if row.fixed[i]:
                        delta = 0.25
                        if row_id == n_rows:
                            origin[i].z = 0.25 * self.panel_x
                    s.y += delta * self.panel_x
            if 0 < row_id < n_rows:
                for i, s in enumerate(size):
                    delta = 0.5
                    if row.fixed[i]:
                        delta = 0
                        origin[i].z = 0.25 * self.panel_x
                    s.y += delta * self.panel_x

            if row_id > 0:
                offset.y -= 0.5 * self.panel_y

        for i, o in enumerate(origin):

            panel_y = min(panel_max, self.panel_y)
            if row.fixed[i]:
                panel_y *= 0.5
            frame_axis = - (0.5 * self.frame_y + panel_y) + out_depth + 0.5 * rail_depth + 0.5 * (frame_y - rails_depth)

            o.y = frame_axis + (1 - (row_id % 2)) * panel_y

        for i, o in enumerate(origin):
            materials[i] = (1 - (i % 2)) + 1

    def find_handle(self, o):
        for handle in o.children:
            if 'archipack_handle' in handle:
                return handle
        return None

    def _synch_childs(self, context, o, linked, childs):
        """
            sub synch childs nodes of linked object
        """

        # remove childs not found on source
        l_childs = self.get_childs_panels(context, linked)
        c_names = [c.data.name for c in childs]
        for c in l_childs:
            try:
                id = c_names.index(c.data.name)
            except:
                self.delete_object(context, c)

        # children ordering may not be the same, so get the right l_childs order
        l_childs = self.get_childs_panels(context, linked)
        l_names = [c.data.name for c in l_childs]
        order = []
        for c in childs:
            try:
                id = l_names.index(c.data.name)
            except:
                id = -1
            order.append(id)

        # add missing childs and update other ones
        for i, child in enumerate(childs):
            if order[i] < 0:
                p = bpy.data.objects.new("Window Panel", child.data)
                # Link object into scene
                self.link_object_to_scene(context, p)
                self.link_materials(context, o, p)
                p.color = (0, 1, 0, 1)
                p.show_transparent = True
                p.lock_location[1] = True
                p.lock_location[2] = True
                p.lock_rotation[1] = True
                p.lock_scale[0] = True
                p.lock_scale[1] = True
                p.lock_scale[2] = True
                p.parent = linked
                p.matrix_world = linked.matrix_world.copy()
                # m = p.archipack_material.add()
                # m.category = 'window'
                # m.material = o.archipack_material[0].material
            else:
                p = l_childs[order[i]]
                self.link_materials(context, o, p)

            self.synch_locks(p)

            # update handle
            handle = self.find_handle(child)
            h = self.find_handle(p)
            if handle is not None:
                if h is None:
                    h = create_handle(context, p, handle.data)
                h.location = handle.location.copy()
            elif h is not None:
                self.delete_object(context, h)

            p.location = child.location.copy()

        # restore context
        # self.select_object(context, o, True)

    def _synch_shutters(self, context, o, linked, left_side):
        """
            sub synch childs nodes of linked object
        """
        childs = self.get_childs_shutters(context, o, left_side)
        # remove childs not found on source
        l_childs = self.get_childs_shutters(context, linked, left_side)
        c_names = [c.data.name for c in childs]
        for c in l_childs:
            try:
                id = c_names.index(c.data.name)
            except:
                self.delete_object(context, c)

        # children ordering may not be the same, so get the right l_childs order
        l_childs = self.get_childs_shutters(context, linked, left_side)
        l_names = [c.data.name for c in l_childs]
        order = []
        for c in childs:
            try:
                id = l_names.index(c.data.name)
            except:
                id = -1
            order.append(id)

        # add missing childs and update other ones
        for i, child in enumerate(childs):
            if order[i] < 0:
                p = bpy.data.objects.new("Shutter", child.data)
                # Link object into scene
                self.link_object_to_scene(context, p)
                p.color = (0, 1, 0, 1)
                p.lock_location[1] = True
                p.lock_location[2] = True
                p.lock_rotation[1] = True
                p.lock_scale[0] = True
                p.lock_scale[1] = True
                p.lock_scale[2] = True
                p.parent = linked
                p.matrix_world = linked.matrix_world.copy()
                # m = p.archipack_material.add()
                # m.category = 'window'
                # m.material = o.archipack_material[0].material
            else:
                p = l_childs[order[i]]

            self.link_materials(context, o, p)
            p.location = child.location.copy()
            p.rotation_euler = child.rotation_euler.copy()

        # select and make active
        # self.select_object(context, o, True)
        
    def _synch_hole(self, context, linked, hole):
        l_hole = self.find_hole(linked)
        if l_hole is None:
            l_hole = bpy.data.objects.new("hole", hole.data)
            l_hole['archipack_hole'] = True
            # Link object into scene
            self.link_object_to_scene(context, l_hole)
            l_hole.parent = linked
            l_hole.matrix_world = linked.matrix_world.copy()
            l_hole.location = hole.location.copy()
        else:
            l_hole.data = hole.data

    def synch_locks(self, p):
        s = self.window_type != 'FLAT'
        g = self.window_type != 'HUNG'
        r = self.window_type != 'RAIL'
        p.lock_location = (r, True, g)
        p.lock_rotation = (s, True, s)

    def synch_childs(self, context, o):
        """
            synch childs nodes of linked objects
        """
        # bpy.ops.object.select_all(action='DESELECT')
        # select and make active
        # self.select_object(context, o, True)
        childs = self.get_childs_panels(context, o)
        hole = self.find_hole(o)
        linked_objects = self.get_linked_objects(context, o)
        # bpy.ops.object.select_linked(type='OBDATA')
        for linked in linked_objects:
            if linked != o:
                ld = archipack_window.datablock(linked)
                ld.update_portal(context, linked)
                ld.update_blind(context, linked, True)
                ld.update_blind(context, linked, False)
                self._synch_childs(context, o, linked, childs)
                self._synch_shutters(context, o, linked, True)
                self._synch_shutters(context, o, linked, False)
                if hole is not None:
                    self._synch_hole(context, linked, hole)

    def get_shutter_row(self, x, y, left_side):
        n_shutters = self.shutter_left + self.shutter_right
        size = Vector((x / n_shutters, y, 0))
        origin = []
        ttl = 0
        xh = x / 2
        # offset pivot
        if left_side:
            n_shutters = self.shutter_left
            ttl -= size.x
        else:
            ttl += self.shutter_left * size.x
            n_shutters = self.shutter_right

        for i in range(n_shutters):
            ttl += size.x
            origin.append(Vector((ttl - xh, 0)))
        return size, origin

    def update_shutter(self, context, o, left_side, hinge_space):
        # wanted childs
        if self.shutter_enable:
            if left_side:
                side = 0
                pivot = 1
                n_shutters = self.shutter_left
            else:
                pivot = -1
                n_shutters = self.shutter_right
                side = n_shutters - 1

        else:
            n_shutters = 0

        # real childs
        childs = self.get_childs_shutters(context, o, left_side)
        n_childs = len(childs)

        # remove child
        if n_childs > n_shutters:
            self.remove_shutters(context, childs, n_childs - n_shutters)

        if not self.shutter_enable or n_shutters == 0:
            return

        childs = self.get_childs_shutters(context, o, left_side)
        n_childs = len(childs)

        location_y = -0.5 * self.y - 0.25 * self.shutter_depth - self.finishing_out
        if self.out_frame:
            location_y -= self.out_frame_y2
        
        # Note: radius is slightly wrong: not taking overflow in account
        center, origin, size, radius = self.get_radius(self.x, self.z)
        offset = Vector((0.05, 0))
        size, origin = self.get_shutter_row(self.x, self.z, left_side)

        if hinge_space > 1.5:
            hinge_count = 3
        else:
            hinge_count = 2

        for panel in range(n_shutters):

            if panel >= n_childs:
                m = bpy.data.meshes.new("Shutter")
                child = bpy.data.objects.new("Shutter", m)

                d = m.archipack_window_shutter.add()

                # Link object into scene
                self.link_object_to_scene(context, child)
                child.color = (0, 1, 0, 1)
                child.lock_location = (False, True, True)
                child.lock_rotation = (False, True, False)
                child.lock_scale = (True, True, True)

                # parenting at 0, 0, 0 before set object matrix_world
                # so location remains local from frame
                child.parent = o
                child.matrix_world = o.matrix_world.copy()
                child.rotation_euler.z = pi
            else:
                child = childs[panel]
                # select and make active
                # self.select_object(context, child, True)
                d = archipack_window_shutter.datablock(child)

            if d is not None:
                self.link_materials(context, o, child)
                d.update(context,
                         child,
                         self,
                         Vector((origin[panel].x, offset.y, 0)),
                         center,
                         radius,
                         size,
                         pivot,
                         pivot * offset.x,
                         panel == side,
                         hinge_count,
                         hinge_space
                         )

            # location y + frame width.
            child.location = Vector((
                origin[panel].x - pivot * offset.x + (side - panel) * size.x,
                origin[panel].y + location_y + (side - panel) * pivot * 0.5 * self.shutter_depth,
                self.altitude + offset.y
                ))

    def update_childs(self, context, o):
        """
            pass params to childrens
            :type o: object
        """
        self.update_rows(context, o)
        childs = self.get_childs_panels(context, o)
        n_childs = len(childs)
        child_n = 0
        row_n = 0
        handle_model = 0
        location_y = 0.5 * self.y - self.offset + 0.5 * self.frame_y
        center, origin, size, radius = self.get_radius(self._x, self._z)
        offset = Vector((0, 0))
        handle = 'NONE'
        if self.shape != 'CIRCLE':
            if self.handle_enable:
                if self._z > 1.8:
                    handle = 'BOTH'
                else:
                    handle = 'INSIDE'
            is_circle = False
        else:
            is_circle = True

        if self.window_type == 'RAIL':
            handle_model = 2
        elif self.window_type == 'FLAT':
            handle_model = 1
        else:
            # TODO: handle for hung windows
            handle = 'NONE'

        for row in self.rows:
            row_n += 1
            if row_n < self.n_rows and not is_circle and self.window_type != 'RAIL':
                z = row.height
                shape = 'RECTANGLE'
            else:
                z = max(2 * self.frame_x + 0.001, self._z - offset.y)
                shape = self.shape

            self.warning = bool(z > self._z - offset.y)
            if self.warning:
                break

            size, origin, pivot = row.get_row(self._x, z)

            # side materials

            materials = [0] * row.cols

            handle_altitude = min(
                max(self.panel_x, self.handle_altitude + self._overflow - self.altitude),
                z - self.panel_x
                )

            if self.window_type == 'RAIL':
                self.adjust_size_for_rail(row, size, origin, pivot, materials)
                
            elif self.window_type == 'HUNG':
                self.adjust_size_for_hung(self.n_rows - 1, row_n - 1, row, size, origin, offset, materials)
            
            for panel in range(row.cols):
                child_n += 1

                if row.fixed[panel]:
                    enable_handle = 'NONE'
                else:
                    enable_handle = handle

                if child_n > n_childs:

                    m = bpy.data.meshes.new("Window Panel")
                    child = bpy.data.objects.new("Window Panel", m)
                    d = m.archipack_window_panel.add()
                    # Link object into scene
                    self.link_object_to_scene(context, child)
                    # select and make active
                    # self.select_object(context, child, True)
                    child.color = (0, 1, 0, 1)

                    child.show_transparent = True
                    child.lock_location = (False, True, True)
                    child.lock_rotation = (False, True, False)
                    child.lock_scale = (True, True, True)
                    # parenting at 0, 0, 0 before set object matrix_world
                    # so location remains local from frame
                    child.parent = o
                    child.matrix_world = o.matrix_world.copy()

                else:
                    child = childs[child_n - 1]
                    # select and make active
                    # self.select_object(context, child, True)
                    d = archipack_window_panel.datablock(child)

                if d is not None:
                    self.link_materials(context, o, child)

                    d.update(context,
                        child,
                        self,
                        Vector((origin[panel].x, offset.y, 0)),
                        center,
                        radius,
                        size[panel],
                        pivot[panel],
                        shape,
                        row.fixed[panel],
                        enable_handle,
                        handle_model,
                        handle_altitude,
                        materials[panel])

                # location y + frame width. frame depends on choosen profile (fixed or not)
                # update linked childs location too
                child.location = Vector((
                    origin[panel].x,
                    origin[panel].y + location_y + self.panel_y,
                    origin[panel].z + self.altitude - self._overflow + offset.y))

                self.synch_locks(child)

                if not row.fixed[panel]:
                    handle = 'NONE'

                # only one single panel allowed for circle
                if is_circle:
                    return

            # only one single row allowed for rail window
            if self.window_type == 'RAIL':
                return
            offset.y += row.height

    @property
    def _overflow(self):
        return min(0, self.frame_overflow - self.frame_x)

    @property
    def _x(self):
        return self.x + 2 * self._overflow

    @property
    def _z(self):
        return self.z + 2 * self._overflow

    def _get_tri_radius(self, _x, _z):
        return Vector((0, self.y, 0)), Vector((0, 0, 0)), \
            Vector((_x, _z, 0)), Vector((_x, 0, 0))

    def _get_quad_radius(self, _x, _z):
        fx_z = _z / _x
        center_y = min(_x / (_x - self.frame_x) * _z - self.frame_x * (1 + sqrt(1 + fx_z * fx_z)),
            abs(tan(self.angle_y) * _x))
        if self.angle_y < 0:
            center_x = 0.5 * _x
        else:
            center_x = -0.5 * _x
        return Vector((center_x, center_y, 0)), Vector((0, 0, 0)), \
            Vector((_x, _z, 0)), Vector((_x, 0, 0))

    def _get_round_radius(self, _x, _z):
        """
            bound radius to available space
            return center, origin, size, radius
        """
        x = 0.5 * _x - self.frame_x
        # minimum space available
        y = _z - sum([row.height for row in self.rows[:self.n_rows - 1]]) - 2 * self.frame_x
        y = min(y, x)
        # minimum radius inside
        r = y + x * (x - (y * y / x)) / (2 * y)
        radius = max(self.radius, 0.001 + self.frame_x + r)
        return Vector((0, _z - radius, 0)), Vector((0, 0, 0)), \
            Vector((_x, _z, 0)), Vector((radius, 0, 0))

    def _get_circle_radius(self, _x, _z):
        """
            return center, origin, size, radius
        """
        return Vector((0, 0.5 * _x, 0)), Vector((0, 0, 0)), \
            Vector((_x, _z, 0)), Vector((0.5 * _x, 0, 0))

    def _get_ellipsis_radius(self, _x, _z):
        """
            return center, origin, size, radius
        """
        y = self.z - sum([row.height for row in self.rows[:self.n_rows - 1]])
        radius_b = max(0, 0.001 - 2 * self.frame_x + min(y, self.elipsis_b))
        return Vector((0, _z - radius_b, 0)), Vector((0, 0, 0)), \
            Vector((_x, _z, 0)), Vector((_x / 2, radius_b, 0))

    def get_radius(self, _x, _z):
        """
            return center, origin, size, radius
        """
        if self.shape == 'ROUND':
            return self._get_round_radius(_x, _z)
        elif self.shape == 'ELLIPSIS':
            return self._get_ellipsis_radius(_x, _z)
        elif self.shape == 'CIRCLE':
            return self._get_circle_radius(_x, _z)
        elif self.shape == 'QUADRI':
            return self._get_quad_radius(_x, _z)
        elif self.shape in ['TRIANGLE', 'PENTAGON']:
            return self._get_tri_radius(_x, _z)
        else:
            return Vector((0, 0, 0)), Vector((0, 0, 0)), \
                Vector((_x, _z, 0)), Vector((0, 0, 0))

    def soft_bmesh(self, bm, tM, itM, angle, scale, x):
        plane_co = tM.translation
        plane_no = tM.col[0].to_3d()

        if x > 0:
            side = [v for v in bm.verts if (itM @ v.co).x > 0.001]
        else:
            side = [v for v in bm.verts if (itM @ v.co).x < 0.001]
        if len(side) < len(bm.verts):
            bmed.bisect(bm, plane_co, plane_no)
            center = [v for v in bm.verts if 0.001 > (itM @ v.co).x > -0.001]
            bmesh.ops.rotate(bm, verts=center,
                             cent=plane_co,
                             matrix=Matrix.Rotation(0.5 * angle, 3, 'Z'),
                             space=Matrix())
            bmesh.ops.scale(bm, verts=center, space=tM.inverted(), vec=scale)
        bmesh.ops.rotate(bm, verts=side,
                         cent=plane_co,
                         matrix=Matrix.Rotation(angle, 3, 'Z'),
                         space=Matrix())

    def soft_segment(self, w, o, angle, x):
        """
        :param w: window
        :param o: object to update
        :param angle:
        :param x:
        :return:
        """
        # y location of pivot near panels
        if self.window_type == 'FLAT':
            y =  self.hole_center_y + 0.5 * self.frame_y
            y2 = self.panel_y
        else:
            y =  self.hole_center_y + self.rail_frame_depth
            y2 = 2 * self.panel_y

        if angle > 0:
            y += y2

        if x < 0:
            angle = -angle

        # blind bend itself
        sel = [c for c in o.children if c.type == "MESH" and "archipack_blind" not in c.data]
        # 2nd level of childs (handles) ugly transitions but required to rotate handle
        for c in o.children:
            sel.extend([c2 for c2 in c.children if c2.type == "MESH"])
        sel.append(o)

        scale = 1.0 / cos(0.5 * abs(angle))
        scale = Vector((scale, scale, 1))
        plane_loc = Vector((x, y, 0))
        plane_tM = w.matrix_world @ Matrix.Translation(plane_loc)
        plane_itM = plane_tM.inverted()

        for c in sel:
            # plane matrix in object coordsys
            tM = c.matrix_world.inverted() @ plane_tM
            # vertex in plane coordsys
            itM = plane_itM @ c.matrix_world

            if "archipack_window_curtain" in c.data:
                continue
            bm = bmed._start(c)
            self.soft_bmesh(bm, tM, itM, angle, scale, x)
            bmed._end(bm, c)

    def soft_segment_2d(self, coords, angle, x, add_section=True):
        """ Bend 2d symbol
        :param w: window
        :param o: object to update
        :param angle:
        :param x:
        :param add_section: add section at rotation center
        :return:
        """
        if self.window_type == 'FLAT':
            y =  self.hole_center_y + 0.5 * self.frame_y
            y2 = self.panel_y
        else:
            y =  (self.hole_center_y + self.rail_frame_depth)
            y2 = 2 * self.panel_y

        if angle > 0:
            y += y2

        if x < 0:
            angle = -angle

        closed = coords[0] == coords[-1]

        scale = 1.0 / cos(0.5 * abs(angle))
        scale = Vector((scale, scale, 1))
        plane_co = Vector((x, y, 0))
        plane_tM = Matrix.Translation(plane_co)
        plane_itM = plane_tM.inverted()
        plane_no = plane_tM.col[0].to_3d()

        # vertex in plane coordsys
        if x > 0:
            side = [i for i, v in enumerate(coords) if (plane_itM @ v).x > 0.001]
        else:
            side = [i for i, v in enumerate(coords) if (plane_itM @ v).x < 0.001]

        to_add = []
        if len(side) < len(coords):
            p0 = coords[-1]
            ip0 = plane_itM @ p0
            for i, p1 in enumerate(coords):
                ip1 = plane_itM @ p1
                if ip1.x * ip0.x < 0:
                    # both sides, insert a coord before p0
                    u = p1 - p0
                    d = plane_no.dot(u)
                    w = p0 - plane_co
                    t = -plane_no.dot(w) / d
                    if i > 0 or closed:
                        to_add.append((i, p0 + t * u))
                p0 = p1
                ip0 = ip1

        rM = plane_tM @ Matrix.Rotation(angle, 4, 'Z') @ plane_itM
        for i in side:
            coords[i] = rM @ coords[i]

        if add_section:
            tM = plane_tM @ Matrix.Rotation(0.5 * angle, 4, 'Z') @ Matrix.Scale(1, 4, scale) @ plane_itM
            for i, co in reversed(to_add):
                coords.insert(i, tM @ co)


    def update(self, context, childs_only=False):
        # support for "copy to selected"
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        t = time.time()
        # logger.debug("window.update is_updating:%s" % (self.is_updating))

        self.setup_manipulators()

        if childs_only is False:
            center, origin, size, radius = self.get_radius(self._x, self._z)
            is_not_circle = self.shape != 'CIRCLE'
            offset = Vector((0, self.altitude - self._overflow, 0))
            window = self.window
            verts = window.vertices(self.curve_steps, offset, center, origin, size, radius,
                                    self.angle_y, 0, shape_z=None, path_type=self.shape)
            logger.debug("window.update verts %.4f" % (time.time() - t))
            faces = window.faces(self.curve_steps, path_type=self.shape)

            logger.debug("window.update faces %.4f" % (time.time() - t))
            mat = window.mat(self.curve_steps, 0, 0, path_type=self.shape)
            logger.debug("window.update mat %.4f" % (time.time() - t))
            uvs = window.uv(self.curve_steps, center, origin, size, radius,
                                 self.angle_y, 0, 0, self.frame_x, path_type=self.shape)
            logger.debug("window.update uvs %.4f" % (time.time() - t))
            col = random_color()
            vcolors = window.vcolors(self.curve_steps, col, path_type=self.shape)
            logger.debug("window.update vcolors %.4f" % (time.time() - t))
            if self.out_frame:
                frame = self.frame
                _size = Vector((self.x, self.z, 0))
                _offset = Vector((0, self.altitude, 0))
                _center = Vector((center.x, center.y - self._overflow, center.z))

                if self.shape == 'ELLIPSIS':
                    _radius = Vector((
                        radius.x - self._overflow,
                        radius.y - self._overflow,
                        0))

                elif self.shape == 'QUADRI':
                    _radius = Vector((self.x, 0, 0))

                    if self.angle_y < 0:
                        _center.x = 0.5 * self.x
                    else:
                        _center.x = -0.5 * self.x
                    fx_z = self.z / self.x
                    _center.y = min(
                        self.x / (self.x - self.frame_x) * self.z - self.frame_x * (1 + sqrt(1 + fx_z * fx_z)),
                        abs(tan(self.angle_y) * (self.x))
                    )
                else:
                    _radius = Vector((radius.x - self._overflow, 0, 0))

                faces.extend(
                    frame.faces(self.curve_steps, path_type=self.shape, offset=len(verts))
                )
                verts.extend(
                    frame.vertices(self.curve_steps, _offset, _center, origin, _size, _radius,
                                   self.angle_y, 0, shape_z=None, path_type=self.shape)
                )
                mat.extend(
                    frame.mat(self.curve_steps, 0, 0, path_type=self.shape)
                )
                uvs.extend(
                    frame.uv(self.curve_steps, _center, origin, _size, _radius,
                                  self.angle_y, 0, 0, self.frame_x, path_type=self.shape)
                )
                vcolors.extend(
                    frame.vcolors(self.curve_steps, col, path_type=self.shape)
                )
            if is_not_circle:

                if self.out_tablet_enable:
                    _offset = Vector((0, self.altitude, 0))
                    _size = Vector((
                        self.x + 2 * (self.out_tablet_x),
                        size.y,
                        size.z))
                    tablet = self.out_tablet
                    faces.extend(
                        tablet.faces(self.curve_steps, path_type='HORIZONTAL', offset=len(verts))
                    )
                    verts.extend(
                        tablet.vertices(self.curve_steps, _offset, center, origin, _size, radius,
                                        self.angle_y, 0, shape_z=None, path_type='HORIZONTAL')
                    )
                    mat.extend(
                        tablet.mat(self.curve_steps, 0, 0, path_type='HORIZONTAL')
                    )
                    uvs.extend(
                        tablet.uv(self.curve_steps, center, origin, _size, radius,
                                  self.angle_y, 0, 0, self.frame_x, path_type='HORIZONTAL')
                    )
                    vcolors.extend(
                        tablet.vcolors(self.curve_steps, col, path_type='HORIZONTAL')
                    )
                if self.in_tablet_enable:
                    tablet = self.in_tablet
                    _size = Vector((size.x + 2 * (self.frame_x + self.in_tablet_x),
                                    size.y,
                                    size.z))
                    faces.extend(
                        tablet.faces(self.curve_steps, path_type='HORIZONTAL', offset=len(verts))
                    )
                    verts.extend(
                        tablet.vertices(self.curve_steps, offset, center, origin, _size, radius,
                                        self.angle_y, 0, shape_z=None, path_type='HORIZONTAL')
                    )
                    mat.extend(
                        tablet.mat(self.curve_steps, 0, 0, path_type='HORIZONTAL')
                    )
                    uvs.extend(
                        tablet.uv(self.curve_steps, center, origin, _size, radius,
                                  self.angle_y, 0, 0, self.frame_x, path_type='HORIZONTAL')
                    )
                    vcolors.extend(
                        tablet.vcolors(self.curve_steps, col, path_type='HORIZONTAL')
                    )

            if self.curtain_sill_enable:
                col = random_color()
                tablet = self.curtain_tablet
                _offset = Vector((offset.x, 0, 0))
                _size = Vector((size.x + 2 * (self.curtain_sill_x + self.frame_x),
                                size.y,
                                size.z))
                faces.extend(
                    tablet.faces(self.curve_steps, path_type='HORIZONTAL', offset=len(verts))
                )
                verts.extend(
                    tablet.vertices(self.curve_steps, _offset, center, origin, _size, radius,
                                    self.angle_y, 0, shape_z=None, path_type='HORIZONTAL')
                )
                mat.extend(
                    tablet.mat(self.curve_steps, 0, 0, path_type='HORIZONTAL')
                )
                uvs.extend(
                    tablet.uv(self.curve_steps, center, origin, _size, radius,
                              self.angle_y, 0, 0, self.frame_x, path_type='HORIZONTAL')
                )
                vcolors.extend(
                    tablet.vcolors(self.curve_steps, col, path_type='HORIZONTAL')
                )

            if self.curtain_rod_enable:
                col = random_color()
                rod, side = self.curtain_rod
                _size = Vector((size.x + 2 * (self.curtain_rod_x + self.frame_x),
                                size.y,
                                size.z))
                _offset = Vector((offset.x, 0, 0))
                faces.extend(
                    rod.faces(self.curve_steps, path_type='HORIZONTAL', offset=len(verts))
                )
                verts.extend(
                    rod.vertices(self.curve_steps, _offset, center, origin, _size, radius,
                                    self.angle_y, 0, shape_z=None, path_type='HORIZONTAL')
                )

                mat.extend(
                    rod.mat(self.curve_steps, 0, 0, path_type='HORIZONTAL')
                )
                uvs.extend(
                    rod.uv(self.curve_steps, center, origin, _size, radius,
                              self.angle_y, 0, 0, self.frame_x, path_type='HORIZONTAL')
                )
                vcolors.extend(
                    rod.vcolors(self.curve_steps, col, path_type='HORIZONTAL')
                )
                _size = Vector((self.x, self.z, 0))
                # lefts
                z = self.altitude + self.z + self.curtain_rod_alt + min(self.frame_x, self.frame_overflow)
                _radius = Vector((0, 0, 0))
                _center = _radius
                _offset = Vector((-0.5 * self.x, z, 0))
                faces.extend(
                    side.faces(self.curve_steps, path_type='CIRCLE', offset=len(verts))
                )
                verts.extend(
                    side.vertices(self.curve_steps, _offset, _center, origin, _size, _radius,
                                 self.angle_y, 0, shape_z=None, path_type='CIRCLE')
                )
                mat.extend(
                    side.mat(self.curve_steps, 0, 0, path_type='CIRCLE')
                )
                uvs.extend(
                    side.uv(self.curve_steps, _center, origin, _size, _radius,
                           self.angle_y, 0, 0, self.frame_x, path_type='CIRCLE')
                )
                vcolors.extend(
                    side.vcolors(self.curve_steps, col, path_type='CIRCLE')
                )
                # right
                _offset = Vector((0.5 * self.x, z, 0))
                faces.extend(
                    side.faces(self.curve_steps, path_type='CIRCLE', offset=len(verts))
                )
                verts.extend(
                    side.vertices(self.curve_steps, _offset, _center, origin, _size, _radius,
                                  self.angle_y, 0, shape_z=None, path_type='CIRCLE')
                )
                mat.extend(
                    side.mat(self.curve_steps, 0, 0, path_type='CIRCLE')
                )
                uvs.extend(
                    side.uv(self.curve_steps, _center, origin, _size, _radius,
                            self.angle_y, 0, 0, self.frame_x, path_type='CIRCLE')
                )
                vcolors.extend(
                    side.vcolors(self.curve_steps, col, path_type='CIRCLE')
                )
            logger.debug("window.update frame %.4f" % (time.time() - t))
            bmed.buildmesh(o, verts, faces, mat, uvs, vcolors=vcolors)
            logger.debug("window.update buildmesh %.4f" % (time.time() - t))
            self.shade_smooth(context, o, 0.20944)

        logger.debug("window.update 0 %.4f" % (time.time() - t))

        self.update_portal(context, o)
        self.update_blind(context, o, True)
        self.update_blind(context, o, False)
        self.update_childs(context, o)

        logger.debug("window.update 1 %.4f" % (time.time() - t))

        left, right = self.vertical_space
        self.update_shutter(context, o, True, left)
        self.update_shutter(context, o, False, right)

        # update hole
        if childs_only is False and self.find_hole(o) is not None:
            self.interactive_hole(context, o)

        logger.debug("window.update 2 %.4f" % (time.time() - t))

        if len(self.bend) > 0:
            bend = self.bend[:]
            bend.sort(key=lambda x: x.x)
            for b in bend:
                if b.x < 0:
                    self.soft_segment(o, o, b.a, b.x)

            for b in reversed(bend):
                if b.x > 0:
                    self.soft_segment(o, o, b.a, b.x)

        # store 3d points for gl manipulators
        x, y = 0.5 * self.x, 0.5 * self.y
        self.manipulators[0].set_pts([(-x, -y, 0), (x, -y, 0), (0.5, 0, 0)])
        self.manipulators[1].set_pts([(-x, -y, 0), (-x, y, 0), (-1, 0, 0)])
        self.manipulators[2].set_pts([(x, -y, self.altitude), (x, -y, self.altitude + self.z), (-1, 0, 0)])
        self.manipulators[3].set_pts([(x, -y, 0), (x, -y, self.altitude), (-1, 0, 0)])

        # soft_segment_2d for dimension points
        p0, p1 = Vector((-x, -y, 0)), Vector((x, -y, 0))
        p2, p3 = Vector((-0.5 * self._x - self.frame_x, y, 0)), Vector((0.5 * self._x + self.frame_x, y, 0))
        coords = [p0, p1, p2, p3]
        if len(self.bend) > 0:
            for b in bend:
                if b.x < 0:
                    self.soft_segment_2d(coords, b.a, b.x, add_section=False)
            for b in reversed(bend):
                if b.x > 0:
                    self.soft_segment_2d(coords, b.a, b.x, add_section=False)

        self.add_dimension_point(0, coords[0])
        self.add_dimension_point(1, coords[1])
        self.add_dimension_point(2, coords[2])
        self.add_dimension_point(3, coords[3])
        
        # support for instances childs, update at object level
        self.synch_childs(context, o)
        logger.debug("window.update 3 %.4f" % (time.time() - t))

        # synch dimensions when apply
        if o.parent:
            self.update_dimensions(context, o)

        logger.debug("window.update end %.4f" % (time.time() - t))
        # restore context
        self.restore_context(context)
        logger.debug("window.restore_context %.4f" % (time.time() - t))

    def find_hole(self, o):
        for child in o.children:
            if 'archipack_hole' in child:
                return child
        return None

    def interactive_hole(self, context, o):
        hole_obj = self.find_hole(o)

        if hole_obj is None:
            m = bpy.data.meshes.new("hole")
            hole_obj = bpy.data.objects.new("hole", m)
            hole_obj['archipack_hole'] = True
            hole_obj['archipack_skip_material'] = True
            # Link object into scene
            self.link_object_to_scene(context, hole_obj)
            hole_obj.parent = o
            hole_obj.matrix_world = o.matrix_world.copy()
        
        hole = self.hole
        center, origin, size, radius = self.get_radius(self._x, self._z)
        x0 = 0

        if self.out_frame:
            x0 += min(self.frame_x + 0.001, self.out_frame_y + self.out_frame_offset)

        if self.out_tablet_enable:
            x0 -= self.out_tablet_z

        x0 = min(x0, -0.001)

        shape_z = [-0.001, x0]

        verts = hole.vertices(self.curve_steps,
            Vector((0, self.altitude - self._overflow, 0)),
            center, origin, size, radius,
            self.angle_y, 0, shape_z=shape_z, path_type=self.shape)

        faces = hole.faces(self.curve_steps, path_type=self.shape)

        matids = hole.mat(self.curve_steps, 2, 2, path_type=self.shape)

        uvs = hole.uv(self.curve_steps, center, origin, size, radius,
            self.angle_y, 0, 0, self.frame_x, path_type=self.shape)
        col = random_color()
        vcolors = hole.vcolors(self.curve_steps, col, path_type=self.shape)
        bmed.buildmesh(hole_obj, verts, faces, matids, uvs, vcolors=vcolors)
        return hole_obj

    def _add_spline(self, curve, coords):
        spline = curve.splines.new('POLY')
        spline.use_endpoint_u = False
        spline.use_cyclic_u = coords[-1] == coords[0]
        if coords[-1] == coords[0]:
            coords.pop()
        spline.points.add(len(coords) - 1)
        for i, coord in enumerate(coords):
            x, y, z = coord
            spline.points[i].co = (x, y, z, 1)

    def _to_curve(self, context, coords, name: str, dimensions: str='3D'):
        curve = bpy.data.curves.new(name, type='CURVE')
        curve.dimensions = dimensions
        for co in coords:
            self._add_spline(curve, co)
        curve_obj = bpy.data.objects.new(name, curve)
        # Link object into scene
        self.link_object_to_scene(context, curve_obj)
        # select and make active
        self.select_object(context, curve_obj, True)
        return curve_obj

    def as_2d(self, context, o):

        # frame
        center, origin, size, radius = self.get_radius(self._x, self._z)
        offset = Vector((0, 0, 0))

        # draw borders
        connect = 2
        if self.window_type == 'RAIL':
            connect = 3

        coords = self.window.as_2d(self.curve_steps, offset, center, origin,
            size, radius, 0, 0, connect=connect)

        # panels
        childs = self.get_childs_panels(context, o)

        location_y = 0.5 * self.y - self.offset + 0.5 * self.frame_y

        row = self.rows[0]
        size, origin, pivot = row.get_row(self._x, row.height)

        materials = [0] * row.cols

        if self.window_type == 'RAIL':
            self.adjust_size_for_rail(row, size, origin, pivot, materials)
        elif self.window_type == 'HUNG':
            self.adjust_size_for_hung(1, 0, row, size, origin, offset, materials)

        n_childs = len(childs)
        
        for panel in range(row.cols):
            if panel >= n_childs:
                break
            child = childs[panel]
            
            # location y + frame width. frame depends on choosen profile (fixed or not)
            # update linked childs location too
            d = child.data.archipack_window_panel[0]
            if self.shape == 'CIRCLE':
                s = 2
            else: 
                s = 1

            location = Vector((
                origin[panel].x,
                origin[panel].y + location_y + self.panel_y,
                0))

            window = d.window(self, row.fixed[panel], materials[panel])
            coords.extend(
                window.as_2d(
                    self.curve_steps, location, center, Vector((origin[panel].x, offset.y, 0)),
                    s * size[panel], radius, 0, pivot[panel], path_type='RECTANGLE')
                )
            # arc
            if self.window_type == 'FLAT' and not row.fixed[panel]:
                x, y = location.x, location.y
                r = s * size[panel].x
                steps = 8
                # 30 deg
                da = pi / (6 * steps)
                arc = [Vector((
                        x + pivot[panel] * r * cos(da * i),
                        y + r * sin(da * i),
                        0))
                    for i in range(steps + 1)]
                arc.append(location)
                coords.append(arc)

        # 1 make a mesh based geometry with edges only
        # 2 bend
        # 3 get coords from mesh
        # C.object.data.archipack_window[0].as_2d(C, C.object)
        # self.bend_2d(context, o, coords)
        if len(self.bend) > 0:
            coords = [[Vector(co) for co in coord] for coord in coords]

            bend = self.bend[:]
            bend.sort(key=lambda x: x.x)
            for b in bend:
                if b.x < 0:
                    for coord in coords:
                        self.soft_segment_2d(coord, b.a, b.x)
            for b in reversed(bend):
                if b.x > 0:
                    for coord in coords:
                        self.soft_segment_2d(coord, b.a, b.x)

        curve = self._to_curve(context, coords, name="{}-2d".format(o.name), dimensions='2D')
        curve.matrix_world = o.matrix_world.copy()
        return curve

    def hole_2d(self, mode='SYMBOL'):
        """
          return coords of full / inside hole in 2d top ortho view
        """
        if mode == 'BOUND':
            x, y = 0.5 * self._x + self.frame_x, 0.5 * self.y + 0.01
            coords = [(-x, -y, 0), (-x, y, 0), (x, y, 0), (x, -y, 0), (-x, -y, 0)]

        else:
            center, origin, size, radius = self.get_radius(self._x, self._z)

            if mode in {'FLOORS', 'FLOORS_CHILD'}  :
                coords = self.inside_hole.as_2d(self.curve_steps,
                    Vector((0, 0, 0)),
                    center, origin, size, radius,
                    0, 0, shape_z=None, path_type=self.shape)
                coords = coords[0]
            else:
                coords = self.hole.as_2d(self.curve_steps,
                    Vector((0, 0, 0)),
                    center, origin, size, radius,
                    0, 0, shape_z=None, path_type=self.shape)
                coords = coords[0]

        if len(self.bend) > 0:
            coords = [Vector(co) for co in coords]
            bend = self.bend[:]
            bend.sort(key=lambda x: x.x)
            for b in bend:
                if b.x < 0:
                    self.soft_segment_2d(coords, b.a, b.x)
            for b in reversed(bend):
                if b.x > 0:
                    self.soft_segment_2d(coords, b.a, b.x)

        # Use only first curve
        return coords

    def remove_hole(self, context, hole, walls):
        ctx = context.copy()
        ctx['object'] = hole
        ctx['selected_objects'] = walls
        bpy.ops.archipack.remove_hole(ctx)

    def on_delete(self, context, obj):
        # print("window.on_delete")
        walls = set()
        wall2 = {}
        sel = context.selected_objects[:]
        # collect walls
        ref = obj.parent
        if ref is not None:
            walls = [c for c in ref.children
                     if (c.data and (
                        "archipack_wall2" in c.data
                        or "archipack_wall" in c.data
                        or "archipack_custom_wall" in c
                ))]
            wall2 = {c: c.data.archipack_wall2[0]
                     for c in ref.children
                     if c.data and "archipack_wall2" in c.data
                     }

        for o in sel:
            if archipack_window.filter(o):
                hole = self.find_hole(o)
                if hole is not None:
                    self.remove_hole(context, hole, walls)

                if o.name != obj.name:
                    self.delete_object(context, o)

        for c, d in wall2.items():
            # update providers
            d.setup_childs(context, c, openings_only=True)
            for i, child in enumerate(d.childs):
                if child.child_name == obj.name:
                    d.childs.remove(i)
                    break
            d.synch_dimension(context, c, remove_object=obj.name)
        # print("window.on_delete success")


class ARCHIPACK_PT_window(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_window"
    bl_label = "Window"

    @classmethod
    def poll(cls, context):
        return archipack_window.filter(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_window.datablock(o)
        if d is None:
            return
        layout = self.layout
        icons = icon_man["main"]

        self.draw_common(context, layout)

        row = layout.row(align=True)
        self.draw_op(context, layout, row, 'archipack.window', icon='FILE_REFRESH', text="Refresh").mode = 'REFRESH'
        if o.data.users > 1:
            self.draw_op(context, layout, row, 'archipack.window', icon='UNLINKED',
                         text="Make unique", postfix="({})".format(o.data.users)).mode = 'UNIQUE'
        # self.draw_op(context, layout, row, 'archipack.window', text="Delete", icon='ERROR').mode = 'DELETE'
        self.draw_op(context, layout, layout, "archipack.window_array", icon='MOD_ARRAY', text="Array")

        box = layout.box()
        # self.draw_label(context, layout, box, "Styles")
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.window_preset_menu",
                     text=bpy.types.ARCHIPACK_OT_window_preset_menu.bl_label, icon="PRESET")
        self.draw_op(context, layout, row, "archipack.window_preset", icon='ADD', text="")
        self.draw_op(context, layout, row, "archipack.window_preset", icon='REMOVE', text="").remove_active = True

        self.draw_prop(context, layout, layout, d, 'tabs', expand=True)

        box = layout.box()

        if d.tabs == 'MAIN':
            self.draw_prop(context, layout, box, d, 'window_type')
            self.draw_prop(context, layout, box, d, 'x')
            self.draw_prop(context, layout, box, d, 'y')
            if d.window_shape != 'CIRCLE':
                self.draw_prop(context, layout, box, d, 'z')
                if d.warning:
                    self.draw_label(context, layout, box, "Insufficient height")
            self.draw_prop(context, layout, box, d, 'altitude')
            self.draw_prop(context, layout, box, d, 'offset')
            # self.draw_prop(context, layout, box, d, 'finishing_out')

            if d.window_type == 'FLAT':
                box = layout.box()
                self.draw_prop(context, layout, box, d, 'window_shape')
                if d.window_shape in ['ROUND', 'CIRCLE', 'ELLIPSIS']:
                    self.draw_prop(context, layout, box, d, 'curve_steps')
                if d.window_shape in ['ROUND']:
                    self.draw_prop(context, layout, box, d, 'radius')
                elif d.window_shape == 'ELLIPSIS':
                    self.draw_prop(context, layout, box, d, 'elipsis_b')
                elif d.window_shape == 'QUADRI':
                    self.draw_prop(context, layout, box, d, 'angle_y')

        elif d.tabs == 'SUB':

            icon = "TRIA_RIGHT"
            if d.frame_expand:
                icon = "TRIA_DOWN"
            self.draw_prop(context, layout, box, d, 'frame_expand', icon=icon)
            if d.frame_expand:
                self.draw_prop(context, layout, box, d, 'frame_x')
                self.draw_prop(context, layout, box, d, 'frame_y')
                self.draw_prop(context, layout, box, d, 'frame_overflow')

            box = layout.box()
            icon = "TRIA_RIGHT"
            if d.panel_expand:
                icon = "TRIA_DOWN"
            self.draw_prop(context, layout, box, d, 'panel_expand', icon=icon)
            if d.panel_expand:
                self.draw_prop(context, layout, box, d, 'panel_x')
                self.draw_prop(context, layout, box, d, 'panel_y')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.out_frame_expand:
                icon = "TRIA_DOWN"
            self.draw_prop(context, layout, row, d, 'out_frame_expand', icon=icon)
            self.draw_prop(context, layout, row, d, 'out_frame', text="Enable")
            if d.out_frame_expand:
                self.draw_prop(context, layout, box, d, 'out_frame_x')
                self.draw_prop(context, layout, box, d, 'out_frame_y2')
                self.draw_prop(context, layout, box, d, 'out_frame_y')
                self.draw_prop(context, layout, box, d, 'out_frame_offset')

            if d.window_shape != 'CIRCLE':

                box = layout.box()
                row = box.row(align=True)
                icon = "TRIA_RIGHT"
                if d.out_tablet_expand:
                    icon = "TRIA_DOWN"
                self.draw_prop(context, layout, row, d, 'out_tablet_expand', icon=icon)
                self.draw_prop(context, layout, row, d, 'out_tablet_enable', text="Enable")
                if d.out_tablet_expand:
                    self.draw_prop(context, layout, box, d, 'out_tablet_x')
                    self.draw_prop(context, layout, box, d, 'out_tablet_y')
                    self.draw_prop(context, layout, box, d, 'out_tablet_z')

                box = layout.box()
                row = box.row(align=True)
                icon = "TRIA_RIGHT"
                if d.in_tablet_expand:
                    icon = "TRIA_DOWN"
                self.draw_prop(context, layout, row, d, 'in_tablet_expand', icon=icon)
                self.draw_prop(context, layout, row, d, 'in_tablet_enable', text="Enable")
                if d.in_tablet_expand:
                    self.draw_prop(context, layout, box, d, 'in_tablet_x')
                    self.draw_prop(context, layout, box, d, 'in_tablet_y')
                    self.draw_prop(context, layout, box, d, 'in_tablet_z')

                box = layout.box()
                row = box.row(align=True)
                icon = "TRIA_RIGHT"
                if d.shutter_expand:
                    icon = "TRIA_DOWN"
                self.draw_prop(context, layout, row, d, 'shutter_expand', icon=icon)
                self.draw_prop(context, layout, row, d, 'shutter_enable', text="Enable")
                if d.shutter_expand:
                    self.draw_prop(context, layout, box, d, 'shutter_left')
                    self.draw_prop(context, layout, box, d, 'shutter_right')
                    self.draw_prop(context, layout, box, d, 'shutter_depth')
                    self.draw_prop(context, layout, box, d, 'shutter_border')
                    self.draw_prop(context, layout, box, d, 'shutter_hinge')

            box = layout.box()
            row = box.row(align=True)
            icon = "TRIA_RIGHT"
            if d.curtain_expand:
                icon = "TRIA_DOWN"
            self.draw_prop(context, layout, row, d, 'curtain_expand', icon=icon)
            if d.curtain_expand:
                self.draw_op(context, layout, box, "archipack.window_curtain")
                self.draw_prop(context, layout, box, d, 'curtain_sill_enable')
                if d.curtain_sill_enable:
                    self.draw_prop(context, layout, box, d, 'curtain_sill_x')
                    self.draw_prop(context, layout, box, d, 'curtain_sill_y')
                    self.draw_prop(context, layout, box, d, 'curtain_sill_z')
                    self.draw_prop(context, layout, box, d, 'curtain_sill_alt')
                self.draw_prop(context, layout, box, d, 'curtain_rod_enable')
                if d.curtain_rod_enable:
                    self.draw_prop(context, layout, box, d, 'curtain_rod_x')
                    self.draw_prop(context, layout, box, d, 'curtain_rod_y')
                    self.draw_prop(context, layout, box, d, 'curtain_rod_alt')
                    self.draw_prop(context, layout, box, d, 'curtain_rod_radius')

            if d.window_shape != 'CIRCLE':
                box = layout.box()
                row = box.row(align=True)
                self.draw_prop(context, layout, row, d, 'handle_enable')
                if d.handle_enable:
                    self.draw_prop(context, layout, box, d, 'handle_altitude')

                box = layout.box()
                self.draw_prop(context, layout, box, d, 'blind_inside', icon_value=icons["blind"].icon_id)
                self.draw_prop(context, layout, box, d, 'blind_outside', icon_value=icons["blind"].icon_id)

            box = layout.box()
            self.draw_prop(context, layout, box, d, 'enable_glass')



            box = layout.box()
            self.draw_prop(context, layout, box, d, 'portal', icon="LIGHT_AREA", emboss=True)
            self.draw_prop(context, layout, box, d, 'portal_energy')

        elif d.tabs == 'PARTS':

            if d.window_shape != 'CIRCLE':
                row = box.row(align=False)

                if d.window_type != 'RAIL':
                    self.draw_prop(context, layout, row, d, 'n_rows')

                if d.window_type != 'RAIL':
                    last_row = d.n_rows - 1
                    for i, d_row in enumerate(d.rows):
                        box = layout.box()
                        self.draw_label(context, layout, box, "Row", postfix=str(i + 1))
                        d_row.draw(context, layout, box, i == last_row)
                else:
                    d.rows[0].draw(context, layout, box, True)
            else:
                self.draw_label(context, layout, box, "Option not available")

        elif d.tabs == 'MATERIALS':

            if "archipack_material" in o:
                o.archipack_material[0].draw(context, box)

            box = layout.box()
            self.draw_label(context, layout, box, "Apply materials")
            self.draw_prop(context, layout, box, d, 'mat_frame_inside')
            self.draw_prop(context, layout, box, d, 'mat_frame_outside')
            self.draw_prop(context, layout, box, d, 'mat_sill_in')
            self.draw_prop(context, layout, box, d, 'mat_sill_out')
            self.draw_prop(context, layout, box, d, 'mat_spoiler')
            self.draw_prop(context, layout, box, d, 'mat_handle')
            self.draw_prop(context, layout, box, d, 'mat_curtain_sill')
            self.draw_prop(context, layout, box, d, 'mat_curtain_rod')
            self.draw_prop(context, layout, box, d, 'mat_shutter')
            self.draw_prop(context, layout, box, d, 'mat_hinge')
            self.draw_prop(context, layout, box, d, 'mat_glass')
            self.draw_prop(context, layout, box, d, 'mat_glass_frame')
            self.draw_prop(context, layout, box, d, 'mat_joint')

            icon = "TRIA_RIGHT"
            if not d.auto_mat:
                icon = "TRIA_DOWN"
            self.draw_prop(context, layout, box, d, 'auto_mat', icon=icon, emboss=True, text="Hole material auto")
            if not d.auto_mat:
                self.draw_prop(context, layout, box, d, 'mat_hole_inside')
                self.draw_prop(context, layout, box, d, 'mat_hole_outside')
                self.draw_prop(context, layout, box, d, 'mat_finish_inside')
                self.draw_prop(context, layout, box, d, 'mat_finish_outside')


class ARCHIPACK_PT_window_panel(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_window_panel"
    bl_label = "Window leaf"

    @classmethod
    def poll(cls, context):
        return archipack_window_panel.filter(context.active_object)

    def draw(self, context):
        layout = self.layout
        self.draw_op(context, layout, layout, "archipack.select_parent", icon="RESTRICT_SELECT_OFF")


class ARCHIPACK_PT_window_shutter(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_window_shutter"
    bl_label = "Shutter"

    @classmethod
    def poll(cls, context):
        return archipack_window_shutter.filter(context.active_object)

    def draw(self, context):
        layout = self.layout
        self.draw_op(context, layout, layout, "archipack.select_parent", icon="RESTRICT_SELECT_OFF")


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


class ARCHIPACK_OT_window(ArchipackCreateTool, Operator):
    bl_idname = "archipack.window"
    bl_label = "Window"
    bl_description = "Window"

    x: FloatProperty(
        name='width',
        min=0.1, max=10000,
        default=2.0, precision=5,
        description='Width'
    )
    y: FloatProperty(
        name='depth',
        min=0.1, max=10000,
        default=0.20, precision=5,
        description='Depth'
    )
    z: FloatProperty(
        name='height',
        min=0.1, max=10000,
        default=1.2, precision=5,
        description='height'
    )
    altitude: FloatProperty(
        name='altitude',
        min=0.0, max=10000,
        default=1.0, precision=5,
        description='altitude'
    )
    mode: EnumProperty(
        items=(
            ('CREATE', 'Create', '', 0),
            ('REFRESH', 'Refresh', '', 2),
            ('UNIQUE', 'Make unique', '', 3),
        ),
        default='CREATE'
    )
    # auto_manipulate : BoolProperty(default=True)

    def create(self, context):
        m = bpy.data.meshes.new("Window")
        o = bpy.data.objects.new("Window", m)
        d = m.archipack_window.add()
        d.x = self.x
        d.y = self.y
        d.z = self.z
        d.altitude = self.altitude
        # Link object into scene
        self.link_object_to_scene(context, o)
        o.color = (0, 1, 0, 1)
        # select and make active
        self.select_object(context, o, True)
        self.add_material(context, o)
        self.load_preset(d)
        # select frame
        self.select_object(context, o, True)
        return o

    def update(self, context):
        o = context.active_object
        d = archipack_window.datablock(o)
        if d is not None:
            d.update(context)
            linked_objects = self.get_linked_objects(context, o)
            # bpy.ops.object.select_linked(type='OBDATA')
            for linked in linked_objects:
                if linked != o:
                    archipack_window.datablock(linked).update(context)

        # bpy.ops.object.select_all(action="DESELECT")
        # select and make active
        # self.select_object(context, o, True)

    def unique(self, context):
        act = context.active_object
        sel = context.selected_objects[:]
        uniques = []
        for o in sel:
            if archipack_window.filter(o):
                # select and make active
                uniques.append(o)
                for child in o.children:
                    d = child.data
                    if 'archipack_hole' in child or (
                            d is not None and (
                            'archipack_window_panel' in d or
                            'archipack_window_shutter' in d or
                            'archipack_dimension' in d
                            )):
                        uniques.append(child)
                        # select text and handles too
                        for c in child.children:
                            uniques.append(c)
                            
        if len(uniques) > 0:
            with ensure_select_and_restore(context, act, uniques):
                bpy.ops.object.make_single_user(type='SELECTED_OBJECTS', object=True,
                    obdata=True, material=False, animation=False)

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            if self.mode == 'CREATE':
                bpy.ops.object.select_all(action="DESELECT")
                o = self.create(context)
                o.location = self.get_cursor_location(context)
                # select and make active
                self.select_object(context, o, True)

            elif self.mode == 'REFRESH':
                self.update(context)
            elif self.mode == 'UNIQUE':
                self.unique(context)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_window_array(ArchipackCreateTool, Operator):
    bl_idname = "archipack.window_array"
    bl_label = "Window Array"
    bl_description = "Duplicate selected windows"
    bl_options = {'REGISTER', 'UNDO'}

    x: FloatProperty(
        name='Spacing',
        unit='LENGTH', subtype='DISTANCE',
        default=2.0, precision=5,
        description='Spacing, positive on the left'
    )
    count: IntProperty(
        name="Duplicates",
        min=1,
        default=1
    )
    first_location: FloatProperty(
        name='Offset',
        unit='LENGTH', subtype='DISTANCE',
        default=0.0, precision=5,
        description='Current offset from segment start'
    )
    objects_size: FloatProperty(
        name='Overall width',
        unit='LENGTH', subtype='DISTANCE',
        default=0.0, precision=5,
        description='Overall width of objects on segment'
    )
    available_space: FloatProperty(
        name='Avaliable',
        unit='LENGTH', subtype='DISTANCE',
        default=0.0, precision=5,
        description='Spacing, positive on the left'
    )
    gap_start: FloatProperty(
        name='Gap start',
        unit='LENGTH', subtype='DISTANCE',
        default=0.0, precision=5,
        description='Spacing, positive on the left'
    )
    gap_end: FloatProperty(
        name='Gap end',
        unit='LENGTH', subtype='DISTANCE',
        default=0.0, precision=5,
        description='Spacing, positive on the left'
    )
    limit_count: BoolProperty(
        name="Limit count",
        default=True
    )
    mode: EnumProperty(
        name="Mode",
        items=(
            ("START_END_COUNT", "(Auto) Start - End - Count", "Auto equal spacing"),
            ("COUNT", "(Auto) Count", "Equal space, half space on borders"),
            ("SPACING", "(Border) Spacing", "Auto equal space on borders"),
            ("START_COUNT_SPACING", "(Border) Start - Spacing - Count", ""),
            ("END_COUNT_SPACING", "(Border) End - Spacing - Count", ""),
            ("CUR_COUNT_SPACING", "(Current, Border) Spacing - Count", "About current location"),
            ("AXIS_SPACING", "(Axis) Spacing", "Auto equal space on borders"),
            ("AXIS_START_COUNT_SPACING", "(Axis) Start - Spacing- Count", ""),
            ("AXIS_END_COUNT_SPACING", "(Axis) End - Spacing - Count ", ""),
            ("AXIS_CUR_COUNT_SPACING", "(Current, Axis) Spacing - Count", "About current location")
        )
    )

    def make_array(self, context, sel):
        startplace, spacing, count = self.distribute_objects()
        for o in sel:
            d = archipack_window.datablock(o)
            if d is not None:
                # find walls this hole belongs
                hole = d.find_hole(o)
                ref = self.get_reference_point(o)
                # get custom wall, wall and wall2
                objs = self.filter_selection_loose(ref.children)
                walls_holes = set()
                for key, walls in objs.items():
                    if "wall" in key:
                        for name in walls:
                            c = self.get_scene_object(context, name)
                            m = c.modifiers.get("AutoMixedBoolean")
                            if m is not None and m.object is not None:
                                basis = m.object
                                for h_mod in basis.modifiers:
                                    if h_mod.type =="BOOLEAN" and h_mod.object == hole:
                                        walls_holes.add(basis)

                for i in range(count - 1):
                    c = self.duplicate_object(context, o, True)
                    c.parent = ref
                    p = o.matrix_world @ Vector((startplace + (i + 1) * spacing, 0, 0))
                    c.location = ref.matrix_world.inverted() @ p
                    c.matrix_world.translation = p

                    hole = d.find_hole(c)
                    if hole is not None:
                        # unlink hole
                        hole.data = hole.data.copy()
                        for basis in walls_holes:
                            m = basis.modifiers.new('AutoMerge', 'BOOLEAN')
                            m.operation = 'UNION'
                            m.object = hole

                p = o.matrix_world @ Vector((startplace, 0, 0))
                o.location = ref.matrix_world.inverted() @ p
                o.matrix_world.translation = p

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "mode", text_ctxt="archipack")

        if "START" in self.mode:
            layout.prop(self, "gap_start", text_ctxt="archipack")

        if "END" in self.mode:
            layout.prop(self, "gap_end", text_ctxt="archipack")

        if "COUNT" in self.mode:
            layout.prop(self, "count", text_ctxt="archipack")
            layout.prop(self, "limit_count", text_ctxt="archipack")

        if "SPACING" in self.mode:
            layout.prop(self, "x", text_ctxt="archipack")

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            # select and make active
            bpy.ops.archipack.disable_manipulate()
            sel = context.selected_objects[:]
            o = context.active_object
            with ensure_select_and_restore(context, o, sel):
                self.make_array(context, sel)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}

    def distribute_objects(self):

        startplace, spacing, count = 0, self.x, self.count

        if self.mode == "START_END_COUNT":
            startplace = self.gap_start + self.first_location
            available = self.available_space - (self.gap_end + self.gap_start)
            max_count = floor(available / self.objects_size)
            if self.limit_count:
                count = min(count, max_count)
            emptyspace = available - (count * self.objects_size)
            gap = emptyspace / (count - 1)
            spacing = self.objects_size + gap

        elif self.mode == "AXIS_SPACING":
            n_spaces = floor((self.available_space - self.objects_size) / spacing)
            startplace = self.first_location +  0.5 * (self.available_space - n_spaces * spacing - self.objects_size)
            count = n_spaces + 1

        elif self.mode == "SPACING":
            spacing = self.objects_size + self.x
            n_spaces = floor((self.available_space - self.objects_size) / spacing)
            count = n_spaces + 1
            gap = self.available_space - (n_spaces * spacing + self.objects_size)
            startplace = self.first_location + 0.5 * gap

        elif self.mode == "COUNT":
            max_count = floor(self.available_space / self.objects_size)
            if self.limit_count:
                count = min(count, max_count)
            emptyspace = self.available_space - (count * self.objects_size)
            gap = emptyspace / count
            spacing = self.objects_size + gap
            startplace = self.first_location + 0.5 * gap

        elif self.mode == "AXIS_START_COUNT_SPACING":
            startplace = self.gap_start + self.first_location - 0.5 * self.objects_size
            n_spaces = floor((self.available_space - self.objects_size - self.gap_start) / spacing)
            if self.limit_count:
                count = min(count, n_spaces + 1)

        elif self.mode == "START_COUNT_SPACING":
            spacing = self.objects_size + self.x
            startplace = self.gap_start + self.first_location
            available = self.available_space - self.gap_start
            max_count = floor(available / spacing)
            if self.limit_count:
                count = min(count, max_count)

        elif self.mode == "AXIS_END_COUNT_SPACING":
            n_spaces = floor((self.available_space - self.objects_size - self.gap_end) / spacing)
            if self.limit_count:
                count = min(count, n_spaces + 1)
            startplace =  self.available_space + self.first_location - count * spacing - 0.5 * self.objects_size

        elif self.mode == "END_COUNT_SPACING":
            spacing = self.objects_size + self.x
            available = self.available_space - self.gap_end
            max_count = floor(available / spacing)
            if self.limit_count:
                count = min(count, max_count)
            startplace =  self.available_space + self.first_location - count * spacing

        elif self.mode == "AXIS_CUR_COUNT_SPACING":
            n_spaces = floor((self.available_space + self.first_location - self.objects_size) / spacing)
            if self.limit_count:
                count = min(count, n_spaces + 1)

        elif self.mode == "CUR_COUNT_SPACING":
            spacing = self.objects_size + self.x
            available = self.available_space
            max_count = floor(available / spacing)
            if self.limit_count:
                count = min(count, max_count)

        return startplace, spacing, count

    def find_wall_child(self, wd, o):
        for c in wd.childs:
            if c.child_name == o.name:
                return c
        return None

    def find_wall(self, context, o):
        ref = self.get_reference_point(o)
        if ref is not None:
            objs = self.filter_selection_loose(ref.children)
            if "archipack_wall2" in objs:
                for name in objs['archipack_wall2']:
                    wall = self.get_scene_object(context, name)
                    wd = wall.data.archipack_wall2[0]
                    wg = wd.get_generator(wall)
                    child = self.find_wall_child(wd, o)
                    if child is not None:
                        return wall, wd, wg, child

        return None, None, None, -1

    def invoke(self, context, event):
        o = context.active_object
        sel = [c for c in context.selected_objects if archipack_window.filter(c)]
        d = archipack_window.datablock(o)
        # find wall segment for the active object
        wall, wd, wg, child = self.find_wall(context, o)
        if wall is not None:
            _seg_idx = child.wall_idx
            # TODO: handle multiple collinear segments
            _seg = wg.outside.segs[_seg_idx]

            childs = []
            childs.append((o, child.pos.x, d.x))
            for c in sel:
                # ensure other objects are on same segment
                if c.name != o.name:
                    child = self.find_wall_child(wd, c)
                    if child.wall_idx == _seg_idx:
                        d = archipack_window.datablock(c)
                        t = child.pos.x
                        childs.append((c, t, d.x))
            # sort by distance from start
            childs.sort(key=lambda x: x[1])

            # offset so the first border is at wall segment start
            self.first_location = (0.5 * childs[0][2]) - childs[0][1]
            # max distance between childs border - border
            self.objects_size = childs[0][1] - childs[-1][1] + 0.5 * (childs[0][2] + childs[-1][2])
            self.available_space = _seg.length
            return context.window_manager.invoke_props_dialog(self)
        else:
            self.report({'WARNING'}, "Must draw window over wall before using array")
        return {'CANCELLED'}


class ARCHIPACK_OT_window_draw(ArchipackDrawTool, Operator):
    bl_idname = "archipack.window_draw"
    bl_label = "Draw Windows"
    bl_description = "Draw Windows over walls"

    filepath: StringProperty(default="")
    feedback = None
    stack = []
    object_name = ""
    auto_manipulating = False

    @classmethod
    def poll(cls, context):
        return True

    def draw_callback(self, _self, context):
        # print("feedback.draw()")
        self.feedback.draw(context)

    def add_object(self, context, event):
        bpy.ops.object.select_all(action="DESELECT")
        # print("add_object bpy.ops.archipack.window")
        bpy.ops.archipack.window(filepath=self.filepath)
        o = context.active_object

        if o is None:
            o = context.object

        self.object_name = o.name
        # print("add_object bpy.ops.archipack.generate_hole")
        # bpy.ops.archipack.generate_hole()
        return o

    def remove_object(self, context, o):
        if archipack_window.filter(o):
            ctx = context.copy()
            ctx['object'] = o
            ctx['selected_objects'] = [o]
            bpy.ops.archipack.delete(ctx)

    def exit(self, context, o):
        self.remove_object(context, o)
        self.feedback.disable()
        context.space_data.show_gizmo = True
        bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')
        self.restore_walls(context)
        # print("window_draw.exit() success")

    def modal(self, context, event):

        context.area.tag_redraw()
        o = self.get_scene_object(context, self.object_name)

        if o is None:
            self.exit(context, None)
            return {'FINISHED'}

        d = archipack_window.datablock(o)
        hole = None
        if d is not None:
            hole = d.find_hole(o)

        # hide hole from raycast
        to_hide = [o]
        to_hide.extend(list(o.children))
        if hole is not None:
            to_hide.append(hole)

        for obj in to_hide:
            self.hide_object(obj)

        res, tM, wall, width, y, z_offset = self.mouse_hover_wall(context, event)

        for obj in to_hide:
            self.show_object(obj)
            
        if res and d is not None:
            o.matrix_world = tM.copy()
            if abs(d.y - width) > 0.001:
                self.select_object(context, o, True)
                d.y = width
                
        if event.value == 'PRESS':

            if event.type in {'C', 'D'}:
                self.exit(context, o)
                bpy.ops.archipack.window_preset_menu('INVOKE_DEFAULT', preset_operator="archipack.window_draw")
                return {'FINISHED'}

            if event.type in {'LEFTMOUSE', 'RET', 'NUMPAD_ENTER', 'SPACE'}:
                if wall is not None:

                    # self.select_object(context, wall, True)
                    ctx = context.copy()
                    ctx['object'] = wall
                    ctx['selected_objects'] = [o]
                    bpy.ops.archipack.single_boolean(ctx)

                    # o must be a window here
                    if d is not None:
                        # do it first as bend depends on this
                        if "archipack_wall2" in wall.data:
                            # ensure windows always look outside
                            with ensure_select_and_restore(context, wall, [wall]):
                                wd = wall.data.archipack_wall2[0]
                                wg = wd.get_generator()
                                wd.setup_childs(context, wall, g=wg, openings_only=True)
                                wd.relocate_childs(context, wall, g=wg)
                                wd.update_dimension(context, wall, wg)
                        # Link if y does match
                        # with last stacked one
                        # unless shift pressed
                        if len(self.stack) > 0 and not event.shift:
                            last = self.stack[-1]
                            d_last = last.data.archipack_window[0]
                            if d_last.y == d.y and len(d_last.bend) == 0 and len(d.bend) == 0:
                                # Must disable manipulators before link !!
                                bpy.ops.archipack.disable_manipulate()
                                self.link_object(last, o)

                        self.stack.append(o)
                        o = self.add_object(context, event)

                        o.matrix_world = tM
                        
                    return {'RUNNING_MODAL'}

            # prevent selection of other object
            if event.type in {'RIGHTMOUSE'}:
                return {'RUNNING_MODAL'}

        if self.keymap.check(event, self.keymap.undo) or (
                event.type in {'BACK_SPACE'} and event.value == 'RELEASE'
                ):
            if len(self.stack) > 0:
                last = self.stack.pop()
                self.remove_object(context, last)
            return {'RUNNING_MODAL'}

        if event.value == 'RELEASE':

            if event.type in {'ESC', 'RIGHTMOUSE'}:
                self.exit(context, o)
                return {'FINISHED'}

        return {'PASS_THROUGH'}

    def invoke(self, context, event):

        if context.mode == "OBJECT":
            o = context.active_object

            self.stack = []
            self.keymap = Keymaps(context)

            # exit manipulate_mode if any
            bpy.ops.archipack.disable_manipulate()
            # Hide manipulators
            context.space_data.show_gizmo = False
            # invoke with alt pressed will use current object as basis for linked copy
            if self.filepath == '' and archipack_window.filter(o):
                self.stack.append(o)
                o = self.duplicate_object(context, o, False)
                self.object_name = o.name
            else:
                o = self.add_object(context, event)

            # select and make active
            # bpy.ops.object.select_all(action="DESELECT")
            # self.select_object(context, o, True)
            
            self.feedback = FeedbackPanel()
            self.feedback.instructions(context, "Draw a window", "Click & Drag over a wall", [
                ('LEFTCLICK, RET, SPACE, ENTER', 'Create a window'),
                ('BACKSPACE, CTRL+Z', 'undo last'),
                ('D', 'Draw another window'),
                ('SHIFT', 'Make independant copy'),
                ('RIGHTCLICK or ESC', 'exit')
                ])
            self.feedback.enable()
            args = (self, context)
            self._handle = bpy.types.SpaceView3D.draw_handler_add(self.draw_callback, args, 'WINDOW', 'POST_PIXEL')
            context.window_manager.modal_handler_add(self)
            return {'RUNNING_MODAL'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_window_portals(Operator):
    bl_idname = "archipack.window_portals"
    bl_label = "Portals"
    bl_description = "Create portal for each window"
    bl_options = {'REGISTER', 'UNDO'}

    @classmethod
    def poll(self, context):
        return True

    def invoke(self, context, event):
        for o in context.scene.objects:
            d = archipack_window.datablock(o)
            if d is not None:
                d.update_portal(context)

        return {'FINISHED'}


class archipack_window_curtain(ArchipackObject, PropertyGroup):

    def update(self, context, d, x, z,
               side="LEFT",
               random_y=0.02,
               open=1.0,
               self_distance_min=0.015):

        process_frames = d.process_frames
        end_frame = d.process_frames
        air_damping = 0.001

        o = context.object
        actions = set([a.name for a in bpy.data.actions])

        frame_current = context.scene.frame_current
        n_attach = max(3, x / d.attach_spacing)
        rh = max(2, int(n_attach * d.resolution))
        rv = d.resolution_v  #max(5, int(rh / x * z))
        vg_spacing = max(1, int(rh / n_attach))
        dx = x / (rh - 2)
        dz = z / (rv - 1)
        # left
        if side == "LEFT":
            h0 = 0
        elif side == "RIGHT":
            h0 = -x
        else:
            h0 = -0.5 * x

        # Build a sinuzoid between attachs
        #    _
        #   / \
        #  |   |   |
        #       \_/
        #
        # steps 1 2
        # segs  4 8
        steps = (d.resolution - 1)
        da = pi / steps
        y = d.y
        dy = 0.1 * y
        verts = [Vector((h0 + h * dx, uniform(-dy, dy) + y * sin(da * h), z - v * dz)) for v in range(rv) for h in range(rh)]
        faces = [(j * rh + i,
                  j * rh + i + 1,
                  j * rh + i + rh + 1,
                  j * rh + i + rh)
                 for i in range(rh - 1)
                 for j in range(rv - 1)]
        du = x / rh
        dv = z / rv
        uvs = [[(i * du, j * dv),
                ((i + 1) * du, j * dv),
                ((i + 1) * du, (j + 1) * dv),
                (i * du , (j + 1) * dv)]
               for i in range(rh - 1)
               for j in range(rv - 1)]
        bmed.buildmesh(o, verts, faces, uvs=uvs)
        self.shade_smooth(context, o, pi)

        # Setup pin vertex group
        off = 0
        if (rh % vg_spacing) > 0.5 * vg_spacing:
            off = int(0.5 * vg_spacing)
        vgroup_top = [int(off + i) for i in range(0, rh, vg_spacing)]
        for v in o.data.vertices:
            v.select = v.index in vgroup_top
        vgroup = o.vertex_groups.get("Top")
        if vgroup is None:
            vgroup = o.vertex_groups.new(name="Top")
        vgroup.add(vgroup_top, 1, 'REPLACE')

        # Add shape keys
        basis = o.shape_key_add()
        basis.name = "Basis"
        o.data.update()
        sk = o.shape_key_add()
        sk.name = "Rescale"
        o.data.update()

        # Animate rescale shape key
        sk.keyframe_insert("value", frame=1)
        sk.value = 1.0
        sk.keyframe_insert("value", frame=end_frame)

        # Set closed vertex location to shape
        scale = n_attach * 1.0 * self_distance_min / x
        verts = o.data.shape_keys.key_blocks[sk.name].data
        for i in vgroup_top:
            px, py, pz = verts[i].co.to_3d()
            verts[i].co = Vector((px * scale, py * uniform(-random_y, random_y), pz))

        # Setup cloth modifier
        context.scene.frame_current = 1
        m = o.modifiers.new("Cloth", type="CLOTH")
        m.settings.mass = d.mass
        m.settings.air_damping = air_damping
        m.settings.vertex_group_mass = "Top"
        m.settings.tension_stiffness = 5
        m.settings.compression_stiffness = 5
        m.settings.shear_stiffness = 5
        m.settings.bending_stiffness = 0.05
        m.collision_settings.use_collision = False
        m.collision_settings.use_self_collision = True
        m.collision_settings.self_impulse_clamp = 0.1
        m.collision_settings.self_distance_min = self_distance_min
        m.collision_settings.vertex_group_self_collisions = "Top"

        # Process cloth
        for i in range(process_frames):
            context.scene.frame_set(i)

        bpy.ops.object.modifier_apply(apply_as='SHAPE', modifier="Cloth")

        # Remove rescale keyframes and shape key
        sk.keyframe_delete("value", frame=1)
        sk.keyframe_delete("value", frame=end_frame)
        o.active_shape_key_index = 1
        bpy.ops.object.shape_key_remove(all=False)

        # Set open value to Curtain shape key
        o.active_shape_key_index = 1
        o.active_shape_key.value = open

        # clean up actions
        to_remove = set([
            a.name for a in bpy.data.actions
            if a.name not in actions and a.users < 1])

        for a_name in to_remove:
            a = bpy.data.actions[a_name]
            bpy.data.actions.remove(a)

        # Clean up vertex group
        o.vertex_groups.remove(vgroup)

        # Corners crease
        bm = bmed._start(o)
        bmed.ensure_bmesh(bm)
        layer = bm.edges.layers.crease.verify()
        edges = [ed for v in bm.verts for ed in v.link_edges if len(v.link_edges) == 2]
        for ed in edges:
            ed[layer] = 1.0
        bmed._end(bm, o)

        m = o.modifiers.new("Subsurf", type="SUBSURF")
        m.levels = 2

        context.scene.frame_current = frame_current


class ARCHIPACK_PT_window_curtain(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_window_curtain"
    bl_label = "Curtain"

    @classmethod
    def poll(cls, context):
        return archipack_window_curtain.filter(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_window_curtain.datablock(o)
        if d is None:
            return
        layout = self.layout

        self.draw_op(context, layout, layout, "archipack.delete", icon="TRASH")
        box = layout.box()

        if hasattr(o.data, 'shape_keys') and "Cloth" in o.data.shape_keys.key_blocks:
            layout.prop(o.data.shape_keys.key_blocks['Cloth'], "value", text="Open")

        if "archipack_material" in o:
            o.archipack_material[0].draw(context, box)


class ARCHIPACK_OT_window_curtain(Archipacki18n, ArchipackCreateTool, Operator):
    bl_idname = "archipack.window_curtain"
    bl_label = "Create curtains (very slow)"
    bl_description = "Create random pyhsical based curtain for each selected window, very slow (6 - 30+ sec / window)"
    y: FloatProperty(
        precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        name="Curtain Depth",
        description="Curtain depth when open",
        min=0,
        default=0.06
    )
    dy: FloatProperty(
        precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        name="Wall Distance",
        description="Offset from wall",
        min=0,
        default=0.15
    )
    x: FloatProperty(
        precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        name="Horizontal overflow",
        description="Horizontal overflow",
        min=0,
        default=0
    )
    z: FloatProperty(
        precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        name="Vertical overflow",
        description="Vertical overflow",
        min=0,
        default=0
    )
    z0: FloatProperty(
        precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        name="Bottom offset",
        description="Bottom offset from window frame",
        default=0
    )
    oversize: FloatProperty(
        precision=1, step=1,
        subtype='PERCENTAGE',
        name="Oversize",
        description="Oversize curtain so they are not flat when closed",
        min=0, max=100,
        default=30
    )

    open: FloatProperty(
        precision=1, step=1,
        subtype='PERCENTAGE',
        name="Open",
        description="Open percent",
        min=0, max=100,
        default=50
    )
    random_open: FloatProperty(
        precision=1, step=1,
        subtype='PERCENTAGE',
        name="Random open",
        description="Randomize open",
        min=0, max=100,
        default=50
    )
    attach_spacing: FloatProperty(
        precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        name="Attach spacing",
        description="Attach spacing",
        min=0.05,
        max=0.5,
        default=0.2
    )
    resolution: IntProperty(
        name="Resolution",
        description="Amount of geometry subdivisions between attaches",
        min=2,
        max=10,
        default=4
    )
    resolution_v: IntProperty(
        name="Resolution vertical",
        description="Amount of vertical subdivisions of curtain",
        min=2,
        max=20,
        default=10
    )
    process_frames: IntProperty(
        name="Processing iterations",
        description="Amount of frames for cloth simulation",
        min=100,
        max=500,
        default=300
    )

    mass: FloatProperty(
        precision=5, step=1,
        name="Mass",
        description="Mass of curtain tissue",
        min=0.01,
        default=5
    )
    advanced: BoolProperty(
        name="Show advanced params",
        default=False
    )
    @classmethod
    def poll(self, context):
        return True

    def create(self, context):
        m = bpy.data.meshes.new("Curtain")
        o = bpy.data.objects.new("Curtain", m)
        o['archipack_skip_material'] = True
        d = m.archipack_window_curtain.add()
        self.link_object_to_scene(context, o)
        o.color = (0, 1, 0, 1)
        # select and make active
        self.select_object(context, o, True)
        self.add_material(context, o, material="DEFAULT")
        self.select_object(context, o, True)
        return o, d

    def execute(self, context):
        sel = context.selected_objects[:]
        bpy.ops.object.select_all(action="DESELECT")
        t = time.time()
        n = 0
        # absolute size when open at maximum
        self_distance_min = 0.01
        context.window.cursor_set("WAIT")
        for o in sel:
            d = archipack_window.datablock(o)
            if d is not None:
                x, y = 0.5 * d.x + d.frame_overflow + self.x, 0.5 * d.y
                sz = d.z + min(d.frame_x, d.frame_overflow) + self.z - self.z0
                sx = x * (1 + self.oversize /100)
                ro = self.random_open / 200

                open = min(1, max(0, self.open / 100 + uniform(-ro, ro)))
                c, cd = self.create(context)
                self.select_object(context, c, True)
                cd.update(context, self, sx, sz, side="LEFT", self_distance_min=self_distance_min, open=open)
                c.parent = o
                c.matrix_world = o.matrix_world.copy()
                c.location = Vector((-x, self.dy + y, d.altitude + self.z0))

                open = min(1, max(0, self.open / 100 + uniform(-ro, ro)))
                c, cd = self.create(context)
                self.select_object(context, c, True)
                cd.update(context, self, sx, sz, side="RIGHT", self_distance_min=self_distance_min, open=open)
                c.parent = o
                c.matrix_world = o.matrix_world.copy()
                c.location = Vector((x, self.dy + y, d.altitude + self.z0))
                n += 2
                print("Curtains %s done" % n)
        context.window.cursor_set("DEFAULT")
        dt = time.time()-t
        print("Curtains %s %.2f seconds (%.2f seconds each)" % (n, dt, dt / n))
        return {'FINISHED'}

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "dy", text_ctxt=self.translation_context)
        layout.prop(self, "x", text_ctxt=self.translation_context)
        layout.prop(self, "z", text_ctxt=self.translation_context)
        layout.prop(self, "z0", text_ctxt=self.translation_context)
        layout.prop(self, "open", text_ctxt=self.translation_context)
        layout.prop(self, "random_open", text_ctxt=self.translation_context)
        layout.separator()
        icon = "TRIA_RIGHT"
        if self.advanced:
            icon = "TRIA_DOWN"
        layout.prop(self, "advanced", icon=icon, text_ctxt=self.translation_context)
        if self.advanced:
            layout.prop(self, "y", text_ctxt=self.translation_context)
            layout.prop(self, "oversize", text_ctxt=self.translation_context)
            layout.prop(self, "mass", text_ctxt=self.translation_context)
            layout.prop(self, "attach_spacing", text_ctxt=self.translation_context)
            layout.prop(self, "resolution", text_ctxt=self.translation_context)
            layout.prop(self, "resolution_v", text_ctxt=self.translation_context)
            layout.prop(self, "process_frames", text_ctxt=self.translation_context)
        layout.separator()

    def invoke(self, context, event):
        return context.window_manager.invoke_props_dialog(self)


# ------------------------------------------------------------------
# Define operator class to load / save presets
# ------------------------------------------------------------------

class ARCHIPACK_OT_window_preset_draw(PresetMenuOperator, Operator):
    bl_description = "Choose a preset and draw windows over wall"
    bl_idname = "archipack.window_preset_draw"
    bl_label = "Window preset"
    preset_subdir = "archipack_window"
    
    
class ARCHIPACK_OT_window_preset_menu(PresetMenuOperator, Operator):
    bl_description = "Show Window Presets"
    bl_idname = "archipack.window_preset_menu"
    bl_label = "Window preset"
    preset_subdir = "archipack_window"


class ARCHIPACK_OT_window_preset(ArchipackPreset, Operator):
    bl_description = "Add / remove a Window Preset"
    bl_idname = "archipack.window_preset"
    bl_label = "Window preset"
    preset_menu = "ARCHIPACK_OT_window_preset_menu"

    @property
    def blacklist(self):
        return ['y']


def register():
    bpy.utils.register_class(archipack_window_bend)
    bpy.utils.register_class(archipack_window_panelrow)
    bpy.utils.register_class(archipack_window_panel)
    Mesh.archipack_window_panel = CollectionProperty(type=archipack_window_panel)
    bpy.utils.register_class(ARCHIPACK_PT_window_panel)

    bpy.utils.register_class(archipack_window_shutter)
    Mesh.archipack_window_shutter = CollectionProperty(type=archipack_window_shutter)
    bpy.utils.register_class(ARCHIPACK_PT_window_shutter)

    bpy.utils.register_class(archipack_window)
    Mesh.archipack_window = CollectionProperty(type=archipack_window)
    bpy.utils.register_class(ARCHIPACK_OT_window_preset_menu)
    bpy.utils.register_class(ARCHIPACK_OT_window_preset_draw)
    bpy.utils.register_class(ARCHIPACK_PT_window)
    bpy.utils.register_class(ARCHIPACK_OT_window)
    bpy.utils.register_class(ARCHIPACK_OT_window_preset)
    bpy.utils.register_class(ARCHIPACK_OT_window_draw)
    bpy.utils.register_class(ARCHIPACK_OT_window_portals)
    bpy.utils.register_class(archipack_window_curtain)
    Mesh.archipack_window_curtain = CollectionProperty(type=archipack_window_curtain)
    bpy.utils.register_class(ARCHIPACK_OT_window_curtain)
    bpy.utils.register_class(ARCHIPACK_PT_window_curtain)
    bpy.utils.register_class(ARCHIPACK_OT_window_array)


def unregister():

    bpy.utils.unregister_class(ARCHIPACK_OT_window_array)
    bpy.utils.unregister_class(archipack_window_panelrow)
    bpy.utils.unregister_class(archipack_window_panel)

    bpy.utils.unregister_class(ARCHIPACK_PT_window_panel)
    del Mesh.archipack_window_panel

    bpy.utils.unregister_class(archipack_window_shutter)
    bpy.utils.unregister_class(ARCHIPACK_PT_window_shutter)
    del Mesh.archipack_window_shutter

    bpy.utils.unregister_class(archipack_window)
    bpy.utils.unregister_class(archipack_window_bend)
    del Mesh.archipack_window

    bpy.utils.unregister_class(ARCHIPACK_OT_window_preset_menu)
    bpy.utils.unregister_class(ARCHIPACK_OT_window_preset_draw)
    bpy.utils.unregister_class(ARCHIPACK_PT_window)
    bpy.utils.unregister_class(ARCHIPACK_OT_window)
    bpy.utils.unregister_class(ARCHIPACK_OT_window_preset)
    bpy.utils.unregister_class(ARCHIPACK_OT_window_draw)
    bpy.utils.unregister_class(ARCHIPACK_OT_window_portals)

    bpy.utils.unregister_class(ARCHIPACK_OT_window_curtain)
    bpy.utils.unregister_class(ARCHIPACK_PT_window_curtain)
    bpy.utils.unregister_class(archipack_window_curtain)
    del Mesh.archipack_window_curtain
