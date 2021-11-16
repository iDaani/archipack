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
# Author: Stephen Leger (s-leger)
#
# ----------------------------------------------------------

# noinspection PyUnresolvedReferences
import bpy
# noinspection PyUnresolvedReferences
from bpy.types import Operator, PropertyGroup, Mesh, Panel
from bpy.props import (
    FloatProperty, IntProperty, CollectionProperty, IntVectorProperty,
    EnumProperty, BoolProperty, StringProperty
    )
from mathutils import Vector
from math import pi, cos, sin
from random import uniform
# door component objects (panels, handles ..)
from .bmesh_utils import BmeshEdit as bmed
from .panel import Panel as DoorPanel
from .archipack_handle import create_handle, door_handle_horizontal_01
from .archipack_manipulator import Manipulable
from .archipack_preset import ArchipackPreset, PresetMenuOperator
from .archipack_abstraction import ensure_select_and_restore
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackPanel,
    ArchipackObject, 
    ArchipackCreateTool, 
    ArchipackDrawTool,
    ArchipackObjectsManager
    )
from .archipack_gl import FeedbackPanel
from .archipack_segments2 import OpeningGenerator
from .archipack_keymaps import Keymaps
from .archipack_dimension import DimensionProvider
from .archipack_material import build_mat_enum


SPACING = 0.005
BATTUE = 0.01
BOTTOM_HOLE_MARGIN = 0.001
FRONT_HOLE_MARGIN = 0.1


def update(self, context):
    self.update(context)


def update_childs(self, context):
    self.update(context, childs_only=True)


def random_color():
    return Vector((uniform(0.0, 1.0), uniform(0.0, 1.0), uniform(0.0, 1.0), 1))


MAT_FRAME_INSIDE = 0
MAT_FRAME_OUTSIDE = 1
MAT_DOOR_INSIDE = 2
MAT_DOOR_OUTSIDE = 3
MAT_PANEL_INSIDE = 4
MAT_PANEL_OUTSIDE = 5
MAT_SILL = 6
MAT_HANDLE = 7
MAT_SOIL = 8


# keep a reference to material enums
material_enums = []
mat_enum, mat_index_getter, mat_index_setter = build_mat_enum(
    'idmat', material_enums)


class archipack_door_panel(ArchipackObject, PropertyGroup):

    def symbol_2d(self, x):

        #  __ y0
        # |__ y1
        # x0
        y0 = x
        y1 = 0
        x0 = 0
        return DoorPanel(
            False,               # profil closed
            [0, 0],           # x index
            [x0],
            [y0, y1],
            [0, 0],           # material index
            closed_path=True,    #
            side_cap_front=1,     # cap index
            side_cap_back=0
            )

    def panels(self, d, y):

        # subdivide side to weld panels
        subdiv_x = d.panels_x - 1

        if d.panels_distrib == 'REGULAR':
            subdiv_y = d.panels_y - 1
        else:
            subdiv_y = 2

        #  __ y0
        # |__ y1
        # x0 x1
        y0 = -y
        y1 = 0
        x0 = 0
        x1 = max(0.001, d.panel_border - 0.5 * d.panel_spacing)
        mi, mo = d.id_mat(MAT_DOOR_INSIDE), d.id_mat(MAT_DOOR_OUTSIDE)
        mpi, mpo = d.id_mat(MAT_PANEL_INSIDE), d.id_mat(MAT_PANEL_OUTSIDE)
        side = DoorPanel(
            False,               # profil closed
            [1, 0, 0, 1],           # x index
            [x0, x1],
            [y0, y0, y1, y1],
            [mo, mi, mi, mi],           # material index
            closed_path=True,    #
            subdiv_x=subdiv_x,
            subdiv_y=subdiv_y
            )

        face = None
        back = None

        if d.model == 1:
            #     /   y2-y3
            #  __/    y1-y0
            #   x2 x3
            x2 = 0.5 * d.panel_spacing
            x3 = x2 + d.chanfer
            y2 = y1 + d.chanfer
            y3 = y0 - d.chanfer

            face = DoorPanel(
                False,              # profil closed
                [0, 1, 2],            # x index
                [0, x2, x3],
                [y1, y1, y2],
                [mi, mpi, mpi],             # material index
                side_cap_front=2,    # cap index
                closed_path=True
                )

            back = DoorPanel(
                False,              # profil closed
                [0, 1, 2],            # x index
                [x3, x2, 0],
                [y3, y0, y0],
                [mpo, mo, mo],             # material index
                side_cap_back=0,     # cap index
                closed_path=True
                )

        elif d.model == 2:
            #               /   y2-y3
            #  ___    _____/    y1-y0
            #     \  /
            #      \/           y4-y5
            # 0 x2 x4 x5 x6 x3
            x2 = 0.5 * d.panel_spacing
            x4 = x2 + d.chanfer
            x5 = x4 + d.chanfer
            x6 = x5 + 4 * d.chanfer
            x3 = x6 + d.chanfer
            y2 = y1 - d.chanfer
            y4 = y1 + d.chanfer
            y3 = y0 + d.chanfer
            y5 = y0 - d.chanfer
            face = DoorPanel(
                False,                    # profil closed
                [0, 1, 2, 3, 4, 5],            # x index
                [0, x2, x4, x5, x6, x3],
                [y1, y1, y4, y1, y1, y2],
                [mi, mi, mi, mpi, mpi, mpi],  # material index
                side_cap_front=5,          # cap index
                closed_path=True
                )

            back = DoorPanel(
                False,                    # profil closed
                [0, 1, 2, 3, 4, 5],            # x index
                [x3, x6, x5, x4, x2, 0],
                [y3, y0, y0, y5, y0, y0],
                [mpo, mpo, mo, mo, mo, mo],  # material index
                side_cap_back=0,          # cap index
                closed_path=True
                )

        elif d.model == 3:
            #      _____      y2-y3
            #     /     \     y4-y5
            #  __/            y1-y0
            # 0 x2 x3 x4 x5
            x2 = 0.5 * d.panel_spacing
            x3 = x2 + d.chanfer
            x4 = x3 + 4 * d.chanfer
            x5 = x4 + 2 * d.chanfer
            y2 = y1 - d.chanfer
            y3 = y0 + d.chanfer
            y4 = y2 + d.chanfer
            y5 = y3 - d.chanfer
            face = DoorPanel(
                False,              # profil closed
                [0, 1, 2, 3, 4],            # x index
                [0, x2, x3, x4, x5],
                [y1, y1, y2, y2, y4],
                [mi, mi, mpi, mpi, mpi],  # material index
                side_cap_front=4,    # cap index
                closed_path=True
                )

            back = DoorPanel(
                False,              # profil closed
                [0, 1, 2, 3, 4],            # x index
                [x5, x4, x3, x2, 0],
                [y5, y3, y3, y0, y0],
                [mpo, mpo, mo, mo, mo],  # material index
                side_cap_back=0,     # cap index
                closed_path=True
                )

        else:
            side.side_cap_front = 3
            side.side_cap_back = 0

        return side, face, back

    def verts(self, d, side, face, back, x, z, direction):
        if d.panels_distrib == 'REGULAR':
            subdiv_y = d.panels_y - 1
        else:
            subdiv_y = 2

        radius = Vector((0.8, 0.5, 0))
        center = Vector((0, z - radius.x, 0))

        if direction == 0:
            pivot = 1
        else:
            pivot = -1

        path_type = 'RECTANGLE'
        curve_steps = 16

        x1 = max(0.001, d.panel_border - 0.5 * d.panel_spacing)
        bottom_z = d.panel_bottom
        shape_z = [0, bottom_z, bottom_z, 0]
        origin = Vector((-pivot * 0.5 * x, 0, 0))
        offset = Vector((0, 0, 0))
        size = Vector((x, z, 0))
        verts = side.vertices(curve_steps, offset, center, origin,
            size, radius, 0, pivot, shape_z=shape_z, path_type=path_type)
        if face is not None:
            p_radius = radius.copy()
            p_radius.x -= x1
            p_radius.y -= x1
            if d.panels_distrib == 'REGULAR':
                p_size = Vector(((x - 2 * x1) / d.panels_x,
                    (z - 2 * x1 - bottom_z) / d.panels_y, 0))
                for i in range(d.panels_x):
                    for j in range(d.panels_y):
                        if j < subdiv_y:
                            shape = 'RECTANGLE'
                        else:
                            shape = path_type
                        offset = Vector(((pivot * 0.5 * x) + p_size.x * (i + 0.5) - 0.5 * size.x + x1,
                            bottom_z + p_size.y * j + x1, 0))
                        origin = Vector((p_size.x * (i + 0.5) - 0.5 * size.x + x1, bottom_z + p_size.y * j + x1, 0))
                        verts += face.vertices(curve_steps, offset, center, origin,
                            p_size, p_radius, 0, 0, shape_z=None, path_type=shape)
                        if back is not None:
                            verts += back.vertices(curve_steps, offset, center, origin,
                                p_size, p_radius, 0, 0, shape_z=None, path_type=shape)
            else:
                ####################################
                # Ratio vertical panels 1/3 - 2/3
                ####################################
                p_size = Vector(((x - 2 * x1) / d.panels_x, (z - 2 * x1 - bottom_z) / 3, 0))
                p_size_2x = Vector((p_size.x, p_size.y * 2, 0))
                for i in range(d.panels_x):
                    j = 0
                    offset = Vector(((pivot * 0.5 * x) + p_size.x * (i + 0.5) - 0.5 * size.x + x1,
                        bottom_z + p_size.y * j + x1, 0))
                    origin = Vector((p_size.x * (i + 0.5) - 0.5 * size.x + x1, bottom_z + p_size.y * j + x1, 0))
                    shape = 'RECTANGLE'
                    face.subdiv_y = 0
                    verts += face.vertices(curve_steps, offset, center, origin,
                        p_size, p_radius, 0, 0, shape_z=None, path_type=shape)
                    if back is not None:
                        back.subdiv_y = 0
                        verts += back.vertices(curve_steps, offset, center, origin,
                            p_size, p_radius, 0, 0, shape_z=None, path_type=shape)
                    j = 1
                    offset = Vector(((pivot * 0.5 * x) + p_size.x * (i + 0.5) - 0.5 * size.x + x1,
                        bottom_z + p_size.y * j + x1, 0))
                    origin = Vector((p_size.x * (i + 0.5) - 0.5 * size.x + x1,
                        bottom_z + p_size.y * j + x1, 0))
                    shape = path_type
                    face.subdiv_y = 1
                    verts += face.vertices(curve_steps, offset, center, origin,
                        p_size_2x, p_radius, 0, 0, shape_z=None, path_type=path_type)
                    if back is not None:
                        back.subdiv_y = 1
                        verts += back.vertices(curve_steps, offset, center, origin,
                            p_size_2x, p_radius, 0, 0, shape_z=None, path_type=path_type)

        return verts

    def faces(self, d, side, face, back):
        if d.panels_distrib == 'REGULAR':
            subdiv_y = d.panels_y - 1
        else:
            subdiv_y = 2

        path_type = 'RECTANGLE'
        curve_steps = 16

        faces = side.faces(curve_steps, path_type=path_type)
        faces_offset = side.n_verts(curve_steps, path_type=path_type)

        if face is not None:
            if d.panels_distrib == 'REGULAR':
                for i in range(d.panels_x):
                    for j in range(d.panels_y):
                        if j < subdiv_y:
                            shape = 'RECTANGLE'
                        else:
                            shape = path_type
                        faces += face.faces(curve_steps, path_type=shape, offset=faces_offset)
                        faces_offset += face.n_verts(curve_steps, path_type=shape)
                        if back is not None:
                            faces += back.faces(curve_steps, path_type=shape, offset=faces_offset)
                            faces_offset += back.n_verts(curve_steps, path_type=shape)
            else:
                ####################################
                # Ratio vertical panels 1/3 - 2/3
                ####################################
                for i in range(d.panels_x):
                    j = 0
                    shape = 'RECTANGLE'
                    face.subdiv_y = 0
                    faces += face.faces(curve_steps, path_type=shape, offset=faces_offset)
                    faces_offset += face.n_verts(curve_steps, path_type=shape)
                    if back is not None:
                        back.subdiv_y = 0
                        faces += back.faces(curve_steps, path_type=shape, offset=faces_offset)
                        faces_offset += back.n_verts(curve_steps, path_type=shape)
                    j = 1
                    shape = path_type
                    face.subdiv_y = 1
                    faces += face.faces(curve_steps, path_type=path_type, offset=faces_offset)
                    faces_offset += face.n_verts(curve_steps, path_type=path_type)
                    if back is not None:
                        back.subdiv_y = 1
                        faces += back.faces(curve_steps, path_type=path_type, offset=faces_offset)
                        faces_offset += back.n_verts(curve_steps, path_type=path_type)

        return faces

    def uvs(self, d, side, face, back, x, z, direction):
        if d.panels_distrib == 'REGULAR':
            subdiv_y = d.panels_y - 1
        else:
            subdiv_y = 2

        radius = Vector((0.8, 0.5, 0))
        center = Vector((0, z - radius.x, 0))

        if direction == 0:
            pivot = 1
        else:
            pivot = -1

        path_type = 'RECTANGLE'
        curve_steps = 16

        x1 = max(0.001, d.panel_border - 0.5 * d.panel_spacing)
        bottom_z = d.panel_bottom
        origin = Vector((-pivot * 0.5 * x, 0, 0))
        size = Vector((x, z, 0))
        uvs = side.uv(curve_steps, center, origin, size, radius, 0, pivot, 0, d.panel_border, path_type=path_type)
        if face is not None:
            p_radius = radius.copy()
            p_radius.x -= x1
            p_radius.y -= x1
            if d.panels_distrib == 'REGULAR':
                p_size = Vector(((x - 2 * x1) / d.panels_x, (z - 2 * x1 - bottom_z) / d.panels_y, 0))
                for i in range(d.panels_x):
                    for j in range(d.panels_y):
                        if j < subdiv_y:
                            shape = 'RECTANGLE'
                        else:
                            shape = path_type
                        origin = Vector((p_size.x * (i + 0.5) - 0.5 * size.x + x1, bottom_z + p_size.y * j + x1, 0))
                        uvs += face.uv(curve_steps, center, origin, p_size, p_radius, 0, 0, 0, 0, path_type=shape)
                        if back is not None:
                            uvs += back.uv(curve_steps, center, origin,
                                p_size, p_radius, 0, 0, 0, 0, path_type=shape)
            else:
                ####################################
                # Ratio vertical panels 1/3 - 2/3
                ####################################
                p_size = Vector(((x - 2 * x1) / d.panels_x, (z - 2 * x1 - bottom_z) / 3, 0))
                p_size_2x = Vector((p_size.x, p_size.y * 2, 0))
                for i in range(d.panels_x):
                    j = 0
                    origin = Vector((p_size.x * (i + 0.5) - 0.5 * size.x + x1, bottom_z + p_size.y * j + x1, 0))
                    shape = 'RECTANGLE'
                    face.subdiv_y = 0
                    uvs += face.uv(curve_steps, center, origin, p_size, p_radius, 0, 0, 0, 0, path_type=shape)
                    if back is not None:
                        back.subdiv_y = 0
                        uvs += back.uv(curve_steps, center, origin, p_size, p_radius, 0, 0, 0, 0, path_type=shape)
                    j = 1
                    origin = Vector((p_size.x * (i + 0.5) - 0.5 * size.x + x1, bottom_z + p_size.y * j + x1, 0))
                    shape = path_type
                    face.subdiv_y = 1
                    uvs += face.uv(curve_steps, center, origin, p_size_2x, p_radius, 0, 0, 0, 0, path_type=path_type)
                    if back is not None:
                        back.subdiv_y = 1
                        uvs += back.uv(curve_steps, center, origin,
                            p_size_2x, p_radius, 0, 0, 0, 0, path_type=path_type)
        return uvs

    def matids(self, d, side, face, back):
        if d.panels_distrib == 'REGULAR':
            subdiv_y = d.panels_y - 1
        else:
            subdiv_y = 2

        path_type = 'RECTANGLE'
        curve_steps = 16
        mi, mo = d.id_mat(MAT_DOOR_INSIDE), d.id_mat(MAT_DOOR_OUTSIDE)
        mat = side.mat(curve_steps, mi, mo, path_type=path_type)

        if face is not None:
            mi, mo = d.id_mat(MAT_PANEL_INSIDE), d.id_mat(MAT_PANEL_OUTSIDE)
            if d.panels_distrib == 'REGULAR':
                for i in range(d.panels_x):
                    for j in range(d.panels_y):
                        if j < subdiv_y:
                            shape = 'RECTANGLE'
                        else:
                            shape = path_type
                        mat.extend(
                            face.mat(curve_steps, mi, mi, path_type=shape)
                        )
                        if back is not None:
                            mat.extend(
                                back.mat(curve_steps, mo, mo, path_type=shape)
                            )
            else:
                ####################################
                # Ratio vertical panels 1/3 - 2/3
                ####################################
                for i in range(d.panels_x):
                    j = 0
                    shape = 'RECTANGLE'
                    face.subdiv_y = 0
                    mat.extend(
                        face.mat(curve_steps, mi, mi, path_type=shape)
                    )
                    if back is not None:
                        back.subdiv_y = 0
                        mat.extend(
                            back.mat(curve_steps, mo, mo, path_type=shape)
                        )
                    j = 1
                    shape = path_type
                    face.subdiv_y = 1
                    mat.extend(
                        face.mat(curve_steps, mi, mi, path_type=shape)
                    )
                    if back is not None:
                        back.subdiv_y = 1
                        mat.extend(
                            back.mat(curve_steps, mo, mo, path_type=shape)
                        )
        return mat

    def vcolors(self, d, side, face, back):
        if d.panels_distrib == 'REGULAR':
            subdiv_y = d.panels_y - 1
        else:
            subdiv_y = 2

        path_type = 'RECTANGLE'
        curve_steps = 16

        col = random_color()
        vcolors = side.vcolors(curve_steps, col, path_type=path_type)

        if face is not None:
            if d.panels_distrib == 'REGULAR':
                for i in range(d.panels_x):
                    for j in range(d.panels_y):
                        col = random_color()
                        if j < subdiv_y:
                            shape = 'RECTANGLE'
                        else:
                            shape = path_type
                        vcolors.extend(
                            face.vcolors(curve_steps, col, path_type=shape)
                        )
                        if back is not None:
                            vcolors.extend(
                                back.vcolors(curve_steps, col, path_type=shape)
                            )
            else:
                ####################################
                # Ratio vertical panels 1/3 - 2/3
                ####################################
                for i in range(d.panels_x):
                    col = random_color()
                    j = 0
                    shape = 'RECTANGLE'
                    face.subdiv_y = 0
                    vcolors.extend(
                        face.vcolors(curve_steps, col, path_type=shape)
                    )
                    if back is not None:
                        back.subdiv_y = 0
                        vcolors.extend(
                            back.vcolors(curve_steps, col, path_type=shape)
                        )
                    j = 1
                    shape = path_type
                    face.subdiv_y = 1
                    vcolors.extend(
                        face.vcolors(curve_steps, col, path_type=path_type)
                    )
                    if back is not None:
                        back.subdiv_y = 1
                        vcolors.extend(
                            back.vcolors(curve_steps, col, path_type=path_type)
                        )
        return vcolors

    def find_handle(self, o):
        for child in o.children:
            if 'archipack_handle' in child:
                return child
        return None

    def update_handle(self, context, o, d, x, y, z, direction):
        handle = self.find_handle(o)
        if handle is None:
            m = bpy.data.meshes.new("Handle")
            handle = create_handle(context, o, m)
        self.link_materials(context, o, handle)
        verts, faces = door_handle_horizontal_01(direction, 1)
        b_verts, b_faces = door_handle_horizontal_01(direction, 0, offset=len(verts))
        b_verts = [(v[0], v[1] - y, v[2]) for v in b_verts]
        handle_y = 0.07
        handle.location = ((1 - direction * 2) * (x - handle_y), 0, 0.5 * z)
        bmed.buildmesh(handle, verts + b_verts, faces + b_faces, [d.id_mat(MAT_HANDLE)] * (len(faces) + len(b_faces)))
        self.shade_smooth(context, handle, 0.20944)

    def remove_handle(self, context, o):
        handle = self.find_handle(o)
        self.delete_object(context, handle)

    def update(self, context, o, d, x, y, z, handle, direction):

        side, face, back = self.panels(d, y)
        verts = self.verts(d, side, face, back, x, z, direction)
        faces = self.faces(d, side, face, back)
        matids = self.matids(d, side, face, back)
        uvs = self.uvs(d, side, face, back, x, z, direction)
        vcolors = self.vcolors(d, side, face, back)
        bmed.buildmesh(o, verts, faces, matids, uvs, vcolors=vcolors, weld=True)
        self.shade_smooth(context, o, 0.20944)

        if handle == 'NONE':
            self.remove_handle(context, o)
        else:
            self.update_handle(context, o, d, x, y, z, direction)


class ARCHIPACK_PT_door_panel(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_door_panel"
    bl_label = "Door leaf"

    @classmethod
    def poll(cls, context):
        return archipack_door_panel.poll(context.active_object)

    def draw(self, context):
        layout = self.layout
        self.draw_op(context, layout, layout, "archipack.select_parent", icon="RESTRICT_SELECT_OFF")


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------

class ARCHIPACK_OT_select(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.select"
    bl_label = "Select"
    bl_description = "Select object"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    name: StringProperty(default="")

    def execute(self, context):
        if context.mode == "OBJECT":
            o = self.get_scene_object(context, self.name)
            if o:
                bpy.ops.object.select_all(action='DESELECT')

                # select and make active
                self.select_object(context, o, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_select_parent(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.select_parent"
    bl_label = "Edit parameters"
    bl_description = "Edit parameters located on parent"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            if o is not None and o.parent is not None:
                bpy.ops.object.select_all(action="DESELECT")
                # select and make active
                self.select_object(context, o.parent, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


def get_direction(self):
    return self.direction


def set_direction(self, value):
    self.direction = value
    return None


def get_model(self):
    return self.model


def set_model(self, value):
    self.model = value
    return None


class archipack_door(ArchipackObject, Manipulable, DimensionProvider, PropertyGroup):
    """
        The frame is the door main object
        parent parametric object
        create/remove/update her own childs
    """
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display main settings', 'NONE', 0),
            ('SUB', 'Parts', 'Display components settings', 'NONE', 2),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 3)
        ),
        default='MAIN',
    )
    x: FloatProperty(
        name='Width',
        min=0.25,
        default=100.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Width', update=update,
    )
    y: FloatProperty(
        name='Depth',
        min=0.1,
        default=0.20, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Depth', update=update,
    )
    z: FloatProperty(
        name='Height',
        min=0.1,
        default=2.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='height', update=update,
    )
    frame_x: FloatProperty(
        name='Width',
        min=0,
        default=0.1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='frame width', update=update,
    )
    frame_y: FloatProperty(
        name='Depth',
        default=0.03, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='frame depth', update=update,
    )
    direction: IntProperty(
        name="Direction",
        min=0,
        max=1,
        description="open direction",
    )
    direction_ui: EnumProperty(
        options={'SKIP_SAVE'},
        name="Direction",
        items=(
            ("RIGHT", "Right", "Right"),
            ("LEFT", "Left", "Left")
        ),
        update=update,
        get=get_direction,
        set=set_direction
    )
    door_y: FloatProperty(
        name='Depth',
        min=0.001,
        default=0.02, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='depth', update=update,
    )
    door_offset: FloatProperty(
        name='Offset',
        min=0,
        default=0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='offset', update=update,
    )
    model: IntProperty(
        name="Model",
        min=0,
        max=3,
        default=0,
        description="Model",
    )
    model_ui: EnumProperty(
        options={'SKIP_SAVE'},
        name="Direction",
        items=(
            ("FLAT", "Flat", "Flat"),
            ("PANEL", "Panel", "Panel"),
            ("FRAME", "Frame", "Frame"),
            ("BEVEL", "Bevel", "Bevel panel")
        ),
        get=get_model,
        set=set_model,
        default="FLAT",
        update=update
    )
    n_panels: IntProperty(
        name="Door leaf",
        min=1,
        max=2,
        default=1,
        description="number of door leaf", update=update
    )
    chanfer: FloatProperty(
        name='Bevel',
        min=0.001,
        default=0.005, precision=3, step=0.01,
        unit='LENGTH', subtype='DISTANCE',
        description='Chanfer', update=update_childs,
    )
    panel_spacing: FloatProperty(
        name='Spacing',
        min=0.001,
        default=0.1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Distance between panels', update=update_childs,
    )
    panel_bottom: FloatProperty(
        name='Bottom',
        min=0.0,
        default=0.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Distance from bottom', update=update_childs,
    )
    panel_border: FloatProperty(
        name='Border',
        min=0.001,
        default=0.2, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Distance from border', update=update_childs,
    )
    panels_x: IntProperty(
        name="# h",
        min=1,
        max=50,
        default=1,
        description="Panels h", update=update_childs,
    )
    panels_y: IntProperty(
        name="# v",
        min=1,
        max=50,
        default=1,
        description="Panels v", update=update_childs,
    )
    panels_distrib: EnumProperty(
        name='Distribution',
        items=(
            ('REGULAR', 'Regular', '', 0),
            ('ONE_THIRD', '1/3 2/3', '', 1)
        ),
        default='REGULAR', update=update_childs,
    )
    handle: EnumProperty(
        name='Handle',
        items=(
            ('NONE', 'No handle', '', 0),
            ('BOTH', 'Inside and outside', '', 1)
        ),
        default='BOTH', update=update_childs,
    )
    sill_enable: BoolProperty(
        name="Sill",
        default=False, update=update,
    )
    sill_x: FloatProperty(
        name='Width',
        min=0.0,
        default=0.04, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='tablet width', update=update,
    )
    sill_y: FloatProperty(
        name='Depth',
        min=0.001,
        default=0.04, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='tablet depth', update=update,
    )
    sill_z: FloatProperty(
        name='Height',
        min=0.001,
        default=0.03, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='tablet height', update=update,
    )
    soil_enable: BoolProperty(
        name="Step",
        default=True, update=update,
    )
    soil_z: FloatProperty(
        name='Height',
        min=0.001,
        default=0.001, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Soil height', update=update,
    )
    soil_y: FloatProperty(
        name='Width',
        min=0.001,
        default=0.02, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='Soil width', update=update,
    )
    hole_margin: FloatProperty(
        name='Hole margin',
        min=0.0,
        default=0.1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        description='how much hole surround wall'
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
    flip: BoolProperty(
        default=False,
        # update=update,
        description='flip outside and outside material of hole'
    )
    z_offset: FloatProperty(
        name="Hole depth z",
        unit='LENGTH', subtype='DISTANCE',
        description='Depth of hole under the door',
        default=0.1, precision=5, step=1,
        update=update
    )
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update
    )

    mat_panel_inside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Panel in",
        description="Material index of door panel inside",
        items=mat_enum,
        get=mat_index_getter(MAT_PANEL_INSIDE),
        set=mat_index_setter(MAT_PANEL_INSIDE),
        update=update
    )

    mat_panel_outside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Panel out",
        description="Material index of door panel outside",
        items=mat_enum,
        get=mat_index_getter(MAT_PANEL_OUTSIDE),
        set=mat_index_setter(MAT_PANEL_OUTSIDE),
        update=update
    )

    mat_door_inside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Door in",
        description="Material index of door inside",
        items=mat_enum,
        get=mat_index_getter(MAT_DOOR_INSIDE),
        set=mat_index_setter(MAT_DOOR_INSIDE),
        update=update
    )

    mat_door_outside: EnumProperty(
        options={'SKIP_SAVE'},
        name="Door out",
        description="Material index of door outside",
        items=mat_enum,
        get=mat_index_getter(MAT_DOOR_OUTSIDE),
        set=mat_index_setter(MAT_DOOR_OUTSIDE),
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

    mat_handle: EnumProperty(
        options={'SKIP_SAVE'},
        name="Handle",
        description="Material index of handle",
        items=mat_enum,
        get=mat_index_getter(MAT_HANDLE),
        set=mat_index_setter(MAT_HANDLE),
        update=update
    )

    mat_sill: EnumProperty(
        options={'SKIP_SAVE'},
        name="Sill",
        description="Material index of sill",
        items=mat_enum,
        get=mat_index_getter(MAT_SILL),
        set=mat_index_setter(MAT_SILL),
        update=update
    )

    mat_soil: EnumProperty(
        options={'SKIP_SAVE'},
        name="Step",
        description="Material index of step",
        items=mat_enum,
        get=mat_index_getter(MAT_SOIL),
        set=mat_index_setter(MAT_SOIL),
        update=update
    )
    mat_hole_outside: IntProperty(
        name="Outside",
        description="Material index of wall for outside part of the hole",
        min=0,
        max=128,
        default=1, update=update,
    )
    mat_hole_inside: IntProperty(
        name="Inside",
        description="Material index of wall for inside part of the hole",
        min=0,
        max=128,
        default=0, update=update,
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
    idmat: IntVectorProperty(
        default=[
            1, 0,
            1, 0,
            1, 0,
            4, 5,
            3
        ],
        size=9
    )
    auto_mat: BoolProperty(
        name="Auto materials",
        default=True
    )

    @property
    def frame(self):
        finishing_out = self.finishing_out
        finishing_int = self.finishing_int
        if self.flip:
            finishing_out, finishing_int = finishing_int, finishing_out
        x0 = 0
        x1 = -BATTUE
        x2 = -self.frame_x
        y0 = max(0.25 * self.door_y + 0.0005, self.y / 2 + self.frame_y)
        y1 = max(y0 - 0.5 * self.door_y - self.door_offset, -y0 + 0.001)
        y2 = -(y0 + finishing_out)
        y3 = 0
        mo, mi = self.id_mat(MAT_FRAME_INSIDE), \
                 self.id_mat(MAT_FRAME_OUTSIDE)
        if self.frame_y > 0:
            # when frame_y > 0
            # the shape of frame change
            #    _____      y0
            #   |___  |_    y1
            #       |   |   y5
            #   x   x   |   y3
            #    ___|   |   y4
            #   |_______|   y2
            #
            #   x2 x3 x1  x0
            y4 = -y0 + self.frame_y
            y5 = y0 - self.frame_y
            x3 = x0 - self.frame_y
            y0 += finishing_int

            return DoorPanel(
                True,           # closed
                [0, 0, 3, 3, 0, 0, 1, 1, 2, 2],  # x index
                [x2, x1, x0, x3],
                [y2, y4, y4, y5, y5, y0, y0, y1, y1, y2],
                [mi, mi, mo, mo, mo, mo, mo, mo, mi, mi],  # material index
                closed_path=False
                )
        else:
            #   Frames inside wall
            #
            #    _____        y0
            #   |     |___    y1
            #   x         |   y3
            #   |         |
            #   |_________|   y2
            #
            #   x2    x1  x0
            y0 += finishing_int

            return DoorPanel(
                True,           # closed
                [0, 0, 0, 1, 1, 2, 2],  # x index
                [x2, x1, x0],
                [y2, y3, y0, y0, y1, y1, y2],
                [mi, mo, mo, mo, mo, mi, mi],  # material index
                closed_path=False
                )

    @property
    def sill_in(self):
        # profil tablette
        #  __  y0
        # |  |
        # |  |
        # |__| y1
        # x0  x1
        y0 = -0.5 * self.y - self.sill_y
        y1 = self.hole_center_y
        x0 = -self.sill_z
        x1 = 0
        # y = depth
        # x = width1
        return DoorPanel(
            True,  # closed profil
            [0, 0, 1, 1],  # x index
            [x0, x1],
            [y1, y0, y0, y1],
            [self.id_mat(MAT_SILL)] * 4,  # material index
            closed_path=False  # closed path
        )

    @property
    def soil(self):
        # profil tablette
        #  __  y0
        # |  |
        # |  |
        # |__| y1
        # x0  x1
        y1 = self.hole_center_y
        y0 = y1 + self.soil_y
        y2 = y1 - self.soil_y
        x0 = 0
        x1 = self.soil_z
        # y = depth
        # x = width1
        return DoorPanel(
            False,  # closed profil
            [0, 1, 0],  # x index
            [x0, x1],
            [y0, y1, y2],
            [self.id_mat(MAT_SOIL)] * 2,  # material index
            closed_path=False  # closed path
        )

    @property
    def hole(self):
        #
        #    _____   y0
        #   |
        #   x        y2
        #   |
        #   |_____   y1
        #
        #   x0
        x0 = 0
        gap = 0.0001

        y = 0.5 * self.y + gap
        y2 = 0

        outside_mat = self.mat_hole_outside
        inside_mat = self.mat_hole_inside
        in_finish_mat = self.mat_finish_inside
        out_finish_mat = self.mat_finish_outside
        finishing_int = self.finishing_int
        finishing_out = self.finishing_out

        if self.flip:
            outside_mat, inside_mat = inside_mat, outside_mat
            out_finish_mat, in_finish_mat = in_finish_mat, out_finish_mat
            finishing_int, finishing_out = finishing_out, finishing_int

        y0 = y + finishing_int + self.hole_margin
        y1 = -(y + finishing_out + self.hole_margin)

        return DoorPanel(
            False,       # closed
            [0, 0, 0, 0, 0],  # x index
            [x0],
            [y1, -y, y2, y, y0],
            [out_finish_mat, outside_mat, inside_mat, in_finish_mat, in_finish_mat],  # material index
            closed_path=True,
            side_cap_front=4,
            side_cap_back=0     # cap index
            )

    @property
    def hole_symbol(self):
        #   Provide hole for moldings
        #    _____   y0
        #   |
        #   x        y2
        #   |
        #   |_____   y1
        #
        #   x0
        x0 = 0
        y0 = self.y / 2 + max(0.001, self.frame_y)
        y1 = -y0
        y2 = 0

        return DoorPanel(
            False,  # closed
            [0, 0, 0],  # x index
            [x0],
            [y1, y2, y0],
            [0, 0, 0],  # material index
            closed_path=True,
            side_cap_front=2,
            side_cap_back=0  # cap index
        )

    @property
    def hole_center_y(self):
        """
        Hole center on y axis - this is the location of ground
        :return:
        """
        # y is location of panel
        y = max(0.25 * self.door_y, self.y / 2 + self.frame_y)
        return max(y - 0.5 * self.door_y - self.door_offset, -y)

    @property
    def inside_hole(self):
        #
        #    _____   y0
        #   |
        #   x        y2
        #   |
        #   |_____   y1
        #
        #   x0
        x0 = 0
        y2 = self.hole_center_y
        y0 = self.y / 2 + self.hole_margin
        outside_mat = 0
        inside_mat = 1
        if self.flip:
            outside_mat, inside_mat = inside_mat, outside_mat
        return DoorPanel(
            False,       # closed
            [0, 0],  # x index
            [x0],
            [y2, y0],
            [inside_mat, inside_mat],  # material index
            closed_path=True,
            side_cap_front=0,
            side_cap_back=1     # cap index
            )

    @property
    def outside_hole(self):
        #
        #    _____   y0
        #   |
        #   x        y2
        #   |
        #   |_____   y1
        #
        #   x0
        x0 = 0
        y2 = self.hole_center_y
        y1 = -(self.y / 2 + self.hole_margin)
        outside_mat = 0
        inside_mat = 1
        if self.flip:
            outside_mat, inside_mat = inside_mat, outside_mat
        return DoorPanel(
            False,       # closed
            [0, 0],  # x index
            [x0],
            [y1, y2],
            [outside_mat, outside_mat],  # material index
            closed_path=True,
            side_cap_front=0,
            side_cap_back=1     # cap index
            )

    def get_generator(self, o=None):
        return OpeningGenerator(self, o)
    
    def setup_manipulators(self):
        if len(self.manipulators) == 3:
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

    def remove_childs(self, context, o, to_remove):
        for child in o.children:
            if to_remove < 1:
                return
            if archipack_door_panel.filter(child):
                to_remove -= 1
                self.delete_object(context, child)

    def find_handle(self, o):
        for handle in o.children:
            if 'archipack_handle' in handle:
                return handle
        return None

    def get_childs_panels(self, context, o):
        return [child for child in o.children if archipack_door_panel.filter(child)]

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
                p = bpy.data.objects.new("Door Panel", child.data)
                # Link object into scene
                self.link_object_to_scene(context, p)
                p.color = (0, 1, 0, 1)

                p.show_transparent = True
                p.lock_location = (False, True, True)
                p.lock_rotation = (False, True, False)
                p.lock_scale = (True, True, True)
                p.parent = linked
                p.matrix_world = linked.matrix_world.copy()

            else:
                p = l_childs[order[i]]

            p.location = child.location.copy()

            # update handle
            handle = self.find_handle(child)
            h = self.find_handle(p)
            if handle is not None:
                if h is None:
                    h = create_handle(context, p, handle.data)
                    # MaterialUtils.add_handle_materials(h)
                h.location = handle.location.copy()
            else:
                self.delete_object(context, h)

    def _synch_hole(self, context, linked, hole):
        l_hole = self.find_hole(linked)
        if l_hole is None:
            l_hole = bpy.data.objects.new("hole", hole.data)
            l_hole['archipack_hole'] = True
            self.link_object_to_scene(context, l_hole)
            l_hole.parent = linked
            l_hole.matrix_world = linked.matrix_world.copy()
            l_hole.location = hole.location.copy()
        else:
            l_hole.data = hole.data

    def synch_childs(self, context, o):
        """
            synch childs nodes of linked objects
        """
        childs = self.get_childs_panels(context, o)
        hole = self.find_hole(o)
        linked_objects = self.get_linked_objects(context, o)
        for linked in linked_objects:
            if linked != o:
                self._synch_childs(context, o, linked, childs)
                if hole is not None:
                    self._synch_hole(context, linked, hole)

    def update_childs(self, context, o):
        """
            pass params to childrens
        """
        childs = self.get_childs_panels(context, o)
        n_childs = len(childs)
        self.remove_childs(context, o, n_childs - self.n_panels)

        childs = self.get_childs_panels(context, o)
        n_childs = len(childs)
        child_n = 0

        # location_y = self.y / 2 + self.frame_y - SPACING
        # location_y = min(max(self.door_offset, - location_y), location_y) + self.door_y

        location_y = max(0.25 * self.door_y + 0.0005, self.y / 2 + self.frame_y)
        location_y = max(location_y - self.door_offset + 0.5 * self.door_y, -location_y + self.door_y + 0.001)

        x = self.x / self.n_panels + (3 - self.n_panels) * (BATTUE - SPACING)
        y = self.door_y
        z = self.z + BATTUE - SPACING

        if self.n_panels < 2:
            direction = self.direction
        else:
            direction = 0

        for panel in range(self.n_panels):
            child_n += 1

            if child_n == 1:
                handle = self.handle
            else:
                handle = 'NONE'

            if child_n > 1:
                direction = 1 - direction

            if child_n > n_childs:
                m = bpy.data.meshes.new("Door Panel")
                p = bpy.data.objects.new("Door Panel", m)
                d = m.archipack_door_panel.add()
                # Link object into scene
                self.link_object_to_scene(context, p)

                p.color = (0, 1, 0, 1)

                p.show_transparent = True
                p.lock_location = (False, True, True)
                p.lock_rotation = (False, True, False)
                p.lock_scale = (True, True, True)
                # parenting at 0, 0, 0 before set object matrix_world
                # so location remains local from frame
                p.parent = o
                p.matrix_world = o.matrix_world.copy()
            else:
                p = childs[child_n - 1]
                # select and make active
                d = archipack_door_panel.datablock(p)

            if d is not None:
                self.link_materials(context, o, p)
                d.update(context, p, self, x, y, z, handle, direction)

            location_x = (2 * direction - 1) * (self.x / 2 + BATTUE - SPACING)
            p.location = Vector((location_x, location_y, 0))

    def update(self, context, childs_only=False):

        # support for "copy to selected"
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        self.setup_manipulators()

        if childs_only is False:
            v = Vector((0, 0, 0))
            size = Vector((self.x, self.z, self.y))
            frame = self.frame
            verts = frame.vertices(16, v, v, v, size, v, 0, 0, shape_z=None, path_type='RECTANGLE')
            faces = frame.faces(16, path_type='RECTANGLE')
            matids = frame.mat(16, 0, 0, path_type='RECTANGLE')
            uvs = frame.uv(16, v, v, size, v, 0, 0, 0, 0, path_type='RECTANGLE')
            col = random_color()
            vcolors = frame.vcolors(16, col, path_type='RECTANGLE')
            if self.sill_enable:
                sill_in = self.sill_in

                size = Vector((self.x + 2 * (self.frame_x + self.sill_x), self.y, self.z))
                faces.extend(
                    sill_in.faces(16, path_type='HORIZONTAL', offset=len(verts))
                )
                verts.extend(
                    sill_in.vertices(16, v, v, v, size, v, 0, 0, shape_z=None, path_type='HORIZONTAL')
                )
                matids.extend(
                    sill_in.mat(16, 0, 0, path_type='HORIZONTAL')
                )
                uvs.extend(
                    sill_in.uv(16, v, v, size, v, 0, 0, 0, 0, path_type='HORIZONTAL')
                )
                col = random_color()
                vcolors.extend(
                    sill_in.vcolors(16, col, path_type='HORIZONTAL')
                )
            if self.soil_enable:
                soil = self.soil

                size = Vector((self.x, self.y, self.z))
                faces.extend(
                    soil.faces(16, path_type='HORIZONTAL', offset=len(verts))
                )
                verts.extend(
                    soil.vertices(16, v, v, v, size, v, 0, 0, shape_z=None, path_type='HORIZONTAL')
                )
                matids.extend(
                    soil.mat(16, 0, 0, path_type='HORIZONTAL')
                )
                uvs.extend(
                    soil.uv(16, v, v, size, v, 0, 0, 0, 0, path_type='HORIZONTAL')
                )
                col = random_color()
                vcolors.extend(
                    soil.vcolors(16, col, path_type='HORIZONTAL')
                )
            bmed.buildmesh(o, verts, faces, matids, uvs, vcolors=vcolors)
            self.shade_smooth(context, o, 0.20944)

        self.update_childs(context, o)

        if childs_only is False and self.find_hole(o) is not None:
            self.interactive_hole(context, o)

        # setup 3d points for gl manipulators
        dir = 1
        if "archipack_flip" in o and o["archipack_flip"]:
            dir = -1

        x, y = dir * 0.5 * self.x, dir * 0.5 * self.y

        self.manipulators[0].set_pts([(-x, -y, 0), (x, -y, 0), (0.5, 0, 0)])
        self.manipulators[1].set_pts([(-x, -y, 0), (-x, y, 0), (-1, 0, 0)])
        self.manipulators[2].set_pts([(x, -y, 0), (x, -y, self.z), (-dir, 0, 0)])

        # support for instances childs, update at object level
        # self.update_dimension(context, o)
        dx = x + dir * self.frame_y
        self.add_dimension_point(0, Vector((-x, -y, 0)))
        self.add_dimension_point(1, Vector((x, -y, 0)))
        self.add_dimension_point(2, Vector((-dx, -y, 0)))
        self.add_dimension_point(3, Vector((dx, -y, 0)))
        self.add_dimension_point(4, Vector((-x, y, 0)))
        self.add_dimension_point(5, Vector((x, y, 0)))
        self.add_dimension_point(6, Vector((-dx, y, 0)))
        self.add_dimension_point(7, Vector((dx, y, 0)))
        
        self.synch_childs(context, o)

        # synch wall dimensions when apply
        self.update_dimensions(context, o)
        
        # restore context
        self.restore_context(context)

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

        # XXX Wrong material is wall one
        # self.link_materials(o, hole_obj)

        hole = self.hole
        v = Vector((0, 0, 0))
        offset = Vector((0, -self.z_offset - 0.001, 0))

        if self.frame_y > 0:
            size = Vector((
                self.x + 2 * self.frame_y,
                self.z + self.frame_y + self.z_offset + 0.001,
                self.y))
        else:
            size = Vector((
                self.x + 2 * self.frame_x,
                self.z + self.frame_x + self.z_offset + 0.001,
                self.y))

        verts = hole.vertices(16, offset, v, v, size, v, 0, 0, shape_z=None, path_type='RECTANGLE')
        faces = hole.faces(16, path_type='RECTANGLE')
        matids = hole.mat(16, 0, 1, path_type='RECTANGLE')
        uvs = hole.uv(16, v, v, size, v, 0, 0, 0, 0, path_type='RECTANGLE')
        col = random_color()
        # without vcolors boolean wont transfer vcolors to result mesh
        vcolors = hole.vcolors(16, col, path_type='RECTANGLE')
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
        self.link_object_to_scene(context, curve_obj)
        self.select_object(context, curve_obj)
        return curve_obj

    def as_2d(self, context, o):

        # frame
        v = Vector((0, 0, 0))
        size = Vector((self.x, self.z, self.y))

        coords = self.frame.as_2d(16, v, v, v,
            size, v, 0, 0, connect=0)
        
        childs = self.get_childs_panels(context, o)

        location_y = max(0.25 * self.door_y + 0.0005, self.y / 2 + self.frame_y)
        location_y = max(location_y - self.door_offset + 0.5 * self.door_y, -location_y + self.door_y + 0.001)

        radius = Vector((0.8, 0.5, 0))
        center = Vector((0, 0, 0))
        
        if self.direction == 0:
            pivot = 1
        else:
            pivot = -1

        for panel in range(self.n_panels):
            child = childs[panel]
            d = archipack_door_panel.datablock(child)
            x = self.x / self.n_panels + (3 - self.n_panels) * (BATTUE - SPACING)
            y = self.door_y
            z = self.z + BATTUE - SPACING

            origin = Vector((-pivot * 0.5 * x, 0, 0))
            size = Vector((y, z, 0))
            location_x = -pivot * (0.5 * self.x + BATTUE - SPACING)
            location = Vector((location_x, location_y, 0))
            coords.extend(
                d.symbol_2d(x).as_2d(
                    1, location, center, origin,
                    size, radius, 0, pivot, shape_z=None, path_type='RECTANGLE')
                )
            # arc
            r = x
            steps = 16
            da = pi / (2 * steps)
            arc = [Vector((
                    location_x + pivot * r * cos(da * i), 
                    location_y + r * sin(da * i), 
                    0)) 
                for i in range(steps + 1)]
            coords.append(arc)
            pivot = -pivot
            
        curve = self._to_curve(context, coords, name="{}-2d".format(o.name), dimensions='2D')
        curve.matrix_world = o.matrix_world.copy()

        return curve

    def hole_2d(self, mode='SYMBOL'):
        """
          return coords of full / inside hole in 2d top ortho view
        """
        if mode == 'BOUND':
            # Boundarys of whole door to detect wall intersection
            x, y = 0.5 * self.x + self.frame_x, 0.5 * self.y + self.hole_margin
            return [(-x, -y, 0), (-x, y, 0), (x, y, 0), (x, -y, 0), (-x, -y, 0)]

        v = Vector((0, 0, 0))

        if self.frame_y > 0 and mode not in {'MOLDINGS'}:
            # moldings always depends on frame width
            size = Vector((self.x + 2 * self.frame_y, self.z + self.frame_y + 0.001, self.y))
        else:
            size = Vector((self.x + 2 * self.frame_x, self.z + self.frame_x + 0.001, self.y))

        if mode == 'INSIDE':
            # in side hole for floors
            coords = self.inside_hole.as_2d(16,
                v,
                v, v, size, v,
                0, 0, shape_z=None, path_type='RECTANGLE')
        elif mode == 'OUTSIDE':
            # out side hole for floors
            coords = self.outside_hole.as_2d(16,
                v,
                v, v, size, v,
                0, 0, shape_z=None, path_type='RECTANGLE')
        else:
            # whole hole for symbols / moldings
            coords = self.hole_symbol.as_2d(16,
                v,
                v, v, size, v,
                0, 0, shape_z=None, path_type='RECTANGLE')

        # Use only first curve
        return coords[0]

    def remove_hole(self, context, hole, walls):
        ctx = context.copy()
        ctx['object'] = hole
        ctx['selected_objects'] = walls
        bpy.ops.archipack.remove_hole(ctx)

    def on_delete(self, context, obj):
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
            if archipack_door.filter(o):
                hole = self.find_hole(o)
                if hole is not None:
                    self.remove_hole(context, hole, walls)

                if o.name != obj.name:
                    self.delete_object(context, o)

        for c, d in wall2.items():
            d.setup_childs(context, c, openings_only=True)
            for i, child in enumerate(d.childs):
                if child.child_name == obj.name:
                    d.childs.remove(i)
                    break
            d.synch_dimension(context, c, remove_object=obj.name)


class ARCHIPACK_PT_door(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_door"
    bl_label = "Door"

    @classmethod
    def poll(cls, context):
        return archipack_door.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_door.datablock(o)

        if d is None:
            return

        layout = self.layout

        self.draw_common(context, layout)

        row = layout.row(align=True)
        self.draw_op(context, layout, row, 'archipack.door', icon='FILE_REFRESH', text="Refresh").mode = 'REFRESH'
        if o.data.users > 1:
            self.draw_op(context, layout, row, 'archipack.door', icon='UNLINKED', text="Make unique",
                         postfix="({})".format(o.data.users)).mode = 'UNIQUE'
        # self.draw_op(context, layout, row, 'archipack.door', text="Delete", icon='ERROR').mode = 'DELETE'
        box = layout.box()
        # self.draw_label(context, layout, box, "Styles")
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.door_preset_menu",
                     text=bpy.types.ARCHIPACK_OT_door_preset_menu.bl_label, icon="PRESET")
        self.draw_op(context, layout, row, "archipack.door_preset", icon='ADD', text="")
        self.draw_op(context, layout, row, "archipack.door_preset", icon='REMOVE', text="").remove_active = True
        self.draw_op(context, layout, layout, "archipack.door_open")

        self.draw_prop(context, layout, layout, d, 'tabs', expand=True)

        box = layout.box()

        if d.tabs == 'MAIN':
            self.draw_label(context, layout, box, "Size")
            self.draw_prop(context, layout, box, d, 'x')
            self.draw_prop(context, layout, box, d, 'y')
            self.draw_prop(context, layout, box, d, 'z')
            self.draw_prop(context, layout, box, d, 'door_offset')
            row = box.row(align=True)
            self.draw_prop(context, layout, row, d, 'direction_ui', expand=True)
            self.draw_prop(context, layout, box, d, 'n_panels')
            for c in o.children:
                if archipack_door_panel.datablock(c) is not None and hasattr(c, "rotation_euler"):
                    box.prop(c, "rotation_euler", index=2, text="Open angle")

        elif d.tabs == 'SUB':
            # self.draw_prop(context, layout, box, d, 'flip')
            self.draw_label(context, layout, box, "Door")
            self.draw_prop(context, layout, box, d, 'door_y')
            box = layout.box()
            self.draw_label(context, layout, box, "Handle")
            self.draw_prop(context, layout, box, d, 'handle', text="")
            box = layout.box()
            self.draw_label(context, layout, box, "Frame")
            row = box.row(align=True)
            self.draw_prop(context, layout, row, d, 'frame_x')
            self.draw_prop(context, layout, row, d, 'frame_y')
            self.draw_prop(context, layout, box, d, 'soil_enable')
            if d.soil_enable:
                self.draw_prop(context, layout, box, d, 'soil_z')
                self.draw_prop(context, layout, box, d, 'soil_y')
            self.draw_prop(context, layout, box, d, 'sill_enable')
            if d.sill_enable:
                self.draw_prop(context, layout, box, d, 'sill_x')
                self.draw_prop(context, layout, box, d, 'sill_y')
                self.draw_prop(context, layout, box, d, 'sill_z')
            self.draw_label(context, layout, box, "Hole")
            self.draw_prop(context, layout, box, d, 'z_offset')
            box = layout.box()
            self.draw_label(context, layout, box, "Panels")
            self.draw_prop(context, layout, box, d, 'model_ui', text="Type")
            if d.model > 0:
                self.draw_prop(context, layout, box, d, 'panels_distrib', text="")
                row = box.row(align=True)
                self.draw_prop(context, layout, row, d, 'panels_x')
                if d.panels_distrib == 'REGULAR':
                    self.draw_prop(context, layout, row, d, 'panels_y')
                self.draw_prop(context, layout, box, d, 'panel_bottom')
                self.draw_prop(context, layout, box, d, 'panel_spacing')
                self.draw_prop(context, layout, box, d, 'panel_border')
                self.draw_prop(context, layout, box, d, 'chanfer')

        elif d.tabs == 'MATERIALS':

            if "archipack_material" in o:
                o.archipack_material[0].draw(context, box)

            box = layout.box()
            self.draw_label(context, layout, box, "Apply materials")
            self.draw_prop(context, layout, box, d, 'mat_frame_inside')
            self.draw_prop(context, layout, box, d, 'mat_frame_outside')
            self.draw_prop(context, layout, box, d, 'mat_door_inside')
            self.draw_prop(context, layout, box, d, 'mat_door_outside')

            if d.model > 0:
                self.draw_prop(context, layout, box, d, 'mat_panel_inside')
                self.draw_prop(context, layout, box, d, 'mat_panel_outside')

            self.draw_prop(context, layout, box, d, 'mat_handle')
            self.draw_prop(context, layout, box, d, 'mat_sill')
            self.draw_prop(context, layout, box, d, 'mat_soil')


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


class ARCHIPACK_OT_door(ArchipackCreateTool, Operator):
    bl_idname = "archipack.door"
    bl_label = "Door"
    bl_description = "Door"

    x: FloatProperty(
        name='width',
        min=0.1,
        default=0.80, precision=5,
        unit='LENGTH', subtype='DISTANCE',
        description='Width'
    )
    y: FloatProperty(
        name='depth',
        min=0.1,
        default=0.20, precision=5,
        unit='LENGTH', subtype='DISTANCE',
        description='Depth'
    )
    z: FloatProperty(
        name='height',
        min=0.1,
        default=2.0, precision=5,
        unit='LENGTH', subtype='DISTANCE',
        description='height'
    )
    direction: IntProperty(
        name="direction",
        min=0,
        max=1,
        description="open direction"
    )
    n_panels: IntProperty(
        name="Door leaf",
        min=1,
        max=2,
        default=1,
        description="number of panels"
    )
    chanfer: FloatProperty(
        name='Chanfer',
        min=0.001,
        default=0.005, precision=3,
        unit='LENGTH', subtype='DISTANCE',
        description='chanfer'
    )
    panel_spacing: FloatProperty(
        name='Spacing',
        min=0.001,
        default=0.1, precision=5,
        unit='LENGTH', subtype='DISTANCE',
        description='distance between panels'
    )
    panel_bottom: FloatProperty(
        name='bottom',
        default=0.0, precision=5,
        unit='LENGTH', subtype='DISTANCE',
        description='distance from bottom'
    )
    panel_border: FloatProperty(
        name='border',
        min=0.001,
        default=0.2, precision=5,
        unit='LENGTH', subtype='DISTANCE',
        description='distance from border'
    )
    panels_x: IntProperty(
        name="panels h",
        min=1,
        max=50,
        default=1,
        description="panels h"
    )
    panels_y: IntProperty(
        name="panels v",
        min=1,
        max=50,
        default=1,
        description="panels v"
    )
    panels_distrib: EnumProperty(
        name='distribution',
        items=(
            ('REGULAR', 'Regular', '', 0),
            ('ONE_THIRD', '1/3 2/3', '', 1)
        ),
        default='REGULAR'
    )
    handle: EnumProperty(
        name='Shape',
        items=(
            ('NONE', 'No handle', '', 0),
            ('BOTH', 'Inside and outside', '', 1)
        ),
        default='BOTH'
    )
    mode: EnumProperty(
        items=(
            ('CREATE', 'Create', '', 0),
            ('REFRESH', 'Refresh', '', 2),
            ('UNIQUE', 'Make unique', '', 3),
        ),
        default='CREATE'
    )

    def create(self, context):
        """
            expose only basic params in operator
            use object property for other params
        """
        m = bpy.data.meshes.new("Door")
        o = bpy.data.objects.new("Door", m)
        d = m.archipack_door.add()
        d.x = self.x
        d.y = self.y
        d.z = self.z
        d.direction = self.direction
        d.n_panels = self.n_panels
        d.chanfer = self.chanfer
        d.panel_border = self.panel_border
        d.panel_bottom = self.panel_bottom
        d.panel_spacing = self.panel_spacing
        d.panels_distrib = self.panels_distrib
        d.panels_x = self.panels_x
        d.panels_y = self.panels_y
        d.handle = self.handle
        # Link object into scene
        self.link_object_to_scene(context, o)
        o.color = (0, 1, 0, 1)

        # select and make active
        self.select_object(context, o, True)
        self.add_material(context, o)
        self.load_preset(d)
        self.select_object(context, o, True)
        return o

    def update(self, context):
        o = context.active_object
        d = archipack_door.datablock(o)
        if d is not None:
            d.update(context)
            bpy.ops.object.select_linked(type='OBDATA')
            for linked in context.selected_objects:
                if linked != o:
                    archipack_door.datablock(linked).update(context)
        bpy.ops.object.select_all(action="DESELECT")
        # select and make active
        self.select_object(context, o, True)

    def unique(self, context):
        obj = context.active_object
        sel = context.selected_objects
        uniques = []
        for o in sel:
            if archipack_door.filter(o):
                uniques.append(o)
                # panel and holes
                uniques.extend(list(o.children))
                for c in o.children:
                    # handles
                    uniques.extend(list(c.children))

        with ensure_select_and_restore(context, obj, uniques) as (ctx, act, sel):
            bpy.ops.object.make_single_user(
                type='SELECTED_OBJECTS',
                object=True,
                obdata=True,
                material=False,
                animation=False)

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


class ARCHIPACK_OT_door_open(Operator):
    bl_idname = "archipack.door_open"
    bl_label = "Open Door(s)"
    bl_description = "Open selected doors"
    bl_options = {'UNDO', 'REGISTER'}

    angle: FloatProperty(
        name='Angle',
        min=0, max=pi,
        default=pi / 2, precision=5,
        subtype='ANGLE', unit='ROTATION',
        description='Rotation'
    )

    @classmethod
    def poll(cls, context):
        o = context.active_object
        return o and archipack_door.filter(o)

    def execute(self, context):
        sel = context.selected_objects
        for o in sel:
            d = archipack_door.datablock(o)
            if d is not None:
                if d.n_panels == 1:
                    for c in o.children:
                        if archipack_door_panel.filter(c):
                            c.rotation_euler = (0, 0, (1 - 2 * d.direction) * self.angle)
        return {'FINISHED'}

    def invoke(self, context, event):
        return context.window_manager.invoke_props_dialog(self)


class ARCHIPACK_OT_door_draw(ArchipackDrawTool, Operator):
    bl_idname = "archipack.door_draw"
    bl_label = "Draw Doors"
    bl_description = "Draw Doors over walls"

    filepath: StringProperty(default="")
    feedback = None
    stack = []
    object_name = ""

    @classmethod
    def poll(cls, context):
        return True

    def draw_callback(self, _self, context):
        self.feedback.draw(context)

    def add_object(self, context, event):

        bpy.ops.object.select_all(action="DESELECT")
        bpy.ops.archipack.door(filepath=self.filepath)
        o = context.active_object

        if o is None:
            o = context.object

        self.object_name = o.name
        # print("add_object bpy.ops.archipack.generate_hole")
        # bpy.ops.archipack.generate_hole()
        return o

    def remove_object(self, context, o):
        if archipack_door.filter(o):
            ctx = context.copy()
            ctx['object'] = o
            ctx['selected_objects'] = [o]
            bpy.ops.archipack.delete(ctx)

    def exit(self, context, o):
        self.remove_object(context, o)
        self.feedback.disable()
        context.space_data.show_gizmo = True
        bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')

    def modal(self, context, event):

        context.area.tag_redraw()
        o = self.get_scene_object(context, self.object_name)
        
        if o is None:
            self.exit(context, None)
            return {'FINISHED'}

        d = archipack_door.datablock(o)
        hole = None

        if d is not None:
            hole = d.find_hole(o)

        # hide door and hole from ray cast, broken till 28.02 ..
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
            o.matrix_world = tM
            if abs(d.z_offset - z_offset) > 0.001:
                self.select_object(context, o, True)
                d.z_offset = z_offset
            if abs(d.y - width) > 0.001:
                self.select_object(context, o, True)
                d.y = width

        if event.value == 'PRESS':

            if event.type in {'C', 'D'}:
                self.exit(context, o)
                bpy.ops.archipack.door_preset_menu(
                    'INVOKE_DEFAULT',
                    preset_operator="archipack.door_draw")
                self.restore_walls(context)
                return {'FINISHED'}

            if event.type in {'LEFTMOUSE', 'RET', 'NUMPAD_ENTER', 'SPACE'}:
                if wall is not None:
                    # select and make active
                    ctx = context.copy()
                    ctx['object'] = wall
                    ctx['selected_objects'] = [o]
                    bpy.ops.archipack.single_boolean(ctx)

                    # o must be a door here
                    if d is not None:                        
                        # make linked object
                        if len(self.stack) > 0 and not event.shift:
                            last = self.stack[-1]
                            d_last = last.data.archipack_door[0]
                            if d_last.y == d.y:
                                # Must disable manipulators before link !!
                                bpy.ops.archipack.disable_manipulate()
                                self.link_object(last, o)

                        if "archipack_wall2" in wall.data:
                            # update dimensions
                            with ensure_select_and_restore(context, wall, [wall]):
                                wd = wall.data.archipack_wall2[0]
                                wg = wd.get_generator()
                                wd.setup_childs(context, wall, g=wg, openings_only=True)
                                wd.relocate_childs(context, wall, g=wg)
                                wd.update_dimension(context, wall, wg)

                        # self.select_object(context, o, True)
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
                # self.select_object(context, o, True)
            return {'RUNNING_MODAL'}

        if event.value == 'RELEASE':

            if event.type in {'ESC', 'RIGHTMOUSE'}:
                self.exit(context, o)
                self.restore_walls(context)

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
            if self.filepath == '' and archipack_door.filter(o):
                self.stack.append(o)
                o = self.duplicate_object(context, o, False)
                self.object_name = o.name
            else:
                o = self.add_object(context, event)

            # select and make active
            bpy.ops.object.select_all(action="DESELECT")
            self.select_object(context, o, True)
            
            self.feedback = FeedbackPanel()
            self.feedback.instructions(context, "Draw a door", "Click & Drag over a wall", [
                ('LEFTCLICK, RET, SPACE, ENTER', 'Create a door'),
                ('BACKSPACE, CTRL+Z', 'undo last'),
                ('D', 'Draw another door'),
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


# ------------------------------------------------------------------
# Define operator class to load / save presets
# ------------------------------------------------------------------


class ARCHIPACK_OT_door_preset_draw(PresetMenuOperator, Operator):
    bl_description = "Choose a preset and draw doors over wall"
    bl_idname = "archipack.door_preset_draw"
    bl_label = "Door Presets"
    preset_subdir = "archipack_door"
    

class ARCHIPACK_OT_door_preset_menu(PresetMenuOperator, Operator):
    bl_description = "Show Doors presets"
    bl_idname = "archipack.door_preset_menu"
    bl_label = "Door Presets"
    preset_subdir = "archipack_door"


class ARCHIPACK_OT_door_preset(ArchipackPreset, Operator):
    bl_description = "Add / remove a Door Preset"
    bl_idname = "archipack.door_preset"
    bl_label = "Door Preset"
    preset_menu = "ARCHIPACK_OT_door_preset_menu"

    @property
    def blacklist(self):
        return ['y']


def register():
    bpy.utils.register_class(archipack_door_panel)
    Mesh.archipack_door_panel = CollectionProperty(type=archipack_door_panel)
    bpy.utils.register_class(ARCHIPACK_PT_door_panel)
    bpy.utils.register_class(ARCHIPACK_OT_select_parent)
    bpy.utils.register_class(ARCHIPACK_OT_select)
    bpy.utils.register_class(archipack_door)
    Mesh.archipack_door = CollectionProperty(type=archipack_door)
    bpy.utils.register_class(ARCHIPACK_OT_door_preset_menu)
    bpy.utils.register_class(ARCHIPACK_OT_door_preset_draw)
    bpy.utils.register_class(ARCHIPACK_PT_door)
    bpy.utils.register_class(ARCHIPACK_OT_door)
    bpy.utils.register_class(ARCHIPACK_OT_door_preset)
    bpy.utils.register_class(ARCHIPACK_OT_door_draw)
    bpy.utils.register_class(ARCHIPACK_OT_door_open)


def unregister():
    bpy.utils.unregister_class(archipack_door_panel)
    del Mesh.archipack_door_panel
    bpy.utils.unregister_class(ARCHIPACK_PT_door_panel)
    bpy.utils.unregister_class(ARCHIPACK_OT_select_parent)
    bpy.utils.unregister_class(ARCHIPACK_OT_select)
    bpy.utils.unregister_class(archipack_door)
    del Mesh.archipack_door
    bpy.utils.unregister_class(ARCHIPACK_OT_door_preset_menu)
    bpy.utils.unregister_class(ARCHIPACK_OT_door_preset_draw)
    bpy.utils.unregister_class(ARCHIPACK_PT_door)
    bpy.utils.unregister_class(ARCHIPACK_OT_door)
    bpy.utils.unregister_class(ARCHIPACK_OT_door_preset)
    bpy.utils.unregister_class(ARCHIPACK_OT_door_draw)
    bpy.utils.unregister_class(ARCHIPACK_OT_door_open)
