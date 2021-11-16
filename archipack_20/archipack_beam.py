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
#
# ----------------------------------------------------------
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
import bpy
from bpy.types import Operator, PropertyGroup, Mesh, Panel
from bpy.props import (
    FloatProperty, IntProperty, BoolProperty,
    CollectionProperty, EnumProperty
)
from .bmesh_utils import BmeshEdit as bmed
from random import uniform
from mathutils import Vector, Matrix
from math import sin, cos, pi
from .archipack_manipulator import Manipulable
from .archipack_i18n import Archipacki18n
from .archipack_object import ArchipackCreateTool, ArchipackObject, ArchipackPanel
from .archipack_curveman import ArchipackProfile
from .panel import Panel as Lofter


def random_color():
    return Vector((uniform(0.0, 1.0), uniform(0.0, 1.0), uniform(0.0, 1.0), 1))


def update(self, context):
    self.update(context)


def get_align(self):
    return self.align


def set_align(self, value):
    self.align = value
    return None


class archipack_beam(Archipacki18n, ArchipackObject, ArchipackProfile, Manipulable, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display main settings', 'NONE', 0),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 1)
        ),
        default='MAIN',
    )
    profil: EnumProperty(
        name="Profil",
        items=(
                ('SQUARE', 'Square', '', 0),
                ('CIRCLE', 'Circle', '', 1),
                ('USER', 'User defined', '', 2)
            ),
        default='SQUARE',
        update=update
    )
    align: IntProperty(default=4)
    align_ui: EnumProperty(
        options={'SKIP_SAVE'},
        name="Axis",
        description="Location of axis",
        items=(
            ('0', "Top left", "Top left"),
            ('1', "Top axis", "Top axis"),
            ('2', "Top right", "Top right"),
            ('3', "Center left", "Center left"),
            ('4', "Center axis", "Center axis"),
            ('5', "Center right", "Center right"),
            ('6', "Bottom left", "Bottom left"),
            ('7', "Bottom axis", "Bottom axis"),
            ('8', "Bottom right", "Bottom right")
        ),
        default="4",
        get=get_align,
        set=set_align,
        update=update
    )

    z: FloatProperty(
        name="Height",
        default=2.0, min=0.01, precision=5,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    x: FloatProperty(
        name="Width",
        min=0.001,
        default=0.1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    y: FloatProperty(
        name="Depth",
        min=0.001,
        default=0.1, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    auto_update: BoolProperty(
            options={'SKIP_SAVE'},
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

    def refresh_profile_size(self, context, x, y):
        self.x = x
        self.y = y
        self.auto_update = True
        self.update(context)

    def make_profile(self, profile, closed, verts, faces, matids, uvs, vcolors):

        lofter = Lofter(
            # closed_shape, index, x, y, idmat
            closed,
            [i for i in range(len(profile))],
            [p.x for p in profile],
            [p.y for p in profile],
            [0] * len(profile),
            closed_path=False,
            user_path_uv_v=[self.z],
            user_path_verts=2
        )
        color = random_color()
        v = Vector((0, 0))
        faces += lofter.faces(16, offset=len(verts), path_type='USER_DEFINED')
        matids += lofter.mat(16, 0, 0, path_type='USER_DEFINED')
        vcolors += lofter.vcolors(16, color, path_type='USER_DEFINED')
        uvs += lofter.uv(16, v, v, v, v, 0, v, 0, 0, path_type='USER_DEFINED')

        z = 0
        for p in profile:
            x, y = p.x, p.y
            verts.append((x, y, z))
        z = self.z
        for p in profile:
            x, y = p.x, p.y
            verts.append((x, y, z))

    def update(self, context):

        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        self.setup_manipulators()
        verts, faces, matids, uvs, vcolors = [], [], [], [], []

        x, y = 0.5 * self.x, 0.5 * self.y
        h = int(self.align % 3)
        v = int((self.align - h) / 3)
        cx, cy = 0, 0
        if h == 0:
            cx = x
        elif h == 2:
            cx = -x

        if v == 0:
            cy = -y
        elif v == 2:
            cy = y

        c = Vector((cx, cy))
        closed = True

        if self.profil == 'SQUARE':
            profil = [c + v for v in [Vector((-x, y)), Vector((-x, -y)), Vector((x, -y)), Vector((x, y))]]
            self.make_profile(profil, closed, verts, faces, matids, uvs, vcolors)
        elif self.profil == 'CIRCLE':
            profil = [c + v for v in [Vector((x * sin(0.1 * -a * pi), x * cos(0.1 * -a * pi))) for a in range(0, 20)]]
            self.make_profile(profil, closed, verts, faces, matids, uvs, vcolors)
        else:
            curve = self.update_profile(context)
            if curve and curve.type == 'CURVE':
                sx, sy = 1, 1
                if self.user_profile_dimension.x > 0:
                    sx = self.x / self.user_profile_dimension.x
                if self.user_profile_dimension.y > 0:
                    sy = self.y / self.user_profile_dimension.y
                wM = Matrix([
                    [sx, 0, 0, 0],
                    [0, sy, 0, 0],
                    [0, 0, 1, 0],
                    [0, 0, 0, 1]
                ])
                for spline in curve.data.splines:
                    profil = self.coords_from_spline(spline, wM, 12, ccw=True)
                    sx, sy, sz = zip(*profil)
                    dx, dy = min(sx), min(sy)
                    c = c - Vector((x + dx, y + dy))
                    profil = [c + Vector((x, y)) for x, y in zip(sx, sy)]
                    closed = spline.use_cyclic_u
                    self.make_profile(profil, closed, verts, faces, matids, uvs, vcolors)
            else:
                profil = [Vector((-x, y)), Vector((-x, -y)), Vector((x, -y)), Vector((x, y))]
                self.make_profile(profil, closed, verts, faces, matids, uvs, vcolors)

        bmed.buildmesh(o, verts, faces, matids, uvs, vcolors=vcolors)
        self.shade_smooth(context, o, 1.15)

        x0, x1 = cx - x, cx + x
        y0, y1 = cy - y, cy + y
        self.manipulators[0].set_pts([(x0, y0, 0), (x0, y0, self.z), (0.5, 0, 0)])
        self.manipulators[1].set_pts([(x0, y0, 0), (x1, y0, 0), (0.5, 0, 0)])
        self.manipulators[2].set_pts([(x1, y0, 0), (x1, y1, 0), (0.5, 0, 0)])

        self.restore_context(context)


class ARCHIPACK_PT_beam(ArchipackPanel, Archipacki18n, Panel):
    """Archipack Beam"""
    bl_idname = "ARCHIPACK_PT_beam"
    bl_label = "Beam"

    @classmethod
    def poll(cls, context):
        return archipack_beam.poll(context.active_object)


    def update(self, context, manipulable_refresh=False):
        if self.auto_update:
            self.parent_data.update(context, manipulable_refresh)


    def draw(self, context):
        o = context.active_object
        d = archipack_beam.datablock(o)

        if d is None:
            return

        layout = self.layout

        self.draw_common(context, layout)

        self.draw_prop(context, layout, layout, d, 'tabs', expand=True)

        box = layout.box()
        if d.tabs == 'MAIN':
            self.draw_prop(context, layout, box, d, 'z')
            self.draw_prop(context, layout, box, d, 'x')
            self.draw_prop(context, layout, box, d, 'y')
            # self.draw_prop(context, layout, box, d, 'align_ui')
            self.draw_prop(context, layout, box, d, 'profil')
            if d.profil == 'USER':
                d.draw_user_profile(context, layout)

        elif d.tabs == 'MATERIALS':
            if "archipack_material" in o:
                o.archipack_material[0].draw(context, box)


class ARCHIPACK_OT_beam(ArchipackCreateTool, Operator):
    bl_idname = "archipack.beam"
    bl_label = "Beam"
    bl_description = "Create Beam at cursor location"

    def create(self, context):
        m = bpy.data.meshes.new("Beam")
        o = bpy.data.objects.new("Beam", m)
        d = m.archipack_beam.add()

        self.link_object_to_scene(context, o)
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


def register():
    bpy.utils.register_class(archipack_beam)
    Mesh.archipack_beam = CollectionProperty(type=archipack_beam)
    bpy.utils.register_class(ARCHIPACK_PT_beam)
    bpy.utils.register_class(ARCHIPACK_OT_beam)


def unregister():
    bpy.utils.unregister_class(archipack_beam)
    del Mesh.archipack_beam
    bpy.utils.unregister_class(ARCHIPACK_PT_beam)
    bpy.utils.unregister_class(ARCHIPACK_OT_beam)
