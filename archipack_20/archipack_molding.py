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
    FloatProperty, BoolProperty, CollectionProperty,
    StringProperty, EnumProperty
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
from .archipack_preset import ArchipackPreset, PresetMenuOperator
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackCreateTool,
    ArchipackObject,
    ArchipackPanel,
    ArchipackDrawTool,
    stop_auto_manipulate
)
from .archipack_dimension import DimensionProvider
from .archipack_curveman import ArchipackProfile, ArchipackUserDefinedPath
from .archipack_segments2 import ArchipackSegment
from .archipack_generator import Generator
from .archipack_prefs import get_prefs
from .archipack_keymaps import Keymaps

import logging
logger = logging.getLogger("archipack")


class MoldingGenerator(Generator):

    __slots__= ('line')

    def __init__(self, o=None):
        Generator.__init__(self, o)
        self.line = None

    @property
    def random_color(self):
        return Vector((uniform(0.0, 1.0), uniform(0.0, 1.0), uniform(0.0, 1.0), 1))

    def make_profile(self, profile, idmat,
            x_offset, z_offset, extend, closed, verts, faces, matids, uvs, vcolors):

        _segs = self.line.valid_segs

        n_moldings = len(_segs) - 1

        if n_moldings < 0:
            return

        sections = []

        f = _segs[0]
        f_last = _segs[-1]
        closed_path = (f.p0 - f_last.p1).length < 0.0001

        # add first section
        if closed_path:
            n = f_last.normal(1, 1)
        else:
            n = f.normal(0, 1)

        # n.p = f.lerp(x_offset)
        sections.append(n)

        for s, f in enumerate(_segs):
            if f.length == 0:
                continue
            n = f.normal(1, 1)
            sections.append(n)

        user_path_verts = len(sections)
        offset = len(verts)
        if user_path_verts > 0:
            user_path_uv_v = []
            n_sections = user_path_verts - 1
            n = sections[0]
            p0 = n.p0
            v0 = n.v_normalized
            for s, n in enumerate(sections):
                p1 = n.p0
                if s > 0:
                    user_path_uv_v.append((p1 - p0).length)
                if s < n_sections:
                    v1 = sections[s + 1].v_normalized
                elif closed_path:
                    break
                dir = (v0 + v1).normalized()
                scale = min(10, 1 / cos(0.5 * acos(min(1, max(-1, v0 @ v1)))))
                for p in profile:
                    # x, y = n.p + scale * (x_offset + p.x) * dir
                    x, y, z = n.p0 + scale * p.x * dir
                    z = p.y + z_offset
                    verts.append((x, y, z))
                p0 = p1
                v0 = v1
            if closed_path:
                user_path_verts -= 1
            # build faces using Panel
            lofter = Lofter(
                # closed_shape, index, x, y, idmat
                closed,
                [i for i in range(len(profile))],
                [p.x for p in profile],
                [p.y for p in profile],
                [idmat for i in range(len(profile))],
                closed_path=closed_path,
                user_path_uv_v=user_path_uv_v,
                user_path_verts=user_path_verts
                )
            faces += lofter.faces(1, offset=offset, path_type='USER_DEFINED')
            matids += lofter.mat(1, idmat, idmat, path_type='USER_DEFINED')
            v = Vector((0, 0))
            uvs += lofter.uv(1, v, v, v, v, 0, v, 0, 0, path_type='USER_DEFINED')
            vcolors += lofter.vcolors(1, self.random_color, path_type='USER_DEFINED')


def update(self, context):
    if self.auto_update:
        self.update(context)


def update_manipulators(self, context):
    self.update(context, manipulable_refresh=True)


class archipack_molding_part(Archipacki18n, ArchipackSegment, PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_molding[0]


class archipack_molding(
        Archipacki18n,
        ArchipackUserDefinedPath,
        ArchipackObject,
        ArchipackProfile,
        Manipulable,
        DimensionProvider,
        PropertyGroup):

    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display main settings', 'NONE', 0),
            ('PARTS', 'Axis', 'Display molding segments settings', 'NONE', 1),
            ('MATERIALS', '', 'Display materials settings', 'MATERIAL', 2)
        ),
        default='MAIN',
    )
    parts: CollectionProperty(type=archipack_molding_part)
    x_offset: FloatProperty(
        name="Offset",
        default=0.0, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
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
    profil_radius: FloatProperty(
        name="Radius",
        min=0.001,
        default=0.02, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    always_closed = False

    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update_manipulators
    )

    def setup_manipulators(self):

        if len(self.manipulators) < 1:
            s = self.manipulators.add()
            s.prop1_name = "n_parts"
            s.type_key = 'COUNTER'

        self.setup_parts_manipulators('profil_y')

    def refresh_profile_size(self, context, x, y):
        self.profil_x = x
        self.profil_y = y

    def get_generator(self, o=None):
        g = MoldingGenerator(o)
        g.add_parts(self)
        g.line = g.make_offset(self.x_offset)
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
        # reset user def posts

        if self.profil == 'USER':
            curve = self.update_profile(context)
            if curve and curve.type == 'CURVE':
                sx, sy = 1, 1
                if self.user_profile_dimension.x > 0:
                    sx = self.profil_x / self.user_profile_dimension.x
                if self.user_profile_dimension.y > 0:
                    sy = self.profil_y / self.user_profile_dimension.y

                wM = Matrix([
                    [sx, 0, 0, 0],
                    [0, sy, 0, 0],
                    [0, 0, 1, 0],
                    [0, 0, 0, 1]
                    ])

                for spline in curve.data.splines:

                    molding = self.coords_from_spline(spline, wM, 12, ccw=True)
                    closed = molding[0] == molding[-1]
                    if closed:
                        molding.pop()
                    g.make_profile(molding, 0, self.x_offset,
                        0, 0, True, verts, faces, matids, uvs, vcolors)
            else:
                x = self.profil_x
                y = self.profil_y
                molding = [Vector((0, y)), Vector((0, 0)), Vector((x, 0)), Vector((x, y))]
                g.make_profile(molding, 0, self.x_offset,
                    0, 0, True, verts, faces, matids, uvs, vcolors)
        else:
            if self.profil == 'SQUARE':
                x = self.profil_x
                y = self.profil_y
                molding = [Vector((0, y)), Vector((0, 0)), Vector((x, 0)), Vector((x, y))]

            elif self.profil == 'CIRCLE':
                x = self.profil_x
                y = self.profil_y
                r = min(self.profil_radius, x, y)
                segs = 6
                da = pi / (2 * segs)
                molding = [Vector((0, y)), Vector((0, 0)), Vector((x, 0))]
                molding.extend([
                    Vector((x + r * (cos(a * da) - 1), y + r * (sin(a * da) - 1)))
                    for a in range(segs + 1)
                    ])

            g.make_profile(molding, 0, self.x_offset,
                0, 0, True, verts, faces, matids, uvs, vcolors)

        bmed.buildmesh(o, verts, faces, matids, uvs, vcolors, weld=False, clean=False)
        self.shade_smooth(context, o, 0.20944)

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
        for m in self.manipulators:
            self.manip_stack.append(m.setup(context, o, self))


class ARCHIPACK_PT_molding(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_molding"
    bl_label = "Molding"


    @classmethod
    def poll(cls, context):
        return archipack_molding.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_molding.datablock(o)
        if d is None:
            return
        layout = self.layout

        # template with icon right on object
        # layout.template_icon_view(prop, "preset", show_labels=True, scale=10)

        self.draw_common(context, layout)
        box = layout.box()
        # self.draw_label(context, layout, box, "Styles")
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.molding_preset_menu",
                     text=bpy.types.ARCHIPACK_OT_molding_preset_menu.bl_label, icon="PRESET")
        self.draw_op(context, layout, row, "archipack.molding_preset", icon='ADD', text="")
        self.draw_op(context, layout, row, "archipack.molding_preset", icon='REMOVE', text="").remove_active = True

        self.draw_prop(context, layout, layout, d, "tabs", expand=True)

        box = layout.box()
        if d.tabs == 'PARTS':
            d.template_user_path(context, box, focus=False)
            d.template_parts(context, layout, draw_type=False)

        elif d.tabs == 'MAIN':
            row = box.row(align=False)
            self.draw_prop(context, layout, row, d, 'profil', text="")
            self.draw_prop(context, layout, box, d, 'profil_x')
            self.draw_prop(context, layout, box, d, 'profil_y')
            if d.profil == 'CIRCLE':
                self.draw_prop(context, layout, box, d, 'profil_radius')
            self.draw_prop(context, layout, box, d, 'x_offset')
            if d.profil == 'USER':
                d.draw_user_profile(context, box)

        elif d.tabs == 'MATERIALS':
            if "archipack_material" in o:
                o.archipack_material[0].draw(context, box)

# ------------------------------------------------------------------
# Define operator class to create object
# TODO: turn into internal operator, allow molding from wall, from curve and regular
# ------------------------------------------------------------------


class ARCHIPACK_OT_molding(ArchipackCreateTool, Operator):
    bl_idname = "archipack.molding"
    bl_label = "Molding"
    bl_description = "Molding"

    def create(self, context):
        m = bpy.data.meshes.new("Molding")
        o = bpy.data.objects.new("Molding", m)
        d = m.archipack_molding.add()
        # make manipulators selectable
        d.manipulable_selectable = True
        d.set_parts(1)
        self.link_object_to_scene(context, o)
        o.color = (1, 0.25, 0, 1)

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
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


class ARCHIPACK_OT_molding_from_curve(ArchipackCreateTool, Operator):
    bl_idname = "archipack.molding_from_curve"
    bl_label = "Molding curve"
    bl_description = "Create molding from curve"

    @classmethod
    def poll(self, context):
        return context.active_object is not None and context.active_object.type == 'CURVE'

    def create(self, context):
        o = None
        curve = context.active_object
        sel = []
        for i, spline in enumerate(curve.data.splines):
            bpy.ops.archipack.molding()
            o = context.active_object
            d = archipack_molding.datablock(o)
            d.auto_update = False
            d.user_defined_spline = i
            d.user_defined_path = curve.name
            d.auto_update = True
            sel.append(o)

        for obj in sel:
            self.select_object(context, obj)

        return o

    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            o = self.create(context)
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_molding_from_wall(ArchipackCreateTool, Operator):
    bl_idname = "archipack.molding_from_wall"
    bl_label = "->Molding"
    bl_description = "Create molding from a wall"

    mode: EnumProperty(
        name="Mode",
        items=(
            ('MOLDINGS', 'Moldings', 'Floor moldings'),
            ('CEILING', 'Ceiling', 'Ceiling moldings')
        ),
        default='MOLDINGS'
    )

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o is not None and o.data is not None and 'archipack_wall2' in o.data

    def create(self, context):
        tim = time.time()
        m = bpy.data.meshes.new("Molding")
        o = bpy.data.objects.new("Molding", m)
        d = m.archipack_molding.add()
        # make manipulators selectable
        d.manipulable_selectable = True
        d.set_parts(1)
        d.auto_update = False
        self.link_object_to_scene(context, o)
        o.color = (1, 0.25, 0, 1)

        logger.debug("create link_object_to_scene() :%.4f seconds", time.time() - tim)
        self.select_object(context, o, True)
        self.add_material(context, o, category="molding")
        self.load_preset(d, auto_update=False)
        logger.debug("create done :%.4f seconds", time.time() - tim)
        return o

    def create_one(self, context, w, wd, io, line, ccw):
        boundary = io._to_curve(line, "{}-boundary".format(w.name), '2D')
        boundary.location.z = w.matrix_world.translation.z
        if self.mode == 'CEILING':
            boundary.location.z += wd.z - wd.z_offset
        # logger.debug("molding_from_wall boundary :%.4f seconds", time.time() - tim)
        o = self.create(context)
        # logger.debug("molding_from_wall create :%.4f seconds", time.time() - tim)
        # o.matrix_world = w.matrix_world.copy()
        d = archipack_molding.datablock(o)
        # d.user_defined_spline = 0
        # from_spline(self, context, o, curve, ccw=False, cw=False)
        d.from_spline(context, o, boundary, ccw=ccw)
        # d.user_defined_path = boundary.name
        d.auto_update = True
        # logger.debug("molding_from_wall update :%.4f seconds", time.time() - tim)
        self.delete_object(context, boundary)
        # d.user_defined_path = ""

        self.unselect_object(context, o)
        return o

    def molding_from_wall(self, context, w, wd):
        """
         Create flooring from surrounding wall
         Use slab cutters, windows and doors, T childs walls
        """
        tim = time.time()
        # wall is either a single or collection of polygons
        try:
            io, wall, childs = wd.as_geom(context, w, self.mode, [], [], [])

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

        logger.debug("molding_from_wall wd.as_geom :%.4f seconds", time.time() - tim)

        # find slab holes if any
        o = None
        sel = []
        # MultiLineString - Multipolygon
        if wall.type_id in {5, 6}:
            lines = wall.geoms
        else:
            lines = [wall]

        for i, line in enumerate(lines):
            o = self.create_one(context, w, wd, io, line, ccw=False)
            sel.append(o)
            # handle interiors spaces bounds by non touching walls
            if hasattr(line, "interiors"):
                for interior in line.interiors:
                    o = self.create_one(context, w, wd, io, interior, ccw=True)
                    sel.append(o)

        self.select_object(context, w, True)
        for obj in sel:
            self.select_object(context, obj)
        bpy.ops.archipack.add_reference_point()
        self.unselect_object(context, w)
        logger.debug("molding_from_wall() :%.4f seconds", time.time() - tim)
        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            logger.debug("ARCHIPACK_OT_molding_from_wall.execute() %s %s", self.mode, self.filepath)
            wall = context.active_object
            bpy.ops.object.select_all(action="DESELECT")
            wd = wall.data.archipack_wall2[0]
            with stop_auto_manipulate(context):
                o = self.molding_from_wall(context, wall, wd)
            self.select_object(context, wall, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_molding_draw(ArchipackDrawTool, Operator):
    bl_idname = "archipack.molding_draw"
    bl_label = "Draw a molding"
    bl_description = "Create a molding by drawing its baseline in 3D view"

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
        logger.debug("ARCHIPACK_OT_molding_draw.sp_draw")
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
                bpy.ops.archipack.molding('INVOKE_DEFAULT', filepath=self.filepath)
                # self.unselect_object(context, self.act)

                # context.window_manager.archipack.auto_manipulate = False

                o = context.object
                o.location = takeloc
                self.o = o
                d = archipack_molding.datablock(o)
                # d.manipulable_selectable = False
                part = d.parts[0]
                part.length = delta.length
                state = "CALL_MANIPULATE"
            else:
                o = self.o
                # select and make active
                # self.select_object(context, o, True)
                d = archipack_molding.datablock(o)
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
        logger.debug("ARCHIPACK_OT_molding_draw.sp_init event %s %s state:%s" % (event.type, event.value, state))
        if state == 'SUCCESS':
            # point placed, check if a wall was under mouse
            logger.debug("self.mouse_hover_wall(context, event)")

            res, pt, y, i, o, tM = self.mouse_to_scene_raycast(context, event)
            logger.debug("self.mouse_hover_wall done")

            if res:
                if self.is_snapping(context, event, sp.placeloc):
                    # user snap, use direction as constraint
                    tM.translation = sp.placeloc.copy()
                else:
                    # without snap must use ray intersection on xy plane
                    pt = self.mouse_to_plane(context, event)
                    if pt is not None:
                        tM.translation = pt

                self.takeloc = tM.translation

                self.act = o
                self.takemat = tM
            else:
                self.takeloc = sp.placeloc.copy()
                if not self.is_snapping(context, event, sp.placeloc):
                    # without snap must use ray intersection on xy plane
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
                logger.debug("ARCHIPACK_OT_molding_draw.modal(STARTING) location:%s", takeloc)
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
                logger.debug("ARCHIPACK_OT_molding_draw.modal(%s) D pressed", self.state)
                self.exit(context)
                bpy.ops.archipack.molding_preset_menu('INVOKE_DEFAULT', preset_operator="archipack.molding_draw")
                return {'FINISHED'}

            elif event.type in {'C', 'c'}:

                logger.debug("ARCHIPACK_OT_molding_draw.modal(%s) C pressed", self.state)
                d = archipack_molding.datablock(self.o)
                if d is not None:
                    d.closed = True
                self.exit(context)
                return {'FINISHED'}

            elif event.type in {'LEFTMOUSE', 'RET', 'NUMPAD_ENTER', 'SPACE'}:

                # print('LEFTMOUSE %s' % (event.value))
                self.feedback.instructions(context, "Draw a molding", "Click & Drag to add a segment", [
                    ('ENTER', 'Add part'),
                    ('BACK_SPACE', 'Remove part'),
                    ('CTRL', 'Snap'),
                    ('C', 'Close molding and exit'),
                    ('D', 'Draw another molding'),
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

                        d = archipack_molding.datablock(o)
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
                        logger.debug("ARCHIPACK_OT_molding_draw.modal(CREATE) location:%s", takeloc)

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
                d = archipack_molding.datablock(o)
                if d.num_parts > 1:
                    d.n_parts -= 1
            return {'RUNNING_MODAL'}

        if self.state == 'CANCEL' or (event.type in {'ESC', 'RIGHTMOUSE'}):

            logger.debug("ARCHIPACK_OT_molding_draw.modal(CANCEL) %s", event.type)
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
            self.feedback.instructions(context, "Draw a molding", "Click & Drag to start", [
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


class ARCHIPACK_OT_molding_preset_from_wall(PresetMenuOperator, Operator):
    bl_description = "Create molding from wall"
    bl_idname = "archipack.molding_preset_from_wall"
    bl_label = "-> Molding"
    preset_subdir = "archipack_molding"
    preset_operator: StringProperty(
        options={'SKIP_SAVE'},
        default="archipack.molding_from_wall"
    )
    mode: EnumProperty(
        name="Mode",
        items=(
            ('MOLDINGS', 'Moldings', 'Floor moldings'),
            ('CEILING', 'Ceiling', 'Ceiling moldings')
        ),
        default='MOLDINGS'
    )
    def args(self, preset):
        return {'filepath': preset, 'mode': self.mode}

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o and o.data and "archipack_wall2" in o.data


class ARCHIPACK_OT_molding_preset_from_curve(PresetMenuOperator, Operator):
    bl_description = "Create molding(s) from a curve"
    bl_idname = "archipack.molding_preset_from_curve"
    bl_label = "-> Molding"
    preset_subdir = "archipack_molding"
    preset_operator: StringProperty(
        options={'SKIP_SAVE'},
        default="archipack.molding_from_curve"
    )

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o and o.type == 'CURVE'


class ARCHIPACK_OT_molding_preset_create(PresetMenuOperator, Operator):
    bl_description = "Show Molding presets and create object at cursor location"
    bl_idname = "archipack.molding_preset_create"
    bl_label = "Molding Styles"
    preset_subdir = "archipack_molding"


class ARCHIPACK_OT_molding_preset_menu(PresetMenuOperator, Operator):
    bl_description = "Display Molding presets"
    bl_idname = "archipack.molding_preset_menu"
    bl_label = "Molding preset"
    preset_subdir = "archipack_molding"


class ARCHIPACK_OT_molding_preset_draw(PresetMenuOperator, Operator):
    bl_description = "Display Molding presets and draw"
    bl_idname = "archipack.molding_preset_draw"
    bl_label = "Molding preset"
    preset_subdir = "archipack_molding"


class ARCHIPACK_OT_molding_preset(ArchipackPreset, Operator):
    bl_description = "Add / remove a Molding Preset"
    bl_idname = "archipack.molding_preset"
    bl_label = "Molding preset"
    preset_menu = "ARCHIPACK_OT_molding_preset_menu"

    @property
    def blacklist(self):
        return ["parts", "n_parts"]


def register():
    bpy.utils.register_class(archipack_molding_part)
    bpy.utils.register_class(archipack_molding)
    Mesh.archipack_molding = CollectionProperty(type=archipack_molding)
    bpy.utils.register_class(ARCHIPACK_OT_molding_preset_menu)
    bpy.utils.register_class(ARCHIPACK_OT_molding_preset_create)
    bpy.utils.register_class(ARCHIPACK_OT_molding_preset_draw)
    bpy.utils.register_class(ARCHIPACK_OT_molding_preset_from_curve)
    bpy.utils.register_class(ARCHIPACK_OT_molding_preset_from_wall)
    bpy.utils.register_class(ARCHIPACK_PT_molding)
    bpy.utils.register_class(ARCHIPACK_OT_molding)
    bpy.utils.register_class(ARCHIPACK_OT_molding_preset)
    bpy.utils.register_class(ARCHIPACK_OT_molding_from_curve)
    bpy.utils.register_class(ARCHIPACK_OT_molding_from_wall)
    bpy.utils.register_class(ARCHIPACK_OT_molding_draw)


def unregister():
    bpy.utils.unregister_class(archipack_molding_part)
    bpy.utils.unregister_class(archipack_molding)
    del Mesh.archipack_molding
    bpy.utils.unregister_class(ARCHIPACK_OT_molding_preset_menu)
    bpy.utils.unregister_class(ARCHIPACK_OT_molding_preset_create)
    bpy.utils.unregister_class(ARCHIPACK_OT_molding_preset_draw)
    bpy.utils.unregister_class(ARCHIPACK_OT_molding_preset_from_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_molding_preset_from_wall)
    bpy.utils.unregister_class(ARCHIPACK_PT_molding)
    bpy.utils.unregister_class(ARCHIPACK_OT_molding)
    bpy.utils.unregister_class(ARCHIPACK_OT_molding_preset)
    bpy.utils.unregister_class(ARCHIPACK_OT_molding_from_curve)
    bpy.utils.unregister_class(ARCHIPACK_OT_molding_from_wall)
    bpy.utils.unregister_class(ARCHIPACK_OT_molding_draw)
