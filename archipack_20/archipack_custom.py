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
# ----------------------------------------------------------
# noinspection PyUnresolvedReferences
import bpy
import bmesh
# noinspection PyUnresolvedReferences
from bpy.types import Operator, PropertyGroup, Mesh, Panel
from bpy.props import (
    FloatProperty, FloatVectorProperty, BoolProperty,
    CollectionProperty, StringProperty, IntProperty,
    EnumProperty
)
from .archipack_snap import snap_point
from .bmesh_utils import BmeshEdit as bmed
from bpy_extras import view3d_utils
from math import pi, floor
from mathutils import Vector, Matrix
from mathutils.geometry import (
    intersect_line_plane
    )
from .archipack_manipulator import Manipulable
# from .archipack_preset import ArchipackPreset, PresetMenuOperator
from .archipack_gl import (
    FeedbackPanel,
    GlPolygon,
    SquareHandle,
    GlLine,
    GlText,
    TriHandle
)
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackPanel,
    ArchipackObject,
    ArchipackDrawTool,
    ArchipackCreateTool,
    ArchipackObjectsManager,
    ensure_select_and_restore,
    stop_auto_manipulate
    )
from .archipack_segments2 import OpeningGenerator
from .archipack_keymaps import Keymaps
from .archipack_dimension import DimensionProvider
xAxis = Vector((1, 0, 0))
yAxis = Vector((0, 1, 0))
zAxis = Vector((0, 0, 1))


def update(self, context):
    self.update(context)


def update_manipulators(self, context):
    self.manipulable_refresh = True
    self.update(context)


class Changed:
    """
    Fake part to store last state on change
    """
    def __init__(self):
        self.prop = ""
        self.last = 0
        pass

    def init(self, d, prop):
        # changed property name
        # print("Changed.init()", prop)
        self.prop = prop
        self.last = getattr(d, prop)

    def get_change(self):
        prop, last = self.prop, self.last
        self.prop = ""
        self.last = 0
        return prop, last


def change_getter(attr):
    def getter(self):
        return getattr(self, attr)

    return getter


def change_setter(attr):
    def setter(self, value):
        self.__class__.change.init(self, attr)
        setattr(self, attr, value)
        return None

    return setter


class archipack_custom_part(ArchipackObject, PropertyGroup):
    """
      Defines and custom part
      Provide access to custom parent ui
      While parent resize from axis
      parts might have pivot not matching parent one
      so we must recompute pivot location
    """

    # pivot weight = same weight as a vertex in that location
    # ['left', 'right', 'front', 'back', 'bottom', 'top']
    pivot_weight: FloatVectorProperty(
        default=[0, 0, 0, 0, 0, 0],
        size=6
    )

    def find_custom(self, context):
        o = self.find_in_selection(context)
        if o and o.parent:
            self.restore_auto_manipulate(context)
            return o.parent


class archipack_custom(ArchipackObject, Manipulable, DimensionProvider, PropertyGroup):
    """
      This is the custom root
      use parent-child relationship to identify parts
    """
    change = Changed()

    mode: EnumProperty(
        name="Mode",
        items=(('FULL', 'Full', 'Use xyz and altitude', 0),
               ('WALL', 'Wall', 'Use xz and altitude', 1),
               ('FLOOR', 'Floor', 'Use xyz', 2),
               ('ALT', 'Altitude', 'Use only altitude', 3)
        ),
        default="FULL",
        update=update_manipulators
    )
    x_cur: FloatProperty(
        name="Width",
        min=0.01
    )
    y_cur: FloatProperty(
        name="Depth",
        min=0.01
    )
    z_cur: FloatProperty(
        name="Height",
        min=0.01
    )
    altitude_cur: FloatProperty(
        name="Altitude"
    )
    # use xyz instead of _ui to make compatible with wall's maniuplators
    x: FloatProperty(
        options={'SKIP_SAVE', 'HIDDEN'},
        name="Width",
        min=0.01, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        get=change_getter("x_cur"),
        set=change_setter("x_cur"),
        update=update
    )
    y: FloatProperty(
        options={'SKIP_SAVE', 'HIDDEN'},
        name="Depth",
        min=0.01, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        get=change_getter("y_cur"),
        set=change_setter("y_cur"),
        update=update
    )
    z: FloatProperty(
        options={'SKIP_SAVE', 'HIDDEN'},
        name="Height",
        min=0.01, precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        get=change_getter("z_cur"),
        set=change_setter("z_cur"),
        update=update
    )
    altitude: FloatProperty(
        options={'SKIP_SAVE', 'HIDDEN'},
        name="Altitude", precision=5, step=1,
        unit='LENGTH', subtype='DISTANCE',
        get=change_getter("altitude_cur"),
        set=change_setter("altitude_cur"),
        update=update
    )

    flip: BoolProperty(
        description="Flag store object orientation in wall",
        default=False
    )
    auto_update: BoolProperty(
        default=True,
        update=update,
        options={'SKIP_SAVE'}
    )

    def find_parts(self, o):
        return [
            c for c in o.children
            if archipack_custom_part.filter(c)
            ]

    def relocate(self, o, dx, dy, dz, da):
        """
          apply delta to child location
        """
        d = archipack_custom_part.datablock(o)
        # child location relative to parent
        # loc = o.location
        # ['left', 'right', 'front', 'back', 'top']
        # delta location of pivot
        pivot = Vector()
        pivot.x = dx * (d.pivot_weight[0] + d.pivot_weight[1])
        pivot.y = dy * (d.pivot_weight[2] + d.pivot_weight[3])
        # pivot.z = dz * (d.pivot_weight[4])

        # Move child so the pivot stay in same location relative to parent
        o.location += pivot + Vector((0, 0, da))

        return pivot

    def get_weights(self, o, group_index):
        for i, v in enumerate(o.data.vertices):
            for g in v.groups:
                if g.group == group_index:
                    yield (i, g.weight)
                    break

    def resize(self, bm, o, vgroups, group_name, delta, use_weights=True):
        # recompute locations in object coordsys

        if group_name in vgroups:
            group_index = vgroups[group_name]
            weights = self.get_weights(o, group_index)
            if use_weights:
                for i, w in weights:
                    bm.verts[i].co += delta * w
            else:
                verts = [bm.verts[i] for i, w in weights]
                bmesh.ops.translate(bm, verts=verts, vec=delta)
                # for i, w in weights:
                #    m.vertices[i].co += delta

    def get_generator(self, o=None):
        return OpeningGenerator(self, o, typ="CUSTOM")
    
    def setup_manipulators(self):
        # update manipulators data model
        n_manips = len(self.manipulators)
        if n_manips < 1:
            s = self.manipulators.add()
            s.type_key = "SNAP_SIZE_LOC"
            s.prop2_name = "x"
        else:
            s = self.manipulators[0]
        s.prop1_name = "x"

        if n_manips < 2:
            s = self.manipulators.add()
            s.type_key = "SNAP_SIZE_LOC"
            s.prop2_name = "y"
        else:
            s = self.manipulators[1]
        s.prop1_name = "y"
        if n_manips < 3:
            s = self.manipulators.add()
            s.normal = Vector((0, 1, 0))
        else:
            s = self.manipulators[2]
        s.prop1_name = "z"
        if n_manips < 4:
            s = self.manipulators.add()
            s.normal = Vector((0, 1, 0))
        else:
            s = self.manipulators[3]
        s.prop1_name = "altitude"

    def manipulable_setup(self, context, o):

        self.setup_manipulators()

        if self.mode != 'ALT':
            # x
            self.manip_stack.append(self.manipulators[0].setup(context, o, self))
            if self.mode != 'WALL':
                # y
                self.manip_stack.append(self.manipulators[1].setup(context, o, self))
            # z
            self.manip_stack.append(self.manipulators[2].setup(context, o, self))

        if self.mode != 'FLOOR':
            # altitude
            self.manip_stack.append(self.manipulators[3].setup(context, o, self))

    def _synch_childs(self, o, dx, dy, dz, da):
        childs = self.find_parts(o)
        # update linked child location relative to parent
        # vertices are updated through linked data
        for c in childs:
            self.relocate(c, dx, dy, dz, da)

    def synch_childs(self, context, o, dx, dy, dz, da):
        """
            synch childs nodes of linked objects
            update location relative to parent
        """
        linked = self.get_linked_objects(context, o)
        for c in linked:
            if c != o:
                self._synch_childs(c, dx, dy, dz, da)

    def update(self, context):
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        self.setup_manipulators()

        parts = self.find_parts(o)
        parts.append(o)

        dx, dy, dz, da = 0, 0, 0, 0

        prop, last = self.change.get_change()

        for c in parts:

            vgroups = {vgroup.name: vgroup.index for vgroup in c.vertex_groups}

            bm = bmed._start(c)
            bmed.ensure_bmesh(bm)
            if prop == "x_cur":
                dx = 0.5 * (self.x_cur - last)
                self.resize(bm, c, vgroups, "right", Vector((dx, 0, 0)))
                self.resize(bm, c, vgroups, "left", Vector((-dx, 0, 0)))

            elif prop == "y_cur":
                dy = 0.5 * (self.y_cur - last)
                self.resize(bm, c, vgroups, "front", Vector((0, -dy, 0)))
                self.resize(bm, c, vgroups, "back", Vector((0, dy, 0)))

            elif prop == "z_cur":
                dz = self.z_cur - last
                self.resize(bm, c, vgroups, "top", Vector((0, 0, dz)))

            elif prop == "altitude_cur":
                da = self.altitude_cur - last
                bmesh.ops.translate(bm, verts=bm.verts, vec=Vector((0, 0, da)))

            if dx != 0 or dy != 0 or dz != 0 or da != 0:
                if o.name != c.name:
                    delta = self.relocate(c, dx, dy, dz, da)
                    # relocate on pivot change
                    cM = Matrix.Translation(-delta)
                    bmesh.ops.transform(bm, verts=bm.verts, matrix=cM, space=Matrix())
                    # for v in c.data.vertices:
                    #    v.co = cM @ v.co
            bmed._end(bm, c)

        x, y, z, alt = 0.5 * self.x_cur, 0.5 * self.y_cur, self.z_cur, self.altitude_cur
        self.manipulators[0].set_pts([(-x, -y, 0), (x, -y, 0), (1, 0, 0)])
        self.manipulators[1].set_pts([(-x, -y, 0), (-x, y, 0), (-1, 0, 0)])
        self.manipulators[2].set_pts([(x, -y, alt), (x, -y, alt + z), (-1, 0, 0)])
        self.manipulators[3].set_pts([(x, -y, 0), (x, -y, alt), (-1, 0, 0)])

        self.add_dimension_point(0, Vector((-x, -y, 0)))
        self.add_dimension_point(1, Vector((x, -y, 0)))
        self.update_dimensions(context, o)

        # synch linked childs location
        self.synch_childs(context, o, dx, dy, dz, da)

        self.restore_context(context)

    def find_hole(self, o):
        hole = None
        for c in o.children:
            if "archipack_custom_hole" in c:
                hole = c
                break
        return hole

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
            if archipack_custom.filter(o):
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
            d.synch_dimension(context, c)


class ARCHIPACK_PT_custom(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_custom"
    bl_label = "Custom object"

    @classmethod
    def poll(cls, context):
        return archipack_custom.poll(context.active_object)

    def draw(self, context):
        layout = self.layout
        o = context.active_object
        d = archipack_custom.datablock(o)
        if d is None:
            return

        self.draw_common(context, layout)
        self.draw_op(context, layout, layout, 'archipack.custom_array', icon="MOD_ARRAY", text="Array")

        box = layout.box()
        self.draw_prop(context, layout, box, d, 'mode', text="")

        if d.mode != 'ALT':
            self.draw_prop(context, layout, box, d, 'x')
            if d.mode != 'WALL':
                self.draw_prop(context, layout, box, d, 'y')
            self.draw_prop(context, layout, box, d, 'z')

        if d.mode != 'FLOOR':
            self.draw_prop(context, layout, box, d, 'altitude')

        self.draw_op(context, layout, box, 'archipack.custom_draw', icon="GREASEPENCIL")
        # self.draw_op(context, layout, layout, 'archipack.custom_manipulators', icon="TOOL_SETTINGS")


class ARCHIPACK_PT_custom_part(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_custom_part"
    bl_label = "Custom part"

    @classmethod
    def poll(cls, context):
        return archipack_custom_part.poll(context.active_object)

    def draw(self, context):
        layout = self.layout
        self.draw_op(context, layout, layout, "archipack.select_parent", icon="RESTRICT_SELECT_OFF")


class ARCHIPACK_OT_make_custom(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.make_custom"
    bl_label = "Make Custom object"
    bl_description = "Add custom parametric ability to selection"
    bl_category = 'Archipack'
    bl_options = {'UNDO'} # 'REGISTER',

    x: FloatProperty(
        name="x"
    )
    y: FloatProperty(
        name="y"
    )
    z: FloatProperty(
        name="z"
    )

    x_min: FloatProperty(
        name="min",
        precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )
    x_max: FloatProperty(
        name="max",
        precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )
    x_soft: FloatProperty(
        name="soft",
        min=0, max=100,
        subtype="PERCENTAGE"
    )
    y_min: FloatProperty(
        name="min",
        precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )
    y_max: FloatProperty(
        name="max",
        precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )
    y_soft: FloatProperty(
        name="soft",
        min=0, max=100,
        subtype="PERCENTAGE"
    )
    z_min: FloatProperty(
        name="max",
        min=0,
        precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )
    z_max: FloatProperty(
        name="max",
        min=0,
        precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )
    z_soft: FloatProperty(
        name="soft",
        min=0, max=100,
        subtype="PERCENTAGE"
    )

    @classmethod
    def poll(cls, context):
        return context.active_object is not None

    def draw(self, context):
        layout = self.layout
        row = layout.row(align=True)
        row.label(text="min", text_ctxt="archipack")
        row.label(text="max", text_ctxt="archipack")
        row.label(text="soft", text_ctxt="archipack")
        row = layout.row(align=True)
        row.prop(self,"x_min", text="x")
        row.prop(self,"x_max", text="")
        row.prop(self,"x_soft", text="")
        row = layout.row(align=True)
        row.prop(self,"y_min", text="y")
        row.prop(self,"y_max", text="")
        row.prop(self,"y_soft", text="")
        row = layout.row(align=True)
        row.label(text="z")
        row.prop(self,"z_max", text="")
        row.prop(self,"z_soft", text="")

    def weight(self, o, group, itM, soft, limit):
        """                 p.z
                    0          limit    p.z max
                          | soft |
                        start

                Matrix in at soft end inside
                         min         soft        soft          max
                         |                                      |
                         |          <-|            |->          |

                so when soft enabled z coord in [0|1] is weight
                when not enabled matrix is at min / max and positive is weight 1
                negative are weight 0
        """
        for v in o.data.vertices:
            p = itM @ v.co
            if soft > 0:
                start = max(0, limit - soft)
                w = min(1, max(0, (p.z - start) / soft))
            else:
                # fix precision issue
                if p.z + 0.0001 >= limit:
                    w = 1
                else:
                    w = 0
            if w > 0:
                group.add([v.index], w, 'REPLACE')
            else:
                group.remove([v.index])

    def pivot_weight(self, d, i, p, soft, limit):
        if soft > 0:
            start = max(0, limit - soft)
            w = min(1, max(0, (p.z - start) / soft))
        else:
            if p.z + 0.0001 >= limit:
                w = 1
            else:
                w = 0
        if i in {0, 2}:
            w = -w
        d.pivot_weight[i] = w

    def add_vertex_groups(self, o, d, dc, pivot_weight):
        vgroups = {vgroup.name: vgroup.index for vgroup in o.vertex_groups}
        rot_90 = pi / 2
        print("add_vertex_groups")
        # matrix to retrieve vertex distance from center
        # tM = Matrix.Translation(self.origin)
        rM_top = Matrix()
        rM_front = Matrix.Rotation(rot_90, 4, 'X')
        rM_back = Matrix.Rotation(-rot_90, 4, 'X')
        rM_left = Matrix.Rotation(-rot_90, 4, 'Y')
        rM_right = Matrix.Rotation(rot_90, 4, 'Y')
        itM = [(self.tM @ rM).inverted() @ o.matrix_world for rM in [rM_left, rM_right, rM_front, rM_back, rM_top]]
        # self.z_min,
        limit = [
            self.x_min, self.x_max,
            self.y_min, self.y_max,
            self.z_max + (self.bound_z - self.z_max) * 0.005 * self.z_soft
            ]
        soft = [
            (self.x_max + self.x_min) * 0.005 * self.x_soft, (self.x_max + self.x_min) * 0.005 * self.x_soft,
            (self.y_max + self.y_min) * 0.005 * self.y_soft, (self.y_max + self.y_min) * 0.005 * self.y_soft,
            (self.bound_z - self.z_max) * 0.005 * self.z_soft #, (self.z_max + self.z_min) * 0.005 * self.z_soft,
            #d.z_cur * 0.01 * self.z_soft,
            ]
        groups = ['left', 'right', 'front', 'back', 'top'] #'bottom',

        for n, tm, s, l in zip(groups, itM, soft, limit):

            if n not in vgroups:
                g = o.vertex_groups.new()
                g.name = n
            else:
                g = o.vertex_groups[n]
            self.weight(o, g, tm, s, l)

        if pivot_weight:
            # in world space
            # itM = [(tM @ rM).inverted() @ o.matrix_world for rM in [rM_left, rM_right, rM_back, rM_front, rM_top]]
            i = 0
            for tm, s, l in zip(itM, soft, limit):
                self.pivot_weight(dc, i, tm @ Vector(), s, l)
                i += 1

    def clear_parent_inverse(self, tM, o):
        """
          Set matrix_parent_inverse to identity
          keeping visual transforms
          so .location of child is in parent coordsys
        """
        loc = tM.inverted() @ o.matrix_world.translation
        o.matrix_world = tM.copy()
        o.location = loc

    def create(self, context):

        act = context.object

        if act is None:
            return
        print("create", act.name)

        d = archipack_custom.datablock(act)
        if d is None:
            print("Add archipack_custom")
            d = act.data.archipack_custom.add()

        if archipack_custom_part.filter(act):
            act.data.archipack_custom_part.remove(0)

        """
        tM = act.matrix_world
        itM = tM.inverted()

        # 2d bound, bottom of geometry
        bound = Vector(act.bound_box[0]) + Vector(act.bound_box[7])
        center = tM @ (0.5 * bound)

        d.auto_update = False
        # d.last_size = act.dimensions.copy()
        d.x_cur, d.y_cur, d.z_cur = act.dimensions
        d.altitude_cur = act.bound_box[0][2]
        """

        sel = [o for o in context.selected_objects if o.type == 'MESH']
        names = set([o.name for o in sel])
        sel.extend([c for c in act.children if c.name not in names and c.type == 'MESH'])

        self.add_vertex_groups(act, d, d, False)

        for o in sel:

            if o.name != act.name:
                if archipack_custom.filter(o):
                    o.data.archipack_custom.remove(0)
                dc = archipack_custom_part.datablock(o)
                if dc is None:
                    dc = o.data.archipack_custom_part.add()
                self.add_vertex_groups(o, d, dc, True)

                # dc = archipack_custom_part.datablock(o)
                # dc.pivot_location = itM @ o.matrix_world.translation
                # self.clear_parent_inverse(act.matrix_world, o)

        # d.auto_update = True
        return act

    def check_hover(self):
        self.min.check_hover(self.mouse_pos)
        self.max.check_hover(self.mouse_pos)
        self.z_handle.check_hover(self.mouse_pos)
        self.xsoft.check_hover(self.mouse_pos)
        self.ysoft.check_hover(self.mouse_pos)
        self.zsoft.check_hover(self.mouse_pos)

    def mouse_release(self, context, event):
        self.active = False
        self.check_hover()
        self.min.active = False
        self.max.active = False
        self.z_handle.active = False
        self.xsoft.active = False
        self.ysoft.active = False
        self.zsoft.active = False
        self.create(context)
        return False

    def sp_callback(self, context, event, state, sp):

        if state != 'CANCEL':
            if self.min.active:
                x, y, z = self.itM @ sp.placeloc
                # print("xy", x, y)
                self.x_min = -min(self.x_max, x)
                self.y_min = -min(self.y_max, y)
                # print(self.x_min, self.y_min)
                self.create(context)
            if self.max.active:
                x, y, z = self.itM @ sp.placeloc
                # print("xy", x, y)
                self.x_max = max(-self.x_min, x)
                self.y_max = max(-self.y_min, y)
                # print(self.x_max, self.y_max)
                self.create(context)
        if state != 'RUNNING':
            self.mouse_release(context, event)

    def mouse_move(self, context, event):
        self.mouse_pos = Vector((event.mouse_region_x, event.mouse_region_y))
        if self.active:
            # draggin mouse
            if self.min.active:
                pos_3d = self.get_pos3d(context)
                dp = self.itM @ pos_3d

                self.x_min = -min(self.x_max, dp.x)
                self.y_min = -min(self.y_max, dp.y)

            elif self.max.active:
                pos_3d = self.get_pos3d(context)
                dp = self.itM @ pos_3d
                self.x_max = max(-self.x_min, dp.x)
                self.y_max = max(-self.y_min, dp.y)

            elif self.z_handle.active:
                pos_3d = self.get_pos3d(context, normal=Vector((1, 0, 0)))
                dp = self.itM @ pos_3d
                self.z_max = min(self.bound_z, dp.z)

            elif self.xsoft.active:
                pos_3d = self.get_pos3d(context)
                dp = self.itM @ pos_3d
                dx = self.x_max + self.x_min
                print("dx", dx, "xmax", self.x_max, "xmin", self.x_min)
                print("dp.x", dp.x)
                if dx > 0:
                    self.x_soft = 200 * (dp.x + self.x_min) / dx
                print("x_soft", self.x_soft)

            elif self.ysoft.active:
                pos_3d = self.get_pos3d(context)
                dp = self.itM @ pos_3d
                dy = self.y_max + self.y_min
                if dy > 0:
                    self.y_soft = 200 * (dp.y + self.y_min) / dy
                print(self.y_soft)

            elif self.zsoft.active:
                pos_3d = self.get_pos3d(context, normal=Vector((1, 0, 0)))
                dp = self.itM @ pos_3d
                dz = self.bound_z - self.z_max
                if dz > 0:
                    self.z_soft = 100 * (dp.z - self.z_max) / dz
                print(self.z_soft)
        else:
            self.check_hover()

    def mouse_press(self, context, event):
        if self.min.hover:

            self.active = True
            self.min.active = True
            takemat = self.tM @ Matrix.Translation(Vector((-self.x_min, -self.y_min, 0)))
            snap_point(takemat=takemat,
                       callback=self.sp_callback,
                       constraint_axis=(True, True, False))

        elif self.max.hover:
            self.active = True

            self.max.active = True
            takemat = self.tM @ Matrix.Translation(Vector((self.x_max, self.y_max, 0)))
            snap_point(takemat=takemat,
                       callback=self.sp_callback,
                       constraint_axis=(True, True, False))

        elif self.z_handle.hover:
            self.active = True
            self.z_handle.active = True

        elif self.xsoft.hover:
            self.active = True
            self.xsoft.active = True

        elif self.ysoft.hover:
            self.active = True
            self.ysoft.active = True

        elif self.zsoft.hover:
            self.active = True
            self.zsoft.active = True

    def get_pos3d(self, context, normal=Vector((0, 0, 1))):
        """
            convert mouse pos to 3d point over plane defined by origin and normal
            pt is in world space
        """
        region = context.region
        rv3d = context.region_data
        view_vector_mouse = view3d_utils.region_2d_to_vector_3d(region, rv3d, self.mouse_pos)
        ray_origin_mouse = view3d_utils.region_2d_to_origin_3d(region, rv3d, self.mouse_pos)
        pt = intersect_line_plane(ray_origin_mouse, ray_origin_mouse + view_vector_mouse,
            self.origin, normal, False)
        # fix issue with parallel plane
        if pt is None:
            pt = intersect_line_plane(ray_origin_mouse, ray_origin_mouse + view_vector_mouse,
                self.origin, view_vector_mouse, False)
        return pt

    def modal(self, context, event):

        context.area.tag_redraw()

        if event.type == 'MOUSEMOVE':
            self.mouse_move(context, event)

        elif event.value == 'PRESS':

            if event.type == 'LEFTMOUSE':
                self.mouse_press(context, event)

            elif event.type in {'ESC', 'RIGHTMOUSE'}:
                if self.active:
                    self.mouse_release(context, event)
                self.create(context)
                bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')
                return {'FINISHED'}

        elif event.value == 'RELEASE':

            if event.type == 'LEFTMOUSE':
                self.mouse_release(context, event)

        if self.active:
            return {'RUNNING_MODAL'}

        return {'PASS_THROUGH'}

    def draw_handler(self, _self, context):
        #  p3     p2
        #  p0     p1
        #
        p0 = self.tM @ Vector((-self.x_min, -self.y_min, 0))
        p1 = self.tM @ Vector((self.x_max, -self.y_min, 0))
        p2 = self.tM @ Vector((self.x_max, self.y_max, 0))
        p3 = self.tM @ Vector((-self.x_min, self.y_max, 0))
        sx = (p1 - p0).normalized()
        sy = (p3 - p0).normalized()
        vx = sx * self.bound_x
        vy = sy * self.bound_y
        pz = self.tM @ Vector((0, 0, self.z_max))
        sz = self.tM.col[2].to_3d()

        # pa = self.tM @ Vector((0, 0, -self.z_min))
        self.rect.set_pos([p0, p1, p2, p3])

        self.min.set_pos(context, p0, xAxis)
        self.max.set_pos(context, p2, xAxis)
        self.z_handle.set_pos(context, pz, zAxis, normal=yAxis)
        # self.a_handle.set_pos(context, pa, zAxis, normal=yAxis)

        self.line_xmin.p = 0.5 * (p0 + p1 - vx)
        self.line_xmax.p = 0.5 * (p2 + p3 - vx)
        self.line_ymin.p = 0.5 * (p0 + p3 - vy)
        self.line_ymax.p = 0.5 * (p1 + p2 - vy)
        self.line_xmin.v = vx
        self.line_xmax.v = vx
        self.line_ymin.v = vy
        self.line_ymax.v = vy
        dx = (self.x_min + self.x_max) * self.x_soft / 200
        dy = (self.y_min + self.y_max) * self.y_soft / 200
        dz = (self.bound_z - self.z_max) * self.z_soft / 100

        self.line_xsoft_min.p = self.line_ymin.p + sx * dx
        self.line_ysoft_min.p = self.line_xmin.p + sy * dy
        self.line_xsoft_max.p = self.line_ymax.p - sx * dx
        self.line_ysoft_max.p = self.line_xmax.p - sy * dy
        self.rect_xmin.set_pos([self.line_xsoft_min.p, self.line_ymin.p, self.line_ymin.p1, self.line_xsoft_min.p1])
        self.rect_xmax.set_pos([self.line_xsoft_max.p, self.line_ymax.p, self.line_ymax.p1, self.line_xsoft_max.p1])
        self.rect_ymin.set_pos([self.line_ysoft_min.p, self.line_xmin.p, self.line_xmin.p1, self.line_ysoft_min.p1])
        self.rect_ymax.set_pos([self.line_ysoft_max.p, self.line_xmax.p, self.line_xmax.p1, self.line_ysoft_max.p1])

        self.line_xsoft_min.v = vy
        self.line_ysoft_min.v = vx
        self.line_xsoft_max.v = vy
        self.line_ysoft_max.v = vx

        self.xsoft.set_pos(context, 0.5 * (p0 + p3) + sx * dx, xAxis)
        self.ysoft.set_pos(context, 0.5 * (p0 + p1) + sy * dy, yAxis)
        self.zsoft.set_pos(context, pz + sz * dz, zAxis, normal=yAxis)

        self.line_zmax.p = pz - 0.5 * vx
        self.line_zsoft.p = self.line_zmax.p + sz * dz
        self.line_zmax.v = vx
        self.line_zsoft.v = vx
        self.rect_zmax.set_pos([self.line_zmax.p, self.line_zmax.p1, self.line_zsoft.p1, self.line_zsoft.p])
        self.rect.draw(context)

        self.text_xmin.set_pos(context, None, self.line_ymin.sized_normal(0.5, -0.5).p1, xAxis)
        self.text_xmax.set_pos(context, None, self.line_ymax.sized_normal(0.5, 0.5).p1, xAxis)
        self.text_ymin.set_pos(context, None, self.line_xmin.sized_normal(0.5, 0.5).p1, xAxis)
        self.text_ymax.set_pos(context, None, self.line_xmax.sized_normal(0.5, -0.5).p1, xAxis)
        self.text_z.set_pos(context, None, self.line_zmax.sized_normal(0.5, 0.1).p1, xAxis, normal=yAxis)
        # self.text_a.set_pos(context, None, self.line_zmin.sized_normal(0.5, 0.1).p1, xAxis, normal=yAxis)

        self.line_xmin.draw(context)
        self.line_xmax.draw(context)
        self.line_ymin.draw(context)
        self.line_ymax.draw(context)
        # self.line_zmin.draw(context)
        self.line_zmax.draw(context)

        self.line_xsoft_min.draw(context)
        self.line_ysoft_min.draw(context)
        self.line_xsoft_max.draw(context)
        self.line_ysoft_max.draw(context)
        self.line_zsoft.draw(context)

        self.rect_xmin.draw(context)
        self.rect_xmax.draw(context)
        self.rect_ymin.draw(context)
        self.rect_ymax.draw(context)
        self.rect_zmax.draw(context)

        self.xsoft.draw(context)
        self.ysoft.draw(context)
        self.zsoft.draw(context)

        self.text_xmin.draw(context)
        self.text_xmax.draw(context)
        self.text_ymin.draw(context)
        self.text_ymax.draw(context)
        self.text_z.draw(context)
        # self.text_a.draw(context)

        self.min.draw(context)
        self.max.draw(context)
        self.z_handle.draw(context)
        # self.a_handle.draw(context)

    def invoke(self, context, event):
        o = context.active_object
        try:
            o.rotation_euler = (0, 0, 0)
        except:
            pass

        # bpy.ops.archipack.disable_manipulate()
        # context.view_layer.update()
        print("invoke")
        p0 = Vector(o.bound_box[0])
        p1 = Vector(o.bound_box[7])
        bound = p0 + p1
        x, y, z = 0.5 * o.dimensions

        self.bound_x = 10 * x
        self.bound_y = 10 * y
        self.bound_z = 2 * z

        self.x, self.y, self.z = x, y, z
        self.x_min = x
        self.x_max = x
        self.y_min = y
        self.y_max = y
        self.z_max = z

        sensor_size = 10
        size = 0.05
        self.active = False
        self.pmin = o.matrix_world @ p0
        self.origin = o.matrix_world @ (0.5 * bound)

        tM = o.matrix_world @ Matrix.Translation(0.5 * bound)
        self.tM = tM
        self.itM = tM.inverted()

        self.mouse_pos = Vector((0, 0))
        self.rect = GlPolygon(d=3, colour=(1, 1, 1, 0.2))
        self.min = SquareHandle(sensor_size, size, draggable=True)
        self.max = SquareHandle(sensor_size, size, draggable=True)
        self.z_handle = SquareHandle(sensor_size, size, draggable=True)

        self.xsoft = TriHandle(sensor_size, size, draggable=True)
        self.ysoft = TriHandle(sensor_size, size, draggable=True)
        self.zsoft = TriHandle(sensor_size, size, draggable=True)

        self.line_xmin = GlLine()
        self.line_xmax = GlLine()
        self.line_ymin = GlLine()
        self.line_ymax = GlLine()
        self.line_zmax = GlLine()

        self.line_xmin.colour_inactive = (1, 0, 0, 1)
        self.line_xmax.colour_inactive = (1, 0, 0, 1)
        self.line_ymin.colour_inactive = (0, 1, 0, 1)
        self.line_ymax.colour_inactive = (0, 1, 0, 1)
        self.line_zmax.colour_inactive = (0, 0, 1, 1)

        self.rect_xmin = GlPolygon(d=3, colour=(0, 1, 0, 0.2))
        self.rect_xmax = GlPolygon(d=3, colour=(0, 1, 0, 0.2))
        self.rect_ymin = GlPolygon(d=3, colour=(1, 0, 0, 0.2))
        self.rect_ymax = GlPolygon(d=3, colour=(1, 0, 0, 0.2))
        # self.rect = GlPolygon(d=3, colour=(1, 1, 1, 0.2))
        self.rect_zmax = GlPolygon(d=3, colour=(0, 0, 1, 0.2))

        self.line_xsoft_min = GlLine()
        self.line_xsoft_max = GlLine()
        self.line_ysoft_min = GlLine()
        self.line_ysoft_max = GlLine()
        self.line_zsoft = GlLine()

        self.line_xsoft_min.colour_inactive = (0, 1, 0, 1)
        self.line_xsoft_max.colour_inactive = (0, 1, 0, 1)
        self.line_ysoft_min.colour_inactive = (1, 0, 0, 1)
        self.line_ysoft_max.colour_inactive = (1, 0, 0, 1)
        self.line_zsoft.colour_inactive = (0, 0, 1, 1)

        self.line_xmin.colour_inactive = (1, 0, 0, 1)
        self.line_xmax.colour_inactive = (1, 0, 0, 1)
        self.line_ymin.colour_inactive = (0, 1, 0, 1)
        self.line_ymax.colour_inactive = (0, 1, 0, 1)
        self.line_zmax.colour_inactive = (0, 0, 1, 1)


        self.text_xmin = GlText(label="Left", colour=(0, 1, 0, 1), font_size=16)
        self.text_xmax = GlText(label="Right", colour=(0, 1, 0, 1), font_size=16)
        self.text_ymin = GlText(label="Outside", colour=(1, 0, 0, 1), font_size=16)
        self.text_ymax = GlText(label="Inside", colour=(1, 0, 0, 1), font_size=16)
        self.text_z = GlText(label="Top", colour=(0, 0, 1, 1), font_size=16, z_axis=yAxis)

        args = (self, context)
        self._handle = bpy.types.SpaceView3D.draw_handler_add(self.draw_handler,
                args, 'WINDOW', 'POST_PIXEL')

        context.window_manager.modal_handler_add(self)
        return {'RUNNING_MODAL'}

    def execute(self, context):
        print("execute")
        with stop_auto_manipulate(context):
            self.create(context)
        print("execute select object")
        #    self.select_object(context, o, True)
        return {'FINISHED'}


class ARCHIPACK_OT_custom_manipulators(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.custom_manipulators"
    bl_label = "Manipulators location"
    bl_description = "Define location of manipulators"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    x: FloatProperty(
        name="x",
        min=0,
        precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )

    y: FloatProperty(
        name="y",
        min=0,
        precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )

    z: FloatProperty(
        name="z",
        min=0,
        precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )
    altitude: FloatProperty(
        name="Altitude",
        min=0,
        precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )

    @classmethod
    def poll(cls, context):
        return archipack_custom.poll(context.active_object)

    def draw(self, context):
        layout = self.layout
        layout.prop(self,"x", text_ctxt="archipack")
        layout.prop(self,"y", text_ctxt="archipack")
        layout.prop(self,"z", text_ctxt="archipack")
        layout.prop(self,"altitude", text_ctxt="archipack")

    def clear_parent_inverse(self, tM, itM, o):
        """
          Set matrix_parent_inverse to identity
          keeping visual transforms
          so .location of child is in parent coordsys
        """
        loc = itM @ o.matrix_world.translation
        o.matrix_world = tM.copy()
        o.location = loc

    def create(self, context):
        o = context.object
        d = archipack_custom.datablock(o)
        if d is not None:
            d.auto_update = False
            d.x_cur, d.y_cur, d.z_cur = 2 * self.x, 2 * self.y, self.z
            d.altitude_cur = self.altitude
            d.auto_update = True

    def check_hover(self):
        self.x_handle.check_hover(self.mouse_pos)
        self.y_handle.check_hover(self.mouse_pos)
        self.z_handle.check_hover(self.mouse_pos)
        self.a_handle.check_hover(self.mouse_pos)

    def mouse_release(self, context, event):
        self.active = False
        self.check_hover()
        self.x_handle.active = False
        self.y_handle.active = False
        self.z_handle.active = False
        self.a_handle.active = False
        self.create(context)
        return False

    def mouse_move(self, context, event):
        self.mouse_pos = Vector((event.mouse_region_x, event.mouse_region_y))
        if self.active:

            if self.z_handle.active:
                pos_3d = self.get_pos3d(context, normal=Vector((1, 0, 0)))
                dp = self.itM @ pos_3d
                self.z = abs(dp.z) - self.altitude

            if self.a_handle.active:
                pos_3d = self.get_pos3d(context, normal=Vector((1, 0, 0)))
                dp = self.itM @ pos_3d
                self.altitude = abs(dp.z)

        else:
            self.check_hover()

    def sp_callback(self, context, event, state, sp):

        if state != 'CANCEL':
            if self.x_handle.active or self.y_handle.active:

                x, y, z = self.itM @ sp.placeloc
                self.x = abs(x)
                self.y = abs(y)
                self.create(context)

        if state != 'RUNNING':
            self.mouse_release(context, event)

    def mouse_press(self, context, event):
        if self.x_handle.hover:

            self.active = True
            self.x_handle.active = True
            takemat = self.tM @ Matrix.Translation(Vector((self.x, -self.y, 0)))
            snap_point(takemat=takemat,
                       callback=self.sp_callback,
                       constraint_axis=(True, True, False))

        elif self.y_handle.hover:
            self.active = True

            self.y_handle.active = True
            takemat = self.tM @ Matrix.Translation(Vector((-self.x, self.y, 0)))
            snap_point(takemat=takemat,
                       callback=self.sp_callback,
                       constraint_axis=(True, True, False))

        elif self.z_handle.hover:
            self.active = True
            self.z_handle.active = True

        elif self.a_handle.hover:
            self.active = True
            self.a_handle.active = True

    def get_pos3d(self, context, normal=Vector((0, 0, 1))):
        """
            convert mouse pos to 3d point over plane defined by origin and normal
            pt is in world space
        """
        region = context.region
        rv3d = context.region_data
        view_vector_mouse = view3d_utils.region_2d_to_vector_3d(region, rv3d, self.mouse_pos)
        ray_origin_mouse = view3d_utils.region_2d_to_origin_3d(region, rv3d, self.mouse_pos)
        pt = intersect_line_plane(ray_origin_mouse, ray_origin_mouse + view_vector_mouse,
            self.origin, normal, False)
        # fix issue with parallel plane
        if pt is None:
            pt = intersect_line_plane(ray_origin_mouse, ray_origin_mouse + view_vector_mouse,
                self.origin, view_vector_mouse, False)
        return pt

    def modal(self, context, event):

        context.area.tag_redraw()

        if event.type == 'MOUSEMOVE':
            self.mouse_move(context, event)

        elif event.value == 'PRESS':

            if event.type == 'LEFTMOUSE':
                self.mouse_press(context, event)

            elif event.type in {'ESC', 'RIGHTMOUSE'}:
                if self.active:
                    self.mouse_release(context, event)
                self.create(context)
                bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')
                return {'FINISHED'}

        elif event.value == 'RELEASE':

            if event.type == 'LEFTMOUSE':
                self.mouse_release(context, event)

        if self.active:
            return {'RUNNING_MODAL'}

        return {'PASS_THROUGH'}

    def draw_handler(self, _self, context):
        # p6   p2     p1     p5
        # p7   p3     p0     p4
        p0 = self.tM @ Vector((self.x, -self.y, 0))
        p1 = self.tM @ Vector((self.x, self.y, 0))
        p2 = self.tM @ Vector((-self.x, self.y, 0))
        p3 = self.tM @ Vector((-self.x, -self.y, 0))
        p4 = self.tM @ Vector((4 * self.x, -self.y, 0))
        p5 = self.tM @ Vector((4 * self.x, self.y, 0))
        p6 = self.tM @ Vector((-4 * self.x, self.y, 0))
        p7 = self.tM @ Vector((-4 * self.x, -self.y, 0))
        vz = Vector((0, 0, self.altitude + self.z))
        pz = self.tM @ vz
        pa = self.tM @ Vector((0, 0, self.altitude))

        self.wall_r.set_pos([p4, p5, p1, p0])
        self.wall_l.set_pos([p3, p2, p6, p7])

        self.x_handle.set_pos(context, p0, xAxis)
        self.y_handle.set_pos(context, p2, xAxis)
        self.z_handle.set_pos(context, pz, zAxis, normal=yAxis)
        self.a_handle.set_pos(context, pa, zAxis, normal=yAxis)
        vx, vy = p4 - p7, p1 - p0
        self.line_xmin.p = p7
        self.line_xmin.v = vx
        self.line_xmax.p = p6
        self.line_xmax.v = vx
        self.line_ymin.p = p0
        self.line_ymin.v = vy
        self.line_ymax.p = p3
        self.line_ymax.v = vy
        self.line_zmin.p = pa - 0.5 * vx
        self.line_zmax.p = pz - 0.5 * vx
        self.line_zmin.v = vx
        self.line_zmax.v = vx

        self.wall_l.draw(context)
        self.wall_r.draw(context)

        self.text_x.set_pos(context, None, 0.5 * (p2 + p7), xAxis)
        self.text_y.set_pos(context, None, 0.5 * (p0 + p3 - vy), xAxis)
        self.text_z.set_pos(context, None, self.tM @ Vector((0, 0, self.altitude + self.z + 0.1)), xAxis, normal=yAxis)
        self.text_a.set_pos(context, None, self.tM @ Vector((0, 0, self.altitude + 0.1)), xAxis, normal=yAxis)

        self.line_xmin.draw(context)
        self.line_xmax.draw(context)
        self.line_ymin.draw(context)
        self.line_ymax.draw(context)
        self.line_zmin.draw(context)
        self.line_zmax.draw(context)

        self.text_x.draw(context)
        self.text_y.draw(context)
        self.text_z.draw(context)
        self.text_a.draw(context)

        self.x_handle.draw(context)
        self.y_handle.draw(context)
        self.z_handle.draw(context)
        self.a_handle.draw(context)

    def invoke(self, context, event):
        o = context.active_object

        d = archipack_custom.datablock(o)

        if d is None:
            d = o.data.archipack_custom.add()

        if archipack_custom_part.filter(o):
            o.data.archipack_custom_part.remove(0)

        tM = o.matrix_world

        self.tM = tM
        self.itM = tM.inverted()

        x, y, z = o.dimensions
        self.x, self.y, self.z = 0.5 * x, 0.5 * y, z
        self.altitude = o.bound_box[0][2]

        sensor_size = 10
        size = 0.05
        self.active = False

        self.origin = tM.translation
        self.mouse_pos = Vector((0, 0))
        self.wall_l = GlPolygon(d=3, colour=(1, 0, 0, 0.2))
        self.wall_r = GlPolygon(d=3, colour=(1, 0, 0, 0.2))

        self.x_handle = SquareHandle(sensor_size, size, draggable=True)
        self.y_handle = SquareHandle(sensor_size, size, draggable=True)
        self.z_handle = SquareHandle(sensor_size, size, draggable=True)
        self.a_handle = SquareHandle(sensor_size, size, draggable=True)

        self.line_xmin = GlLine()
        self.line_xmax = GlLine()
        self.line_ymin = GlLine()
        self.line_ymax = GlLine()
        self.line_zmin = GlLine()
        self.line_zmax = GlLine()

        self.line_xmin.colour_inactive = (1, 0, 0, 1)
        self.line_xmax.colour_inactive = (1, 0, 0, 1)
        self.line_ymin.colour_inactive = (0, 1, 0, 1)
        self.line_ymax.colour_inactive = (0, 1, 0, 1)
        self.line_zmin.colour_inactive = (0, 0, 1, 1)
        self.line_zmax.colour_inactive = (0, 0, 1, 1)

        self.text_x = GlText(label="Wall", colour=(1, 0, 0, 1), font_size=16)
        self.text_y = GlText(label="Outside", colour=(0, 1, 0, 1), font_size=16)
        self.text_z = GlText(label="Top", colour=(0, 0, 1, 1), font_size=16)
        self.text_a = GlText(label="Altitude", colour=(0, 0, 1, 1), font_size=16)

        args = (self, context)
        self._handle = bpy.types.SpaceView3D.draw_handler_add(self.draw_handler,
                args, 'WINDOW', 'POST_PIXEL')

        context.window_manager.modal_handler_add(self)
        return {'RUNNING_MODAL'}

    def execute(self, context):
        self.create(context)
        return {'FINISHED'}


class ARCHIPACK_OT_custom_draw(ArchipackDrawTool, Operator):
    bl_idname = "archipack.custom_draw"
    bl_label = "Draw Custom"
    bl_description = "Draw Custom object over walls"

    filepath: StringProperty(default="")
    feedback = None
    stack = []
    object_name = ""

    @classmethod
    def poll(cls, context):
        return archipack_custom.poll(context.active_object)

    def draw_callback(self, _self, context):
        self.feedback.draw(context)

    def add_object(self, context, event):
        o = context.active_object
        bpy.ops.object.select_all(action="DESELECT")
        o = self.duplicate_object(context, o, False)
        self.object_name = o.name
        self.select_object(context, o, True)
        return o

    def remove_object(self, context, o):
        if archipack_custom.filter(o):
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

    def modal(self, context, event):

        context.area.tag_redraw()
        o = self.get_scene_object(context, self.object_name)

        if o is None:
            self.exit(context, None)
            return {'FINISHED'}

        d = archipack_custom.datablock(o)

        # hide hole from raycast
        to_hide = [o]
        to_hide.extend(list(o.children))

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

            if event.type in {'LEFTMOUSE', 'RET', 'NUMPAD_ENTER', 'SPACE'}:
                if wall is not None:

                    if d is not None:
                        # o must be a custom here
                        if d.find_hole(o) is not None:
                            ctx = context.copy()
                            ctx['object'] = wall
                            ctx['selected_objects'] = [o]
                            bpy.ops.archipack.single_boolean(ctx)

                        # Ensure custom is child of reference point when there is no hole
                        elif o.parent is None:
                            ref = self.get_reference_point(wall)
                            if ref is not None:
                                loc = ref.matrix_world.inverted() @ o.matrix_world.translation
                                o.parent = ref
                                o.location = loc

                        if len(self.stack) > 0 and not event.shift:
                            last = self.stack[-1]
                            d_last = last.data.archipack_custom[0]
                            if d_last.y == d.y:
                                # Must disable manipulators before link !!
                                bpy.ops.archipack.disable_manipulate()
                                self.link_object(last, o)

                        if "archipack_wall2" in wall.data:
                            # link as wall child
                            with ensure_select_and_restore(context, wall, [wall]):
                                wd = wall.data.archipack_wall2[0]
                                wg = wd.get_generator()
                                wd.setup_childs(context, wall, g=wg, openings_only=True)
                                wd.relocate_childs(context, wall, g=wg)
                                wd.update_dimension(context, wall, wg)

                        self.stack.append(o)
                        self.select_object(context, o, True)
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
            o = None
            self.stack = []
            self.keymap = Keymaps(context)
            # exit manipulate_mode if any
            bpy.ops.archipack.disable_manipulate()
            # Hide manipulators
            context.space_data.show_gizmo = False
            # invoke with shift pressed will use current object as basis for linked copy
            o = context.active_object
            # context.scene.objects.active = None
            bpy.ops.object.select_all(action="DESELECT")
            self.select_object(context, o, True)
            self.add_object(context, event)

            self.feedback = FeedbackPanel()
            self.feedback.instructions(context, "Draw a custom", "Click & Drag over a wall", [
                ('LEFTCLICK, RET, SPACE, ENTER', 'Create a custom'),
                ('BACKSPACE, CTRL+Z', 'undo last'),
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


class ARCHIPACK_OT_custom_array(ArchipackCreateTool, Operator):
    bl_idname = "archipack.custom_array"
    bl_label = "Custom Array"
    bl_description = "Duplicate selected customs"
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
    limit_count: BoolProperty(
        name="Limit count",
        default=True
    )

    def make_array(self, context, sel):
        startplace, spacing, count = self.distribute_objects()
        for o in sel:
            d = archipack_custom.datablock(o)
            if d is not None:
                # find walls this hole belongs
                hole = d.find_hole(o)
                ref = self.get_reference_point(o)
                # get custom wall, wall and wall2
                objs = self.filter_selection_loose(ref.children)
                walls_holes = set()
                for key, walls in objs.items():
                    print(key)
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
        sel = [c for c in context.selected_objects if archipack_custom.filter(c)]
        d = archipack_custom.datablock(o)
        # find wall segment for the active object
        wall, wd, wg, child = self.find_wall(context, o)
        if wall is not None:

            # fails when custom is not cutting wall at all

            _seg_idx = child.wall_idx
            _seg = wg.outside.segs[_seg_idx]
            childs = []
            childs.append((o, child.pos.x, d.x))
            for c in sel:
                # ensure other objects are on same segment
                if c.name != o.name:
                    child = self.find_wall_child(wd, c)
                    if child.wall_idx == _seg_idx:
                        d = archipack_custom.datablock(c)
                        t = child.pos.x
                        childs.append((c, t, d.x))
            # sort by distance from start
            childs.sort(key=lambda x: x[1])

            # how much we should offset so the first one border is at wall segment start
            self.first_location = (0.5 * childs[0][2]) - childs[0][1]
            # max distance between childs border - border
            self.objects_size = childs[0][1] - childs[-1][1] + 0.5 * (childs[0][2] + childs[-1][2])
            self.available_space = _seg.length
            return context.window_manager.invoke_props_dialog(self)
        else:
            self.report({'WARNING'}, "Must draw custom over wall before using array")
        return {'CANCELLED'}


def register():
    bpy.utils.register_class(archipack_custom_part)
    bpy.utils.register_class(archipack_custom)
    Mesh.archipack_custom = CollectionProperty(type=archipack_custom)
    Mesh.archipack_custom_part = CollectionProperty(type=archipack_custom_part)
    bpy.utils.register_class(ARCHIPACK_PT_custom)
    bpy.utils.register_class(ARCHIPACK_PT_custom_part)
    bpy.utils.register_class(ARCHIPACK_OT_custom_draw)
    bpy.utils.register_class(ARCHIPACK_OT_custom_array)
    bpy.utils.register_class(ARCHIPACK_OT_make_custom)
    bpy.utils.register_class(ARCHIPACK_OT_custom_manipulators)


def unregister():
    bpy.utils.unregister_class(archipack_custom)
    bpy.utils.unregister_class(archipack_custom_part)
    bpy.utils.unregister_class(ARCHIPACK_PT_custom)
    bpy.utils.unregister_class(ARCHIPACK_PT_custom_part)
    bpy.utils.unregister_class(ARCHIPACK_OT_custom_draw)
    bpy.utils.unregister_class(ARCHIPACK_OT_custom_array)
    bpy.utils.unregister_class(ARCHIPACK_OT_make_custom)
    bpy.utils.unregister_class(ARCHIPACK_OT_custom_manipulators)
    del Mesh.archipack_custom
    del Mesh.archipack_custom_part
