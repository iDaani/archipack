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
from math import pi, atan2, cos, sin
from mathutils import Vector, Matrix
import json
import bpy
from bpy.types import Operator
from bpy.props import (
    FloatProperty, IntProperty, BoolProperty, IntVectorProperty,
    EnumProperty
)
from .archipack_generator import Generator, Arc, Line
from .archipack_i18n import Archipacki18n
from .archipack_object import ArchipackGenericOperator
from .archipack_material import build_mat_enum
import logging

logger = logging.getLogger("archipack")


class FakeGenerator:
    def __init__(self, segs):
        self.segs = segs
        self.valid_segs = []


class OpeningGenerator:
    """
     Provide relocate compatibility
     to handle openings / customs
     the same way as other objects
    """
    def __init__(self, d, o=None, typ="DOOR"):

        if o is None:
            # delta angle between local and world absolute
            tM = Matrix()
        else:
            if type(o).__name__ == "Matrix":
                tM = o
            else:
                tM = o.matrix_world

        x = 0.5 * d.x
        x1 = x
        y = 0.5 * d.y
        y1 = 0
        is_window = typ == "WINDOW"

        if is_window:
            y1 = d.hole_center_y
            x += d.frame_x
        elif typ == 'DOOR':
            y1 = d.hole_center_y
            x1 += d.frame_x
            x += d.frame_y

        #       c    x  x1
        # 0_1          4_5 inside
        #    |_______ |
        #    2        3  y1
        # 6_7|        |8_9 outside
        #
        # both closed so p0 always valid
        p0 = (-x1 , y)
        p1 = (-x, y)
        p2 = (-x, y1)
        p3 = (x, y1)
        p4 = (x, y)
        p5 = (x1, y)
        p6 = (-x1, -y)
        p7 = (-x, -y)
        p8 = (x,  -y)
        p9 = (x1, -y)

        inside, outside = [], []
        if is_window:
            pts = [Vector((x, y, 0)) for x, y in [p1, p2, p3, p4]]
            if len(d.bend) > 0:
                bend = d.bend[:]
                bend.sort(key=lambda x: x.x)
                for b in bend:
                    if b.x < 0:
                        d.soft_segment_2d(pts, b.a, b.x)
                for b in reversed(bend):
                    if b.x > 0:
                        d.soft_segment_2d(pts, b.a, b.x)
        else:
            pts = [Vector((x, y, 0)) for x, y in [p0, p1, p2, p3, p4, p5]]

        last = None
        for pt in pts:
            last = Line(tM @ pt, last=last)
            inside.append(last)
        last._next = inside[0]
        inside[0]._last = last

        if is_window:
            pts = [Vector((x, y, 0)) for x, y in [p7, p2, p3, p8]]
            if len(d.bend) > 0:
                bend = d.bend[:]
                bend.sort(key=lambda x: x.x)
                for b in bend:
                    if b.x < 0:
                        d.soft_segment_2d(pts, b.a, b.x)
                for b in reversed(bend):
                    if b.x > 0:
                        d.soft_segment_2d(pts, b.a, b.x)
        else:
            pts = [Vector((x, y, 0)) for x, y in [p6, p7, p2, p3, p8, p9]]

        last = None
        for pt in pts:
            last = Line(tM @ pt, last=last)
            outside.append(last)
        last._next = outside[0]
        outside[0]._last = last

        self.outside = FakeGenerator(outside)
        self.inside = FakeGenerator(inside)
        self.axis = FakeGenerator([])

    def get_verts(self, verts: list, segs: list) -> None:
        """Fill in given list with vertices coordinates
        :param verts: list to fill
        :return:
        """
        for _k in segs:
            _k.pts(verts)

        # if self.closed:
        #    verts.append(segs[0]._p0)

    def make_curve(self, context, pts, name):
        curve = bpy.data.curves.new("debug_%s_" % name, type='CURVE')
        curve.dimensions = '2D'
        spline = curve.splines.new('POLY')
        spline.use_endpoint_u = False
        spline.use_cyclic_u = True
        spline.points.add(len(pts) - 1)
        for i, p in enumerate(pts):
            spline.points[i].co = p.to_4d()
        curve_obj = bpy.data.objects.new("debug_%s_" % name, curve)
        context.scene.collection.objects.link(curve_obj)

    def as_curve(self, context=None, name="debug"):
        """Build a blender curve from data
        :param context: Blender context
        return: Blender curve object
        """
        if context is None:
            context = bpy.context
        pts = []
        self.get_verts(pts, self.inside.segs)
        self.make_curve(context, pts, "inside_%s" % name)
        pts = []
        self.get_verts(pts, self.outside.segs)
        self.make_curve(context, pts, "outside_%s" % name)


class ChangedPart:
    """
    Fake part to store last state on change
    """
    def __init__(self):
        pass

    def init(self, d, prop):
        # changed property name
        print("ChangedPart.init()", prop)
        self.prop = prop
        self.a0 = d.a0
        self.length = d.length
        self.da = d.da
        self.radius = d.radius
        self.type = d.type

        # store last value
        setattr(self, prop, getattr(d, prop))


def update_change(self, context):
    self.update_change(context)


def update(self, context):
    self.update(context)


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


MAT_PART = 0


material_enums = []
mat_enum, mat_index_getter, mat_index_setter= build_mat_enum('idmat', material_enums)


class ArchipackSegment:
    """
        Base class for all archipack's line based objects parts
        wall, fence, slab, floor, cutter, roof

        Use 3 vars for each param (get/setter, last value and current value)
        where:
        - main store current value (saved)
        - last_ store previous value on change (not saved)
        - _ui is getter/setter and trigger auto update

        _ui is ment to be in use in ui and simple manipulators

        main doesnt auto update so use when
        change does affect neighboors segments
        and call update by hand
    """
    change = ChangedPart()
    lock: BoolProperty(
        description="Lock so changes apply till end of line",
        default=False,
        options={'SKIP_SAVE'}
    )

    change_side: EnumProperty(
        description="Side the changes apply, eg: size may change in left or right side of segment",
        items=(
            ('RIGHT', 'Right', 'Right'),
            ('LEFT', 'Left', 'Left')
        ),
        default='RIGHT',
        options={'SKIP_SAVE'}
    )
    material_override: BoolProperty(
        name="Override Material",
        default=False,
        update=update
    )
    idmat: IntVectorProperty(
        default=[0, 1],
        size=2
    )
    material: EnumProperty(
        options={'SKIP_SAVE'},
        name="Material",
        items=mat_enum,
        get=mat_index_getter(MAT_PART),
        set=mat_index_setter( MAT_PART),
        update=update
    )

    type: IntProperty(
        default=0
    )
    type_ui: EnumProperty(
        items=(
            ('S_SEG', 'Line', 'Linear segment', 0),
            ('C_SEG', 'Arc', 'Arc segment', 1),
        ),
        default='S_SEG',
        set=change_setter('type'),
        get=change_getter('type'),
        update=update_change,
        options={'SKIP_SAVE'}
    )

    # Segment length
    length: FloatProperty(
        description="Store segment length",
        min=0.001,
        default=2.0, precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )
    l_ui: FloatProperty(
        name="Length",
        description="Length of segment",
        min=0.001,
        default=2.0, precision=5,
        set=change_setter("length"),
        get=change_getter("length"),
        update=update_change,
        unit='LENGTH', subtype='DISTANCE',
        options={'SKIP_SAVE'}
    )

    # Curved parts
    radius: FloatProperty(
        description="Store segment radius",
        min=0.001,
        default=0.7, precision=5,
        unit='LENGTH', subtype='DISTANCE'
    )
    r_ui: FloatProperty(
        name="Radius",
        description="Radius of curved segments",
        min=0.001,
        default=0.7, precision=5,
        update=update_change,
        set=change_setter("radius"),
        get=change_getter("radius"),
        unit='LENGTH', subtype='DISTANCE',
        options={'SKIP_SAVE'}
    )
    da: FloatProperty(
        description="Store angle of curved segments",
        min=-pi,
        max=pi,
        default=pi / 2, precision=4,
        subtype='ANGLE', unit='ROTATION'
    )
    da_ui: FloatProperty(
        name="Angle",
        description="Angle between segments",
        min=-pi,
        max=pi,
        default=pi / 2, precision=4,
        subtype='ANGLE', unit='ROTATION',
        set=change_setter("da"),
        get=change_getter("da"),
        update=update_change,
        options={'SKIP_SAVE'}
    )

    a0: FloatProperty(
        description="Store angle between segments",
        min=-pi,
        max=pi,
        default=0, precision=4,
        subtype='ANGLE', unit='ROTATION'
    )
    a_ui: FloatProperty(
        name="Start angle",
        min=-pi,
        max=pi,
        default=0, precision=4,
        subtype='ANGLE', unit='ROTATION',
        set=change_setter("a0"),
        get=change_getter("a0"),
        update=update_change,
        options={'SKIP_SAVE'}
    )

    # Lateral offset
    offset: FloatProperty(
        name="Offset",
        description="Side offset of segment",
        default=0, precision=5,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    expand: BoolProperty(
        name="Seg",
        description="Expand Segment settings",
        options={'SKIP_SAVE'},
        default=False
    )

    @property
    def parent_data(self):
        raise NotImplementedError

    @property
    def auto_update(self):
        return self.parent_data.auto_update

    # @property
    # def is_updating(self):
    #    return self.parent_data.is_updating

    # DimensionProvider
    uid: IntProperty(default=-1)

    def id_mat(self, index):
        _idx = self.idmat[index]
        if _idx < len(self.id_data.materials):
            return _idx
        return 0

    def _add_parts(self, g, d, change_idx: int) -> None:
        """Init a generator from archipack_segments datablock
        :param d: archipack_segments derived datablock
        :param change_idx: index of changed segment
        :return:
        """
        _k = None
        _co = g.location.copy()
        _rM = g.rot
        _type = 'NONE'

        for idx, part in enumerate(d.parts):
            if change_idx == idx:
                _part = part.__class__.change
            else:
                _part = part
            _k, _co, _rM, _type = g._add_part(d, _part, _k, _co, _rM, _type)

        g.close(d.is_closed)

    def update_change(self, context):
        """
         Update neighboor segments on change
        """
        o = context.object
        idx = -1
        d = self.parent_data

        for i, part in enumerate(d.parts):
            if part == self:
                idx = i
                break

        # flag to prevent update when limit are reached
        res = True

        # Init generator in last state (state without current changes)
        g = Generator()
        self._add_parts(g, d, idx)

        manipulable_refresh = False

        _change = self.__class__.change
        _side = self.change_side
        _prop = _change.prop
        # new value
        value = getattr(self, _prop)
        # last value
        last = getattr(_change, _prop)
        logger.debug("ArchipackSegment.update_change changed: %s %s %s => %s" % (_side, _prop, last, value))

        # apply changes to segs
        if _prop == "length":
            g.move(idx, value, _side, self.lock)

        elif _prop == "a0":
            g.rotate(idx, value - last, _side, self.lock)

        elif _prop == "radius":
            # TODO: handle left and right radius changes
            g.segs[idx].radius_right(value)

        elif _prop == "da":
            # TODO: handle left and right da changes

            g.segs[idx].da = value
            # g.da(idx, value)

        elif _prop == "type":
            if last != value:

                manipulable_refresh = True

                _k = g.segs[idx]

                if value == 0:
                    s0 = Line(_k._p0, last=_k._last, after=_k._next)
                else:
                    s0 = Arc(_k._p0, 0.5 * _k.v_length, pi, last=_k._last, after=_k._next)

                g.segs[idx] = s0

        if hasattr(d, "snap_baseline"):
            d.snap_baseline(o, g)

        # Reset origin is in local coordsys, use only rotation part to ensure stability
        # over loops as setting location does not update matrix_world in the between
        # loc, rot, scale = o.matrix_world.decompose()
        # p = o.location + rot.to_matrix() @ g.reset_origin()
        p = o.matrix_world @ g.reset_origin()
        # update matrix by hand
        d.move_object(o, p)
        g.update_parts(d)

        self.update(context, manipulable_refresh)

    def update(self, context, manipulable_refresh=False):

        # Reset change side
        self.change_side = 'RIGHT'

        if self.auto_update:
            self.parent_data.update(context, manipulable_refresh=manipulable_refresh)

    def draw_insert(self, context, layout, index, closed):
        """
            May implement draw for insert / remove segment operators
        """
        row = layout.row(align=True)
        self.draw_op(context, layout, row, "archipack.segment_insert", text="", icon="ADD").index = index
        self.draw_op(context, layout, row, "archipack.segment_remove", text="", icon="REMOVE").index = index
        if closed:
            self.draw_op(context, layout, row, "archipack.segment_make_first", text="Make First").index = index

    def draw_material(self, context, layout, index):
        box = layout.box()
        self.draw_prop(context, layout, box, self, "material_override", text="Override seg", postfix=str(index + 1), toggle=True)
        if self.material_override:
            self.draw_prop(context, layout, box, self, "material", text="Mat")

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

            if self.type == 1:
                self.draw_prop(context, layout, layout, self, "r_ui")
                self.draw_prop(context, layout, layout, self, "da_ui")
            else:
                self.draw_prop(context, layout, layout, self, "l_ui")
            self.draw_prop(context, layout, layout, self, "a_ui")


# ------------------------------------------------------------------
# Define operator class to manage parts
# ------------------------------------------------------------------


class ARCHIPACK_OT_segment_insert(ArchipackGenericOperator, Operator):
    bl_idname = "archipack.segment_insert"
    bl_label = "Insert"
    bl_description = "Insert segment splitting current in 2 equal parts"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    index: IntProperty(default=0)

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            d = self.datablock(o)
            if d is None:
                return {'CANCELLED'}
            d.insert_part(context, o, self.index)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_segment_remove(ArchipackGenericOperator, Operator):
    bl_idname = "archipack.segment_remove"
    bl_label = "Remove"
    bl_description = "Remove segment merging with previous one"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    index: IntProperty(default=0)

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            d = self.datablock(o)
            if d is None:
                return {'CANCELLED'}
            d.remove_part(context, o, self.index)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_segment_make_first(ArchipackGenericOperator, Operator):
    bl_idname = "archipack.segment_make_first"
    bl_label = "Make first"
    bl_description = "Make this segment first one"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    index: IntProperty(default=0)

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            d = self.datablock(o)
            if d is None:
                return {'CANCELLED'}
            d.make_first(context, o, self.index)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


def register():
    bpy.utils.register_class(ARCHIPACK_OT_segment_insert)
    bpy.utils.register_class(ARCHIPACK_OT_segment_remove)
    bpy.utils.register_class(ARCHIPACK_OT_segment_make_first)


def unregister():
    bpy.utils.unregister_class(ARCHIPACK_OT_segment_insert)
    bpy.utils.unregister_class(ARCHIPACK_OT_segment_remove)
    bpy.utils.unregister_class(ARCHIPACK_OT_segment_make_first)
