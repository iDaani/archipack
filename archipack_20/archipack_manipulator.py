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
import bpy
import json
import functools
from math import atan2, pi, degrees
from mathutils import Vector, Matrix
from mathutils.geometry import intersect_line_plane, intersect_point_line, intersect_line_sphere
from bpy_extras import view3d_utils
from bpy.types import PropertyGroup, Operator, GizmoGroup, Gizmo
from bpy.props import FloatVectorProperty, StringProperty, CollectionProperty, BoolProperty
from bpy.app.handlers import persistent, depsgraph_update_post, load_pre
from .archipack_snap import snap_point
from .archipack_keymaps import Keymaps
from .archipack_prefs import get_prefs
from .archipack_i18n import Archipacki18n
from .archipack_object import ArchipackGenericOperator, ArchipackObjectsManager
from .archipack_gl import (
    GlLine, GlArc, GlText,
    GlPolyline, GlPolygon,
    TriHandle, SquareHandle, EditableText,
    CruxHandle, PlusHandle,
    FeedbackPanel, GlCursorArea, USE_REGION_ITER
)
import logging
import time
import os

logger = logging.getLogger("archipack")

"""
 Change object location when moving 1 point
 When False, change data.origin instead
"""
USE_MOVE_OBJECT = True


# NOTE:
# Snap aware manipulators use a TIMER event
# as transform.translate in use to allow snap
# does catch all events but TIMER one.
# This however has a wanted side effect:
# the manipulator take precedence over already running
# ones, and prevent select mode to start.
#
# TODO:
# Other manipulators should use same technique to take
# precedence over already running ones when active
#
# NOTE:
# Select mode does suffer from this stack effect:
# the last running wins. The point is left mouse select mode
# requiring left drag to be RUNNING_MODAL to prevent real
# objects select and move during manipulators selection.
#
# TODO:
# First run a separate modal dedicated to select mode.
# Selecting in whole manips stack when required
# (manips[key].manipulable.manip_stack)
# Must investigate for a way to handle un-select after drag done.


# Arrow sizes (world units)
arrow_size = 0.05
# Handle area size (pixels)
handle_size = 10


# a global manipulator stack reference
# prevent Blender "ACCESS_VIOLATION" crashes
# use a dict to prevent collisions
# between many objects being in manipulate mode
# use object names as loose keys
# NOTE : use app.drivers to reset before file load


manips = {}


class ThrottleStore:
    callback = None
    draw = None


class ARCHIPACK_OT_manipulate_throttle(Operator):
    """
     Throttle manipulators data update (20x / sec)
     and check for update state so events wont overflow
     as simple modal do
    """
    bl_idname = 'archipack.manipulate_throttle'
    bl_label = 'Throttle manipulator'
    bl_options = {'INTERNAL', 'BLOCKING'}

    _timer = None
    _draw_handler = None

    def modal(self, context, event):

        context.area.tag_redraw()

        if event.type in ('TIMER'):
            ThrottleStore.callback(context, event, 'RUNNING', self)
            return {'PASS_THROUGH'}

        if event.type not in ('ESC', 'RIGHTMOUSE', 'LEFTMOUSE'):
            return {'PASS_THROUGH'}

        print(event.type)

        if event.type in ('ESC', 'RIGHTMOUSE'):
            ThrottleStore.callback(context, event, 'CANCEL', self)
        else:
            ThrottleStore.callback(context, event, 'SUCCESS', self)

        if self._draw_handler is not None:
            bpy.types.SpaceView3D.draw_handler_remove(self._draw_handler, 'WINDOW')

        if self._timer is not None:
            context.window_manager.event_timer_remove(self._timer)

        ThrottleStore.callback = None
        ThrottleStore.draw = None

        return {'FINISHED'}

    def invoke(self, context, event):
        if context.area.type == 'VIEW_3D':

            if event.type in ('ESC', 'RIGHTMOUSE'):
                return {'FINISHED'}

            if ThrottleStore.draw is not None:
                args = (self, context)
                self._draw_handler = bpy.types.SpaceView3D.draw_handler_add(ThrottleStore.draw, args, 'WINDOW',
                                                                            'POST_PIXEL')
            wm = context.window_manager
            wm.modal_handler_add(self)
            self._timer = wm.event_timer_add(0.05, window=context.window)

            return {'RUNNING_MODAL'}
        else:
            self.report({'WARNING'}, "View3D not found, cannot run operator")
            return {'FINISHED'}


def throttle_manip(callback=None, draw=None):
    """
        Invoke op from outside world
        in a convenient importable function

        transform_orientation in [‘GLOBAL’, ‘LOCAL’, ‘NORMAL’, ‘GIMBAL’, ‘VIEW’]

        draw(sp, context) a draw callback
        callback(context, event, state, sp) action callback

        Use either :
        takeloc Vector, unconstraint or system axis constraints
        takemat Matrix, constaint to this matrix as 'LOCAL' coordsys
            The snap source helper use it as world matrix
            so it is possible to constraint to user defined coordsys.
    """
    ThrottleStore.draw = draw
    ThrottleStore.callback = callback
    bpy.ops.archipack.manipulate_throttle('INVOKE_DEFAULT')


class ArchipackActiveManip:
    """
        Store manipulated object
        - object_name: manipulated object name
        - stack: array of Manipulators instances
        - manipulable: Manipulable instance
    """
    def __init__(self, object_name):
        self.object_name = object_name
        # manipulators stack for object
        self.stack = []
        # reference to object manipulable instance
        self.manipulable = None
        # .datablock() class method
        self.datablock = None
        self.active = False
        self.draw_handler = None

    @property
    def dirty(self):
        """
            Check for manipulable validity
            to disable modal when required
        """
        return bpy.data.objects.find(self.object_name) < 0 or len(self.stack) == 0

    def exit(self):
        """
            Exit manipulation mode
            - exit from all running manipulators
            - empty manipulators stack
            - set manipulable.manipulate_mode to False
            - remove reference to manipulable
        """
        for m in self.stack:
            if m is not None:
                m.exit()

        if self.draw_handler is not None:
            bpy.types.SpaceView3D.draw_handler_remove(self.draw_handler, 'WINDOW')

        self.manipulable = None
        self.datablock = None
        self.draw_handler = None
        self.object_name = ""
        self.stack.clear()


def remove_manipulable(key):
    """
        disable and remove a manipulable from stack
    """
    global manips
    # print("remove_manipulable key:%s" % (key))
    if key in manips:
        manips[key].exit()
        manips.pop(key)


# use a name based stack to store manipulate state even with instances
def manipulate_mode(key):
    global manips
    return key in manips


def check_stack(key):
    """
        check for stack item validity
        use in modal to destroy invalid modals
        return true when invalid / not found
        false when valid
    """
    global manips
    if key not in manips:
        # print("check_stack : key not found %s" % (key))
        return True
    elif manips[key].dirty:
        # print("check_stack : key.dirty %s" % (key))
        remove_manipulable(key)
        return True

    return False


def empty_stack():
    """
        kill every manipulators in stack
        and cleanup stack
    """
    global manips
    for m in manips.values():
        m.exit()
    manips.clear()


def add_manipulable(key, manipulable):
    """
        add a ArchipackActiveManip into the stack
        if not allready present
        setup reference to manipulable
        return manipulators stack
    """
    global manips
    if key not in manips:
        # print("add_manipulable() key:%s not found create new" % (key))
        manips[key] = ArchipackActiveManip(key)

    manips[key].manipulable = manipulable
    # class method for fast datablock access
    manips[key].datablock = manipulable.__class__.datablock
    return manips[key].stack


# ------------------------------------------------------------------
# Define Manipulators
# ------------------------------------------------------------------


class Manipulator(ArchipackObjectsManager):
    """
        Manipulator base class to derive other
        handle keyboard and modal events
        provide convenient funcs including getter and setter for datablock values
        store reference of base object, datablock and manipulator
    """
    keyboard_ascii = {
            ".", ",", "-", "+", "1", "2", "3",
            "4", "5", "6", "7", "8", "9", "0",
            "c", "m", "d", "k", "h", "a",
            " ", "/", "*", "'", "\""
            # "="
            }
    keyboard_type = {
            'BACK_SPACE', 'DEL',
            'LEFT_ARROW', 'RIGHT_ARROW'
            }

    def __init__(self, context, o, datablock, manipulator, snap_callback=None, start_callback=None, end_callback=None):
        """
            o : object to manipulate
            datablock : object data to manipulate
            manipulator: object archipack_manipulator datablock
            snap_callback: on snap enabled manipulators, will be called when drag occurs
        """
        self.is_updating = False

        self.keymap = Keymaps(context)
        self.feedback = FeedbackPanel()
        self.active = False
        self.selectable = False
        self.selected = False
        # active text input value for manipulator
        self.keyboard_input_active = False
        self.label_value = 0
        # unit for keyboard input value
        self.value_type = 'LENGTH'
        self.pts_mode = 'SIZE'

        self.lock = False

        # must hold those data here
        self.o = o
        self.datablock = datablock
        self.manipulator = manipulator

        # hold dict of property names and options
        self.props = self.from_json(self.manipulator.prop1_name)
        self.opts = self.from_json(self.manipulator.prop2_name)

        self._conversion_factor = 1
        if 'factor' in self.opts:
            self._conversion_factor = self.opts['factor']
            # print("conversion factor", self._conversion_factor)

        self.snap_callback = snap_callback
        self.start_callback = start_callback
        self.end_callback = end_callback
        self.origin = self.o.matrix_world.translation.copy()
        self.mouse_pos = Vector((0, 0))
        self.length_entered = ""
        self.line_pos = 0
        args = (self, context)
        self._handle = bpy.types.SpaceView3D.draw_handler_add(self.draw_callback, args, 'WINDOW', 'POST_PIXEL')

    def _draw_cb(self, _self, context, render=False):
        # print("Draw ", self.__class__.__name__)
        self.lock_txt.label = "Lock ({}) press SPACE to lock/unlock".format(['OFF','ON'][int(self.lock)])
        self.lock_txt.colour_inactive = [(0, 1, 0, 1), (1, 0, 0, 1)][int(self.lock)]
        self.lock_txt.draw(context, render)
        self.draw_callback(_self, context, render)
        # print("Draw ", self.__class__.__name__, " Done")

    @classmethod
    def poll(cls, context):
        """
            Allow manipulator enable/disable
            in given context
            handles will not show
        """
        return True

    def exit(self):
        """
            Modal exit, DON'T EVEN TRY TO OVERRIDE
        """
        if self._handle is not None:
            logger.debug("draw_handler_remove")
            bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')
            # Prevent race condition with redraw and the above call
            self._handle = None
            logger.debug("Manipulator.exit() draw_handler_remove done %s", (type(self).__name__))
        else:
            logger.debug("Manipulator.exit() handle not found %s", (type(self).__name__))

    def from_json(self, json_str):
        params = {}
        try:
            params = json.loads(json_str)
        except:
            # logger.debug("cant parse %s", json_str)
            pass
        return params

    # Mouse event handlers, MUST override
    def mouse_press(self, context, event):
        """
            Manipulators must implement
            mouse press event handler
            return True to callback manipulable_manipulate
        """
        raise NotImplementedError

    def mouse_release(self, context, event):
        """
            Manipulators must implement
            mouse mouse_release event handler
            return False to callback manipulable_release
        """
        raise NotImplementedError

    def mouse_move(self, context, event):
        """
            Manipulators must implement
            mouse move event handler
            return True to callback manipulable_manipulate
        """
        raise NotImplementedError

    # Keyboard event handlers, MAY override
    def keyboard_done(self, context, event, value):
        """
            Manipulators may implement
            keyboard value validated event handler
            value: changed by keyboard
            return True to callback manipulable_manipulate
        """
        return False

    def keyboard_editing(self, context, event, value):
        """
            Manipulators may implement
            keyboard value changed event handler
            value: string changed by keyboard
            allow realtime update of label
            return False to show edited value on window header
            return True when feedback show right on screen
        """
        self.label_value = value
        return True

    def keyboard_cancel(self, context, event):
        """
            Manipulators may implement
            keyboard entry cancelled
        """
        return

    def cancel(self, context, event):
        """
            Manipulators may implement
            cancelled event (ESC RIGHTCLICK)
        """
        self.active = False
        return

    def undo(self, context, event):
        """
            Manipulators may implement
            undo event (CTRL+Z)
        """
        return False

    def on_start_manipulate(self, context, event):
        """ Provide automatic on_start callback
        :param context:
        :param event:
        :return:
        """
        # print("manipulator.start_callback")
        if self.start_callback is not None:
            self.start_callback(context, event)

    def on_stop_manipulate(self, context, event):
        """ Provide automatic on_end callback
        :param context:
        :param event:
        :return:
        """
        if self.end_callback is not None:
            self.end_callback(context, event)

    # Internal, do not override unless you realy
    # realy realy know what you are doing
    def keyboard_eval(self, context, event):
        """
            evaluate keyboard entry while typing
            do not override this one
        """
        c = event.ascii
        if c:
            if c == ",":
                c = "."
            self.length_entered = self.length_entered[:self.line_pos] + c + self.length_entered[self.line_pos:]
            self.line_pos += 1

        if self.length_entered:
            if event.type == 'BACK_SPACE':
                self.length_entered = self.length_entered[:self.line_pos - 1] + self.length_entered[self.line_pos:]
                self.line_pos -= 1

            elif event.type == 'DEL':
                self.length_entered = self.length_entered[:self.line_pos] + self.length_entered[self.line_pos + 1:]

            elif event.type == 'LEFT_ARROW':
                self.line_pos = (self.line_pos - 1) % (len(self.length_entered) + 1)

            elif event.type == 'RIGHT_ARROW':
                self.line_pos = (self.line_pos + 1) % (len(self.length_entered) + 1)

        try:
            value = bpy.utils.units.to_value(context.scene.unit_settings.system, self.value_type, self.length_entered)
            draw_on_header = self.keyboard_editing(context, event, value)
        except:  # ValueError:
            draw_on_header = True
            pass

        draw_on_header = False

        # gniiii  draw on header replace top bar ...

        if draw_on_header:
            a = ""
            if self.length_entered:
                pos = self.line_pos
                a = self.length_entered[:pos] + '|' + self.length_entered[pos:]
            context.area.header_text_set("%s" % (a))

        # modal mode: do not let event bubble up
        return True

    def modal(self, context, event):
        """
            Modal handler
            handle mouse, and keyboard events
            enable and disable feedback

            return boolean
            where True means the event was handled here so stack return RUNNING_MODAL
            and False means let the event bubble on stack
        """

        # print("Manipulator.modal event:", event.type, event.value)

        if event.type == 'MOUSEMOVE' or event.type == 'INBETWEEN_MOUSEMOVE':
            return self.mouse_move(context, event)

        elif event.value == 'PRESS':

            if event.type == 'LEFTMOUSE':
                active = self.mouse_press(context, event)
                if active:
                    self.feedback.enable()
                    self.on_start_manipulate(context, event)

                return active

            elif event.type == 'SPACE':
                self.lock = not self.lock

            elif self.keymap.check(event, self.keymap.undo):
                if self.keyboard_input_active:
                    self.keyboard_input_active = False
                    self.keyboard_cancel(context, event)
                self.feedback.disable()
                # prevent undo CRASH
                return True

            elif self.keyboard_input_active and (
                    event.ascii in self.keyboard_ascii or
                    event.type in self.keyboard_type):
                # get keyboard input
                return self.keyboard_eval(context, event)

            elif event.type in {'ESC', 'RIGHTMOUSE'}:
                self.feedback.disable()
                if self.keyboard_input_active:
                    # allow keyboard exit without setting value
                    self.length_entered = ""
                    self.line_pos = 0
                    self.keyboard_input_active = False
                    self.keyboard_cancel(context, event)
                    return True
                elif self.active:
                    self.cancel(context, event)
                    return True
                return False

            elif self.keyboard_input_active and event.type in {'RET', 'NUMPAD_ENTER'}:
                # prevent draw tool to get return event
                return True

        elif event.value == 'RELEASE':

            if event.type == 'LEFTMOUSE':
                if not self.keyboard_input_active:
                    self.feedback.disable()
                    self.on_stop_manipulate(context, event)

                return self.mouse_release(context, event)

            elif self.keyboard_input_active and event.type in {'RET', 'NUMPAD_ENTER'}:
                # validate keyboard input
                if self.length_entered != "":
                    try:
                        value = bpy.utils.units.to_value(
                            context.scene.unit_settings.system,
                            self.value_type, self.length_entered)
                        self.length_entered = ""
                        ret = self.keyboard_done(context, event, value)
                    except:  # ValueError:
                        ret = False
                        self.keyboard_cancel(context, event)
                        pass

                    # context.area.header_text_set("")
                    self.keyboard_input_active = False
                    self.feedback.disable()
                    # return ret
                    return True

        return False

    def mouse_position(self, event):
        """
            store mouse position in a 2d Vector
        """
        self.mouse_pos.x, self.mouse_pos.y = event.mouse_region_x, event.mouse_region_y

    def get_pos3d(self, context):
        """
            convert mouse pos to 3d point over plane defined by origin and normal
            pt is in world space
        """
        region = context.region
        rv3d = context.region_data
        rM = context.active_object.matrix_world.to_3x3()
        view_vector_mouse = view3d_utils.region_2d_to_vector_3d(region, rv3d, self.mouse_pos)
        ray_origin_mouse = view3d_utils.region_2d_to_origin_3d(region, rv3d, self.mouse_pos)
        pt = intersect_line_plane(ray_origin_mouse, ray_origin_mouse + view_vector_mouse,
            self.origin, rM @ self.manipulator.normal, False)
        # fix issue with parallel plane
        if pt is None:
            pt = intersect_line_plane(ray_origin_mouse, ray_origin_mouse + view_vector_mouse,
                self.origin, view_vector_mouse, False)
        return pt

    def get_value(self, data, attr, index=-1):
        """
            Datablock value getter with index support
        """

        try:
            if index > -1:
                return getattr(data, attr)[index] / self._conversion_factor
            else:
                return getattr(data, attr) / self._conversion_factor
        except:
            # print("get_value of %s %s failed" % (data, attr))
            return 0

    def sp_skip_update(self, state):
        """ Eval if sp_callback must run to prevent overflow by timer
        :param data:
        :param state:
        :return:
        """
        res = state == 'RUNNING' and self.is_updating
        self.is_updating = True
        return res

    def set_value(self, context, data, attr, value, index=-1, force=True):
        """
            Datablock value setter with index support
        """


        try:
            if self.get_value(data, attr, index) != value:
                # switch context so unselected object are manipulable too
                old = context.object
                selected = self.is_selected(self.o)
                self.select_object(context, self.o, True)
                if index > -1:
                    getattr(data, attr)[index] = value * self._conversion_factor
                else:
                    setattr(data, attr, value * self._conversion_factor)
                if not selected:
                    self.unselect_object(context, self.o)
                self.select_object(context, old, True)
        except:
            logger.debug("Manipulator.set_value Unable to set value %s %s", attr, value)
            pass

    def preTranslate(self, tM, vec):
        """
            return a preTranslated Matrix
            tM Matrix source
            vec Vector translation
        """
        return tM @ Matrix([
        [1, 0, 0, vec.x],
        [0, 1, 0, vec.y],
        [0, 0, 1, vec.z],
        [0, 0, 0, 1]])

    def _move(self, o, axis, value):
        if axis == 'x':
            vec = Vector((value, 0, 0))
        elif axis == 'y':
            vec = Vector((0, value, 0))
        else:
            vec = Vector((0, 0, value))
        o.matrix_world = self.preTranslate(o.matrix_world, vec)

    def move_linked(self, context, axis, value):
        """
            Move an object along local axis
            takes care of linked too, fix issue #8
        """
        linked_objects = self.get_linked_objects(context, self.o)
        for o in linked_objects:
            if o != self.o:
                self._move(o, axis, value)

    def move(self, context, axis, value):
        """
            Move an object along local axis
        """
        self._move(self.o, axis, value)


# OUT OF ORDER
class SnapPointManipulator(Manipulator):
    """
        np_station based snap manipulator
        doesnt update anything by itself.
        NOTE : currently out of order
        and disabled in __init__
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):

        raise NotImplementedError

        self.handle = SquareHandle(handle_size, 1.2 * arrow_size, draggable=True)
        Manipulator.__init__(self, context, o, datablock, manipulator, snap_callback, start_callback, end_callback)

    def check_hover(self):
        self.handle.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        if self.handle.hover:
            self.handle.hover = False
            self.handle.active = True
            self.select_object(context, self.o)
            # takeloc = self.o.matrix_world @ self.manipulator.p0
            # print("Invoke sp_point_move %s" % (takeloc))
            # @TODO:
            # implement and add draw and callbacks
            # snap_point(takeloc, draw, callback)
            return True
        return False

    def mouse_release(self, context, event):
        self.check_hover()
        self.handle.active = False
        # False to callback manipulable_release
        return False

    def update(self, context, event):
        # NOTE:
        # doesnt set anything internally
        return

    def mouse_move(self, context, event):
        """

        """
        self.mouse_position(event)
        if self.handle.active:
            # self.handle.active = np_snap.is_running
            # self.update(context)
            # True here to callback manipulable_manipulate
            return True
        else:
            self.check_hover()
        return False

    def draw_callback(self, _self, context, render=False):
        logger.debug("SnapPointManipulator.draw_callback")
        left, right, side, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.handle.set_pos(context, left, Vector((1, 0, 0)), normal=normal)
        self.handle.draw(context, render)


# Generic snap tool for line based archipack objects (fence, wall, maybe stair too)
gl_pts3d = []


class WallSnapManipulator(Manipulator):
    """
        np_station snap inspired manipulator
        Use prop1_name as string part index
        Use prop2_name as string identifier height property for placeholders

        Misnamed as it work for all line based archipack's
        primitives, currently wall and fences,
        but may also work with stairs (sharing same data structure)
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        self.placeholder_area = GlPolygon((0.5, 0, 0, 0.2))
        self.placeholder_line = GlPolyline((0.5, 0, 0, 0.8))
        self.placeholder_line.is_closed = True
        self.label = GlText()
        self.line = GlLine()
        self.handle = SquareHandle(handle_size, 1.2 * arrow_size, draggable=True, selectable=True)
        Manipulator.__init__(self, context, o, datablock, manipulator, snap_callback, start_callback, end_callback)
        self.selectable = True

    def select(self, cursor_area):
        self.selected = self.selected or cursor_area.in_area(self.handle.pos_2d)
        self.handle.selected = self.selected

    def deselect(self, cursor_area):
        self.selected = not cursor_area.in_area(self.handle.pos_2d)
        self.handle.selected = self.selected

    def check_hover(self):
        self.handle.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        global gl_pts3d
        global manips
        if self.handle.hover:
            self.active = True
            self.handle.active = True
            gl_pts3d = []
            idx = int(self.manipulator.prop1_name)

            # get selected manipulators idx
            selection = []
            for m in manips[self.o.name].stack:
                if m is not None and m.selected:
                    selection.append(int(m.manipulator.prop1_name))

            # store all points of wall
            for i, part in enumerate(self.datablock.parts):
                p0, p1, side, normal = part.manipulators[2].get_pts(self.o.matrix_world)
                # if selected p0 will move and require placeholder
                gl_pts3d.append((p0, p1, i in selection or i == idx))

            self.feedback.instructions(context, "Move / Snap", "Drag to move, use keyboard to input values", [
                ('CTRL', 'Snap'),
                ('X Y', 'Constraint to axis (toggle Global Local None)'),
                ('SHIFT+Z', 'Constraint to xy plane'),
                ('MMBTN', 'Constraint to axis'),
                ('RIGHTCLICK or ESC', 'exit without change')
                ])
            self.feedback.enable()
            self.handle.hover = False
            self.select_object(context, self.o)
            takeloc, right, side, dz = self.manipulator.get_pts(self.o.matrix_world)
            dx = (right - takeloc).normalized()
            dy = dz.cross(dx)
            takemat = Matrix([
                [dx.x, dy.x, dz.x, takeloc.x],
                [dx.y, dy.y, dz.y, takeloc.y],
                [dx.z, dy.z, dz.z, takeloc.z],
                [0, 0, 0, 1]
            ])
            snap_point(takemat=takemat,
                        draw=self.sp_draw,
                        callback=self.sp_callback,
                        constraint_axis=(True, True, False))
            # this prevent other selected to run
            return True

        return False

    def mouse_release(self, context, event):
        self.check_hover()
        self.handle.active = False
        self.active = False
        self.feedback.disable()
        # False to callback manipulable_release
        return False

    def sp_callback(self, context, event, state, sp):
        """
            np station callback on moving, place, or cancel
        """
        if self.sp_skip_update(state):
            return

        global gl_pts3d
        logger.debug("WallSnapManipulator.sp_callback")

        if state == 'SUCCESS':
            o = self.o
            self.select_object(context, o, True)
            # apply changes to wall
            d = self.datablock
            g = d.get_generator()

            # use last state to setup childs
            # if hasattr(d, "setup_childs"):
            #    d.setup_childs(context, o)

            # rotation relative to object
            loc, rot, scale = o.matrix_world.decompose()
            rM = rot.to_matrix()
            delta = rM.inverted() @ sp.delta

            # update generator
            for idx, pt in enumerate(gl_pts3d):
                p0, p1, selected = pt
                _seg = g.segs[idx]
                if selected:
                    _z = _seg._z
                    if _seg.has_last and hasattr(_seg._last, "_r"):
                        # adjust radius of last
                        _seg._last.p1 = _seg.p0 + delta
                    else:
                        _seg.p0 = _seg.p0 + delta
                    _seg._z = _z

            if hasattr(d, "snap_baseline"):
                d.snap_baseline(o, g)

            # keep generator line origin at 0,0 and move object
            # when point 0 is moving
            p = o.matrix_world @ g.reset_origin()
            # update matrix by hand
            d.move_object(o, p)

            # update properties from generator
            g.update_parts(d)

            self.mouse_release(context, event)

            if hasattr(d, "relocate_childs"):
                d.relocate_childs(context, o)

            d.update(context)

        if state == 'CANCEL':
            self.mouse_release(context, event)
        logger.debug("WallSnapManipulator.sp_callback done")

        self.is_updating = False

        return

    def sp_draw(self, sp, context):
        # draw wall placeholders
        logger.debug("WallSnapManipulator.sp_draw")

        global gl_pts3d

        if self.o is None:
            return

        z = self.get_value(self.datablock, self.manipulator.prop2_name)

        placeholders = []
        for p0, p1, selected in gl_pts3d:
            pt = p0.copy()
            if selected:
                # when selected, p0 is moving
                # last one p1 should move too
                # last one require a placeholder too
                pt += sp.delta
                if len(placeholders) > 0:
                    placeholders[-1][1] = pt
                    placeholders[-1][2] = True
            placeholders.append([pt, p1, selected])

        # first selected and closed -> should move last p1 too
        if gl_pts3d[0][2] and self.datablock.is_closed:
            placeholders[-1][1] = placeholders[0][0].copy()
            placeholders[-1][2] = True

        # last one not visible when not closed
        if not self.datablock.is_closed:
            placeholders[-1][2] = False

        for p0, p1, selected in placeholders:
            if selected:
                self.placeholder_area.set_pos([p0, p1, Vector((p1.x, p1.y, p1.z + z)), Vector((p0.x, p0.y, p0.z + z))])
                self.placeholder_line.set_pos([p0, p1, Vector((p1.x, p1.y, p1.z + z)), Vector((p0.x, p0.y, p0.z + z))])
                self.placeholder_area.draw(context, render=False)
                self.placeholder_line.draw(context, render=False)

        p0, p1, side, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.line.p = p0
        self.line.v = sp.delta
        self.label.set_pos(context, self.line.length, self.line.lerp(0.5), self.line.v, normal=Vector((0, 0, 1)))
        self.line.draw(context, render=False)
        self.label.draw(context, render=False)
        logger.debug("WallSnapManipulator.sp_draw done")

    def mouse_move(self, context, event):
        self.mouse_position(event)
        if self.handle.active:
            # False here to pass_through
            # print("i'm able to pick up mouse move event while transform running")
            return False
        else:
            self.check_hover()
        return False

    def draw_callback(self, _self, context, render=False):
        left, right, side, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.handle.set_pos(context, left, (left - right).normalized(), normal=normal)
        self.handle.draw(context, render)
        self.feedback.draw(context, render)


class LineSnapManipulator(WallSnapManipulator):
    """
        np_station snap inspired manipulator
        Use prop1_name as string part index
        Use prop2_name as string identifier height property for placeholders

        Misnamed as it work for all line based archipack's
        primitives, currently wall and fences,
        but may also work with stairs (sharing same data structure)
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        WallSnapManipulator.__init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback)

    def sp_callback(self, context, event, state, sp):
        """
            np station callback on moving, place, or cancel
        """
        if self.sp_skip_update(state):
            return

        global gl_pts3d
        logger.debug("LineSnapManipulator.sp_callback")

        if state == 'SUCCESS':
            o = self.o
            self.select_object(context, o, True)
            # apply changes to wall
            d = self.datablock
            d.auto_update = False

            g = d.get_generator()

            # rotation relative to object
            loc, rot, scale = o.matrix_world.decompose()
            rM = rot.to_matrix()
            delta = rM @ sp.delta

            # update generator
            for idx, pt in enumerate(gl_pts3d):
                p0, p1, selected = pt
                if selected:
                    g.segs[idx].p0 += delta

            # keep generator line origin at 0,0 and move object
            # when point 0 is moving
            p = o.matrix_world @ g.reset_origin()
            # update matrix by hand
            d.move_object(o, p)

            # update properties from generator
            g.update_parts(d)

            self.mouse_release(context, event)

            d.auto_update = True

        if state == 'CANCEL':
            self.mouse_release(context, event)
        logger.debug("LineSnapManipulator.sp_callback done")
        self.is_updating = False

        return


class CounterManipulator(Manipulator):
    """
        increase or decrease an integer step by step
        right on click to prevent misuse
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        self.handle_left = TriHandle(handle_size, arrow_size, draggable=True)
        self.handle_right = TriHandle(handle_size, arrow_size, draggable=True)
        self.line_0 = GlLine()
        self.label = GlText()
        self.label.unit_mode = 'NONE'
        self.label.precision = 0
        Manipulator.__init__(self, context, o, datablock, manipulator)

    def check_hover(self):
        self.handle_right.check_hover(self.mouse_pos)
        self.handle_left.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        if self.handle_right.hover:
            value = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.set_value(context, self.datablock, self.manipulator.prop1_name, value + 1)
            self.handle_right.active = True
            return True
        if self.handle_left.hover:
            value = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.set_value(context, self.datablock, self.manipulator.prop1_name, value - 1)
            self.handle_left.active = True
            return True
        return False

    def mouse_release(self, context, event):
        self.check_hover()
        self.handle_right.active = False
        self.handle_left.active = False
        return False

    def mouse_move(self, context, event):
        self.mouse_position(event)
        if self.handle_right.active:
            return True
        if self.handle_left.active:
            return True
        else:
            self.check_hover()
        return False

    def draw_callback(self, _self, context, render=False):
        """
            draw on screen feedback using gl.
        """
        # logger.debug("CounterManipulator.draw_callback")

        # won't render counter
        if render:
            return
        left, right, side, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.origin = left
        self.line_0.p = left
        self.line_0.v = right - left
        self.line_0.z_axis = normal
        self.label.z_axis = normal
        value = self.get_value(self.datablock, self.manipulator.prop1_name)
        self.handle_left.set_pos(context, self.line_0.p, -self.line_0.v, normal=normal)
        self.handle_right.set_pos(context, self.line_0.lerp(1), self.line_0.v, normal=normal)
        self.label.set_pos(context, value, self.line_0.lerp(0.5), self.line_0.v, normal=normal)
        self.label.draw(context, render)
        self.handle_left.draw(context, render)
        self.handle_right.draw(context, render)
        # logger.debug("CounterManipulator.draw_callback done")


class DumbStringManipulator(Manipulator):
    """
        not a real manipulator, but allow to show a string
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        self.label = GlText(colour=(0, 0, 0, 1))
        self.label.unit_mode = 'NONE'
        self.label.label = manipulator.prop1_name
        Manipulator.__init__(self, context, o, datablock, manipulator)

    def check_hover(self):
        return False

    def mouse_press(self, context, event):
        return False

    def mouse_release(self, context, event):
        return False

    def mouse_move(self, context, event):
        return False

    def draw_callback(self, _self, context, render=False):
        """
            draw on screen feedback using gl.
        """
        # logger.debug("DumbStringManipulator.draw_callback")

        # won't render string
        if render:
            return
        left, right, side, normal = self.manipulator.get_pts(self.o.matrix_world)
        pos = left + 0.5 * (right - left)
        self.label.set_pos(context, None, pos, pos, normal=normal)
        self.label.draw(context, render)
        # logger.debug("DumbStringManipulator.draw_callback done")


class SizeManipulator(Manipulator):

    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        self.handle_left = TriHandle(handle_size, arrow_size)
        self.handle_right = TriHandle(handle_size, arrow_size, draggable=True)
        self.line_0 = GlLine()
        self.line_1 = GlLine()
        self.line_2 = GlLine()
        self.label = EditableText(handle_size, arrow_size, draggable=True)
        # self.label.label = 'S '
        Manipulator.__init__(self, context, o, datablock, manipulator, snap_callback, start_callback, end_callback)

    def check_hover(self):
        self.handle_right.check_hover(self.mouse_pos)
        self.label.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        global gl_pts3d
        if self.handle_right.hover:
            self.active = True
            self.original_size = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.original_location = self.o.matrix_world.translation.copy()
            self.feedback.instructions(context, "Size", "Drag or Keyboard to modify size", [
                ('CTRL', 'Snap'),
                ('ALT', 'Round'),
                ('SHIFT', 'Small steps'),
                ('RIGHTCLICK or ESC', 'cancel')
                ])
            left, right, side, dz = self.manipulator.get_pts(self.o.matrix_world)
            dx = (right - left).normalized()
            dy = dz.cross(dx)
            takemat = Matrix([
                [dx.x, dy.x, dz.x, right.x],
                [dx.y, dy.y, dz.y, right.y],
                [dx.z, dy.z, dz.z, right.z],
                [0, 0, 0, 1]
            ])
            gl_pts3d = [left, right]
            snap_point(takemat=takemat,
                callback=self.sp_callback,
                constraint_axis=(True, False, False))
            self.handle_right.active = True
            return True
        if self.label.hover:
            self.feedback.instructions(context, "Size", "Use keyboard to modify size",
                [('ENTER', 'Validate'), ('RIGHTCLICK or ESC', 'cancel')])
            self.label.active = True
            self.keyboard_input_active = True
            return True
        return False

    def mouse_release(self, context, event):
        self.active = False
        self.check_hover()
        self.handle_right.active = False
        if not self.keyboard_input_active:
            self.feedback.disable()
        return False

    def mouse_move(self, context, event):
        self.mouse_position(event)
        if self.active:
            self.update(context, event)
            return True
        else:
            self.check_hover()
        return False

    def cancel(self, context, event):
        if self.active:
            self.mouse_release(context, event)
            self.set_value(context, self.datablock, self.manipulator.prop1_name, self.original_size)

    def keyboard_done(self, context, event, value):
        self.set_value(context, self.datablock, self.manipulator.prop1_name, value)
        self.label.active = False
        return True

    def keyboard_cancel(self, context, event):
        self.label.active = False
        return False

    def update(self, context, event):
        # 0  1  2
        # |_____|
        #
        pt = self.get_pos3d(context)
        pt, t = intersect_point_line(pt, self.line_0.p, self.line_2.p)
        length = (self.line_0.p - pt).length
        if event.alt:
            if event.shift:
                length = round(length, 2)
            else:
                length = round(length, 1)
        self.set_value(context, self.datablock, self.manipulator.prop1_name, length)

    def draw_callback(self, _self, context, render=False):
        """
            draw on screen feedback using gl.
        """
        # logger.debug("SizeManipulator.draw_callback")

        left, right, side, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.origin = left
        self.line_1.p = left
        self.line_1.v = right - left
        self.line_0.z_axis = normal
        self.line_1.z_axis = normal
        self.line_2.z_axis = normal
        self.label.z_axis = normal
        self.line_0 = self.line_1.sized_normal(0, side.x * 1.1)
        self.line_2 = self.line_1.sized_normal(1, side.x * 1.1)
        self.line_1.offset(side.x * 1.0)
        self.handle_left.set_pos(context, self.line_1.p, -self.line_1.v, normal=normal)
        self.handle_right.set_pos(context, self.line_1.lerp(1), self.line_1.v, normal=normal)
        if not self.keyboard_input_active:
            self.label_value = self.line_1.length
        self.label.set_pos(context, self.label_value, self.line_1.lerp(0.5), self.line_1.v, normal=normal)
        self.line_0.draw(context, render)
        self.line_1.draw(context, render)
        self.line_2.draw(context, render)
        self.handle_left.draw(context, render)
        self.handle_right.draw(context, render)
        self.label.draw(context, render)
        self.feedback.draw(context, render)
        # logger.debug("SizeManipulator.draw_callback done")

    def sp_callback(self, context, event, state, sp):
        if self.sp_skip_update(state):
            return

        logger.debug("SizeManipulator.sp_callback")
        global gl_pts3d

        p0 = gl_pts3d[0].copy()
        p1 = gl_pts3d[1].copy()

        if state != 'CANCEL':
            p1 += sp.delta

        length = (p0 - p1).length

        if state != 'CANCEL' and event.alt:
            if event.shift:
                length = round(length, 2)
            else:
                length = round(length, 1)

        self.set_value(context, self.datablock, self.manipulator.prop1_name, length)

        if state != 'RUNNING':
            self.mouse_release(context, event)

        self.is_updating = False

        logger.debug("SizeManipulator.sp_callback done")


class DualSnapSizeManipulator(Manipulator):
    """
     Modify dimension in both directions
     set prop_2 to either ['LEFT', 'RIGHT'] according
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        self.handle_left = TriHandle(handle_size, arrow_size, draggable=True)
        self.handle_right = TriHandle(handle_size, arrow_size, draggable=True)
        self.line_0 = GlLine()
        self.line_1 = GlLine()
        self.line_2 = GlLine()
        self.label = EditableText(handle_size, arrow_size, draggable=True)
        # self.label.label = 'S '
        self.direction = 'RIGHT'
        Manipulator.__init__(self, context, o, datablock, manipulator, snap_callback, start_callback, end_callback)

    def check_hover(self):
        self.handle_right.check_hover(self.mouse_pos)
        self.handle_left.check_hover(self.mouse_pos)
        self.label.check_hover(self.mouse_pos)

    def init_gl_pts(self):
        global gl_pts3d
        left, right, side, dz = self.manipulator.get_pts(self.o.matrix_world)
        dx = (right - left).normalized()
        dy = dz.cross(dx)
        gl_pts3d = [left, right]
        return dx, dy, dz, left, right

    def mouse_press(self, context, event):

        logger.debug("DualSnapSizeManipulator.mouse_press")

        if self.handle_right.hover or self.handle_left.hover:
            self.active = True
            self.original_size = self.get_value(self.datablock, self.props['length'])
            self.feedback.instructions(context, "Size", "Drag or Keyboard to modify size", [
                ('SPACE', 'Lock'),
                ('CTRL', 'Snap'),
                ('ALT', 'Round'),
                ('SHIFT', 'Small steps'),
                ('RIGHTCLICK or ESC', 'cancel')
                ])
            dx, dy, dz, left, right = self.init_gl_pts()

        if self.handle_right.hover:
            self.direction = 'RIGHT'

            takemat = Matrix([
                [dx.x, dy.x, dz.x, right.x],
                [dx.y, dy.y, dz.y, right.y],
                [dx.z, dy.z, dz.z, right.z],
                [0, 0, 0, 1]
            ])
            snap_point(takemat=takemat,
                callback=self.sp_callback,
                constraint_axis=(True, False, False))
            self.handle_right.active = True
            return True

        if self.handle_left.hover:
            self.direction = 'LEFT'

            takemat = Matrix([
                [dx.x, dy.x, dz.x, left.x],
                [dx.y, dy.y, dz.y, left.y],
                [dx.z, dy.z, dz.z, left.z],
                [0, 0, 0, 1]
            ])
            snap_point(takemat=takemat,
                callback=self.sp_callback,
                constraint_axis=(True, False, False))
            self.handle_left.active = True
            return True

        if self.label.hover:
            self.direction = "RIGHT"
            self.feedback.instructions(context, "Size", "Use keyboard to modify size",
                [('ENTER', 'Validate'), ('RIGHTCLICK or ESC', 'cancel')])
            self.label.active = True
            self.keyboard_input_active = True
            return True
        return False

    def mouse_release(self, context, event):
        logger.debug("DualSnapSizeManipulator.mouse_release")
        self.active = False
        self.check_hover()
        self.handle_right.active = False
        self.handle_left.active = False
        if not self.keyboard_input_active:
            self.feedback.disable()
        return False

    def mouse_move(self, context, event):
        #
        self.mouse_position(event)
        if self.active:
            return False
            # logger.debug("DualSnapSizeManipulator.mouse_move")
            # self.update(context, event)
            # return True
        else:
            self.check_hover()
        return False

    def cancel(self, context, event):
        logger.debug("DualSnapSizeManipulator.cancel")
        if self.active:
            self.mouse_release(context, event)
            self.set_value(context, self.datablock, self.props['lock'], self.lock)
            self.set_value(context, self.datablock, self.props['dir'], self.direction)
            self.set_value(context, self.datablock, self.props['length'], self.original_size)

    def keyboard_done(self, context, event, value):
        self.set_value(context, self.datablock, self.props['lock'], self.lock)
        self.set_value(context, self.datablock, self.props['dir'], self.direction)
        self.set_value(context, self.datablock, self.props['length'], value)
        self.label.active = False
        return True

    def keyboard_cancel(self, context, event):
        self.label.active = False
        return False

    def update(self, context, event):
        # 0  1  2
        # |_____|
        #
        logger.debug("DualSnapSizeManipulator.update")
        pt = self.get_pos3d(context)
        pt, t = intersect_point_line(pt, self.line_0.p, self.line_2.p)
        if self.direction == 'RIGHT':
            length = (self.line_0.p - pt).length
        else:
            length = (self.line_2.p - pt).length
        if event.alt:
            if event.shift:
                length = round(length, 2)
            else:
                length = round(length, 1)
        self.set_value(context, self.datablock, self.props['lock'], self.lock)
        self.set_value(context, self.datablock, self.props['dir'], self.direction)
        self.set_value(context, self.datablock, self.props['length'], length)

    def draw_callback(self, _self, context, render=False):
        """
            draw on screen feedback using gl.
            """
        # logger.debug("DualSnapSizeManipulator.draw_callback")

        left, right, side, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.origin = left
        self.line_1.p = left
        self.line_1.v = right - left
        self.line_0.z_axis = normal
        self.line_1.z_axis = normal
        self.line_2.z_axis = normal
        self.label.z_axis = normal
        self.line_0 = self.line_1.sized_normal(0, side.x * 1.1)
        self.line_2 = self.line_1.sized_normal(1, side.x * 1.1)
        self.line_1.offset(side.x * 1.0)
        self.handle_left.set_pos(context, self.line_1.p, -self.line_1.v, normal=normal)
        self.handle_right.set_pos(context, self.line_1.lerp(1), self.line_1.v, normal=normal)
        if not self.keyboard_input_active:
            self.label_value = self.line_1.length
        self.label.set_pos(context, self.label_value, self.line_1.lerp(0.5), self.line_1.v, normal=normal)
        self.line_0.draw(context, render)
        self.line_1.draw(context, render)
        self.line_2.draw(context, render)
        self.handle_left.draw(context, render)
        self.handle_right.draw(context, render)
        self.label.draw(context, render)
        self.feedback.draw(context, render)
        # logger.debug("DualSnapSizeManipulator.draw_callback done")

    def sp_callback(self, context, event, state, sp):
        if self.sp_skip_update(state):
            return
        logger.debug("DualSnapSizeManipulator.sp_callback %s", context.object.name)

        global gl_pts3d
        p0 = gl_pts3d[0].copy()
        p1 = gl_pts3d[1].copy()

        if state != 'CANCEL':
            if self.direction == 'RIGHT':
                p1 += sp.delta
            else:
                p0 += sp.delta

        length = (p0 - p1).length

        if state != 'CANCEL' and event.alt:
            if event.shift:
                length = round(length, 2)
            else:
                length = round(length, 1)

        # valeur absolue = stable meme quand l'origine se deplace
        # instable quand la longueur entre les points change par un autre moyen (snap)

        self.set_value(context, self.datablock, self.props['lock'], self.lock)
        self.set_value(context, self.datablock, self.props['dir'], self.direction)
        self.set_value(context, self.datablock, self.props['length'], length)

        if state != 'RUNNING':
            self.mouse_release(context, event)

        self.is_updating = False

        logger.debug("DualSnapSizeManipulator.sp_callback done %s", context.object.name)


class SizeLocationManipulator(SizeManipulator):
    """
        Handle resizing by any of the boundaries
        of objects with centered pivots
        so when size change, object should move of the
        half of the change in the direction of change.

        Also take care of moving linked objects too
        Changing size is not necessary as link does
        allredy handle this and childs panels are
        updated by base object.
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        SizeManipulator.__init__(self, context, o, datablock, manipulator,
                                 handle_size, snap_callback, start_callback, end_callback)
        self.handle_left.draggable = True

    def check_hover(self):
        self.handle_right.check_hover(self.mouse_pos)
        self.handle_left.check_hover(self.mouse_pos)
        self.label.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        if self.handle_right.hover:
            self.active = True
            self.original_location = self.o.matrix_world.translation.copy()
            self.original_size = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.feedback.instructions(context, "Size", "Drag to modify size", [
                ('ALT', 'Round value'), ('RIGHTCLICK or ESC', 'cancel')
                ])
            self.handle_right.active = True
            return True
        if self.handle_left.hover:
            self.active = True
            self.original_location = self.o.matrix_world.translation.copy()
            self.original_size = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.feedback.instructions(context, "Size", "Drag to modify size", [
                ('ALT', 'Round value'), ('RIGHTCLICK or ESC', 'cancel')
                ])
            self.handle_left.active = True
            return True
        if self.label.hover:
            self.feedback.instructions(context, "Size", "Use keyboard to modify size",
                [('ENTER', 'Validate'), ('RIGHTCLICK or ESC', 'cancel')])
            self.label.active = True
            self.keyboard_input_active = True
            return True
        return False

    def mouse_release(self, context, event):
        self.active = False
        self.check_hover()
        self.handle_right.active = False
        self.handle_left.active = False
        if not self.keyboard_input_active:
            self.feedback.disable()
        return False

    def mouse_move(self, context, event):
        self.mouse_position(event)
        if self.handle_right.active or self.handle_left.active:
            self.update(context, event)
            return True
        else:
            self.check_hover()
        return False

    def keyboard_done(self, context, event, value):
        self.set_value(context, self.datablock, self.manipulator.prop1_name, value)
        # self.move_linked(context, self.manipulator.prop2_name, dl)
        self.label.active = False
        self.feedback.disable()
        return True

    def cancel(self, context, event):
        if self.active:
            self.mouse_release(context, event)
            # must move back to original location
            itM = self.o.matrix_world.inverted()
            dl = self.get_value(itM @ self.original_location, self.manipulator.prop2_name)

            self.move(context, self.manipulator.prop2_name, dl)
            self.set_value(context, self.datablock, self.manipulator.prop1_name, self.original_size)
            self.move_linked(context, self.manipulator.prop2_name, dl)

    def update(self, context, event):
        # 0  1  2
        # |_____|
        #


        pt = self.get_pos3d(context)
        pt, t = intersect_point_line(pt, self.line_0.p, self.line_2.p)

        len_0 = (pt - self.line_0.p).length
        len_1 = (pt - self.line_2.p).length

        length = max(len_0, len_1)

        if event.alt:
            if event.shift:
                length = round(length, 2)
            else:
                length = round(length, 1)

        dl = length - self.line_1.length

        if len_0 > len_1:
            dl = 0.5 * dl
        else:
            dl = -0.5 * dl

        self.move(context, self.manipulator.prop2_name, dl)
        self.set_value(context, self.datablock, self.manipulator.prop1_name, length)
        self.move_linked(context, self.manipulator.prop2_name, dl)


class SnapSizeLocationManipulator(SizeLocationManipulator):
    """
        Snap aware extension of SizeLocationManipulator
        Handle resizing by any of the boundaries
        of objects with centered pivots
        so when size change, object should move of the
        half of the change in the direction of change.

        Also take care of moving linked objects too
        Changing size is not necessary as link does
        allredy handle this and childs panels are
        updated by base object.


    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        SizeLocationManipulator.__init__(self, context, o, datablock, manipulator,
                                         handle_size, snap_callback, start_callback, end_callback)

    def mouse_press(self, context, event):
        global gl_pts3d
        if self.handle_right.hover:
            self.active = True
            self.original_size = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.original_location = self.o.matrix_world.translation.copy()
            self.feedback.instructions(context, "Size", "Drag or Keyboard to modify size", [
                ('CTRL', 'Snap'),
                ('SHIFT', 'Round'),
                ('RIGHTCLICK or ESC', 'cancel')
                ])
            left, right, side, dz = self.manipulator.get_pts(self.o.matrix_world)
            dx = (right - left).normalized()
            dy = dz.cross(dx)
            takemat = Matrix([
                [dx.x, dy.x, dz.x, right.x],
                [dx.y, dy.y, dz.y, right.y],
                [dx.z, dy.z, dz.z, right.z],
                [0, 0, 0, 1]
            ])
            gl_pts3d = [left, right]
            snap_point(takemat=takemat,
                callback=self.sp_callback,
                constraint_axis=(True, False, False))

            self.handle_right.active = True
            return True

        if self.handle_left.hover:
            self.active = True
            self.original_size = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.original_location = self.o.matrix_world.translation.copy()
            self.feedback.instructions(context, "Size", "Drag or Keyboard to modify size", [
                ('CTRL', 'Snap'),
                ('SHIFT', 'Round'),
                ('RIGHTCLICK or ESC', 'cancel')
                ])
            left, right, side, dz = self.manipulator.get_pts(self.o.matrix_world)
            dx = (left - right).normalized()
            dy = dz.cross(dx)
            takemat = Matrix([
                [dx.x, dy.x, dz.x, left.x],
                [dx.y, dy.y, dz.y, left.y],
                [dx.z, dy.z, dz.z, left.z],
                [0, 0, 0, 1]
            ])
            gl_pts3d = [left, right]
            snap_point(takemat=takemat,
                       callback=self.sp_callback,
                       constraint_axis=(True, False, False))
            self.handle_left.active = True
            return True

        if self.label.hover:
            self.feedback.instructions(context, "Size", "Use keyboard to modify size",
                [('ENTER', 'Validate'), ('RIGHTCLICK or ESC', 'cancel')])
            self.label.active = True
            self.keyboard_input_active = True
            return True

        return False

    def sp_callback(self, context, event, state, sp):

        logger.debug("SnapSizeLocationManipulator.sp_callback %s %s" % (state, self.is_updating))

        if self.sp_skip_update(state):
            return

        global gl_pts3d
        p0 = gl_pts3d[0].copy()
        p1 = gl_pts3d[1].copy()

        if state != 'CANCEL':
            if self.handle_right.active:
                p1 += sp.delta
            else:
                p0 += sp.delta

        l0 = self.get_value(self.datablock, self.manipulator.prop1_name)
        length = (p0 - p1).length

        if state != 'CANCEL' and event.alt:
            if event.shift:
                length = round(length, 2)
            else:
                length = round(length, 1)

        dp = length - l0

        if self.handle_left.active:
            dp = -dp
        dl = 0.5 * dp

        # snap_helper = context.object
        self.move(context, self.manipulator.prop2_name, dl)
        self.set_value(context, self.datablock, self.manipulator.prop1_name, length)
        self.move_linked(context, self.manipulator.prop2_name, dl)

        # snapping child objects may require base object update
        # eg manipulating windows requiring wall update
        if self.snap_callback is not None:
            snap_helper = context.active_object
            self.snap_callback(context, o=self.o, manipulator=self, sp=sp)
            snap_helper.select_set(state=True)

        if state != 'RUNNING':
            self.mouse_release(context, event)

        self.is_updating = False

        logger.debug("SnapSizeLocationManipulator.sp_callback done")


class DeltaLocationManipulator(SizeManipulator):
    """
        Move a child window or door in wall segment
        not limited to this by the way
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        SizeManipulator.__init__(self, context, o, datablock, manipulator,
                                 handle_size, snap_callback, start_callback, end_callback)
        self.label.label = ''
        self.feedback.instructions(context, "Move", "Drag to move", [
            ('CTRL', 'Snap'),
            ('SHIFT', 'Round value'),
            ('RIGHTCLICK or ESC', 'cancel')
            ])

    def check_hover(self):
        self.handle_right.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        global gl_pts3d
        if self.handle_right.hover:
            self.original_location = self.o.matrix_world.translation.copy()
            self.active = True
            self.feedback.enable()
            self.handle_right.active = True

            left, right, side, dz = self.manipulator.get_pts(self.o.matrix_world)
            dp = (right - left)
            dx = dp.normalized()
            dy = dz.cross(dx)
            p0 = left + 0.5 * dp
            takemat = Matrix([
                [dx.x, dy.x, dz.x, p0.x],
                [dx.y, dy.y, dz.y, p0.y],
                [dx.z, dy.z, dz.z, p0.z],
                [0, 0, 0, 1]
            ])
            gl_pts3d = [p0]
            snap_point(takemat=takemat,
                callback=self.sp_callback,
                constraint_axis=(
                    self.manipulator.prop1_name == 'x',
                    self.manipulator.prop1_name == 'y',
                    self.manipulator.prop1_name == 'z'))
            return True
        return False

    def mouse_release(self, context, event):
        self.check_hover()
        self.feedback.disable()
        self.active = False
        self.handle_right.active = False
        return False

    def mouse_move(self, context, event):
        self.mouse_position(event)
        if self.handle_right.active:
            # self.update(context, event)
            return True
        else:
            self.check_hover()
        return False

    def sp_callback(self, context, event, state, sp):

        if self.sp_skip_update(state):
            return

        logger.debug("DeltaLocationManipulator.sp_callback")

        if state == 'CANCEL':
            self.cancel(context, event)
        else:
            global gl_pts3d
            p0 = gl_pts3d[0].copy()
            p1 = p0 + sp.delta
            itM = self.o.matrix_world.inverted()
            dl = self.get_value(itM @ p1, self.manipulator.prop1_name)
            self.move(context, self.manipulator.prop1_name, dl)

            # snapping child objects may require base object update
            # eg manipulating windows requiring wall update
            if self.snap_callback is not None:
                snap_helper = context.active_object
                self.snap_callback(context, o=self.o, manipulator=self, sp=sp)
                snap_helper.select_set(state=True)
                context.view_layer.objects.active = snap_helper

            if state == 'SUCCESS':
                self.mouse_release(context, event)

        self.is_updating = False

        logger.debug("DeltaLocationManipulator.sp_callback done")

    def cancel(self, context, event):
        if self.active:
            self.mouse_release(context, event)
            # must move back to original location
            itM = self.o.matrix_world.inverted()
            dl = self.get_value(itM @ self.original_location, self.manipulator.prop1_name)
            self.move(context, self.manipulator.prop1_name, dl)

    def draw_callback(self, _self, context, render=False):
        # logger.debug("DeltaLocationManipulator.draw_callback")
        """
            draw on screen feedback using gl.
        """
        left, right, side, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.origin = left
        self.line_1.p = left
        self.line_1.v = right - left
        self.line_1.z_axis = normal
        self.handle_left.set_pos(context, self.line_1.lerp(0.5), -self.line_1.v, normal=normal)
        self.handle_right.set_pos(context, self.line_1.lerp(0.5), self.line_1.v, normal=normal)
        self.handle_left.draw(context, render)
        self.handle_right.draw(context, render)
        self.feedback.draw(context)
        # logger.debug("DeltaLocationManipulator.draw_callback done")


class DumbSizeManipulator(SizeManipulator):
    """
        Show a size while not being editable
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        SizeManipulator.__init__(self, context, o, datablock, manipulator,
                                 handle_size, snap_callback, start_callback, end_callback)
        self.handle_right.draggable = False
        self.label.draggable = False
        self.label.colour_inactive = (0, 0, 0, 1)
        # self.label.label = 'Dumb '

    def mouse_move(self, context, event):
        return False


class AngleManipulator(Manipulator):
    """
        NOTE:
            There is a default shortcut to +5 and -5 on angles with left/right arrows

        Manipulate angle between segments
        bound to [-pi, pi]
    """

    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        # Angle
        self.handle_right = TriHandle(handle_size, arrow_size, draggable=True)
        # self.handle_center = SquareHandle(handle_size, arrow_size)
        self.arc = GlArc()
        self.line_0 = GlLine()
        self.line_1 = GlLine()
        self.label_a = EditableText(handle_size, arrow_size, draggable=True)
        self.label_a.unit_type = 'ANGLE'
        Manipulator.__init__(self, context, o, datablock, manipulator, snap_callback, start_callback, end_callback)
        self.pts_mode = 'RADIUS'

    def check_hover(self):
        self.handle_right.check_hover(self.mouse_pos)
        self.label_a.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        if self.handle_right.hover:
            self.active = True
            self.original_angle = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.feedback.instructions(context, "Angle (degree)", "Drag to modify angle", [
                ('SHIFT', 'Round 1 degree'),
                ('CTRL+SHIFT', 'Round 5 degrees'),
                ('RIGHTCLICK or ESC', 'cancel')
                ])
            self.handle_right.active = True
            throttle_manip(callback=self.sp_callback)
            return True
        if self.label_a.hover:
            self.feedback.instructions(context, "Angle (degree)", "Use keyboard to modify angle",
                [('ENTER', 'validate'),
                ('RIGHTCLICK or ESC', 'cancel')])
            self.value_type = 'ROTATION'
            self.label_a.active = True
            self.label_value = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.keyboard_input_active = True
            return True
        return False

    def mouse_release(self, context, event):
        self.check_hover()
        self.handle_right.active = False
        if self.active:
            self.feedback.disable()
            self.active = False
        return False

    def mouse_move(self, context, event):
        self.mouse_position(event)
        if self.active:
            # print("AngleManipulator.mouse_move")
            return False
        else:
            self.check_hover()
        return False

    def keyboard_done(self, context, event, value):
        self.set_value(context, self.datablock, self.manipulator.prop1_name, value)
        self.label_a.active = False
        return True

    def keyboard_cancel(self, context, event):
        self.label_a.active = False
        return False

    def cancel(self, context, event):
        if self.active:
            self.mouse_release(context, event)
            self.set_value(context, self.datablock, self.manipulator.prop1_name, self.original_angle)

    def sp_callback(self, context, event, state, sp):

        if self.sp_skip_update(state):
            return

        if state == 'CANCEL':
            self.cancel(context, event)
        else:
            self.update(context, event)

            if state != 'RUNNING':
                self.mouse_release(context, event)

        self.is_updating = False

    def update(self, context, event):

        pt = self.get_pos3d(context)
        c = self.arc.c
        v = 2 * self.arc.r * (pt - c).normalized()
        v0 = c - v
        v1 = c + v
        p0, p1 = intersect_line_sphere(v0, v1, c, self.arc.r)
        if p0 is not None and p1 is not None:

            if (p1 - pt).length < (p0 - pt).length:
                p0, p1 = p1, p0

            v = p0 - self.arc.c
            da = atan2(v.y, v.x) - self.line_0.angle
            if da > pi:
                da -= 2 * pi
            if da < -pi:
                da += 2 * pi
            # from there pi > da > -pi
            # print("a:%.4f da:%.4f a0:%.4f" % (atan2(v.y, v.x), da, self.line_0.angle))
            if da > pi:
                da = pi
            if da < -pi:
                da = -pi
            if event.shift:
                if event.ctrl:
                    da = round(da / pi * 36, 0) / 36 * pi
                else:
                    da = round(da / pi * 180, 0) / 180 * pi
            self.set_value(context, self.datablock, self.manipulator.prop1_name, da)

    def draw_callback(self, _self, context, render=False):
        c, left, right, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.line_0.z_axis = normal
        self.line_1.z_axis = normal
        self.arc.z_axis = normal
        self.label_a.z_axis = normal
        self.origin = c
        self.line_0.p = c
        self.line_1.p = c
        self.arc.c = c
        self.line_0.v = left
        self.line_0.v = -left.length * self.line_0.cross.normalized()
        self.line_1.v = right
        self.line_1.v = right.length * self.line_1.cross.normalized()
        self.arc.a0 = self.line_0.angle
        self.arc.da = self.get_value(self.datablock, self.manipulator.prop1_name)
        self.arc.r = 1.0
        self.handle_right.set_pos(context, self.line_1.lerp(1),
                                  self.line_1.sized_normal(1, -1 if self.arc.da > 0 else 1).v)
        # self.handle_center.set_pos(context, self.arc.c, -self.line_0.v)
        label_value = self.arc.da
        if self.keyboard_input_active:
            label_value = self.label_value
        self.label_a.set_pos(context, label_value, self.arc.lerp(0.5), -self.line_0.v)
        self.arc.draw(context, render)
        self.line_0.draw(context, render)
        self.line_1.draw(context, render)
        self.handle_right.draw(context, render)
        # self.handle_center.draw(context, render)
        self.label_a.draw(context, render)
        self.feedback.draw(context, render)


class DualAngleManipulator(Manipulator):
    """
        NOTE:
            There is a default shortcut to +5 and -5 on angles with left/right arrows

        Manipulate angle between segments
        bound to [-pi, pi]
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        # Angle
        self.handle_right = TriHandle(handle_size, arrow_size, draggable=True)
        self.handle_left = TriHandle(handle_size, arrow_size, draggable=True)
        # self.handle_center = SquareHandle(handle_size, arrow_size, draggable=True)
        self.arc = GlArc()
        self.line_0 = GlLine()
        self.line_1 = GlLine()
        self.label_a = EditableText(handle_size, arrow_size, draggable=True)
        self.label_a.unit_type = 'ANGLE'
        Manipulator.__init__(self, context, o, datablock, manipulator, snap_callback, start_callback, end_callback)
        self.pts_mode = 'RADIUS'
        self.direction = 'RIGHT'

    def check_hover(self):
        self.handle_right.check_hover(self.mouse_pos)
        self.handle_left.check_hover(self.mouse_pos)
        self.label_a.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        if self.handle_right.hover or self.handle_left.hover:
            self.active = True
            self.original_angle = self.get_value(self.datablock, self.props['angle'])
            self.feedback.instructions(context, "Angle (degree)", "Drag to modify angle", [
                ('SPACE', 'Lock'),
                ('SHIFT', 'Round 1 degree'),
                ('CTRL+SHIFT', 'Round 5 degrees'),
                ('RIGHTCLICK or ESC', 'cancel')
                ])

        if self.handle_right.hover:
            # print("DualAngleManipulator mouse_press handle_right.active")
            self.direction = 'RIGHT'
            self.handle_right.active = True
            throttle_manip(callback=self.sp_callback)
            return True

        if self.handle_left.hover:
            # print("DualAngleManipulator mouse_press handle_left.active")
            self.direction = 'LEFT'
            self.handle_left.active = True
            throttle_manip(callback=self.sp_callback)
            return True

        if self.label_a.hover:
            self.direction = 'RIGHT'
            self.feedback.instructions(context, "Angle (degree)", "Use keyboard to modify angle",
                [('ENTER', 'validate'),
                ('RIGHTCLICK or ESC', 'cancel')])
            self.value_type = 'ROTATION'
            self.label_a.active = True
            self.label_value = self.get_value(self.datablock, self.props['angle'])
            self.keyboard_input_active = True
            return True

        return False

    def mouse_release(self, context, event):
        self.check_hover()
        self.handle_right.active = False
        self.handle_left.active = False
        if self.active:
            self.feedback.disable()
            self.active = False
        return False

    def mouse_move(self, context, event):
        self.mouse_position(event)
        if self.active:
            return False
        else:
            self.check_hover()
        return False

    def keyboard_done(self, context, event, value):
        self.set_value(context, self.datablock, self.props['lock'], self.lock)
        self.set_value(context, self.datablock, self.props['dir'], self.direction)
        self.set_value(context, self.datablock, self.props['angle'], value)
        self.label_a.active = False
        return True

    def keyboard_cancel(self, context, event):
        self.label_a.active = False
        return False

    def cancel(self, context, event):
        if self.active:
            self.mouse_release(context, event)
            self.set_value(context, self.datablock, self.props['lock'], self.lock)
            self.set_value(context, self.datablock, self.props['dir'], self.direction)
            self.set_value(context, self.datablock, self.props['angle'], self.original_angle)

    def sp_callback(self, context, event, state, sp):

        if self.sp_skip_update(state):
            return

        if state == 'CANCEL':
            self.cancel(context, event)
        else:
            self.update(context, event)

            if state != 'RUNNING':
                self.mouse_release(context, event)

        self.is_updating = False

    def update(self, context, event):

        pt = self.get_pos3d(context)
        c = self.arc.c
        v = 2 * self.arc.r * (pt - c).normalized()
        v0 = c - v
        v1 = c + v
        p0, p1 = intersect_line_sphere(v0, v1, c, self.arc.r)

        if p0 is not None and p1 is not None:

            if (p1 - pt).length < (p0 - pt).length:
                p0, p1 = p1, p0

            v = (p0 - self.arc.c).to_2d()

            if self.direction == 'RIGHT':
                v0, v1 = v, self.line_0.v.to_2d()
            else:
                v0, v1 = self.line_1.v.to_2d(), v

            da = v0.angle_signed(v1, 0)

            if event.shift:
                if event.ctrl:
                    da = round(da / pi * 36, 0) / 36 * pi
                else:
                    da = round(da / pi * 180, 0) / 180 * pi
            self.set_value(context, self.datablock, self.props['lock'], self.lock)
            self.set_value(context, self.datablock, self.props['dir'], self.direction)
            self.set_value(context, self.datablock, self.props['angle'], da)

    def draw_callback(self, _self, context, render=False):
        c, left, right, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.line_0.z_axis = normal
        self.line_1.z_axis = normal
        self.arc.z_axis = normal
        self.label_a.z_axis = normal
        self.origin = c
        self.line_0.p = c
        self.line_1.p = c
        self.arc.c = c
        self.line_0.v = left
        self.line_0.v = -left.length * self.line_0.cross.normalized()
        self.line_1.v = right
        self.line_1.v = right.length * self.line_1.cross.normalized()
        self.arc.a0 = self.line_0.angle
        self.arc.da = self.get_value(self.datablock, self.props['angle'])
        self.arc.r = 1.0
        dir = 1
        if "flip" in self.opts and self.opts['flip'] is True:
            dir = -dir

        self.handle_right.set_pos(context, self.line_1.lerp(1),
                                  self.line_1.sized_normal(1, -dir).v)
        self.handle_left.set_pos(context, self.line_0.lerp(1),
                                  self.line_0.sized_normal(1, dir).v)
        # self.handle_center.set_pos(context, self.arc.c, -self.line_0.v)
        label_value = self.arc.da
        if self.keyboard_input_active:
            label_value = self.label_value
        self.label_a.set_pos(context, label_value, self.arc.lerp(0.5), -self.line_0.v)
        self.arc.draw(context, render)
        self.line_0.draw(context, render)
        self.line_1.draw(context, render)
        self.handle_left.draw(context, render)
        self.handle_right.draw(context, render)
        # self.handle_center.draw(context, render)
        self.label_a.draw(context, render)
        self.feedback.draw(context, render)


class DumbAngleManipulator(DualAngleManipulator):
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        DualAngleManipulator.__init__(self, context, o, datablock, manipulator,
                                      handle_size, snap_callback, start_callback, end_callback)
        self.handle_right.draggable = False
        self.handle_left.draggable = False
        self.label_a.draggable = False

    def draw_callback(self, _self, context, render=False):
        c, left, right, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.line_0.z_axis = normal
        self.line_1.z_axis = normal
        self.arc.z_axis = normal
        self.label_a.z_axis = normal
        self.origin = c
        self.line_0.p = c
        self.line_1.p = c
        self.arc.c = c
        self.line_0.v = left
        self.line_0.v = -self.line_0.cross.normalized()
        self.line_1.v = right
        self.line_1.v = self.line_1.cross.normalized()

        # prevent ValueError in angle_signed
        if self.line_0.length == 0 or self.line_1.length == 0:
            return

        self.arc.a0 = self.line_0.angle
        self.arc.da = self.line_1.v.to_2d().angle_signed(self.line_0.v.to_2d())
        self.arc.r = 1.0
        dir = 1
        if "flip" in self.opts and self.opts['flip'] is True:
            dir = -1
        self.handle_right.set_pos(context, self.line_1.lerp(1),
                                  self.line_1.sized_normal(1, -dir).v)
        self.handle_left.set_pos(context, self.line_0.lerp(1),
                                 self.line_0.sized_normal(1, dir).v)
        # self.handle_center.set_pos(context, self.arc.c, -self.line_0.v)
        label_value = self.arc.da
        self.label_a.set_pos(context, label_value, self.arc.lerp(0.5), -self.line_0.v)
        self.arc.draw(context, render)
        self.line_0.draw(context, render)
        self.line_1.draw(context, render)
        self.handle_right.draw(context, render)
        self.handle_left.draw(context, render)
        # self.handle_center.draw(context, render)
        self.label_a.draw(context, render)
        self.feedback.draw(context, render)


class ArcAngleManipulator(Manipulator):
    """
        Manipulate angle of an arc
        when angle < 0 the arc center is on the left part of the circle
        when angle > 0 the arc center is on the right part of the circle
        bound to [-pi, pi]
    """

    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):

        # Fixed
        self.handle_left = SquareHandle(handle_size, arrow_size)
        # Angle
        self.handle_right = TriHandle(handle_size, arrow_size, draggable=True)
        self.handle_center = SquareHandle(handle_size, arrow_size)
        self.arc = GlArc()
        self.line_0 = GlLine()
        self.line_1 = GlLine()
        self.label_a = EditableText(handle_size, arrow_size, draggable=True)
        self.label_r = EditableText(handle_size, arrow_size, draggable=False)
        self.label_a.unit_type = 'ANGLE'
        Manipulator.__init__(self, context, o, datablock, manipulator, snap_callback, start_callback, end_callback)
        self.pts_mode = 'RADIUS'

    def check_hover(self):
        self.handle_right.check_hover(self.mouse_pos)
        self.label_a.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        if self.handle_right.hover:
            self.active = True
            self.original_angle = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.feedback.instructions(context, "Angle (degree)", "Drag to modify angle", [
                ('SHIFT', 'Round 1 degree'),
                ('CTRL+SHIFT', 'Round 5 degrees'),
                ('RIGHTCLICK or ESC', 'cancel')
                ])
            throttle_manip(callback=self.sp_callback)
            self.handle_right.active = True
            return True
        if self.label_a.hover:
            self.feedback.instructions(context, "Angle (degree)", "Use keyboard to modify angle",
                [('ENTER', 'validate'),
                ('RIGHTCLICK or ESC', 'cancel')])
            self.value_type = 'ROTATION'
            self.label_value = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.label_a.active = True
            self.keyboard_input_active = True
            return True
        if self.label_r.hover:
            self.feedback.instructions(context, "Radius", "Use keyboard to modify radius",
                [('ENTER', 'validate'),
                ('RIGHTCLICK or ESC', 'cancel')])
            self.value_type = 'LENGTH'
            self.label_r.active = True
            self.keyboard_input_active = True
            return True
        return False

    def mouse_release(self, context, event):
        self.check_hover()
        self.handle_right.active = False
        if self.active:
            self.active = False
            self.feedback.disable()
        return False

    def mouse_move(self, context, event):
        self.mouse_position(event)
        if self.handle_right.active:
            return False
        else:
            self.check_hover()
        return False

    def keyboard_done(self, context, event, value):
        self.set_value(context, self.datablock, self.manipulator.prop1_name, value)
        self.label_a.active = False
        self.label_r.active = False
        return True

    def keyboard_cancel(self, context, event):
        self.label_a.active = False
        self.label_r.active = False
        return False

    def cancel(self, context, event):
        if self.active:
            self.mouse_release(context, event)
            self.set_value(context, self.datablock, self.manipulator.prop1_name, self.original_angle)

    def update(self, context, event):

        pt = self.get_pos3d(context)
        c = self.arc.c

        v = 2 * self.arc.r * (pt - c).normalized()
        v0 = c - v
        v1 = c + v
        p0, p1 = intersect_line_sphere(v0, v1, c, self.arc.r)

        if p0 is not None and p1 is not None:
            # find nearest mouse intersection point
            if (p1 - pt).length < (p0 - pt).length:
                p0, p1 = p1, p0

            v = p0 - self.arc.c

            s = self.arc.tangeant(0, 1)
            res, d, t = s.point_sur_segment(pt)
            if d > 0:
                # right side
                a = self.arc.sized_normal(0, self.arc.r).angle
            else:
                a = self.arc.sized_normal(0, -self.arc.r).angle

            da = atan2(v.y, v.x) - a

            # bottom side +- pi
            if t < 0:
                # right
                if d > 0:
                    da = pi
                else:
                    da = -pi
            # top side bound to +- pi
            else:
                if da > pi:
                    da -= 2 * pi
                if da < -pi:
                    da += 2 * pi

            if event.shift:
                if event.ctrl:
                    da = round(da / pi * 36, 0) / 36 * pi
                else:
                    da = round(da / pi * 180, 0) / 180 * pi
            self.set_value(context, self.datablock, self.manipulator.prop1_name, da)

    def sp_callback(self, context, event, state, sp):

        if self.sp_skip_update(state):
            return

        if state == 'CANCEL':
            self.cancel(context, event)
        else:
            self.update(context, event)

            if state != 'RUNNING':
                self.mouse_release(context, event)

        self.is_updating = False

    def draw_callback(self, _self, context, render=False):
        # center : 3d points
        # left   : 3d vector pt-c
        # right  : 3d vector pt-c
        c, left, right, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.line_0.z_axis = normal
        self.line_1.z_axis = normal
        self.arc.z_axis = normal
        self.label_a.z_axis = normal
        self.label_r.z_axis = normal
        self.origin = c
        self.line_0.p = c
        self.line_1.p = c
        self.arc.c = c
        self.line_0.v = left
        self.line_1.v = right
        self.arc.a0 = self.line_0.angle
        self.arc.da = self.get_value(self.datablock, self.manipulator.prop1_name)
        self.arc.r = left.length
        self.handle_left.set_pos(context, self.line_0.lerp(1), self.line_0.v)
        self.handle_right.set_pos(context, self.line_1.lerp(1),
            self.line_1.sized_normal(1, -1 if self.arc.da > 0 else 1).v)
        self.handle_center.set_pos(context, self.arc.c, -self.line_0.v)
        label_a_value = self.arc.da
        label_r_value = self.arc.r
        if self.keyboard_input_active:
            if self.value_type == 'LENGTH':
                label_r_value = self.label_value
            else:
                label_a_value = self.label_value
        self.label_a.set_pos(context, label_a_value, self.arc.lerp(0.5), -self.line_0.v)
        self.label_r.set_pos(context, label_r_value, self.line_0.lerp(0.5), self.line_0.v)
        self.arc.draw(context, render)
        self.line_0.draw(context, render)
        self.line_1.draw(context, render)
        self.handle_left.draw(context, render)
        self.handle_right.draw(context, render)
        self.handle_center.draw(context, render)
        self.label_r.draw(context, render)
        self.label_a.draw(context, render)
        self.feedback.draw(context, render)


class ArcAngleRadiusManipulator(ArcAngleManipulator):
    """
        Manipulate angle and radius of an arc
        when angle < 0 the arc center is on the left part of the circle
        when angle > 0 the arc center is on the right part of the circle
        bound to [-pi, pi]
    """

    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        ArcAngleManipulator.__init__(self, context, o, datablock, manipulator,
                                     handle_size, snap_callback, start_callback, end_callback)
        self.handle_center = TriHandle(handle_size, arrow_size, draggable=True)
        self.label_r.draggable = True

    def check_hover(self):
        self.handle_right.check_hover(self.mouse_pos)
        self.handle_center.check_hover(self.mouse_pos)
        self.label_a.check_hover(self.mouse_pos)
        self.label_r.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        if self.handle_right.hover:
            self.active = True
            self.original_angle = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.feedback.instructions(context, "Angle (degree)", "Drag to modify angle", [
                ('SHIFT', 'Round value'),
                ('RIGHTCLICK or ESC', 'cancel')
                ])
            throttle_manip(callback=self.sp_callback)
            self.handle_right.active = True
            return True

        if self.handle_center.hover:
            self.active = True
            self.original_radius = self.get_value(self.datablock, self.manipulator.prop2_name)
            self.feedback.instructions(context, "Radius", "Drag to modify radius", [
                ('SHIFT', 'Round value'),
                ('RIGHTCLICK or ESC', 'cancel')
                ])
            throttle_manip(callback=self.sp_callback)
            self.handle_center.active = True
            return True

        if self.label_a.hover:
            self.feedback.instructions(context, "Angle (degree)", "Use keyboard to modify angle",
                [('ENTER', 'validate'),
                ('RIGHTCLICK or ESC', 'cancel')])
            self.value_type = 'ROTATION'
            self.label_value = self.get_value(self.datablock, self.manipulator.prop1_name)
            self.label_a.active = True
            self.keyboard_input_active = True
            return True

        if self.label_r.hover:
            self.feedback.instructions(context, "Radius", "Use keyboard to modify radius",
                [('ENTER', 'validate'),
                ('RIGHTCLICK or ESC', 'cancel')])
            self.value_type = 'LENGTH'
            self.label_r.active = True
            self.keyboard_input_active = True
            return True
        return False

    def mouse_release(self, context, event):
        self.check_hover()
        self.handle_right.active = False
        self.handle_center.active = False
        if self.active:
            self.active = False
            self.feedback.disable()
        return False

    def mouse_move(self, context, event):
        self.mouse_position(event)
        if self.handle_right.active or self.handle_center.active:
            return False
        else:
            self.check_hover()
        return False

    def keyboard_done(self, context, event, value):
        if self.value_type == 'LENGTH':
            self.set_value(context, self.datablock, self.manipulator.prop2_name, value)
            self.label_r.active = False
        else:
            self.set_value(context, self.datablock, self.manipulator.prop1_name, value)
            self.label_a.active = False
        return True

    def sp_callback(self, context, event, state, sp):

        if self.sp_skip_update(state):
            return

        if state == 'CANCEL':
            self.cancel(context, event)
        else:
            if self.handle_right.active:
                self.update(context, event)

            elif self.handle_center.active:
                self.update_radius(context, event)

            if state != 'RUNNING':
                self.mouse_release(context, event)

        self.is_updating = False

    def update_radius(self, context, event):
        pt = self.get_pos3d(context)
        c = self.arc.c
        left = self.line_0.lerp(1)
        p, t = intersect_point_line(pt, c, left)
        radius = (left - p).length
        if event.alt:
            if event.shift:
                radius = round(radius, 2)
            else:
                radius = round(radius, 1)
        self.set_value(context, self.datablock, self.manipulator.prop2_name, radius)

    def cancel(self, context, event):
        if self.handle_right.active:
            self.mouse_release(context, event)
            self.set_value(context, self.datablock, self.manipulator.prop1_name, self.original_angle)
        if self.handle_center.active:
            self.mouse_release(context, event)
            self.set_value(context, self.datablock, self.manipulator.prop2_name, self.original_radius)


class SnapVectorManipulator(Manipulator):
    """
        Move a child window or door in wall segment
        not limited to this by the way
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        self.handle = SquareHandle(handle_size, 1.2 * arrow_size, draggable=True, selectable=True)
        Manipulator.__init__(self, context, o, datablock, manipulator, snap_callback, start_callback, end_callback)
        self.feedback.instructions(context, "Move", "Drag to move", [
            ('CTRL', 'Snap'),
            ('SHIFT', 'Round value'),
            ('RIGHTCLICK or ESC', 'cancel')
            ])
        self.selectable = True

    def select(self, cursor_area):
        self.selected = self.selected or cursor_area.in_area(self.handle.pos_2d)
        self.handle.selected = self.selected

    def deselect(self, cursor_area):
        self.selected = not cursor_area.in_area(self.handle.pos_2d)
        self.handle.selected = self.selected

    def check_hover(self):
        self.handle.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        global gl_pts3d
        global manips
        if self.handle.hover:
            self.active = True
            self.handle.active = True
            self.original_location = self.get_value(self.datablock, self.manipulator.prop1_name).copy()
            self.feedback.enable()

            left, right, side, dz = self.manipulator.get_pts(self.o.matrix_world)
            dp = (right - left)
            dx = dp.normalized()
            dy = dz.cross(dx)
            p0 = left
            takemat = Matrix([
                [dx.x, dy.x, dz.x, p0.x],
                [dx.y, dy.y, dz.y, p0.y],
                [dx.z, dy.z, dz.z, p0.z],
                [0, 0, 0, 1]
            ])
            gl_pts3d = [p0]
            snap_point(takemat=takemat,
                callback=self.sp_callback,
                constraint_axis=(
                    self.manipulator.prop1_name == 'x',
                    self.manipulator.prop1_name == 'y',
                    self.manipulator.prop1_name == 'z'))
            return True
        return False

    def mouse_release(self, context, event):
        self.check_hover()
        self.feedback.disable()
        self.active = False
        self.handle.active = False
        return False

    def mouse_move(self, context, event):
        self.mouse_position(event)
        if self.handle.active:
            # False here to pass_through
            # print("i'm able to pick up mouse move event while transform running")
            return False
        else:
            self.check_hover()
        return False

    def sp_callback(self, context, event, state, sp):
        if self.sp_skip_update(state):
            return

        logger.debug("DeltaLocationManipulator.sp_callback")
        if state == 'CANCEL':
            self.cancel(context, event)
        else:
            global gl_pts3d
            p0 = gl_pts3d[0].copy()
            p1 = p0 + sp.delta
            itM = self.o.matrix_world.inverted()
            self.set_value(context, self.datablock, self.manipulator.prop1_name, itM @ p1)

            if state == 'SUCCESS':
                self.mouse_release(context, event)

        self.is_updating = False

        logger.debug("DeltaLocationManipulator.sp_callback done")

    def cancel(self, context, event):
        if self.active:
            self.mouse_release(context, event)
            # must move back to original location
            self.set_value(context, self.datablock, self.manipulator.prop1_name, self.original_location)

    def draw_callback(self, _self, context, render=False):
        left, right, side, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.handle.set_pos(context, left, (left - right).normalized(), normal=normal)
        self.handle.draw(context, render)
        self.feedback.draw(context, render)


class CallOperatorManipulator(Manipulator):
    """
        Call operator
        prop1_name = operator name without bpy.ops.
        prop2_name = json dict of named arguments
    """
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        self.handle = SquareHandle(handle_size, arrow_size, draggable=True)
        Manipulator.__init__(self, context, o, datablock, manipulator, snap_callback, start_callback, end_callback)

    def check_hover(self):
        self.handle.check_hover(self.mouse_pos)

    def mouse_press(self, context, event):
        if self.handle.hover:
            po = self.manipulator.prop1_name.split(".")
            params = self.from_json(self.manipulator.prop2_name)

            # try:
            op = getattr(getattr(bpy.ops, po[0]), po[1])
            if op.poll():
                op(**params)
            # except:
            #    pass
            self.handle.active = True
            return True
        return False

    def mouse_release(self, context, event):
        self.check_hover()
        self.handle.active = False
        return False

    def mouse_move(self, context, event):
        self.mouse_position(event)
        if self.handle.active:
            return True
        else:
            self.check_hover()
        return False

    def draw_callback(self, _self, context, render=False):
        """
            draw on screen feedback using gl.
        """
        logger.debug("CallOperatorManipulator.draw_callback")

        # won't render counter
        if render:
            return
        left, right, side, normal = self.manipulator.get_pts(self.o.matrix_world)
        self.handle.set_pos(context, left, (left - right).normalized(), normal=normal)
        self.handle.draw(context, render)
        logger.debug("CallOperatorManipulator.draw_callback done")


class CallAddOperatorManipulator(CallOperatorManipulator):
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        CallOperatorManipulator.__init__(self, context, o, datablock, manipulator,
                                         handle_size, snap_callback, start_callback, end_callback)
        self.handle = PlusHandle(handle_size, arrow_size, draggable=True)


class CallRemoveOperatorManipulator(CallOperatorManipulator):
    def __init__(self, context, o, datablock, manipulator, handle_size, snap_callback, start_callback, end_callback):
        CallOperatorManipulator.__init__(self, context, o, datablock, manipulator,
                                         handle_size, snap_callback, start_callback, end_callback)
        self.handle = CruxHandle(handle_size, arrow_size, draggable=True)


# ------------------------------------------------------------------
# Define a single Manipulator Properties to store on object
# ------------------------------------------------------------------


# Allow registeration of manipulators classes
manipulators_class_lookup = {}


def register_manipulator(type_key, manipulator_class):
    if type_key in manipulators_class_lookup.keys():
        raise RuntimeError("Manipulator of type {} allready exists, unable to override".format(type_key))
    manipulators_class_lookup[type_key] = manipulator_class


class archipack_manipulator(PropertyGroup):
    """
        A property group to add to manipulable objects
        type_key: type of manipulator
        prop1_name = the property name of object to modify
        prop2_name = another property name of object to modify (eg: angle and radius)
        p0, p1, p2 3d Vectors as base points to represent manipulators on screen
        normal Vector normal of plane on with draw manipulator
    """
    type_key: StringProperty(default='SIZE')

    # How 3d points are stored in manipulators ?
    # SIZE = 2 absolute positionned and a scaling vector
    # RADIUS = 1 absolute positionned (center) and 2 relatives (sides)
    # POLYGON = 2 absolute positionned and a relative vector (for rect polygons)

    pts_mode: StringProperty(default='SIZE')
    # prop names are either string or json
    prop1_name: StringProperty()
    prop2_name: StringProperty()
    p0: FloatVectorProperty(subtype='XYZ')
    p1: FloatVectorProperty(subtype='XYZ')
    p2: FloatVectorProperty(subtype='XYZ')
    # allow orientation of manipulators by default on xy plane,
    # but may be used to constrain heights on local object space
    normal: FloatVectorProperty(subtype='XYZ', default=(0, 0, 1))

    def set_pts(self, pts, normal=None):
        """
            set 3d location of gl points (in object space)
            pts: array of 3 vectors 3d
            normal: optionnal vector 3d default to Z axis

            @TODO:
            set those pts right in the stack holding a safer reference
            how to find the right manipulator in the stack ??
            -> manip idx
        """
        pts = [Vector(p) for p in pts]
        self.p0, self.p1, self.p2 = pts
        if normal is not None:
            self.normal = Vector(normal)

    def get_pts(self, tM):
        """
            convert points from local to world absolute
            to draw them at the right place
            tM : object's world matrix
        """
        rM = tM.to_3x3()
        if self.pts_mode in {'SIZE', 'POLYGON'}:
            return tM @ self.p0, tM @ self.p1, self.p2, rM @ self.normal
        else:
            return tM @ self.p0, rM @ self.p1, rM @ self.p2, rM @ self.normal

    def setup(self, context, o, datablock, snap_callback=None, start_callback=None, end_callback=None):
        """
            Factory return a manipulator object or None
            o:         object
            datablock: datablock to modify
            snap_callback: function call y
        """
        global arrow_size
        global handle_size
        prefs = get_prefs(context)
        arrow_size = prefs.arrow_size
        handle_size = prefs.handle_size

        global manipulators_class_lookup

        if self.type_key not in manipulators_class_lookup.keys() or \
                not manipulators_class_lookup[self.type_key].poll(context):
            # RuntimeError is overkill but may be enabled for debug purposes
            # Silentely ignore allow skipping manipulators if / when deps as not meet
            # manip stack will simply be filled with None objects
            # raise RuntimeError("Manipulator of type {} not found".format(self.type_key))
            return None

        m = manipulators_class_lookup[self.type_key](
            context,
            o,
            datablock,
            self,
            handle_size,
            snap_callback,
            start_callback,
            end_callback)

        # points storage model as described upside
        self.pts_mode = m.pts_mode
        return m

    # XXX DEPRICATED
    def as_dimension(self, context, o, dim):

        if dim is None:
            bpy.ops.archipack.dimension()
            dim = context.active_object
            dim.parent = o
            dim.matrix_world = o.matrix_world.copy()

        dim.select_set(state=True)

        v = Vector(self.p1) - Vector(self.p0)
        d = dim.data.archipack_dimension[0]
        d.auto_update = False
        d.size = (v).length
        x, y, z = self.p2.normalized()
        side = 1
        if x != 0:
            side = x
        elif y != 0:
            side = y
        elif z != 0:
            side = z
        d.distance = -side * self.p2.length
        d.auto_update = True
        dim.location = self.p0
        dim.rotation_euler.z = atan2(v.y, v.x)
        dim.select = False
        o.select_set(state=True)

        return dim


# ------------------------------------------------------------------
# Define Manipulable to make a PropertyGroup manipulable
# ------------------------------------------------------------------


class ARCHIPACK_OT_manipulate(ArchipackGenericOperator, Operator):
    bl_idname = "archipack.manipulate"
    bl_label = "Manipulate"
    bl_description = "Manipulate archipack objects (only work in object mode)"
    bl_options = {'INTERNAL'} #, 'UNDO'

    def execute(self, context):
        o = context.active_object
        res = {'CANCELLED'}
        d = self.datablock(o)
        if d:
            if hasattr(context, "area") and context.area is not None:
                context.area.tag_redraw()
            # print("ARCHIPACK_OT_manipulate.execute()", o.name)
            d.manipulable_invoke(context, o)
            res = {'FINISHED'}
        return res

    def invoke(self, context, event):
        return self.execute(context)


class ARCHIPACK_OT_manipulate_modal(Operator):
    bl_idname = "archipack.manipulate_modal"
    bl_label = "Manipulate"
    bl_description = "Manipulate"
    bl_options = {'INTERNAL'}
    bl_translation = "archipack"

    object_name: StringProperty(default="")

    def __init__(self):
        Operator.__init__(self)
        self.autosave_time=0
        self.lock = False

    def draw_callback(self, _self, context):
        # print("ARCHIPACK_OT_manipulate_modal.draw_callback()", id(self))
        lock = -1
        try:
            lock = int(_self.lock)
        except:
            pass
        if lock > -1:
            label = "Lock ({}) press SPACE to lock/unlock".format(['OFF', 'ON'][lock])
            _self.lock_txt.label = bpy.app.translations.pgettext(label, "archipack")
            _self.lock_txt.colour_inactive = [(0, 1, 0, 1), (1, 0, 0, 1)][lock]
            _self.lock_txt.draw(context, False)

    def exit(self, context):
        return

    @classmethod
    def poll(self, context):
        return context.active_object is not None

    def set_autosave_time(self, context):
        self.autosave_time = time.time() + 60.0 * context.preferences.filepaths.auto_save_time

    def autosave(self, context):
        if hasattr(context.window_manager, "archipack"):
            wm = context.window_manager.archipack
            filepath = bpy.data.filepath
            if wm.auto_save and time.time() > self.autosave_time and filepath != "":
                context.window.cursor_set("WAIT")
                # print("auto-save")
                file, ext = os.path.splitext(filepath)
                src_file = "{}.archipack_bak{}".format(file, ext)
                bpy.ops.wm.save_as_mainfile(copy=True, filepath=src_file)
                # print("auto-save done")
                # store next after save
                context.window.cursor_set("DEFAULT")
                self.set_autosave_time(context)

    def modal(self, context, event):
        global manips
        # Exit on stack change
        # handle multiple object stack
        # use object_name property to find manipulated object in stack
        # select and make object active
        # and exit when not found

        if context.area is None:
            # skip other areas
            return {'PASS_THROUGH'}

        # handle event only when occurs inside 3d view
        if USE_REGION_ITER:
            x, y = event.mouse_x, event.mouse_y
            for region in context.area.regions:
                if region.type == 'WINDOW':
                    if (x < region.x or
                            y < region.y or
                            x > region.width + region.x or
                            y > region.height + region.y):
                        return {'PASS_THROUGH'}

        context.area.tag_redraw()

        key = self.object_name

        o = context.scene.objects.get(key)

        if o is None or check_stack(key) or not o.visible_get():
            remove_manipulable(key)
            self.exit(context)
            return {'FINISHED', 'PASS_THROUGH'}

        self.autosave(context)

        # disable handler when another manipulator is active
        for k, manip in manips.items():
            if manip.active and k != key:
                return {'PASS_THROUGH'}

        if event.type == 'SPACE' and event.value == 'PRESS':
            self.lock = not self.lock
            return {'RUNNING_MODAL'}

        res = manips[key].manipulable.manipulable_modal(context, event, o, self.lock)

        # manip may be removed from manips by modal
        if key in manips:
            # flag as active to disallow other manipulators modal handler
            manips[key].active = 'RUNNING_MODAL' in res

        if check_stack(key) and 'FINISHED' not in res:
            # ensure modal exit on manipulable_refresh
            res = {'FINISHED'}

        if 'FINISHED' in res or 'CANCELLED' in res:
            self.exit(context)

        return res

    def invoke(self, context, event):
        if context.space_data is not None and context.space_data.type == 'VIEW_3D':
            if context.area is not None:
                context.area.tag_redraw()
            # next autosave in seconds
            self.set_autosave_time(context)

            self.lock_txt = GlText(d=2, font_size=12)
            self.lock_txt.set_pos(context, None, Vector((20, 150)), Vector((1, 0, 0)))

            global manips
            for m in manips[self.object_name].stack:
                if m.__class__.__name__ == 'DualSnapSizeManipulator':
                    args = (self, context)
                    manips[self.object_name].draw_handler = bpy.types.SpaceView3D.draw_handler_add(self.draw_callback, args, 'WINDOW', 'POST_PIXEL')
                    break

            context.window_manager.modal_handler_add(self)
            return {'RUNNING_MODAL'}
        else:
            print(context.space_data)
            if context.space_data is not None:
                print(context.space_data.type)
                
            self.report({'WARNING'}, "manipulate_modal Active space must be a View3d")
            return {'CANCELLED'}

class ARCHIPACK_OT_disable_manipulate(Operator):
    bl_idname = "archipack.disable_manipulate"
    bl_label = "Disable Manipulate"
    bl_description = "Disable any active manipulator"
    bl_options = {'INTERNAL'} #, 'UNDO'

    @classmethod
    def poll(self, context):
        return True

    def execute(self, context):
        empty_stack()
        return {'FINISHED'}


class Manipulable:
    """
        A class extending PropertyGroup to setup gl manipulators
        Beware : prevent crash calling manipulable_disable()
                 before changing manipulated data structure
    """
    manipulators: CollectionProperty(
        type=archipack_manipulator,
        # options={'SKIP_SAVE'},
        # options={'HIDDEN'},
        description="store 3d points to draw gl manipulators"
    )
    manipulable_refresh: BoolProperty(
        default=False,
        options={'SKIP_SAVE'},
        description="Flag enable to rebuild manipulators when data model change"
    )

    # as pure python property to keep reference valid on update

    select_mode: BoolProperty(
        default=False,
        options={'SKIP_SAVE'},
        description="Flag select state so we are able to toggle"
    )
    manipulable_selectable: BoolProperty(
        default=False,
        options={'SKIP_SAVE'},
        description="Flag make manipulators selectable"
    )
    keymap = None
    manipulable_area = GlCursorArea()
    manipulable_start_point = Vector((0, 0))
    manipulable_end_point = Vector((0, 0))
    manipulable_draw_handler = None
    manip_stack = []

    def setup_manipulators(self):
        """
            Must implement manipulators creation
            TODO: call from update and manipulable_setup
        """
        raise NotImplementedError

    def manipulable_draw_callback(self, _self, context):
        self.manipulable_area.draw(context)

    def manipulable_enable(self, o):
        self.manip_stack = add_manipulable(o.name, self)

    def manipulable_disable(self, o):
        """
            disable gl draw handlers
        """
        if o is not None:
            remove_manipulable(o.name)

        self.manipulable_exit_selectmode()
        self.select_mode = False
        # self.manipulate_mode = False

    def manipulable_exit_selectmode(self):
        self.manipulable_area.disable()
        self.select_mode = False
        # remove select draw handler
        if self.manipulable_draw_handler is not None:
            bpy.types.SpaceView3D.draw_handler_remove(
                self.manipulable_draw_handler,
                'WINDOW')
        self.manipulable_draw_handler = None

    def manipulable_setup(self, context, o):
        """
            TODO: Implement the setup part as per parent object basis
        """

        self.setup_manipulators()

        for m in self.manipulators:
            self.manip_stack.append(m.setup(context, o, self))

    def _manipulable_invoke(self, context, o):

        # disallow manipulate in other mode than object
        # object_name = o.name

        # copy context so manipulator always use
        # invoke time context
        ctx = context.copy()

        # take care of context switching
        # when call from outside of 3d view
        if context.space_data is None or context.space_data.type != 'VIEW_3D':
            for window in context.window_manager.windows:
                screen = window.screen
                for area in screen.areas:
                    if area.type == 'VIEW_3D':
                        ctx['area'] = area
                        for region in area.regions:
                            if region.type == 'WINDOW':
                                ctx['region'] = region
                        break

        if ctx is not None:
            logger.debug("_manipulable_invoke %s", o.name)
            # self.manipulate_mode = True
            bpy.ops.archipack.manipulate_modal(ctx, 'INVOKE_DEFAULT', object_name=o.name)

        else:
            self.manipulable_disable(o)

    def manipulable_invoke(self, context, o):
        """
            call this in operator invoke()
            NB:
            if override dont forget to call:
                _manipulable_invoke(context)
        """
        if o is None or manipulate_mode(o.name):
            self.manipulable_disable(o)
            return False

        if context.mode != 'OBJECT':
            return False

        self.manipulable_enable(o)
        self.manipulable_setup(context, o)
        self._manipulable_invoke(context, o)

        self.manip_stack = None

        return True

    def manipulable_modal(self, context, event, o, lock):
        """
            call in operator modal()
            should not be overriden
            as it provide all needed
            functionnality out of the box
        """
        global manips
        # setup again when manipulators type change
        if self.manipulable_refresh:
            # print("manipulable_modal: manipulable_refresh=True")
            self.manipulable_refresh = False
            self.manipulable_disable(o)
            self.manipulable_enable(o)
            self.manipulable_setup(context, o)
            # cleanup manip_stack
            self.manip_stack = None

        if context.area is None or context.mode != 'OBJECT':
            self.manipulable_disable(o)
            self.manipulable_exit(o)
            return {'FINISHED'}

        context.area.tag_redraw()

        if self.keymap is None:
            self.keymap = Keymaps(context)

        if self.keymap.check(event, self.keymap.undo):
            # user feedback on undo by disabling manipulators
            self.manipulable_disable(o)
            self.manipulable_exit(o)
            # pass through so system is able to undo
            return {'FINISHED', 'PASS_THROUGH'}

        # clean up manipulator on delete
        if self.keymap.check(event, self.keymap.delete):  # {'X'}:
            if bpy.ops.archipack.delete.poll():
                res = bpy.ops.archipack.delete('INVOKE_DEFAULT')
                if res == {'FINISHED'}:
                    return {'FINISHED'}

            return {'RUNNING_MODAL'}

        """
        # handle keyborad for select mode
        if self.select_mode:
            if event.type in {'A'} and event.value == 'RELEASE':
                return {'RUNNING_MODAL'}
        """
        # let timer and none events pass through to allow changes on ui
        if 'TIMER' in event.type or event.type == 'NONE':
            # print(event.type)
            return {'PASS_THROUGH'}

        global manips
        for manipulator in manips[o.name].stack:
            # manipulator should return false on left mouse release
            # so proper release handler is called
            # and return true to call manipulate when required
            # print("manipulator:%s" % manipulator)
            # TODO: let modal return {'RUNNING_MODAL', 'PASS_THROUGH', 'FINISHED'}
            # @NOTE: should also return None
            if manipulator is not None:
                manipulator.lock = lock

                if manipulator.is_updating:
                    print("manipulator.is_updating")
                    return {"RUNNING_MODAL"}

                if manipulator.modal(context, event):
                    # print("Manipulable modal: %s" % (type(manipulator).__name__))
                    self.manipulable_manipulate(context, event, manipulator)
                    return {'RUNNING_MODAL'}

        # print("Manipulable %s %s" % (event.type, event.value))

        # Manipulators are not active so check for selection
        if self.manipulable_selectable:

            # print("Manipulable %s %s" % (event.type, event.value))

            if event.type == 'LEFTMOUSE':

                # either we are starting select mode
                # user press on area not over maniuplator
                # Prevent 3 mouse emultation to select when alt pressed
                if event.value == 'PRESS' and not event.alt:
                    self.select_mode = True
                    self.manipulable_area.enable()
                    self.manipulable_start_point = Vector((event.mouse_region_x, event.mouse_region_y))
                    self.manipulable_area.set_location(
                        context,
                        self.manipulable_start_point,
                        self.manipulable_start_point)
                    # add a select draw handler
                    args = (self, context)
                    self.manipulable_draw_handler = bpy.types.SpaceView3D.draw_handler_add(
                        self.manipulable_draw_callback,
                        args,
                        'WINDOW',
                        'POST_PIXEL')
                    # don't keep focus
                    # as this prevent click over ui
                    # return {'RUNNING_MODAL'}

                elif event.value == 'RELEASE':
                    if self.select_mode:
                        # confirm selection

                        self.manipulable_exit_selectmode()

                        # keep focus
                        # return {'RUNNING_MODAL'}

                    else:
                        # allow manipulator action on release
                        for manipulator in manips[o.name].stack:
                            if manipulator is not None and manipulator.selectable:
                                manipulator.selected = False
                        self.manipulable_release(context)

            elif self.select_mode and event.type == 'MOUSEMOVE' and event.value == 'PRESS':
                # update select area size
                self.manipulable_end_point = Vector((event.mouse_region_x, event.mouse_region_y))
                self.manipulable_area.set_location(
                    context,
                    self.manipulable_start_point,
                    self.manipulable_end_point)
                if event.shift:
                    # deselect
                    for manipulator in manips[o.name].stack:
                        if manipulator is not None and manipulator.selected:
                            manipulator.deselect(self.manipulable_area)
                else:
                    # select / more
                    for manipulator in manips[o.name].stack:
                        if manipulator is not None and manipulator.selectable:
                            manipulator.select(self.manipulable_area)

                # keep focus to prevent left select mouse to actually move object
                # if self.keymap.mouse_select == 'LEFT':
                # 2.8 must keep focus whatever to prevent other tools from launch
                return {'RUNNING_MODAL'}

        # event.alt here to prevent 3 button mouse emulation exit while PLUSg
        if event.type in {'RIGHTMOUSE', 'ESC'} and event.value == 'PRESS' and not event.alt:
            self.manipulable_disable(o)
            self.manipulable_exit(o)

            # Pass through so other active manipulables also exit
            return {'FINISHED', 'PASS_THROUGH'}

        return {'PASS_THROUGH'}

    # Callbacks
    def manipulable_release(self, context):
        """
            Override with action to do on mouse release
            eg: big update
        """
        return

    def manipulable_exit(self, o):
        """
            Override with action to do when modal exit
        """
        return

    def manipulable_manipulate(self, context, event, manipulator):
        """
            Override with action to do when a handle is active (pressed and mousemove)
        """
        return


def disable_auto_manipulate(ctx):
    bpy.ops.archipack.disable_manipulate(ctx)
    return None


def auto_manipulate(ctx):
    disable_auto_manipulate(ctx)
    bpy.ops.archipack.manipulate(ctx)
    return None


def filter_obj(o):
    if o and o.data:
        for key in o.data.keys():
            if "archipack_" in key:
                d = getattr(o.data, key)[0]
                return hasattr(d, "manipulators")
    return False


prevent_overflow = False


@persistent
def depsgraph_select_post(none):

    # auto_manipulate trigger a depsgraph.update()
    # so this prevent stack overflow by recursive calls
    global prevent_overflow

    if prevent_overflow:
        return

    context = bpy.context

    if hasattr(context, "active_object") and hasattr(context, "window_manager"):
        o = context.active_object

        if (o is not None and o.name == "Archipack_snap_helper") or \
                (hasattr(context.window_manager, "archipack") and
                not context.window_manager.archipack.auto_manipulate):
            return

        if o is not None and (o.select_get() and o.visible_get() and filter_obj(o)):

            if manipulate_mode(o.name):
                # print("depsgraph_select_post.refresh() skip :", o.name)
                return

            if bpy.ops.archipack.manipulate.poll():
                # print("depsgraph_select_post.refresh()", o.name)
                ctx = context.copy()
                ctx['object'] = o
                ctx['selected_objects'] = [o]
                prevent_overflow = True
                auto_manipulate(ctx)
                prevent_overflow = False

        else:
            empty_stack()


@persistent
def cleanup(dummy=None):
    empty_stack()


def register():
    # Register default manipulators
    global manips
    global manipulators_class_lookup
    manipulators_class_lookup = {}
    manips = {}
    register_manipulator('SIZE', SizeManipulator)
    register_manipulator('SIZE_LOC', SizeLocationManipulator)
    register_manipulator('ANGLE', AngleManipulator)
    register_manipulator('DUMB_ANGLE', DumbAngleManipulator)
    register_manipulator('ARC_ANGLE_RADIUS', ArcAngleRadiusManipulator)
    register_manipulator('COUNTER', CounterManipulator)
    register_manipulator('DUMB_SIZE', DumbSizeManipulator)
    register_manipulator('DELTA_LOC', DeltaLocationManipulator)
    register_manipulator('DUMB_STRING', DumbStringManipulator)
    # resize in 2 directions prop2_name in ['LEFT', 'RIGHT']
    register_manipulator('DUAL_SIZE', DualSnapSizeManipulator)
    register_manipulator('DUAL_ANGLE', DualAngleManipulator)
    # snap aware size loc
    register_manipulator('SNAP_SIZE_LOC', SnapSizeLocationManipulator)
    # register_manipulator('SNAP_POINT', SnapPointManipulator)
    # wall's line based object snap
    register_manipulator('WALL_SNAP', WallSnapManipulator)
    # line snap manipulator disable parts update
    register_manipulator('LINE_SNAP', LineSnapManipulator)
    # Dimension manipulator (vector in object space)
    register_manipulator('SNAP_VEC', SnapVectorManipulator)
    # Add / remove operator
    register_manipulator('OP_ADD', CallAddOperatorManipulator)
    register_manipulator('OP_REM', CallRemoveOperatorManipulator)

    bpy.utils.register_class(ARCHIPACK_OT_manipulate_modal)
    bpy.utils.register_class(ARCHIPACK_OT_manipulate)
    bpy.utils.register_class(ARCHIPACK_OT_disable_manipulate)
    bpy.utils.register_class(archipack_manipulator)
    bpy.utils.register_class(ARCHIPACK_OT_manipulate_throttle)
    depsgraph_update_post.append(depsgraph_select_post)
    load_pre.append(cleanup)


def unregister():
    global manips
    global manipulators_class_lookup
    empty_stack()
    del manips
    manipulators_class_lookup.clear()
    del manipulators_class_lookup
    bpy.utils.unregister_class(ARCHIPACK_OT_manipulate_modal)
    bpy.utils.unregister_class(ARCHIPACK_OT_manipulate)
    bpy.utils.unregister_class(ARCHIPACK_OT_disable_manipulate)
    bpy.utils.unregister_class(archipack_manipulator)
    bpy.utils.unregister_class(ARCHIPACK_OT_manipulate_throttle)
    depsgraph_update_post.remove(depsgraph_select_post)
    load_pre.remove(cleanup)
