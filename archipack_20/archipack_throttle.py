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
import bpy
import time
from bpy.app.handlers import persistent
from bpy.types import Operator
from mathutils import Vector
from .archipack_abstraction import ensure_select_and_restore
from .archipack_i18n import Archipacki18n
from .archipack_object import ArchipackObjectsManager
from .archipack_gl import GlText
from .archipack_prefs import get_prefs


class ThrottleObject(ArchipackObjectsManager):
    """
     Represent an object waiting for update
    """
    __slots__ = ('update_func', 'datablock', 'duration', 'waiting', 'timeout')

    def __init__(self, datablock, duration, update_func):
        d = None
        try:
            d = datablock.__class__.datablock
        except:
            pass
        self.update_func = update_func
        self.datablock = d
        self.duration = duration
        self.waiting = True
        self.timeout = 10

    def set_timeout(self, now):
        if self.waiting:
            self.timeout = now + self.duration

    def update(self, context, name, now):
        if now < self.timeout:
            return False
        
        o = self.get_scene_object(context, name)
        with ensure_select_and_restore(context, o, [o]) as (ctx, act, sel):
            d = self.datablock(act)
            # print("ThrottleObject.update()", act.name)
            if d:
                context.area.tag_redraw()
                self.waiting = False
                getattr(d, self.update_func)(context)

        return True
       

class ThrottleHandler():

    __slots__ = ('stack')
    """
     Throtteling core
     Provide throtteling update ability for complex objects
     Run throttle modal and update objects when needed
     
     @TODO: modal MUST prevent undo
     
     Objects throttling implementation:

     from .archipack_throttle import throttle

     - must call throttle.add(context, o, self)
        on top of .update

     - must use throttle.active state
        to provide degenerated geometry on .update
    """
    def __init__(self):
        self.stack = {}

    def add(self, context, o, d, duration=-1, update_func="update"):

        if o is None or d is None:
            return

        prefs = get_prefs(context)
        
        if bpy.app.background or not prefs.throttle_enable:
            return
        
        if duration < 0:
            duration = prefs.throttle_delay

        now = time.time()
        start = not self.active

        if o.name not in self.stack:
            self.stack[o.name] = ThrottleObject(d, duration, update_func)

        for name, item in self.stack.items():
            if item.waiting:
                item.set_timeout(now)

        if start and self.active:
            bpy.ops.archipack.throttle_update('INVOKE_DEFAULT')

    def is_active(self, name):
        return (self.active
                and name in self.stack
                and self.stack[name].waiting)

    @property
    def waiting_objects(self):
        return len([o for o in self.stack.values() if o.waiting])

    @property
    def active(self):
        return len(self.stack) > 0

    def update(self, context):
        
        now = time.time()
        to_remove = set()
        for name, u in self.stack.items():
            if u.update(context, name, now):
                to_remove.add(name)

        for name in to_remove:
            del self.stack[name]

        return not self.active

    def clear(self):
        self.stack.clear()


throttle = ThrottleHandler()


"""
from archipack import archipack_throttle
archipack_throttle.register()
throttle = archipack_throttle.throttle
throttle.add(C, C.object, None)
"""


@persistent
def cleanup(dummy):
    global throttle
    throttle.clear()


_draw_handler = None
_timer = None


def exit(context):
    global _draw_handler
    if _draw_handler is not None:
        bpy.types.SpaceView3D.draw_handler_remove(_draw_handler, 'WINDOW')
        _draw_handler = None
    global _timer
    if _timer is not None:
        context.window_manager.event_timer_remove(_timer)
        _timer = None


class ARCHIPACK_OT_throttle_update(Operator):
    bl_idname = "archipack.throttle_update"
    bl_label = "Update objects after a delay"
    bl_options = {'INTERNAL'}


    def draw_handler(self, _self, context):
        global throttle
        label = bpy.app.translations.pgettext("objects waiting for update", "archipack")
        try:
            self.text.label = "Archipack {} {}".format(throttle.waiting_objects, label)
            self.text.draw(context)
        except:
            exit(context)
            pass

    def modal(self, context, event):
        global throttle
        context.area.tag_redraw()
        if event.type == 'TIMER' and throttle.update(context):
            exit(context)
            return {'PASS_THROUGH', 'FINISHED'}

        return {'PASS_THROUGH'}

    def invoke(self, context, event):

        global _draw_handler
        global _timer

        self.text = GlText(label="Throttle", colour=(0, 1, 0, 1), d=2, font_size=12)
        self.text.set_pos(context, None, Vector((20, 120)), Vector((1, 0, 0)))

        args = (self, context)
        _draw_handler = bpy.types.SpaceView3D.draw_handler_add(
                self.draw_handler,
                args,
                'WINDOW',
                'POST_PIXEL')

        wm = context.window_manager
        _timer = wm.event_timer_add(0.1, window=context.window)

        wm.modal_handler_add(self)
        return {'RUNNING_MODAL'}


def register():
    bpy.utils.register_class(ARCHIPACK_OT_throttle_update)
    bpy.app.handlers.load_pre.append(cleanup)


def unregister():
    global throttle
    throttle.clear()
    bpy.utils.unregister_class(ARCHIPACK_OT_throttle_update)
    bpy.app.handlers.load_pre.remove(cleanup)
