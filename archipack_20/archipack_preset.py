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

import os
import subprocess
# ----------------------------------------------------------
# Author: Stephen Leger (s-leger)
#
# ----------------------------------------------------------
import bpy
# from bl_operators.presets import AddPresetBase
from bpy.props import StringProperty, BoolProperty
from bpy.types import Operator

from mathutils import Vector
from .archipack_i18n import Archipacki18n
from .archipack_object import ArchipackObjectsManager
from .archipack_gl import (
    ThumbHandle, Screen, GlRect,
    GlPolyline, GlPolygon, GlText, GlHandle
)
from .archipack_jsonio import ArchipackJsonIO as jsonio
from .archipack_iconmanager import icons
from .archipack_prefs import get_prefs
preset_paths = bpy.utils.script_paths("presets")
addons_paths = bpy.utils.script_paths("addons")
# Global disallow multiple instance of operator
running = None


class CruxHandle(GlHandle):

    def __init__(self, sensor_size, depth):
        GlHandle.__init__(self, sensor_size, 0, True, False, n_pts=0)
        self.branch_0 = GlPolygon((1, 1, 1, 1), d=2, n_pts=4)
        self.branch_1 = GlPolygon((1, 1, 1, 1), d=2, n_pts=4)
        self.depth = depth

    def set_pos(self, pos_2d):
        self.pos_2d = pos_2d
        o = pos_2d
        w = 0.5 * self.sensor_width
        d = self.depth
        c = d / 1.4242
        s = w - c
        x = Vector((1, 0))
        y = Vector((0, 1))
        xs = x * s
        xw = x * w
        ys = y * s
        yw = y * w
        p0 = o + xs + yw
        p1 = o + xw + ys
        p2 = o - xs - yw
        p3 = o - xw - ys
        p4 = o - xs + yw
        p5 = o + xw - ys
        p6 = o + xs - yw
        p7 = o - xw + ys

        self.branch_0.set_pos([p0, p1, p2, p3])
        self.branch_1.set_pos([p4, p5, p6, p7])
        
    @property
    def pts(self):
        return [self.pos_2d]

    @property
    def sensor_center(self):
        return self.pos_2d

    def draw(self, context, render=False):
        self.render = render
        self.branch_0.colour_inactive = self.colour
        self.branch_1.colour_inactive = self.colour
        self.branch_0.draw(context)
        self.branch_1.draw(context)
        

class SeekBox(GlText, GlHandle):
    """
        Text input to filter items by label
        TODO:
            - add cross to empty text
            - get text from keyboard
    """

    def __init__(self):
        GlHandle.__init__(self, 0, 0, True, False, d=2, n_pts=0)
        GlText.__init__(self, d=2)
        self.sensor_width = 250
        self.pos_3d = Vector((0, 0))
        self.bg = GlRect(colour=(0, 0, 0, 0.7))
        self.frame = GlPolyline((1, 1, 1, 1), d=2, n_pts=4)
        self.frame.is_closed = True
        self.cancel = CruxHandle(16, 4)
        self.line_pos = 0
    
    @property
    def colour(self):
        return (1, 1, 1, 1)
        
    @property
    def pts(self):
        return [self.pos_3d]

    def set_pos(self, context, pos_2d, width):
        x, ty = self.text_size(context)
        # w = self.sensor_width
        w = min(self.sensor_width, width)
        y = 12
        pos_2d.y += y
        pos_2d.x -= 0.5 * w
        self.pos_2d = pos_2d.copy()
        self.pos_3d = pos_2d.copy()
        self.pos_3d.x += 6
        self.sensor_height = y
        p0 = pos_2d + Vector((w, -0.5 * y))
        p1 = pos_2d + Vector((w, 1.5 * y))
        p2 = pos_2d + Vector((0, 1.5 * y))
        p3 = pos_2d + Vector((0, -0.5 * y))
        self.bg.set_pos([p0, p2])
        self.frame.set_pos([p0, p1, p2, p3])
        self.cancel.set_pos(pos_2d + Vector((w + 15, 0.5 * y)))

    def keyboard_entry(self, context, event):
        c = event.ascii
        if c:
            if c == ",":
                c = "."
            self.label = self.label[:self.line_pos] + c + self.label[self.line_pos:]
            self.line_pos += 1

        if self.label:
            if event.type == 'BACK_SPACE':
                self.label = self.label[:self.line_pos - 1] + self.label[self.line_pos:]
                self.line_pos -= 1

            elif event.type == 'DEL':
                self.label = self.label[:self.line_pos] + self.label[self.line_pos + 1:]

            elif event.type == 'LEFT_ARROW':
                self.line_pos = (self.line_pos - 1) % (len(self.label) + 1)

            elif event.type == 'RIGHT_ARROW':
                self.line_pos = (self.line_pos + 1) % (len(self.label) + 1)

    def draw(self, context):
        self.bg.draw(context)
        self.frame.draw(context)
        self.cancel.draw(context)
        GlText.draw(self, context)
        
    @property
    def sensor_center(self):
        return self.pos_3d


class PresetMenuItem():

    def __init__(self, thumbsize, preset, image):
        name = bpy.path.display_name_from_filepath(preset)
        self.preset = preset
        self.image = image
        self.image.gl_load()
        self.handle = ThumbHandle(thumbsize, name, image.bindcode, draggable=True)
        self.enable = True

    def filter(self, keywords):
        for key in keywords:
            if key not in self.handle.label.label:
                return False
        return True

    def cleanup(self):
        self.image.gl_free()

    def set_pos(self, context, pos):
        self.handle.set_pos(context, pos)

    def check_hover(self, mouse_pos):
        self.handle.check_hover(mouse_pos)
        return self.handle.hover

    def mouse_press(self):
        if self.handle.hover:
            # print("PresetMenuItem.press()", self.handle.hover, self.preset)
            self.handle.hover = False
            self.handle.active = True
            return True
        return False

    def draw(self, context):
        if self.enable:
            self.handle.draw(context)


class PresetMenu:

    keyboard_type = {
            'BACK_SPACE', 'DEL',
            'LEFT_ARROW', 'RIGHT_ARROW'
            }

    def __init__(self, context, category, thumbsize=Vector((150, 100)), format='json'):

        prefs = get_prefs(context)
        self.thumbsize = Vector((prefs.thumbsize, int(prefs.thumbsize * 2 / 3)))

        _icons = icons.menuitems(category, format)
        self.menuItems = [
            PresetMenuItem(self.thumbsize, full_path, image)
            for full_path, label, image in _icons
            ]

        self.margin = 50
        self.y_scroll = 0
        self.scroll_max = 1000
        self.spacing = Vector((25, 25))
        self.screen = Screen(self.margin)
        self.mouse_pos = Vector((0, 0))
        self.bg = GlRect(colour=(0, 0, 0, 0.7))
        self.border = GlPolyline((0.7, 0.7, 0.7, 1), d=2, n_pts=4)
        self.keywords = SeekBox()
        self.keywords.colour_normal = (1, 1, 1, 1)

        self.border.is_closed = True
        self.set_pos(context)

    def clearImages(self):
        for item in self.menuItems:
            item.cleanup()
        self.menuItems.clear()

    @property
    def is_empty(self):
        return len(self.menuItems) == 0

    def set_pos(self, context):

        x_min, x_max, y_min, y_max = self.screen.size(context)
        p0, p1, p2, p3 = Vector((x_min, y_min)), Vector((x_min, y_max)), Vector((x_max, y_max)), Vector((x_max, y_min))
        self.keywords.set_pos(context, p1 + 0.5 * (p2 - p1), x_max - x_min)
        self.bg.set_pos([p0, p2])
        self.border.set_pos([p0, p1, p2, p3])
        x_min += 0.5 * self.thumbsize.x + 0.5 * self.margin
        x_max -= 0.5 * self.thumbsize.x + 0.5 * self.margin
        y_max -= 0.5 * self.thumbsize.y + 0.5 * self.margin
        y_min += 0.5 * self.margin
        x = x_min
        y = y_max + self.y_scroll
        n_rows = 0

        keywords = self.keywords.label.split(" ")

        for item in self.menuItems:
            if y > y_max or y < y_min:
                item.enable = False
            else:
                item.enable = True

            # filter items by name
            if len(keywords) > 0 and not item.filter(keywords):
                item.enable = False
                continue

            item.set_pos(context, Vector((x, y)))
            x += self.thumbsize.x + self.spacing.x
            if x > x_max:
                n_rows += 1
                x = x_min
                y -= self.thumbsize.y + self.spacing.y

        self.scroll_max = max(0, n_rows - 1) * (self.thumbsize.y + self.spacing.y)

    def draw(self, context):
        self.bg.draw(context)
        self.border.draw(context)
        for item in self.menuItems:
            item.draw(context)
        self.keywords.draw(context)
        
    def mouse_press(self, context, event):
        self.mouse_position(event)
        if self.keywords.cancel.hover:
            self.keywords.label = ""
            self.keywords.line_pos = 0
            self.set_pos(context)

        for item in self.menuItems:
            if item.enable and item.mouse_press():
                # load item preset
                return item.preset
        return None

    def mouse_position(self, event):
        self.mouse_pos.x, self.mouse_pos.y = event.mouse_region_x, event.mouse_region_y

    def mouse_move(self, context, event):
        self.mouse_position(event)
        self.keywords.check_hover(self.mouse_pos)
        self.keywords.cancel.check_hover(self.mouse_pos)
        if any([h.check_hover(self.mouse_pos) for h in self.menuItems]):
            context.window.cursor_set("HAND")
        else:
            context.window.cursor_set("DEFAULT")

    def scroll_up(self, context, event):
        self.y_scroll = max(0, self.y_scroll - (self.thumbsize.y + self.spacing.y))
        self.set_pos(context)
        # print("scroll_up %s" % (self.y_scroll))

    def scroll_down(self, context, event):
        self.y_scroll = min(self.scroll_max, self.y_scroll + (self.thumbsize.y + self.spacing.y))
        self.set_pos(context)
        # print("scroll_down %s" % (self.y_scroll))

    def keyboard_entry(self, context, event):
        self.keywords.keyboard_entry(context, event)
        self.set_pos(context)


class PresetMenuOperator(ArchipackObjectsManager):
    bl_options = {'INTERNAL'}

    preset_operator: StringProperty(
        default="archipack.preset_loader"
    )
    cursor_location = None

    def __init__(self):
        self.menu = None
        self._handle = None
        self.disable = False

    def args(self, preset):
        """ Allow to pass preset operator parameters as dict
        must at least return {'filepath': preset}
        :param preset: preset full file path
        :return: dict
        """
        return {'filepath': preset}

    def exit(self, context):
        if not self.disable:
            global running
            running = None
        self.menu.clearImages()
        bpy.types.SpaceView3D.draw_handler_remove(self._handle, 'WINDOW')

    def draw_handler(self, _self, context):
        self.menu.draw(context)

    def modal(self, context, event):
        context.area.tag_redraw()
        if self.menu is None:
            if not self.disable:
                global running
                running = None
            return {'FINISHED'}

        if event.type == 'MOUSEMOVE':
            self.menu.mouse_move(context, event)
        elif event.type == 'WHEELUPMOUSE' or \
                (event.type == 'UP_ARROW' and event.value == 'PRESS'):
            self.menu.scroll_up(context, event)
        elif event.type == 'WHEELDOWNMOUSE' or \
                (event.type == 'DOWN_ARROW' and event.value == 'PRESS'):
            self.menu.scroll_down(context, event)
        elif event.type == 'LEFTMOUSE':
            if event.value == 'RELEASE':
                preset = self.menu.mouse_press(context, event)
                if preset is not None:
                    self.exit(context)
                    po = self.preset_operator.split(".")
                    op = getattr(getattr(bpy.ops, po[0]), po[1])
                    args = self.args(preset)

                    if self.preset_operator in {'archipack.preset_loader'}:
                        args['menu_idname'] = self.bl_idname

                        # call from preset menu
                        # ensure right active_object class
                        act = context.active_object
                        sel = context.selected_objects
                        for o in sel:
                            d = None
                            if o.data and self.preset_subdir in o.data:
                                d = getattr(o.data, self.preset_subdir)[0]
                            elif self.preset_subdir in o:
                                d = getattr(o, self.preset_subdir)[0]

                            if d is not None:
                                self.select_object(context, o, True)
                                d.auto_update = False
                                # print("Archipack execute_preset loading auto_update:%s" % d.auto_update)
                                op('INVOKE_DEFAULT', **args)
                                # print("Archipack execute_preset loaded  auto_update: %s" % d.auto_update)
                                d.auto_update = True

                        self.select_object(context, act, True)
                    else:
                        if op.poll():
                            op('INVOKE_DEFAULT', **args)
                        else:
                            print("Poll failed")
                    return {'FINISHED'}
            else:
                # prevent tool from toolbar restart
                return {'RUNNING_MODAL'}
        elif event.ascii or (
                event.type in self.menu.keyboard_type and
                event.value == 'RELEASE'):
            self.menu.keyboard_entry(context, event)
            # prevent keyboard event to buble up when typing on seek box
            return {'RUNNING_MODAL'}

        elif event.type in {'RIGHTMOUSE', 'ESC'} or self.disable:
            self.exit(context)
            return {'CANCELLED'}

        return {'PASS_THROUGH'}

    def invoke(self, context, event):
        if context.area.type == 'VIEW_3D':

            # with alt pressed on invoke, will bypass menu operator and
            # call preset_operator
            # allow start drawing linked copy of active object
            o = context.active_object

            bpy.ops.archipack.disable_manipulate()

            self.disable = False

            if (o and o.data and
                    ("archipack_door" in o.data or "archipack_window" in o.data) and
                    (event.alt or event.ctrl)):

                po = self.preset_operator.split(".")
                op = getattr(getattr(bpy.ops, po[0]), po[1])
                o = context.active_object

                if o is not None and o.data is not None and self.preset_subdir in o.data and op.poll():
                    op('INVOKE_DEFAULT')
                else:
                    self.report({'WARNING'}, "Active object must be a " + self.preset_subdir.split("_")[1].capitalize())
                    return {'CANCELLED'}
                return {'FINISHED'}

            self.menu = PresetMenu(context, self.preset_subdir)

            global running
            try:
                running.disable = True
            except:
                pass

            running = self

            if self.menu.is_empty:
                # Fallback for empty presets
                # call preset operator
                try:
                    po = self.preset_operator.split(".")
                    op = getattr(getattr(bpy.ops, po[0]), po[1])
                    if op.poll():
                        op('INVOKE_DEFAULT')
                except:
                    pass
                self.report({'WARNING'}, "No preset found")
                return {'FINISHED'}

            # the arguments we pass the the callback
            args = (self, context)
            # Add the region OpenGL drawing callback
            # draw in view space with 'POST_VIEW' and 'PRE_VIEW'

            self._handle = bpy.types.SpaceView3D.draw_handler_add(self.draw_handler, args, 'WINDOW', 'POST_PIXEL')
            context.window_manager.modal_handler_add(self)
            return {'RUNNING_MODAL'}
        else:
            self.report({'WARNING'}, "View3D not found, cannot show preset")
            return {'CANCELLED'}


class ArchipackPreset:

    bl_options = {'REGISTER', 'INTERNAL'}

    name: StringProperty(
        name="Name",
        description="Name of the preset, used to make the path name",
        maxlen=64,
        options={'SKIP_SAVE'},
    )

    remove_active: BoolProperty(
        default=False,
        options={'HIDDEN', 'SKIP_SAVE'},
    )

    preset_format = "json"

    @staticmethod
    def as_filename(name):  # could reuse for other presets

        # lazy init maketrans
        def maketrans_init():
            cls = ArchipackPreset
            attr = "_as_filename_trans"

            trans = getattr(cls, attr, None)
            if trans is None:
                trans = str.maketrans({char: "_" for char in " !@#$%^&*(){}:\";'[]<>,.\\/?"})
                setattr(cls, attr, trans)
            return trans

        name = name.lower().strip()
        name = bpy.path.display_name_to_filepath(name)
        trans = maketrans_init()
        return name.translate(trans)

    @classmethod
    def poll(cls, context):
        o = context.active_object
        return o is not None and \
            o.data is not None and \
            "archipack_" + cls.__name__[13:-7] in o.data

    @property
    def preset_subdir(self):
        return "archipack_" + self.__class__.__name__[13:-7]

    @property
    def blacklist(self):
        """
            properties black list for presets
            may override on addon basis
        """
        return []

    def add(self, context, preset):
        """
          Override add method
          deep filter property
        """
        o = context.object
        _json = jsonio.to_json(o, self.preset_subdir, self.blacklist)
        try:
            with open(preset , 'w', encoding="utf-8") as f:
                f.write(_json)
        except:
            self.report({'WARNING'}, "Archipack preset : write error %s" % ex)
            pass

    @property
    def preset_defines(self):
        return []

    def execute(self, context):
        """ Override to support .json based presets
        :param context:
        :return:
        """
        import os

        preset_menu_class = getattr(bpy.types, self.preset_menu)

        ext = ".{}".format(self.preset_format)

        subdir = self.preset_subdir

        if self.remove_active:

            name = preset_menu_class.bl_label

            # fairly sloppy but convenient.
            preset = bpy.utils.preset_find(name,
                                           subdir,
                                           ext=ext)

            if not preset:
                preset = bpy.utils.preset_find(name,
                                               subdir,
                                               display_name=True,
                                               ext=ext)

            if not preset:
                return {'CANCELLED'}

            try:
                self.remove(preset)
            except Exception as e:
                self.report({'WARNING'}, "Unable to remove preset: %r" % e)
                import traceback
                traceback.print_exc()
                return {'CANCELLED'}

            preset_menu_class.bl_label = self.bl_label

        else:

            name = self.name.strip()

            if not name:
                return {'FINISHED'}

            filename = self.as_filename(name)
            target_path = os.path.join("presets", subdir)
            target_path = bpy.utils.user_resource('SCRIPTS',
                                                  target_path,
                                                  create=True)

            if not target_path:
                self.report({'WARNING'}, "Failed to create presets path")
                return {'CANCELLED'}

            preset_base = os.path.join(target_path, filename)
            preset = "{}.{}".format(preset_base, self.preset_format)
            thumb = "{}.png".format(preset_base)

            self.add(context, preset)

            if os.path.exists(thumb):
                os.remove(thumb)

            icons.add_preset(subdir, name, self.preset_format)

            category = subdir[10:]

            self.background_render(context, category, preset)

            preset_menu_class.bl_label = bpy.path.display_name(filename)

        return {'FINISHED'}

    def remove(self, preset):
        # remove preset

        global icons
        folder, file = os.path.split(preset)
        name, ext = os.path.splitext(file)
        icons.remove_preset(self.preset_subdir, name)

        preset_base, ext = os.path.splitext(preset)
        thumb = "{}.png".format(preset_base)

        os.remove(preset)
        os.remove(thumb)

    def background_render(self, context, category, preset):

        generator = os.path.join(os.path.dirname(os.path.realpath(__file__)), "archipack_thumbs.py")
        matlib_path = context.preferences.addons[__package__].preferences.matlib_path
        # Run external instance of blender like the original thumbnail generator.

        cmd = [
            bpy.app.binary_path,
            "--background",
            "--factory-startup",
            "-noaudio",
            "--python", generator,
            "--",
            "addon:" + __package__,
            "matlib:" + matlib_path,
            "cls:" + category,
            "preset:" + preset
            ]
        subprocess.Popen(cmd)

    def check(self, context):
        self.name = self.as_filename(self.name.strip())

    def invoke(self, context, event):
        if not self.remove_active:
            wm = context.window_manager
            preset_menu_class = getattr(bpy.types, self.preset_menu)
            label = preset_menu_class.bl_label
            if label != self.bl_label:
                self.name = label
            return wm.invoke_props_dialog(self)
        else:
            return self.execute(context)


class ARCHIPACK_OT_preset_loader(Operator):
    bl_idname = "archipack.preset_loader"
    bl_label = "Archipack Preset Loader"
    bl_description = "Load json preset"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL'}

    menu_idname: StringProperty()
    filepath: StringProperty()

    @classmethod
    def poll(self, context):
        o = context.object
        return o is not None

    def execute(self, context):
        """
        NOTE: object auto_update must be disabled before this call
        and enabled after
        :param context:
        :return:
        """
        o = context.object

        menu_idname = self.menu_idname
        if menu_idname == "":
            menu_idname = "ARCHIPACK_OT_preset_loader"

        try:
            with open(self.filepath, 'r', encoding="utf-8") as f:
                _json = f.read()
                jsonio.from_json(o, _json)

            preset_menu_class = getattr(bpy.types, menu_idname)
            preset_menu_class.bl_label = bpy.path.display_name(self.filepath)

        except Exception as ex:
            self.report({'WARNING'}, "Archipack preset loader : Read error %s" % ex)
            pass

        return{'FINISHED'}

    def invoke(self, context, event):
        return self.execute(context)


def register():
    bpy.utils.register_class(ARCHIPACK_OT_preset_loader)


def unregister():
    bpy.utils.unregister_class(ARCHIPACK_OT_preset_loader)