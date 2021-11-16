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

bl_info = {
    'name': 'Archipack PRO',
    'description': 'Architectural objects and 2d polygons detection from unordered splines',
    'author': 's-leger support@blender-archipack.org',
    'license': 'GPL',
    'deps': '',
    'version': (2, 0, 9),
    'blender': (2, 80, 0),
    'location': 'View3D > Tools > Create > Archipack',
    'warning': '',
    'wiki_url': 'https://blender-archipack.gitlab.io/',
    'tracker_url': 'https://github.com/s-leger/archipack/issues/',
    'link': 'https://blender-archipack.org',
    'support': 'COMMUNITY',
    'category': 'Add Mesh'
    }

__version__ = ".".join([str(i) for i in bl_info['version']])


import os
import importlib as imp


submodules = (
    'prefs',
    'iconmanager',
    'material',
    'preset',
    'snap',
    'manipulator',
    'curveman',
    'throttle',
    'segments2',
    'dimension',
    'reference_point',
    'autoboolean',
    'cutter',
    'door',
    'window',
    'stair',
    'wall',
    'wall2',
    'slab',
    'roof',
    'fence',
    'truss',
    'custom',
    'floor',
    'blind',
    'kitchen',
    'molding',
    'beam',
    'rendering',
    'section',
    'io',
    '2d_layout',
    'io_export_svg',
    'io_export_ifc',
    'polylines',
    'sun',
    'area',
    'terrain',
    'i18n',
    'py_deps_installer'
)


if "bpy" in locals():
    glob = globals()
    for sub in submodules:
        try:
            imp.reload(glob["archipack_{}".format(sub)])
        except Exception as ex:
            print("{} {} error while reloading {}\n{}".format(bl_info['name'], __version__, sub, ex))
            # import traceback
            # traceback.print_exc()
            raise
    print(bl_info['name'], __version__, ": reload ready")

else:
    glob = globals()
    for sub in submodules:
        try:
            glob["archipack_{}".format(sub)] = imp.import_module(".archipack_{}".format(sub), __package__)
        except Exception as ex:
            print("{} {} error while loading {}\n{}".format(bl_info['name'], __version__, sub, ex))
            # import traceback
            # traceback.print_exc()
            raise
    print(bl_info['name'], __version__,": ready")


# noinspection PyUnresolvedReferences
import bpy
# noinspection PyUnresolvedReferences
from bpy.types import (
    Panel, AddonPreferences, Menu, PropertyGroup, Object
    )
from bpy.props import (
    StringProperty, BoolProperty, EnumProperty,
    IntProperty, FloatProperty,
    FloatVectorProperty, PointerProperty
    )
from bpy.utils import register_class, unregister_class


icon_man = None


def update_manipulate(self, context):
    """
    if self.auto_manipulate:
        #    bpy.ops.archipack.manipulate('INVOKE_DEFAULT')
        print("prefs.auto_manipulate enable")
    else:
        #    bpy.ops.archipack.disable_manipulate()
        print("prefs.auto_manipulate disable")
    """
    return


def get_matlib_path(self):
    return self.matlib_path


def set_matlib_path(self, value):
    abs_path = bpy.path.abspath(value)
    if os.path.exists(abs_path):
        self.matlib_path = abs_path
    else:
        self.matlib_path = ""
    return None


# ----------------------------------------------------
# Addon preferences
# ----------------------------------------------------

def register_translation(self, context):
    try:
        unregister_class(archipack_i18n.ARCHIPACK_PT_translation)
    except:
        pass
    if context.preferences.addons[__name__].preferences.translate:
        archipack_i18n.ARCHIPACK_PT_translation.bl_category = "Translate Archipack"
        register_class(archipack_i18n.ARCHIPACK_PT_translation)


def update_panel(self, context):
    try:
        unregister_class(TOOLS_PT_Archipack_PolyLib)
        unregister_class(TOOLS_PT_Archipack_Tools)
        unregister_class(TOOLS_PT_Archipack_Create)
        unregister_class(TOOLS_PT_Archipack_Create_2d)
        unregister_class(TOOLS_PT_Archipack_Create_Custom)
        unregister_class(TOOLS_PT_Archipack_About)
    except:
        pass

    prefs = context.preferences.addons[__name__].preferences
    TOOLS_PT_Archipack_Create.bl_category = prefs.create_category
    register_class(TOOLS_PT_Archipack_Create)
    TOOLS_PT_Archipack_Create_2d.bl_category = prefs.create_category
    register_class(TOOLS_PT_Archipack_Create_2d)
    TOOLS_PT_Archipack_Create_Custom.bl_category = prefs.create_category
    register_class(TOOLS_PT_Archipack_Create_Custom)
    TOOLS_PT_Archipack_Tools.bl_category = prefs.tools_category
    register_class(TOOLS_PT_Archipack_Tools)
    TOOLS_PT_Archipack_PolyLib.bl_category = prefs.tools_category
    register_class(TOOLS_PT_Archipack_PolyLib)
    TOOLS_PT_Archipack_About.bl_category = prefs.create_category
    register_class(TOOLS_PT_Archipack_About)

    glob = globals()
    for sub in submodules:
        module = glob["archipack_{}".format(sub)]
        for attr in dir(module):
            if attr.startswith("ARCHIPACK_PT_"):
                panel = getattr(module, attr)
                try:
                    unregister_class(panel)
                except:
                    pass
                panel.bl_category = prefs.objects_category
                register_class(panel)


class archipack_wm(PropertyGroup):
    bl_idname = "archipack.wm"
    bl_label = "Archipack"
    auto_save: BoolProperty(
        name="Auto save",
        description="Enable auto-save while manipulating (may crash blender)",
        default=False
    )
    auto_manipulate: BoolProperty(
        name="Auto manipulate",
        description="Auto enable Manipulators on select",
        update=update_manipulate,
        default=True
    )
    polylib: PointerProperty(type=archipack_polylines.archipack_polylib)
    sun: PointerProperty(type=archipack_sun.archipack_sun_presets)
    translation: PointerProperty(type=archipack_i18n.archipack_translation)


class Archipack_Pref(archipack_i18n.Archipacki18n, AddonPreferences):
    bl_idname = __name__

    tools_category: StringProperty(
        name="Tools",
        description="Choose a name for the category of the Tools panel",
        default="Tools",
        update=update_panel
    )
    create_category: StringProperty(
        name="Create",
        description="Choose a name for the category of the Create panel",
        default="Create",
        update=update_panel
    )
    objects_category: StringProperty(
        name="Objects",
        description="Choose a name for the category of Objects paremeters panel",
        default="Archipack",
        update=update_panel
    )
    create_submenu: BoolProperty(
        name="Use Sub-menu",
        description="Put Achipack's object into a sub menu (shift+a)",
        default=True
    )
    max_style_draw_tool: BoolProperty(
        name="Draw a wall use 3dsmax style",
        description="Reverse clic / release & drag cycle for Draw a wall",
        default=True
    )
    draw_wall_tool_direction: EnumProperty(
        name="Draw wall tool direction",
        description="Draw wall tool direction",
        items=(
            ('CCW', "Couterclockwise", "Draw in Couterclockwise order", 0),
            ('CW', "Clockwise", "Draw in Clockwise order", 1)
        ),
        default='CCW'
    )
    throttle_enable: BoolProperty(
        name="Quick edit",
        description="When enabled, prevent complex objects to update in real time",
        default=True
    )
    throttle_delay: IntProperty(
        name="Delay",
        description="Quick edit, how much time to wait before updating complex objects (seconds)",
        default=1
    )
    # Arrow sizes (world units)
    arrow_size: FloatProperty(
        name="Arrow",
        description="Manipulators arrow size (blender units)",
        default=0.05
    )
    # Handle area size (pixels)
    handle_size: IntProperty(
        name="Handle",
        description="Manipulators handle sensitive area size (pixels)",
        min=2,
        default=10
    )
    thumbsize: IntProperty(
        name="Preset thumbs",
        description="Preset thumbs size (pixels)",
        min=50,
        default=150
    )
    constant_handle_size: BoolProperty(
        name="Constant handle size",
        description="When checked, handle size on scree remains constant (handle size pixels)",
        default=False
    )
    text_size: IntProperty(
        name="Manipulators",
        description="Manipulator font size (pixels)",
        min=2,
        default=14
    )
    handle_colour_selected: FloatVectorProperty(
        name="Selected handle colour",
        description="Handle color when selected",
        subtype='COLOR_GAMMA',
        default=(0.0, 0.0, 0.7, 0.75),
        size=4,
        min=0, max=1
    )
    handle_colour_inactive: FloatVectorProperty(
        name="Inactive handle colour",
        description="Handle color when disabled",
        subtype='COLOR_GAMMA',
        default=(0.3, 0.3, 0.3, 0.75),
        size=4,
        min=0, max=1
    )
    handle_colour_normal: FloatVectorProperty(
        name="Handle colour normal",
        description="Base handle color when not selected",
        subtype='COLOR_GAMMA',
        default=(1.0, 1.0, 1.0, 0.75),
        size=4,
        min=0, max=1
    )
    handle_colour_hover: FloatVectorProperty(
        name="Handle colour hover",
        description="Handle color when mouse hover",
        subtype='COLOR_GAMMA',
        default=(1.0, 1.0, 0.0, 0.75),
        size=4,
        min=0, max=1
    )
    handle_colour_active: FloatVectorProperty(
        name="Handle colour active",
        description="Handle colour when moving",
        subtype='COLOR_GAMMA',
        default=(1.0, 0.0, 0.0, 0.75),
        size=4,
        min=0, max=1
    )
    matlib_path: StringProperty(
        name="Folder path",
        description="Absolute path to material library folder",
        default=""
    )
    matlib_path_ui: StringProperty(
        name="Folder path",
        subtype="DIR_PATH",
        description="Absolute path to material library folder",
        default="",
        get=get_matlib_path,
        set=set_matlib_path
    )

    # Font sizes and basic colour scheme
    # kept outside of addon prefs until now
    # as for a generic toolkit it is not appropriate
    # we could provide a template for addon prefs
    # matching those one
    feedback_size_main: IntProperty(
        name="Main",
        description="Main title font size (pixels)",
        min=2,
        default=16
    )
    feedback_size_title: IntProperty(
        name="Title",
        description="Tool name font size (pixels)",
        min=2,
        default=14
    )
    feedback_size_shortcut: IntProperty(
        name="Shortcut",
        description="Shortcuts font size (pixels)",
        min=2,
        default=11
    )
    feedback_shortcut_area: FloatVectorProperty(
        name="Background Shortcut",
        description="Shortcut area background color",
        subtype='COLOR_GAMMA',
        default=(0, 0.4, 0.6, 0.2),
        size=4,
        min=0, max=1
    )
    feedback_title_area: FloatVectorProperty(
        name="Background Main",
        description="Title area background color",
        subtype='COLOR_GAMMA',
        default=(0, 0.4, 0.6, 0.5),
        size=4,
        min=0, max=1
    )
    feedback_colour_main: FloatVectorProperty(
        name="Font Main",
        description="Title color",
        subtype='COLOR_GAMMA',
        default=(0.95, 0.95, 0.95, 1.0),
        size=4,
        min=0, max=1
    )
    feedback_colour_key: FloatVectorProperty(
        name="Font Shortcut key",
        description="KEY label color",
        subtype='COLOR_GAMMA',
        default=(0.67, 0.67, 0.67, 1.0),
        size=4,
        min=0, max=1
    )
    feedback_colour_shortcut: FloatVectorProperty(
        name="Font Shortcut hint",
        description="Shortcuts text color",
        subtype='COLOR_GAMMA',
        default=(0.51, 0.51, 0.51, 1.0),
        size=4,
        min=0, max=1
    )
    experimental_features: BoolProperty(
        name="Experimental features",
        description="Enable experimental features (may be unstable)",
        default=False
    )

    translate: BoolProperty(
        name="Enable translation",
        default=False,
        update=register_translation
    )

    def draw(self, context):
        layout = self.layout
        self.draw_translate(context, layout)

        box = layout.box()
        self.draw_label(context, layout, box, "Material library:")
        self.draw_prop(context, layout, box, self, "matlib_path_ui")

        box = layout.box()
        row = box.row()
        col = row.column()
        self.draw_label(context, layout, col, "Tab Category:")
        self.draw_prop(context, layout, col, self, "tools_category")
        self.draw_prop(context, layout, col, self, "create_category")
        self.draw_prop(context, layout, col, self, "objects_category")
        self.draw_prop(context, layout, col, self, "create_submenu")

        box = layout.box()
        self.draw_label(context, layout, box, "Features")
        row = box.row()
        self.draw_prop(context, layout, row, self,"throttle_enable")
        self.draw_prop(context, layout, row, self,"throttle_delay")
        # self.draw_prop(context, layout, box, self, "auto_save")
        self.draw_prop(context, layout, box, self, "max_style_draw_tool")
        row = box.row(align=True)
        self.draw_label(context, layout, row, "Draw wall direction")
        self.draw_prop(context, layout, row, self, "draw_wall_tool_direction", text="")

        self.draw_prop(context, layout, box, self, "experimental_features")

        if self.experimental_features:
            self.draw_label(context, layout, box, "Setup python modules")

            HAS_SCIPY = False
            try:
                import scipy
                HAS_SCIPY = True
            except ImportError:
                pass

            if HAS_SCIPY:
                self.draw_op(context, layout, box,
                            "archipack.uninstall_python_module",
                            text="Uninstall Scipy (boost terrain ~18x faster)"
                            ).module = "scipy"
            else:
                self.draw_op(context, layout, box,
                            "archipack.install_python_module",
                            text="Install Scipy (boost terrain ~18x faster)"
                            ).module = "scipy==1.2.1"

        box = layout.box()
        self.draw_label(context, layout, box, "Translate archipack on screen")
        self.draw_prop(context, layout, box, self, "translate")
        if self.translate:
            self.draw_op(context, layout, box, "wm.path_open", text="Find translation file"
                         ).filepath="{}".format(os.path.join(os.path.dirname(__file__), "lang"))
            self.draw_op(context, layout, box, "wm.url_open",
                         text="Send translation files to translation@blender-archipack.org"
                         ).url = "mailto:translations@blender-archipack.org?subject=Translation%20{}".format(
                context.preferences.view.language
            )

        box = layout.box()
        row = box.row()
        split = row.split(factor=0.5)
        col = split.column()
        self.draw_label(context, layout, col, "Colors:")
        row = col.row(align=True)
        self.draw_prop(context, layout, row, self,"feedback_title_area")
        row = col.row(align=True)
        self.draw_prop(context, layout, row, self,"feedback_shortcut_area")
        row = col.row(align=True)
        self.draw_prop(context, layout, row, self,"feedback_colour_main")
        row = col.row(align=True)
        self.draw_prop(context, layout, row, self,"feedback_colour_key")
        row = col.row(align=True)
        self.draw_prop(context, layout, row, self,"feedback_colour_shortcut")

        row = col.row(align=True)
        self.draw_prop(context, layout, row, self,"handle_colour_normal")
        row = col.row(align=True)
        self.draw_prop(context, layout, row, self,"handle_colour_hover")
        row = col.row(align=True)
        self.draw_prop(context, layout, row, self,"handle_colour_active")
        row = col.row(align=True)
        self.draw_prop(context, layout, row, self,"handle_colour_selected")
        row = col.row(align=True)
        self.draw_prop(context, layout, row, self,"handle_colour_inactive")

        col = split.column()
        self.draw_label(context, layout, col, "Font size:")
        self.draw_prop(context, layout, col, self, "feedback_size_main")
        self.draw_prop(context, layout, col, self, "feedback_size_title")
        self.draw_prop(context, layout, col, self, "feedback_size_shortcut")
        self.draw_label(context, layout, col, "Manipulators")
        self.draw_prop(context, layout, col, self, "arrow_size")
        self.draw_prop(context, layout, col, self, "text_size")
        self.draw_prop(context, layout, col, self, "handle_size")
        self.draw_prop(context, layout, col, self, "constant_handle_size")
        # addon_updater_ops.update_settings_ui(self, context)
        self.draw_prop(context, layout, col, self, "thumbsize")


# ----------------------------------------------------
# Archipack panels
# ----------------------------------------------------

class TOOLS_PT_Archipack_PolyLib(archipack_i18n.Archipacki18n, Panel):
    bl_label = "2d to 3d"
    bl_idname = "TOOLS_PT_Archipack_PolyLib"
    bl_space_type = "VIEW_3D"
    bl_region_type = 'UI'
    bl_category = "Tools"
    bl_context = "objectmode"

    @classmethod
    def poll(self, context):

        global archipack_polylines
        return ((archipack_polylines.vars_dict['select_polygons'] is not None) or
                (context.object is not None and context.object.type == 'CURVE'))

    def draw(self, context):
        global icon_man
        icons = icon_man["main"]
        layout = self.layout
        self.draw_translate(context, layout)

        params = context.window_manager.archipack.polylib
        row = layout.row(align=True)
        box = row.box()
        row = box.row(align=True)
        if params.polygonize_expand:
            self.draw_prop(context, layout, row, params, "polygonize_expand", icon='TRIA_DOWN', text="")
        else:
            self.draw_prop(context, layout, row, params, "polygonize_expand", icon='TRIA_RIGHT', text="")

        self.draw_op(context, layout, row,
            "archipack.polylib_polygonize",
            icon_value=icons["detect"].icon_id,
            text='Detect'
            )

        if params.polygonize_expand:
            self.draw_prop(context, layout, box, params, "polygonize_bezier_resolution")
            self.draw_prop(context, layout, box, params, "polygonize_extend")
            self.draw_prop(context, layout, box, params, "polygonize_all_segs")

            self.draw_op(context, layout, box,
                "archipack.polylib_pick_2d_polygons",
                icon_value=icons["selection"].icon_id,
                text='Polygons'
                )

            self.draw_op(context, layout, box,
                "archipack.polylib_pick_2d_lines",
                icon_value=icons["selection"].icon_id,
                text='Lines'
                )

            self.draw_op(context, layout, box,
                "archipack.polylib_pick_2d_points",
                icon_value=icons["selection"].icon_id,
                text='Points'
                )

            self.draw_label(context, layout, box, "Walls")
            self.draw_prop(context, layout, box, params, "polygonize_thickness")

        row = layout.row(align=True)
        box = row.box()
        row = box.row(align=True)
        if params.simplify_expand:
            self.draw_prop(context, layout, row, params, "simplify_expand", icon='TRIA_DOWN', text="")
        else:
            self.draw_prop(context, layout, row, params, "simplify_expand", icon='TRIA_RIGHT', text="")
        self.draw_op(context, layout, row, "archipack.polylib_simplify")
        if params.simplify_expand:
            self.draw_prop(context, layout, box, params, "simplify_bezier_resolution")
            self.draw_prop(context, layout, box, params, "simplify_tolerance")
            self.draw_prop(context, layout, box, params, "simplify_preserve_topology")

        row = layout.row(align=True)
        box = row.box()
        row = box.row(align=True)
        if params.offset_expand:
            self.draw_prop(context, layout, row, params, "offset_expand", icon='TRIA_DOWN', text="")
        else:
            self.draw_prop(context, layout, row, params, "offset_expand", icon='TRIA_RIGHT', text="")
        self.draw_op(context, layout, row, "archipack.polylib_offset")
        if params.offset_expand:
            self.draw_prop(context, layout, box, params, "offset_bezier_resolution")
            self.draw_prop(context, layout, box, params, "offset_distance")
            self.draw_prop(context, layout, box, params, "offset_side")
            self.draw_prop(context, layout, box, params, "offset_resolution")
            self.draw_prop(context, layout, box, params, "offset_join_style")
            self.draw_prop(context, layout, box, params, "offset_mitre_limit")

        row = layout.row(align=True)
        box = row.box()
        row = box.row(align=True)
        if params.buffer_expand:
            self.draw_prop(context, layout, row, params, "buffer_expand", icon='TRIA_DOWN', text="")
        else:
            self.draw_prop(context, layout, row, params, "buffer_expand", icon='TRIA_RIGHT', text="")
        self.draw_op(context, layout, row, "archipack.polylib_buffer")
        if params.buffer_expand:
            self.draw_prop(context, layout, box, params, "buffer_bezier_resolution")
            self.draw_prop(context, layout, box, params, "buffer_distance")
            self.draw_prop(context, layout, box, params, "buffer_side")
            self.draw_prop(context, layout, box, params, "buffer_resolution")
            self.draw_prop(context, layout, box, params, "buffer_join_style")
            self.draw_prop(context, layout, box, params, "buffer_cap_style")
            self.draw_prop(context, layout, box, params, "buffer_mitre_limit")

        row = layout.row(align=True)
        box = row.box()
        row = box.row(align=True)
        if params.boolean_expand:
            self.draw_prop(context, layout, row, params, "boolean_expand", icon='TRIA_DOWN', text="")
        else:
            self.draw_prop(context, layout, row, params, "boolean_expand", icon='TRIA_RIGHT', text="")
        self.draw_label(context, layout, row, "2d Boolean")
        if params.boolean_expand:
            self.draw_op(context, layout, box, "archipack.polylib_boolean", text="Active - Selected").opCode = 'DIFFERENCE'
            self.draw_op(context, layout, box, "archipack.polylib_boolean", text="Selected - Active").opCode = 'REVDIFFERENCE'
            self.draw_op(context, layout, box, "archipack.polylib_boolean", text="Intersection").opCode = 'INTERSECTION'
            self.draw_op(context, layout, box, "archipack.polylib_boolean", text="Union").opCode = 'UNION'
            self.draw_op(context, layout, box, "archipack.polylib_boolean", text="SymDifference").opCode = 'SYMDIFFERENCE'
            self.draw_prop(context, layout, box, params, "boolean_bezier_resolution")


class TOOLS_PT_Archipack_Tools(archipack_i18n.Archipacki18n, Panel):
    bl_label = "Tools"
    bl_idname = "TOOLS_PT_Archipack_Tools"
    bl_space_type = "VIEW_3D"
    bl_region_type = 'UI'
    bl_category = "Tools"
    bl_context = "objectmode"
    bl_options = {'DEFAULT_CLOSED'}

    @classmethod
    def poll(self, context):
        return True

    def draw(self, context):
        prefs = context.preferences.addons[__name__].preferences
        layout = self.layout
        self.draw_translate(context, layout)

        box = layout # .box()
        self.draw_label(context, layout, box, "Auto Boolean")
        self.draw_op(context, layout, box, "archipack.auto_boolean", text="Auto Boolean", icon='AUTO')
        self.draw_label(context, layout, box, "Apply holes")
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.apply_holes", text="selected").selected_only = True
        self.draw_op(context, layout, row, "archipack.apply_holes", text="all").selected_only = False

        self.draw_label(context, layout, box, "Remove references")
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.remove_references", text="selected").selected_only = True
        self.draw_op(context, layout, row, "archipack.remove_references", text="all").selected_only = False

        # box = layout.box()
        self.draw_label(context, layout, box, "Kill parameters")
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.kill_archipack", text="selected", icon="ERROR").selected_only = True
        self.draw_op(context, layout, row, "archipack.kill_archipack", text="all", icon="ERROR").selected_only = False


class TOOLS_PT_Archipack_About(archipack_i18n.Archipacki18n, Panel):
    bl_label = "About"
    bl_idname = "TOOLS_PT_Archipack_About"
    bl_space_type = "VIEW_3D"
    bl_region_type = 'UI'
    bl_category = "Create"
    bl_context = "objectmode"

    def draw(self, context):

        layout = self.layout
        box = layout.box()
        box.label(text="Archipack {}".format(__version__))
        box.label(text="by Stephen Leger")
        box = layout.box()
        box.label(text="Internationalization")
        row=box.row(align=True)

        # international char support enabled
        international = context.preferences.view.use_international_fonts

        row.label(text="Chinese")
        if international:
            text="只剩一瓶辣椒酱"
        else:
            text="Xiao"
        row.label(text=text)

        row=box.row(align=True)
        row.label(text="French")
        row.label(text="Stephen Leger")

        row = box.row(align=True)
        row.label(text="Spanish")
        row.label(text="Sara González")


class TOOLS_PT_Archipack_Create(archipack_i18n.Archipacki18n, Panel):
    bl_label = "Add Objects"
    bl_idname = "TOOLS_PT_Archipack_Create"
    bl_space_type = "VIEW_3D"
    bl_region_type = 'UI'
    bl_category = "Create"
    bl_context = "objectmode"

    @classmethod
    def poll(self, context):
        return hasattr(context.window_manager, "archipack")

    def draw(self, context):
        global icon_man
        prefs = context.preferences.addons[__name__].preferences
        wm = context.window_manager.archipack
        # addon_updater_ops.check_for_update_background(context)

        icons = icon_man["main"]
        layout = self.layout

        self.draw_translate(context, layout)

        box = layout.box()
        self.draw_prop(context, layout, box, wm, "auto_manipulate", icon="VIEW_PAN")
        self.draw_prop(context, layout, box, wm, "auto_save", icon="FILE_BACKUP")
        self.draw_prop(context, layout, box, prefs, "throttle_enable", icon="MOD_MULTIRES")
        self.draw_prop(context, layout, box, prefs, "throttle_delay")

        box = layout.column(align=True)
        row = box.row(align=True)
        # self.draw_op(context, layout, row, "archipack.wall2_draw",
        #            text="Wall",
        #            icon_value=icons["wall"].icon_id
        #            )
        self.draw_op(context, layout, row, "archipack.wall2_preset_draw",
                     text="Wall",
                     icon_value=icons["wall"].icon_id
                     ).preset_operator = "archipack.wall2_draw"
        self.draw_op(context, layout, row, "archipack.wall2_preset_from_curve",
                     text="",
                     icon='CURVE_DATA')

        row = box.row(align=True)
        # col = row.column()
        # subrow = col.row(align=True)
        self.draw_op(context, layout, row, "archipack.window_preset_draw",
                    text="Window",
                    icon_value=icons["window"].icon_id
                    ).preset_operator = "archipack.window_draw"
        # col = row.column()
        # subrow = col.row(align=True)
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.door_preset_draw",
                    text="Door",
                    icon_value=icons["door"].icon_id
                    ).preset_operator = "archipack.door_draw"
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.stair_preset_create",
                    text="Stairs",
                    icon_value=icons["stair"].icon_id
                    ).preset_operator = "archipack.stair"
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.fence_preset_draw",
                    text="Fence",
                    icon_value=icons["fence"].icon_id
                    ).preset_operator = "archipack.fence_draw"
        self.draw_op(context, layout, row, "archipack.fence_preset_from_curve",
                     text="",
                     icon='CURVE_DATA')

        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.slab",
                    icon_value=icons["slab"].icon_id
                    )
        self.draw_op(context, layout, row, "archipack.slab_from_curve",
                    text="", icon='CURVE_DATA'
                    )

        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.floor_preset_create",
                    text="Floor",
                    icon_value=icons["floor"].icon_id
                    ).preset_operator = "archipack.floor"
        self.draw_op(context, layout, row, "archipack.floor_preset_from_curve",
                    text="",
                    icon='CURVE_DATA')

        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.molding_preset_draw",
                    text="Molding",
                    icon_value=icons["molding"].icon_id
                    ).preset_operator = "archipack.molding_draw"
        self.draw_op(context, layout, row, "archipack.molding_preset_from_curve",
                    text="",
                    icon='CURVE_DATA')

        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.roof_preset_create",
                    text="Roof",
                    icon_value=icons["roof"].icon_id
                    ).preset_operator = "archipack.roof"
        self.draw_op(context, layout, row, "archipack.roof_draft",
                     text="Draft",
                     icon_value=icons["roof"].icon_id
                     )
        # toolkit
        # row = box.row(align=True)
        # self.draw_op(context, layout, row, "archipack.myobject")
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.kitchen_preset_create",
                    text="Kitchen",
                    icon_value=icons["kitchen"].icon_id
                    ).preset_operator = "archipack.kitchen"

        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.blind_preset_create",
                    text="Blind",
                    icon_value=icons["blind"].icon_id
                    ).preset_operator = "archipack.blind"
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.truss",
                    icon_value=icons["truss"].icon_id
                    )
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.beam",
                     icon_value=icons["beam"].icon_id
                     )
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.sun",
                     icon="LIGHT_SUN"
                     )
        if prefs.experimental_features:
            row = box.row(align=True)
            self.draw_op(context, layout, row, "archipack.terrain",
                        icon="RNDCURVE"
                        )


class TOOLS_PT_Archipack_Create_2d(archipack_i18n.Archipacki18n, Panel):
    bl_label = "Add 2d Objects"
    bl_idname = "TOOLS_PT_Archipack_Create_2d"
    bl_space_type = "VIEW_3D"
    bl_region_type = 'UI'
    bl_category = "Create"
    bl_context = "objectmode"

    @classmethod
    def poll(self, context):
        return True

    def draw(self, context):
        global icon_man
        prefs = context.preferences.addons[__name__].preferences

        icons = icon_man["main"]
        layout = self.layout

        box = layout
        self.draw_op(context, layout, box, "archipack.dimension_auto",
                    icon_value=icons["dimension_auto"].icon_id
                    )
        self.draw_op(context, layout, box, "archipack.layout",
                    icon_value=icons["layout"].icon_id
                    )
        self.draw_op(context, layout, box, "archipack.section",
                    icon_value=icons["section"].icon_id
                    )
        self.draw_op(context, layout, box, "archipack.section_camera",
                    text="Section cam",
                    icon='CAMERA_DATA'
                    )
        self.draw_op(context, layout, box, "archipack.area",
                    text="Area / Volume",
                    icon="MOD_EDGESPLIT"
                    )


class TOOLS_PT_Archipack_Create_Custom(archipack_i18n.Archipacki18n, Panel):
    bl_label = "Custom Objects"
    bl_idname = "TOOLS_PT_Archipack_Create_Custom"
    bl_space_type = "VIEW_3D"
    bl_region_type = 'UI'
    bl_category = "Create"
    bl_context = "objectmode"

    @classmethod
    def poll(self, context):
        return True

    def draw(self, context):
        global icon_man
        # icons = icon_man["main"]
        layout = self.layout
        box = layout
        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.custom_wall", text="Custom wall")
        self.draw_op(context, layout, row, "archipack.custom_wall_remove", text="", icon='X')

        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.custom_hole", text="Custom hole")
        self.draw_op(context, layout, row, "archipack.custom_hole_remove", text="", icon='X')

        row = box.row(align=True)
        self.draw_op(context, layout, row, "archipack.make_custom", text="Custom", icon='MONKEY')
        self.draw_op(context, layout, row, "archipack.custom_manipulators", text="", icon='TOOL_SETTINGS')
        self.draw_op(context, layout, row, "archipack.custom_draw", text="", icon='GREASEPENCIL')


# ----------------------------------------------------
# ALT + A menu
# ----------------------------------------------------


def draw_menu(self, context):
    global icon_man
    icons = icon_man["main"]
    layout = self.layout
    layout.operator_context = 'INVOKE_REGION_WIN'
    self.draw_op(context, layout, layout, "archipack.wall2_preset_menu", text="Wall",
                 icon_value=icons["wall"].icon_id).preset_operator = "archipack.wall2"
    self.draw_op(context, layout, layout, "archipack.wall2_preset_draw", icon='GREASEPENCIL',
                 text="Draw wall").preset_operator = "archipack.wall2_draw"
    self.draw_op(context, layout, layout, "archipack.window_preset_draw", text="Window",
                 icon_value=icons["window"].icon_id).preset_operator = "archipack.window_draw"
    self.draw_op(context, layout, layout, "archipack.door_preset_draw", text="Door",
                 icon_value=icons["door"].icon_id).preset_operator = "archipack.door_draw"
    self.draw_op(context, layout, layout, "archipack.stair_preset_menu", text="Stair",
                 icon_value=icons["stair"].icon_id).preset_operator = "archipack.stair"
    self.draw_op(context, layout, layout, "archipack.fence_preset_menu", text="Fence",
                 icon_value=icons["fence"].icon_id).preset_operator = "archipack.fence"
    self.draw_op(context, layout, layout, "archipack.slab", text="Slab", icon_value=icons["slab"].icon_id)
    self.draw_op(context, layout, layout, "archipack.floor_preset_menu", text="Floor",
                 icon_value=icons["floor"].icon_id).preset_operator = "archipack.floor"
    self.draw_op(context, layout, layout, "archipack.molding_preset_menu", text="Molding",
                 icon_value=icons["molding"].icon_id).preset_operator = "archipack.molding"
    self.draw_op(context, layout, layout, "archipack.roof_preset_menu", text="Roof",
                 icon_value=icons["roof"].icon_id).preset_operator = "archipack.roof"
    self.draw_op(context, layout, layout, "archipack.kitchen_preset_menu", text="Kitchen",
                 icon_value=icons["kitchen"].icon_id).preset_operator = "archipack.kitchen"
    self.draw_op(context, layout, layout, "archipack.blind_preset_menu", text="Blind",
                 icon_value=icons["blind"].icon_id).preset_operator = "archipack.blind"
    self.draw_op(context, layout, layout, "archipack.truss", text="Truss", icon_value=icons["truss"].icon_id)
    self.draw_op(context, layout, layout, "archipack.beam", icon_value=icons["beam"].icon_id)
    self.draw_op(context, layout, layout, "archipack.dimension_auto", icon_value=icons["dimension_auto"].icon_id)
    self.draw_op(context, layout, layout, "archipack.layout", icon_value=icons["layout"].icon_id)
    self.draw_op(context, layout, layout, "archipack.section", icon_value=icons["section"].icon_id)
    self.draw_op(context, layout, layout, "archipack.section_camera", icon='CAMERA_DATA', text="Section cam")
    self.draw_op(context, layout, layout, "archipack.sun", icon="LIGHT_SUN")
    self.draw_op(context, layout, layout, "archipack.area", text="Area / Volume", icon="MOD_EDGESPLIT")


class ARCHIPACK_MT_create_menu(archipack_i18n.Archipacki18n, Menu):
    bl_label = 'Archipack'
    bl_category = 'Add'
    bl_idname = 'ARCHIPACK_MT_create_menu'

    def draw(self, context):
        draw_menu(self, context)


def menu_func(self, context):
    layout = self.layout
    layout.separator()
    global icon_man
    icons = icon_man["main"]

    # either draw sub menu or right at end of this one
    if context.preferences.addons[__name__].preferences.create_submenu:
        layout.operator_context = 'INVOKE_REGION_WIN'
        layout.menu("ARCHIPACK_MT_create_menu", icon_value=icons["archipack"].icon_id)
    else:
        draw_menu(self, context)


def register():

    global icon_man
    # auto_load.register()

    # XXX Backward compatible fix for api change 11.05.2019
    # if "hide_instance" not in Object.bl_rna.properties:
    #
    #    def set_hide(self, value):
    #        self.hide_viewport = value
    #        return None
    #
    #   def get_hide(self):
    #       return self.hide_viewport
    #
    #    Object.hide_instance = BoolProperty(name="hide_instance", get=get_hide, set=set_hide)

    glob = globals()
    for sub in submodules:
        module = glob["archipack_{}".format(sub)]
        if hasattr(module, "register"):
            # print("register", sub)
            try:
                module.register()
            except Exception as ex:
                print("{} {} register() error {}\n{}".format(bl_info['name'], __version__, sub, ex))
                raise

    # load icons, icon_man automatically free on unregister
    icon_man = glob["archipack_iconmanager"].icons
    icon_man.load(os.path.join(os.path.dirname(__file__), "icons"), 'main', format="png")

    register_class(Archipack_Pref)
    register_class(archipack_wm)
    bpy.types.WindowManager.archipack = PointerProperty(type=archipack_wm)
    register_class(ARCHIPACK_MT_create_menu)
    bpy.types.VIEW3D_MT_mesh_add.append(menu_func)
    update_panel(None, bpy.context)
    register_translation(None, bpy.context)


def unregister():
    bpy.types.VIEW3D_MT_mesh_add.remove(menu_func)
    unregister_class(ARCHIPACK_MT_create_menu)
    unregister_class(TOOLS_PT_Archipack_PolyLib)
    unregister_class(TOOLS_PT_Archipack_Tools)
    unregister_class(TOOLS_PT_Archipack_Create)
    unregister_class(TOOLS_PT_Archipack_Create_2d)
    unregister_class(TOOLS_PT_Archipack_Create_Custom)
    unregister_class(TOOLS_PT_Archipack_About)
    unregister_class(Archipack_Pref)
    del bpy.types.WindowManager.archipack
    unregister_class(archipack_wm)

    # unregister submodules
    glob = globals()
    for sub in reversed(submodules):
        module = glob["archipack_{}".format(sub)]
        if hasattr(module, "unregister"):
            module.unregister()

    # XXX Backward compatible fix for api change 11.05.2019
    # if "hide_viewport" in Object.bl_rna.properties and "hide_instance" in Object.bl_rna.properties:
    #    del Object.hide_instance


if __name__ == "__main__":
    register()
