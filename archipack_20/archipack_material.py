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
import os
import json
import subprocess

# noinspection PyUnresolvedReferences
import bpy
# noinspection PyUnresolvedReferences
from bpy.types import (
    Panel, PropertyGroup,
    Object, Operator, WindowManager,
    UIList
    )
from bpy.props import (
    EnumProperty, CollectionProperty,
    IntProperty,
    StringProperty, BoolProperty, PointerProperty
    )
from bpy.app.handlers import persistent
from .archipack_prefs import get_prefs
from .archipack_abstraction import ensure_select_and_restore, USE_SLOTS
from .archipack_object import (
    ArchipackCreateTool,
    ArchipackPanel
)
from .archipack_i18n import Archipacki18n
from .archipack_iconmanager import icons as icon_man


def build_mat_enum(attr, material_enums, parent=False):

    def build_enum(self, context):

        # Ui only show for active object so this is OK
        if context is None:
            return material_enums

        o = context.active_object

        if parent:
            o = o.parent

        if o is None:
            return material_enums

        # print("build_enum", o.name, id(self))

        material_enums.clear()
        material_enums.extend([
            (str(i), slot.name, slot.name)
            for i, slot in enumerate(o.material_slots)
            if slot is not None and slot.material is not None
        ])

        defined = {i for i, j, k in material_enums}
        # provide "Missing material" Enum for missing slots
        n_mats = len(material_enums) - 1

        if hasattr(self, attr):
            prop = getattr(self, attr)
            for index in prop:
                if str(index) not in defined and index > n_mats:
                    defined.add(str(index))
                    material_enums.append((str(index), "Missing", "Missing material"))

        return material_enums

    def mat_index_getter(index):
        def getter(self):
            val = getattr(self, attr)[index]
            if val < len(material_enums):
                return val
            return 0

        return getter

    def mat_index_setter(index):
        def setter(self, value):
            getattr(self, attr)[index] = value
            return None

        return setter

    return build_enum, mat_index_getter, mat_index_setter


setman = None
libman = None


class MatLib:
    """
        A material library .blend file
        Store material name
        Apply material to objects
    """
    __slots__ = ('materials', 'name', 'path')

    def __init__(self, matlib_path, name):
        self.name = name
        # print("Init Matlib %s" % name)
        try:
            self.path = os.path.join(matlib_path, name)
        except:
            pass
        self.materials = set()

    def cleanup(self):
        self.materials.clear()

    def load_list(self, sort=False):
        """
            list material names
        """
        if len(self.materials) < 1:
            # print("MatLib.load_list(%s)" % (self.name))
            try:
                with bpy.data.libraries.load(self.path) as (data_from, data_to):
                    for name in data_from.materials:
                        self.materials.add(name)
            except Exception as ex:
                print("Archipack: Error while loading material list %s" % ex)
            pass

    def has(self, name):
        return name in self.materials

    def load_mat(self, name, link):
        """
            Load a material from library
        """
        # print("MatLib.load_mat()" , name)
        try:
            # print("MatLib.load_mat(%s) linked:%s" % (name, link))
            with bpy.data.libraries.load(self.path, link, False) as (data_from, data_to):
                data_to.materials = [name]
        except Exception as ex:
            print("Archipack: Error while loading material %s  %s" % (name, ex))
            pass

    def check_override_on_save(self, names, overrides):

        self.load_list()

        to_seek = []
        for name in names:
            if self.has(name):
                overrides.append(name)
            else:
                to_seek.append(name)

        return to_seek

    def save_wait(self, cmd):
        # wait for background process to finish
        # only one instance running at time
        popen = subprocess.Popen(cmd, stdout=subprocess.PIPE, universal_newlines=True)
        for stdout_line in iter(popen.stdout.readline, ""):
            yield stdout_line

        popen.stdout.close()
        popen.wait()

    def save_to_lib(self, src_file, to_seek, save_all, wait, nb_libs, timestamp):
        """
        # auto-save materials
        # MatLib.save_to_lib(bpy.file,["current"], True, False, 1, -1)

        :param src_file:
        :param to_seek:
        :param save_all:
        :param wait:
        :param nb_libs:
        :param timestamp:
        :return:
        """
        # print("MatLib.save_to_lib() lib:%s save_all:%s wait:%s names:%s" % (self.name, save_all, wait, names))

        self.load_list()
        names = to_seek[:]
        to_save = []
        to_seek.clear()
        if save_all:
            to_save = names
            self.materials.update(names)
        else:
            # try to save on library the material belong
            for name in names:
                if self.has(name):
                    to_save.append(name)
                    self.materials.add(name)
                else:
                    to_seek.append(name)

        # print(len(to_save), to_save)

        if len(to_save) > 0:
            save_names = json.dumps(to_save, indent=None, sort_keys=True)
            generator = os.path.join(os.path.dirname(os.path.realpath(__file__)), "archipack_matlib.py")
            cmd = [
                bpy.app.binary_path,
                "--background",
                self.path,
                "--factory-startup",
                "-noaudio",
                "--python", generator,
                "--",
                "file:" + src_file,
                "name:" + save_names.replace(", ", ","),
                "clean:" + str(nb_libs),
                "stime:" + str(timestamp)
            ]
            print(" ".join(cmd))
            if wait:

                print("library: %s" % self.name)
                print("   %s\n" % ("\n   ".join(to_save)))

                for res in self.save_wait(cmd):
                    if "[log]" in res:
                        print(res[5:].replace("\n",""))

            else:
                # doesnt wait for background process to finish
                # run as many concurrent instances as library files
                subprocess.Popen(cmd)

        # else:
        #    print("MatLib.save_to_lib() to_seek:%s" % to_seek)


    def get_mat(self, name, link):
        """
            apply a material by name to active_object
            into slot index
            lazy load material list on demand
            return material or None
        """
        # Lazy load material names
        self.load_list()

        # material belongs to this library
        if self.has(name):

            # load material
            self.load_mat(name, link)
            mat = bpy.data.materials.get(name)
            if mat is not None and mat.grease_pencil is None:
                return mat

        return None


class MatlibsManager:
    """
        Manage multiple library
        Lazy load
    """
    __slots__ = ('matlibs', 'file_enum')

    def __init__(self):
        self.matlibs = []
        self.file_enum = []

    def cleanup(self):
        for lib in self.matlibs:
            lib.cleanup()
        self.matlibs.clear()

    @property
    def loaded_path(self):
        """
            Loaded matlibs filenames
        """
        return set([lib.path for lib in self.matlibs])

    def from_data(self, name):
        mat = bpy.data.materials.get(name)
        if mat is None or mat.grease_pencil is not None:
            return None
        return mat

    def has(self, name):
        for lib in self.matlibs:
            if lib.has(name):
                return True
        return False

    def add_to_list(self, path):
        """
            Add material library to list
            only store name of lib
            reloading here doesnt make sense
        """
        loaded_path = self.loaded_path
        self.matlibs.extend(
                [
                MatLib(path, f) for f in os.listdir(path)
                if f.endswith(".blend") and os.path.join(path, f) not in loaded_path
                ]
            )
        self.matlibs.sort(key=lambda m: m.name.lower())

    def load_libs(self, context):
        """
            list available library path
        """
        # default library
        # dir_path = os.path.dirname(os.path.realpath(__file__))
        # mat_path = os.path.join(dir_path, "materials")
        # self.add_to_list(mat_path)

        # user def library path from addon prefs
        if len(self.matlibs) < 1:
            try:
                prefs = get_prefs(context)
                self.add_to_list(prefs.matlib_path)
            except Exception as ex:
                print("Archipack: Unable to load default material library, please check path in addon prefs %s" % ex)
                pass

    def load_mat(self, name, link):
        mat = None
        for lib in self.matlibs:
            mat = lib.get_mat(name, link)
            if mat is not None:
                break
        return mat

    def apply(self, context, o, slot_index, name, link=False, make_unique=False):

        # material with same name exist in scene
        mat = self.from_data(name)

        # mat not in scene: try to load from lib
        if mat is None:
            # print("Archipack: material %s not found in scene, loading" % (name))
            # Lazy build matlibs list
            self.load_libs(context)
            mat = self.load_mat(name, link)

        # nothing found, build a default mat
        if mat is None:
            # print("Archipack: material %s not found at all, create default" % (name))
            mat = bpy.data.materials.new(name)

        if mat is not None:
            # print("MatlibsManager.apply()", name, slot_index)
            if USE_SLOTS:
                o.material_slots[slot_index].material = mat

            else:

                if slot_index >= len(o.data.materials):
                    o.data.materials.append(mat)
                else:
                    o.data.materials[slot_index] = mat

                o.data.materials.update()

        if make_unique:
            # break link
            o.active_material_index = slot_index
            ctx = context.copy()
            ctx['active_object'] = o
            bpy.ops.object.make_local(ctx, type="SELECT_OBDATA_MATERIAL")

    def check_override_on_save(self, context, names):

        self.load_libs(context)

        to_save = names[:]
        overrides = []
        for lib in self.matlibs:
            to_save = lib.check_override_on_save(to_save, overrides)
        return to_save, overrides

    def save_to_lib(self, context, names):

        self.load_libs(context)

        # when libs are > 1 will wait in the between so we only run one bg instance at time
        # save a copy of current file into temp dir
        import time
        timestamp = time.time()
        src_file = os.path.join(bpy.app.tempdir, "Archipack_temp.blend")
        bpy.ops.wm.save_as_mainfile(copy=True, filepath=src_file, check_existing=False)

        to_save = names[:]
        nb_libs = len(self.matlibs)
        wait = nb_libs > 1
        if wait:
            print("\n******************************\nArchipack save materials:\n")

        for lib in self.matlibs:
            lib.save_to_lib(src_file, to_save, nb_libs == 1, wait, nb_libs, timestamp)
            nb_libs -= 1

        if wait:
            print("******************************")

    def refresh_enum(self, context):
        self.load_libs(context)
        self.file_enum.clear()
        self.file_enum.append(('ALL', "All files", "All files", 0))
        self.file_enum.extend([
            (lib.name, lib.name.capitalize(), lib.name.capitalize(), i + 1) for i, lib in enumerate(self.matlibs)
        ])


class MaterialSetManager:
    """
        Manage material sets for objects
        Store material names for each set
        Lazy load at enumerate time
    """
    __slots__ = ('objects', 'enums', 'default_enum')

    def __init__(self):
        """
            Store sets for each object type
        """
        self.objects = {}
        # hold reference of dynamic enumerator
        self.enums = {}
        self.default_enum = [('DEFAULT', 'Default', '', 0)]

    def get_filename(self, object_type):

        target_path = os.path.join("presets", "archipack_materials")
        target_path = bpy.utils.user_resource('SCRIPTS',
                                                target_path,
                                                create=True)
        # return os.path.join(target_path, object_type) + '.txt'
        return os.path.join(target_path, object_type)

    def cleanup(self):
        self.objects.clear()
        self.enums.clear()

    def register_set(self, object_type, set_name, materials_names):

        if object_type not in self.objects.keys():
            self.objects[object_type] = {}

        self.objects[object_type][set_name.upper()] = materials_names

    def load_json(self, filename, object_type):
        res = False
        if object_type not in self.objects.keys():
            self.objects[object_type] = {}

        try:
            with open(filename + ".json", 'r') as f:
                self.objects[object_type].update(json.load(f))
            # print("Archipack: Loading success for {}.json".format(filename))
            res = True
        except Exception as ex:
            # print("Archipack: An error occured while loading {}.json".format(filename), ex)
            pass
        return res

    def load(self, object_type):
        # user preset path
        filename = self.get_filename(object_type)
        # factory preset path
        factory = os.path.join(
                os.path.dirname(os.path.realpath(__file__)),
                "presets",
                "archipack_materials",
                object_type
        )
        # load factory
        self.load_json(factory, object_type)
        # then override if any
        self.load_json(filename, object_type)

        if object_type in self.objects.keys():
            s_keys = self.objects[object_type].keys()
            self.make_enum(object_type, s_keys)

    def save(self, object_type):

        # always save in user prefs
        filename = self.get_filename(object_type)

        # print("filename:%s" % filename)
        if object_type not in self.objects.keys():
            return

        try:
            with open(filename + '.json', 'w') as f:
                json.dump(self.objects[object_type], f)
        except:
            print("Archipack: An error occured while saving {}.json".format(filename))
            pass

        s_keys = self.objects[object_type].keys()

        self.make_enum(object_type, s_keys)

    def add(self, o, set_name):
        if "archipack_material" in o:
            object_type = o.archipack_material[0].category
            materials_names = [slot.name for slot in o.material_slots if slot.name != '']
            self.register_set(object_type, set_name, materials_names)
            self.save(object_type)

    def remove(self, o):
        if "archipack_material" in o:
            d = o.archipack_material[0]
            object_type = d.category
            set_name = d.material
            s_keys = self.objects[object_type].keys()
            if set_name in s_keys:
                self.objects[object_type].pop(set_name)
                self.save(object_type)
            self.make_enum(object_type, s_keys)

    def get_materials(self, object_type, set_name):
        if object_type not in self.objects.keys():
            self.load(object_type)
        if object_type not in self.objects.keys():
            print("Archipack: Unknown object type {}".format(object_type))
            return None
        if set_name not in self.objects[object_type].keys():
            print("Archipack: set {} not found".format(set_name))
            return None
        return self.objects[object_type][set_name]

    def make_enum(self, object_type, s_keys):
        if len(s_keys) > 0:
            enum =  [(s.upper(), s.capitalize(), '', i) for i, s in enumerate(s_keys)]
            enum.sort(key=lambda x: x[0])
            self.enums[object_type] = enum

    def get_enum(self, object_type):

        if object_type not in self.objects.keys():
            self.load(object_type)

        if object_type not in self.objects.keys():
            self.objects[object_type] = {}

        if object_type in self.enums:
            return self.enums[object_type]

        return self.default_enum


def material_enum(self, context):
    global setman
    return setman.get_enum(self.category)


def update(self, context):
    self.update(context)


class archipack_material(Archipacki18n, PropertyGroup):

    category: StringProperty(
        name="Category",
        description="Archipack object name",
        default=""
    )
    material: EnumProperty(
        name="Material",
        description="Material Set name",
        items=material_enum,
        update=update
    )

    def apply_material(self, context, o, mats):
        global libman

        # skip holes and blinds ( tagged obj['archipack_skip_material'] = True )
        sel = set([c for c in o.children
                   if not ('archipack_skip_material' in c)])

        sel.add(o)
        n_mats = len(mats)

        for ob in sel:
            if hasattr(ob, "type") and ob.type in {'MESH', 'CURVE'}:
                if USE_SLOTS:
                    _slots = ob.material_slots
                    n_slots = len(_slots)
                    delta = n_mats - n_slots
                    if delta != 0:
                        # obj must be visible and active so use a context switcher
                        with ensure_select_and_restore(context, ob, [ob]):
                            if delta < 0:
                                for i in range(-delta):
                                    bpy.ops.object.material_slot_remove()
                            else:
                                for i in range(delta):
                                    bpy.ops.object.material_slot_add()

                    # update so slot changes are visible
                    ob.material_slots.update()

                    if len(ob.material_slots) != n_mats:
                        print("Archipack: Error", ob.name, "n_slots", len(ob.material_slots), "!= n_mats", n_mats)
                        return
                else:

                    delta = len(mats) - len(o.data.materials)

                    for i in range(-delta):
                        o.data.materials.pop(index=-1)

                for slot_index, mat_name in enumerate(mats):
                    libman.apply(context, ob, slot_index, mat_name, link=False)

    def update(self, context, o=None, only_active=False):
        global setman

        if o is None and hasattr(context, "object"):
            o = context.object

        if o is None and hasattr(context, "active_object"):
            o = context.active_object

        if o is None:
            return

        mats = setman.get_materials(self.category, self.material)

        if mats is None:
            return False

        if only_active:
            sel = [o]
        else:
            sel = context.selected_objects[:]
            if o not in sel:
                sel.append(o)
            sel = [ob for ob in sel
                   if "archipack_material" in ob and ob.archipack_material[0].category == self.category]

        for ob in sel:
            self.apply_material(context, ob, mats)

        # handle wall's holes ??
        if o.data and "archipack_wall2" in o.data:
            # must use modifiers to do so
            modif = o.modifiers.get("AutoMixedBoolean")
            if modif is not None and modif.object is not None:
                hole_obj = modif.object
                for m in hole_obj.modifiers:
                    if m.type == 'BOOLEAN' and m.object is not None:
                        self.apply_material(context, m.object, mats)
                self.apply_material(context, hole_obj, mats)

        return True

    def draw(self, context, layout):
        self.draw_label(context, layout, layout, "Load / save materials")
        row = layout.row(align=True)
        self.draw_prop(context, layout, row, self, 'material', text="")
        self.draw_op(context, layout, row, 'archipack.material_add', icon='ADD',
                     text="").material = self.material.capitalize()
        self.draw_op(context, layout, row, 'archipack.material_remove', icon='REMOVE', text="")


class archipack_material_override(PropertyGroup):
    material_name: StringProperty()
    override: BoolProperty(default=True)


class ARCHIPACK_OT_material(Operator):
    bl_idname = "archipack.material"
    bl_label = "Material"
    bl_description = "Add archipack material"
    bl_options = {'INTERNAL'}

    category: StringProperty(
        name="Category",
        description="Archipack object name",
        default=""
        )

    material: StringProperty(
        name="Material",
        description="Material Set name",
        default=""
        )

    @classmethod
    def poll(cls, context):
        return context.object is not None

    def execute(self, context):

        o = context.object

        # print("ARCHIPACK_OT_material.execute() material:", self.material, "category", self.category)

        if 'archipack_material' in o:
            m = o.archipack_material[0]
        else:
            m = o.archipack_material.add()

        m.category = self.category
        res = False

        try:
            if self.material != "":
                m.material = self.material
            res = True
        except Exception as ex:
            import traceback
            traceback.print_exc()
            print("Archipack: unable to add material {} for {} {}".format(self.material, self.category, ex))
            pass

        if res:
            # print("ARCHIPACK_OT_material.apply {} {}".format(self.category, self.material))
            return {'FINISHED'}
        else:
            print("Archipack: unable to add material {} for {}".format(self.material, self.category))
            self.report({'WARNING'}, 'Material {} for {} not found'.format(self.material, self.category))
            return {'CANCELLED'}


class ARCHIPACK_OT_material_add(Archipacki18n, Operator):
    bl_idname = "archipack.material_add"
    bl_label = "Material preset"
    bl_description = "Add a set of archipack material"
    bl_options = {'REGISTER', 'UNDO'}

    material: StringProperty(
        name="Preset",
        description="Material Set name",
        default=""
    )
    override: BoolProperty(
        default=True,
        name="Override",
        description="Override library material(s)"
    )
    overrides: CollectionProperty(type=archipack_material_override)

    materials = []

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "material")
        if len(self.overrides) > 0:
            self.draw_label(context, layout, layout, "Materials already exist in library:")
            box = layout.box()
            for ov in self.overrides:
                row = box.row(align=True)
                row.label(text=ov.material_name)
                if self.override:
                    row.prop(ov, "override", text="Replace")
            layout.prop(self, "override",
                           text="Replace materials in library ?",
                           icon="QUESTION")

    @classmethod
    def poll(cls, context):
        o = context.active_object
        return o is not None

    def invoke(self, context, event):
        global libman

        slots = context.active_object.material_slots

        materials = [slot.name for slot in slots if slot.material is not None]
        self.materials, overrides = libman.check_override_on_save(context, materials)
        self.overrides.clear()
        for name in overrides:
            ov = self.overrides.add()
            ov.material_name = name
        return context.window_manager.invoke_props_dialog(self)

    def execute(self, context):
        global libman
        global setman

        if self.material == "":
            print("Unable to save material set without name")
            self.report({'WARNING'}, 'Unable to save material set without name')
            return {'CANCELLED'}

        setman.add(context.active_object, self.material)
        if self.override:
            self.materials.extend([ov.material_name for ov in self.overrides if ov.override])

        libman.save_to_lib(context, self.materials)

        return {'FINISHED'}


class ARCHIPACK_OT_material_remove(Operator):
    bl_idname = "archipack.material_remove"
    bl_label = "Material"
    bl_description = "Remove a set of archipack material"
    bl_options = {'REGISTER', 'UNDO'}

    @classmethod
    def poll(cls, context):
        return context.active_object is not None

    def execute(self, context):

        global setman

        setman.remove(context.active_object)

        return {'FINISHED'}


###########################################
## Library
###########################################


def update_mat(self, context):
    self.update(context)


class archipack_matlib_item(PropertyGroup):
    label: StringProperty(default="Material")
    icon: IntProperty(default=0)


class ARCHIPACK_UL_Material(UIList):
    
    def draw_item(self, context, layout, data, item, icon, active_data, active_propname):

        self.use_filter_show = True
        self.use_filter_sort_alpha = True

        if self.layout_type in {'DEFAULT', 'COMPACT'}:
            layout.label(text=item.label.capitalize(), icon_value=item.icon)

        elif self.layout_type in {'GRID'}:
            layout.label(text=item.label.capitalize(), icon_value=item.icon)
            pass

    # Called once to filter/reorder items.
    def filter_items(self, context, data, propname):

        col = getattr(data, propname)
        filter_name = self.filter_name.lower()

        flt_flags = [self.bitflag_filter_item if any(
                filter_name in filter_set for filter_set in (
                    str(i), item.label.lower()
                    )
                )
            else 0 for i, item in enumerate(col, 1)
        ]

        if self.use_filter_sort_alpha:
            flt_neworder = [x[1] for x in sorted(
                    zip(
                        [x[0] for x in sorted(enumerate(col), key=lambda x: x[1].label)],
                        range(len(col))
            )
        )
            ]
        else:
            flt_neworder = []

        return flt_flags, flt_neworder


def files_enum(self, context):
    global libman
    libman.refresh_enum(context)
    return libman.file_enum


class archipack_matlib(PropertyGroup):

    files_filter: EnumProperty(
        name="Files",
        items=files_enum,
        update=update_mat
    )
    filter: BoolProperty(
        name="Filter",
        default=True,
        update=update_mat
    )

    materials: CollectionProperty(type=archipack_matlib_item)
    material_idx: IntProperty(default=0)
    source: EnumProperty(
        name="Source",
        items=(
            ('SCENE', 'Scene', 'Materials in current scene'),
            ('SELECTED', 'Selected', 'Selected objects'),
            ('LIBRARY', 'Library', 'Materials available in library'),
            ('FILE', 'File', 'Materials available in current file')
        ),
        default='SCENE',
        update=update_mat
    )

    def refresh_list(self, mats):
        global libman
        self.materials.clear()
        for mat in mats:
            m = self.materials.add()
            if type(mat).__name__ == 'str':
                m.label = mat
                mat = libman.from_data(mat)
                if mat is not None:
                    m.icon = mat.preview.icon_id

            else:
                m.label = mat.name
                m.icon = mat.preview.icon_id

    def selected(self, context):
        sel = context.selected_objects
        mats = []
        for o in sel:
            mats.extend([
                slot.material
                for slot in o.material_slots
                if slot.material is not None
            ])
        self.refresh_list(mats)

    def file(self, context):
        mats = bpy.data.materials[:]
        self.refresh_list(mats)

    def scene(self, context):
        mats = [mat for mat in bpy.data.materials if mat.users > 0]
        self.refresh_list(mats)

    def library(self, context):
        libman.load_libs(context)
        filename = self.files_filter
        if filename == "ALL" or not self.filter:
            for lib in libman.matlibs:
                lib.load_list()
            mats = [mat for lib in libman.matlibs for mat in lib.materials]
        else:
            mats = []
            for lib in libman.matlibs:
                if lib.name == filename:
                    lib.load_list()
                    mats.extend(lib.materials)
        self.refresh_list(mats)

    def update(self, context):

        if self.source == 'LIBRARY':
            self.library(context)
        elif self.source == 'SCENE':
            self.scene(context)
        elif self.source == 'SELECTED':
            self.selected(context)
        elif self.source == 'FILE':
            self.file(context)

    @property
    def selected_material(self):
        name = None
        if self.material_idx < len(self.materials):
            name = self.materials[self.material_idx].label
        return name

    def apply(self, context, o, slot_index, mat_name):
        global libman

        if mat_name is not None:
            if USE_SLOTS:
                if slot_index < 0:
                    n_slots = len(o.material_slots)
                    for index, slot in enumerate(o.material_slots):
                        slot_index = index
                        if slot.material is None:
                            slot_index -= 1
                            break
                    slot_index += 1

                    if slot_index >= n_slots:
                        bpy.ops.object.material_slot_add()
                        slot_index = n_slots
            else:
                if slot_index < 0:
                    slot_index = len(o.data.materials)

            libman.apply(context, o, slot_index, mat_name, link=False, make_unique=False)
            self.update(context)


class ARCHIPACK_PT_matlib(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_matlib"
    bl_label = "Archipack material library"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "material"

    @classmethod
    def poll(cls, context):
        return context.active_object is not None and hasattr(context.window_manager, "archipack_matlib")

    def draw(self, context):
        global libman

        layout = self.layout
        icons = icon_man["main"]
        o = context.active_object
        d = context.window_manager.archipack_matlib
        mat_name = d.selected_material
        self.draw_prop(context, layout, layout, d, 'source', expand=True)

        row = layout.row()
        # col = row.column(align=True)
        # col2 = row.column(align=True)

        if d.source == 'LIBRARY':
            self.draw_prop(context, layout, row, d, "files_filter", text="")
            col = row.column(align=True)
            self.draw_prop(context, layout, col, d, "filter", text="", icon="FILTER")

        row = layout.row()
        row.template_list("ARCHIPACK_UL_Material", "", d, "materials", d, "material_idx", rows=1, maxrows=5)

        col = row.column(align=True)
        if mat_name is None:
            col.enabled = False
            mat_name = ""

        # operators
        # if mat_name is not None:

        op = self.draw_op(context, layout, col, "archipack.material_apply",
                 icon_value=icons['material_apply'].icon_id,
                 text=""
                 )
        op.slot_index = o.active_material_index
        op.mat_name = mat_name

        self.draw_op(context, layout, col, "archipack.material_append",
             icon_value=icons['material_add'].icon_id,
             text="").mat_name = mat_name

        self.draw_op(context, layout, col, "archipack.material_preview",
             icon_value=icons['material_preview'].icon_id,
             text="").mat_name = mat_name

        col.separator()
        if d.source != 'LIBRARY':
            self.draw_op(context, layout, col, "archipack.material_save",
                     icon="FILE_TICK",
                     text="").mat_name = mat_name

        if libman.has(mat_name) and libman.from_data(mat_name) is not None:
            self.draw_op(context, layout, col, "archipack.material_reload",
                     icon="FILE_REFRESH",
                     text="").mat_name = mat_name


class ARCHIPACK_OT_material_save(Archipacki18n, Operator):
    bl_idname = "archipack.material_save"
    bl_label = "Save"
    bl_description = "Save a material to library"
    bl_options = {'REGISTER', 'UNDO'}

    mat_name: StringProperty(default="")
    override: BoolProperty(
        default=True,
        name="Override",
        description="Override material"
    )
    overrides: CollectionProperty(type=archipack_material_override)

    materials = []

    def draw(self, context):
        layout = self.layout
        if len(self.overrides) > 0:
            self.draw_label(context, layout, layout, "Materials already exist in library:")
            box = layout.box()
            for ov in self.overrides:
                row = box.row(align=True)
                row.label(text=ov.material_name)
            layout.prop(self, "override",
                           text="Replace materials in library ?",
                           icon="QUESTION")

    @classmethod
    def poll(cls, context):
        return True

    def invoke(self, context, event):
        global libman


        if bpy.data.materials.get(self.mat_name) is None:
            return {'CANCELLED'}

        self.materials, overrides = libman.check_override_on_save(context, [self.mat_name])
        self.overrides.clear()
        for name in overrides:
            ov = self.overrides.add()
            ov.material_name = name

        if len(self.overrides) < 1:
            return self.execute(context)

        return context.window_manager.invoke_props_dialog(self)

    def execute(self, context):
        global libman

        if self.override:
            self.materials.extend([ov.material_name for ov in self.overrides])

        if len(self.materials) > 0:
            libman.save_to_lib(context, self.materials)

        return {'FINISHED'}


class ARCHIPACK_OT_material_reload(Archipacki18n, Operator):
    bl_idname = "archipack.material_reload"
    bl_label = "Reload"
    bl_description = "Reload a material from library and replace all instances"
    bl_options = {'REGISTER', 'UNDO'}
    mat_name: StringProperty(default="")

    @classmethod
    def poll(cls, context):
        global libman
        return libman is not None

    def execute(self, context):
        global libman
        old_mat = bpy.data.materials.get(self.mat_name)
        if old_mat is not None:
            if libman.has(old_mat.name):
                old_mat.name = "{}_tmp".format(self.mat_name)
                new_mat = libman.load_mat(self.mat_name, False)
                if new_mat is not None:
                    old_mat.user_remap(new_mat)
                    bpy.data.materials.remove(old_mat)
                else:
                    old_mat.name = self.mat_name
        return {'FINISHED'}


class ARCHIPACK_OT_material_reload_all(Archipacki18n, Operator):
    bl_idname = "archipack.material_reload_all"
    bl_label = "Reload"
    bl_description = "Reload all materials from library and replace all instances"
    bl_options = {'REGISTER', 'UNDO'}

    @classmethod
    def poll(cls, context):
        global libman
        return libman is not None

    def execute(self, context):
        global libman
        libman.load_libs(context)
        for lib in libman.matlibs:
            lib.load_list()
        for old_mat in bpy.data.materials:
            mat_name = old_mat.name
            if libman.has(mat_name):
                old_mat.name = "{}_tmp".format(mat_name)
                new_mat = libman.load_mat(mat_name, False)
                if new_mat is not None:
                    print("reload %s" % mat_name)
                    old_mat.user_remap(new_mat)
                    bpy.data.materials.remove(old_mat)
                else:
                    old_mat.name = mat_name
        return {'FINISHED'}


class ARCHIPACK_OT_material_apply(Archipacki18n, Operator):
    bl_idname = "archipack.material_apply"
    bl_label = "Apply"
    bl_description = "Apply material to selected slot"
    bl_options = {'REGISTER', 'UNDO'}
    mat_name: StringProperty(default="")
    slot_index: IntProperty(default=-1)

    @classmethod
    def poll(cls, context):
        global libman
        return libman is not None and context.active_object is not None

    def execute(self, context):
        o = context.active_object
        context.window_manager.archipack_matlib.apply(context, o, self.slot_index, self.mat_name)
        return {'FINISHED'}


class ARCHIPACK_OT_material_append(Archipacki18n, Operator):
    bl_idname = "archipack.material_append"
    bl_label = "Append"
    bl_description = "Append material to object slots"
    bl_options = {'REGISTER', 'UNDO'}
    mat_name: StringProperty(default="")
    @classmethod
    def poll(cls, context):
        global libman
        return libman is not None and context.active_object is not None

    def execute(self, context):
        o = context.active_object
        context.window_manager.archipack_matlib.apply(context, o, -1, self.mat_name)
        return {'FINISHED'}


class ARCHIPACK_OT_material_preview(Archipacki18n, ArchipackCreateTool, Operator):
    bl_idname = "archipack.material_preview"
    bl_label = "Preview"
    bl_description = "Preview material"
    bl_options = {'REGISTER', 'UNDO'}
    mat_name: StringProperty(default="")

    @classmethod
    def poll(cls, context):
        global libman
        return libman is not None

    def execute(self, context):
        o_name = self.translate("Archipack Material Preview")
        o = bpy.data.objects.get(o_name)
        if o is None:
            me = bpy.data.meshes.new(name=o_name)
            o = bpy.data.objects.new(name=o_name, object_data=me)
            context.collection.objects.link(o)
            self.hide_object(o)
            o.hide_render = True

        context.window_manager.archipack_matlib.apply(context, o, 0, self.mat_name)
        self.select_object(context, o, True)

        return {'FINISHED'}


class ARCHIPACK_OT_material_library(Operator):
    bl_idname = "archipack.material_library"
    bl_label = "Material Library"
    bl_description = "Add all archipack materials on a single object"
    bl_options = {'REGISTER', 'UNDO'}

    @classmethod
    def poll(cls, context):
        return context.active_object is not None

    def execute(self, context):

        global setman

        o = context.active_object

        if 'archipack_material' in o:
            m = o.archipack_material[0]
        else:
            m = o.archipack_material.add()

        for i in range(len(o.material_slots)):
            bpy.ops.object.material_slot_remove()

        for category in setman.objects.keys():
            prefix = category.capitalize() + "_"
            for part in setman.objects[category]["DEFAULT"]:
                name = prefix + part
                mat = m.get_material(name)
                o.data.materials.append(mat)

        return {'FINISHED'}


@persistent
def cleanup(dummy):
    global libman
    global setman
    if libman is not None:
        libman.cleanup()
    if setman is not None:
        setman.cleanup()


def register():
    global libman
    global setman
    libman = MatlibsManager()
    setman = MaterialSetManager()
    bpy.utils.register_class(archipack_material_override)
    bpy.utils.register_class(archipack_material)
    Object.archipack_material = CollectionProperty(type=archipack_material)
    bpy.utils.register_class(ARCHIPACK_OT_material)
    bpy.utils.register_class(ARCHIPACK_OT_material_add)
    bpy.utils.register_class(ARCHIPACK_OT_material_remove)
    bpy.utils.register_class(ARCHIPACK_OT_material_library)

    bpy.utils.register_class(archipack_matlib_item)
    bpy.utils.register_class(archipack_matlib)
    WindowManager.archipack_matlib = PointerProperty(type=archipack_matlib)

    bpy.utils.register_class(ARCHIPACK_OT_material_apply)
    bpy.utils.register_class(ARCHIPACK_OT_material_append)
    bpy.utils.register_class(ARCHIPACK_OT_material_preview)
    bpy.utils.register_class(ARCHIPACK_OT_material_save)
    bpy.utils.register_class(ARCHIPACK_OT_material_reload)
    bpy.utils.register_class(ARCHIPACK_OT_material_reload_all)

    bpy.utils.register_class(ARCHIPACK_UL_Material)
    bpy.utils.register_class(ARCHIPACK_PT_matlib)
    bpy.app.handlers.load_pre.append(cleanup)


def unregister():
    global libman
    global setman
    bpy.utils.unregister_class(ARCHIPACK_OT_material_library)
    bpy.utils.unregister_class(ARCHIPACK_OT_material_remove)
    bpy.utils.unregister_class(ARCHIPACK_OT_material_add)
    bpy.utils.unregister_class(ARCHIPACK_OT_material)
    del Object.archipack_material
    bpy.utils.unregister_class(archipack_material)
    bpy.utils.unregister_class(archipack_material_override)
    bpy.utils.unregister_class(ARCHIPACK_PT_matlib)
    bpy.utils.unregister_class(ARCHIPACK_UL_Material)
    bpy.utils.unregister_class(archipack_matlib)
    bpy.utils.unregister_class(archipack_matlib_item)
    bpy.utils.unregister_class(ARCHIPACK_OT_material_apply)
    bpy.utils.unregister_class(ARCHIPACK_OT_material_append)
    bpy.utils.unregister_class(ARCHIPACK_OT_material_preview)
    bpy.utils.unregister_class(ARCHIPACK_OT_material_save)
    bpy.utils.unregister_class(ARCHIPACK_OT_material_reload)
    bpy.utils.unregister_class(ARCHIPACK_OT_material_reload_all)

    del WindowManager.archipack_matlib
    """
    """
    bpy.app.handlers.load_pre.remove(cleanup)
    if libman is not None:
        libman.cleanup()
    if setman is not None:
        setman.cleanup()
