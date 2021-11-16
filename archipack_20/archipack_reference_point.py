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
from bpy.types import Operator, PropertyGroup, Object, Panel
from bpy.props import (
    FloatVectorProperty,
    CollectionProperty,
    IntProperty,
    FloatProperty,
    StringProperty,
    EnumProperty,
    BoolProperty
    )
from mathutils import Vector
from .bmesh_utils import BmeshEdit as bmed
from .archipack_abstraction import ensure_select_and_restore
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackObjectsManager, ArchipackObject,
    ArchipackPanel,
    stop_auto_manipulate
)
import time


def update(self, context):
    self.update(context)


class archipack_building(ArchipackObject, PropertyGroup):
    """Provide Building concept for ifc export
    """
    symbol_scale: FloatProperty(
        name="Screen scale",
        default=1,
        min=0.01,
        update=update
    )

    @classmethod
    def poll(cls, o):
        return o and \
            ArchipackObjectsManager.is_selected(cls, o) and \
            archipack_building.filter(o)

    @classmethod
    def filter(cls, o):
        """
            Filter object with this class in data
            return
            True when object contains this datablock
            False otherwhise
            usage:
            class_name.filter(object) from outside world
            self.__class__.filter(object) from instance
        """
        try:
            return cls.__name__ in o and getattr(o, cls.__name__)[0]
        except:
            pass
        return False

    @classmethod
    def datablock(cls, o):
        """
            Retrieve datablock from base object
            return
                datablock when found
                None when not found
            usage:
                class_name.datablock(object) from outside world
                self.__class__.datablock(object) from instance
        """
        try:
            return getattr(o, cls.__name__)[0]
        except:
            pass
        return None

    def buildmesh(self, o):

        s = self.symbol_scale

        verts = [(s * x, s * y, s * z) for x, y, z in [
            (0.341, -0.341, 0.0), (0.341, -0.341, 0.647), (-0.341, -0.341, 0.0), (-0.341, -0.341, 0.647),
            (0.341, 0.341, 0.0), (0.341, 0.341, 0.647), (-0.341, 0.341, 0.0), (-0.341, 0.341, 0.647),
            (0.0, -0.341, 0.897), (0.0, 0.341, 0.897), (0.0, -0.5, 0.897), (-0.5, -0.5, 0.53),
            (-0.5, 0.5, 0.53), (0.0, 0.5, 0.897), (0.5, 0.5, 0.53), (0.5, -0.5, 0.53),
            (0.0, -0.5, 1.0), (-0.5, -0.5, 0.633), (-0.5, 0.5, 0.633), (0.0, 0.5, 1.0),
            (0.5, 0.5, 0.633), (0.5, -0.5, 0.633)
        ]]

        edges = [
            (2, 0), (0, 1), (8, 3),
            (3, 2), (6, 2), (3, 7),
            (7, 6), (4, 6), (9, 5),
            (5, 4), (0, 4), (5, 1),
            (1, 8), (7, 9), (10, 11),
            (11, 12), (13, 14), (14, 15),
            (15, 10), (12, 13), (11, 17),
            (16, 17), (17, 18), (19, 20),
            (20, 21), (21, 16), (18, 19),
            (16, 19), (18, 12), (15, 21),
            (9, 13), (8, 10), (20, 14)
        ]

        bm = bmed._start(o)
        bm.clear()
        for v in verts:
            bm.verts.new(v)
        bm.verts.ensure_lookup_table()
        for ed in edges:
            bm.edges.new((bm.verts[ed[0]], bm.verts[ed[1]]))
        bmed._end(bm, o)

    def update(self, context, o=None):

        o = self.find_in_selection(context, True)

        if o is None:
            return

        self.buildmesh(o)

        self.restore_context(context)

    def manipulable_invoke(self, context, o):
        return

    def on_delete(self, context, o):
        sel = [c for c in context.selected_objects if c.name != o.name and self.filter(c)]
        for c in sel:
            self.delete_object(context, c)
        return


class ARCHIPACK_OT_building(ArchipackObjectsManager, Operator):
    """Add building"""
    bl_idname = "archipack.building"
    bl_label = "Building"
    bl_description = "Add building"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL'}

    @classmethod
    def poll(cls, context):
        return True

    def create(self, context, loc):
        """
        :param context:
        :param loc:
        :param location_3d:
        :return:
        """
        m = bpy.data.meshes.new(name="Building")
        o = bpy.data.objects.new("Building", m)
        o.matrix_world.translation = loc
        o.location = loc
        d = o.archipack_building.add()
        self.link_object_to_scene(context, o, layer_name="Reference")
        o.color = (1, 0, 0, 1)
        d.buildmesh(o)
        return o

    def execute(self, context):
        if context.mode == "OBJECT":
            loc = self.get_cursor_location(context)
            o = self.create(context, loc)
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_PT_building(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_building"
    bl_label = "Building"

    @classmethod
    def poll(cls, context):
        return archipack_building.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_building.datablock(o)
        if d is None:
            return

        layout = self.layout

        self.draw_translate(context, layout)

        box = layout.box()
        self.draw_op(context, layout, box, 'archipack.delete', icon="TRASH")
        self.draw_prop(context, layout, layout, d, 'symbol_scale')


class archipack_reference_point(ArchipackObject, PropertyGroup):

    location_2d: FloatVectorProperty(
        subtype='XYZ',
        name="position 2d",
        default=Vector((0, 0, 0))
    )
    rotation_3d: FloatProperty(
        default=0
    )
    location_3d: FloatVectorProperty(
        subtype='XYZ',
        name="position 3d",
        default=Vector((0, 0, 0))
    )
    symbol_scale: FloatProperty(
        name="Screen scale",
        default=1,
        min=0.01,
        update=update
    )

    @classmethod
    def poll(cls, o):
        return o and \
            ArchipackObjectsManager.is_selected(cls, o) and \
            archipack_reference_point.filter(o)

    @classmethod
    def filter(cls, o):
        """
            Filter object with this class in data
            return
            True when object contains this datablock
            False otherwhise
            usage:
            class_name.filter(object) from outside world
            self.__class__.filter(object) from instance
        """
        try:
            return cls.__name__ in o and getattr(o, cls.__name__)[0]
        except:
            pass
        return False

    @classmethod
    def datablock(cls, o):
        """
            Retrieve datablock from base object
            return
                datablock when found
                None when not found
            usage:
                class_name.datablock(object) from outside world
                self.__class__.datablock(object) from instance
        """
        try:
            return getattr(o, cls.__name__)[0]
        except:
            pass
        return None

    def buildmesh(self, o):

        s = self.symbol_scale

        verts = [(s * x, s * y, s * z) for x, y, z in [
            (-0.25, 0.25, 0.0), (0.25, 0.25, 0.0), (-0.25, -0.25, 0.0), (0.25, -0.25, 0.0),
            (0.0, 0.0, 0.487), (-0.107, 0.107, 0.216), (0.108, 0.107, 0.216), (-0.107, -0.107, 0.216),
            (0.108, -0.107, 0.216), (-0.05, 0.05, 0.5), (0.05, 0.05, 0.5), (0.05, -0.05, 0.5),
            (-0.05, -0.05, 0.5), (-0.193, 0.193, 0.0), (0.193, 0.193, 0.0), (0.193, -0.193, 0.0),
            (-0.193, -0.193, 0.0), (0.0, 0.0, 0.8), (0.0, 0.8, 0.0), (0.0, 0.0, 0.0),
            (0.0, 0.0, 0.0), (0.05, 0.05, 0.674), (-0.05, 0.674, 0), (0.0, 0.8, 0.0),
            (-0.05, -0.05, 0.674), (-0.05, 0.674, 0.05), (0.05, 0.674, 0), (-0.129, 0.129, 0.162),
            (0.129, 0.129, 0.162), (-0.129, -0.129, 0.162), (0.129, -0.129, 0.162), (0.0, 0.0, 0.8),
            (-0.05, 0.05, 0.674), (0.05, -0.05, 0.674), (0.05, 0.674, 0.05), (0.8, 0.0, 0.0),
            (0.0, 0.0, 0.0), (0.674, 0.05, 0), (0.8, 0.0, 0.0), (0.674, 0.05, 0.05),
            (0.674, -0.05, 0), (0.674, -0.05, 0.05)]]

        edges = [(1, 0), (0, 9), (9, 10), (10, 1), (3, 1), (10, 11),
                 (11, 3), (2, 3), (11, 12), (12, 2), (0, 2), (12, 9),
                 (6, 5), (8, 6), (7, 8), (5, 7), (17, 24), (17, 20),
                 (18, 25), (18, 19), (13, 14), (14, 15), (15, 16), (16, 13),
                 (4, 6), (15, 30), (17, 21), (26, 22), (23, 22), (23, 34),
                 (18, 26), (28, 27), (30, 28), (29, 30), (27, 29), (14, 28),
                 (13, 27), (16, 29), (4, 7), (4, 8), (4, 5), (31, 33),
                 (31, 32), (21, 32), (24, 32), (24, 33), (21, 33), (25, 22),
                 (25, 34), (26, 34), (35, 39), (35, 36), (40, 37), (38, 37),
                 (38, 41), (35, 40), (39, 37), (39, 41), (40, 41)]


        bm = bmed._start(o)
        bm.clear()
        for v in verts:
            bm.verts.new(v)
        bm.verts.ensure_lookup_table()
        for ed in edges:
            bm.edges.new((bm.verts[ed[0]], bm.verts[ed[1]]))
        bmed._end(bm, o)

    def update(self, context, o=None):

        o = self.find_in_selection(context, True)

        if o is None:
            return

        self.buildmesh(o)

        self.restore_context(context)

    def manipulable_invoke(self, context, o):
        return

    def on_delete(self, context, o):
        sel = [c for c in context.selected_objects if c.name != o.name and self.filter(c)]
        for c in sel:
            self.delete_object(context, c)
        return


class ARCHIPACK_PT_reference_point(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_reference_point"
    bl_label = "Reference point"

    @classmethod
    def poll(cls, context):
        return archipack_reference_point.poll(context.active_object)

    def can_merge(self, context):
        can_merge = False
        o = context.object
        if o is not None:
            name = o.name
            can_merge = any([archipack_reference_point.filter(o) and o.name != name for o in context.selected_objects])
        return can_merge

    def draw(self, context):
        o = context.active_object
        d = archipack_reference_point.datablock(o)
        if d is None:
            return

        layout = self.layout

        self.draw_translate(context, layout)

        box = layout.box()
        self.draw_op(context, layout, box, 'archipack.delete', icon="TRASH")

        if self.can_merge(context):
            self.draw_op(context, layout, box, 'archipack.merge_references')

        box = layout.box()
        self.draw_label(context, layout, box, "Move to 2d / 3d")
        if (o.matrix_world.translation - d.location_2d).length < 0.01:
            self.draw_op(context, layout, box, 'archipack.move_to_3d')
        else:
            self.draw_op(context, layout, box, 'archipack.move_to_2d')
            self.draw_op(context, layout, box, 'archipack.store_2d_reference')

        self.draw_prop(context, layout, layout, d, 'symbol_scale')

        box = layout.box()
        self.draw_label(context, layout, box, "Project structure")
        self.draw_op(context, layout, box, 'archipack.duplicate_level', icon="DUPLICATE")
        self.draw_op(context, layout, box, 'archipack.make_level_collection', icon="GROUP")
        self.draw_op(context, layout, box, 'archipack.make_building_collection', icon="GROUP")


class ARCHIPACK_OT_add_reference_point(ArchipackObjectsManager, Operator):
    """Add reference point"""
    bl_idname = "archipack.add_reference_point"
    bl_label = "Reference point"
    bl_description = "Add reference point"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL'}
    symbol_type: EnumProperty(
        name="Symbol type",
        default='WALL',
        items=(
            ('WALL', 'Wall', '', 0),
            ('ROOF', 'Roof', '', 1)
        )
    )

    @classmethod
    def poll(cls, context):
        return True

    def create(self, context, loc, location_3d=Vector((0, 0, 0))):
        """
        :param context:
        :param loc:
        :param location_3d:
        :return:
        """
        x, y, z = loc
        m = bpy.data.meshes.new(name="Reference")
        o = bpy.data.objects.new("Reference", m)
        o.matrix_world.translation = loc
        o.location = loc
        d = o.archipack_reference_point.add()
        d.location_2d = Vector((x, y, 0))
        d.location_3d = location_3d
        self.link_object_to_scene(context, o)
        o.color = (1, 0, 0, 1)

        # unable to select in context copy
        d.buildmesh(o)
        return o

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.object

            ref = self.get_reference_point(o)

            if ref is None:
                # print("archipack.add_reference_point.execute() ref not found", o.name)
                # print([Vector(b) for b in o.bound_box[:]])
                loc = o.matrix_world @ Vector(o.bound_box[0])
                if "archipack_wall2" in o.data:
                    loc.z += o.data.archipack_wall2[0].z_offset
                ref = self.create(context, loc)

            with ensure_select_and_restore(context, ref, context.selected_objects):
                ARCHIPACK_OT_parent_to_reference.parent_to_reference(self, context, ref)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}
        
        
class ARCHIPACK_OT_reference_point(ArchipackObjectsManager, Operator):
    """Add reference point"""
    bl_idname = "archipack.reference_point"
    bl_label = "Reference point"
    bl_description = "Add reference point"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL'}
    location_3d: FloatVectorProperty(
        subtype='XYZ',
        name="position 3d",
        default=Vector((0, 0, 0))
    )
    symbol_type: EnumProperty(
        name="Symbol type",
        default='WALL',
        items=(
            ('WALL', 'Wall', '', 0),
            ('ROOF', 'Roof', '', 1))
    )

    @classmethod
    def poll(cls, context):
        return True

    def execute(self, context):
        if context.mode == "OBJECT":

            o = ARCHIPACK_OT_add_reference_point.create(
                self, 
                context,
                self.get_cursor_location(context)
                )
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_make_level_collection(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.make_level_collection"
    bl_label = "Create Level Collection"
    bl_description = "Create a level collection from active reference point"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL'}
    collection_name: StringProperty(
        name="Name",
        default="Level"
    )

    @classmethod
    def poll(cls, context):
        return archipack_reference_point.poll(context.active_object)

    def execute(self, context):
        ref = context.active_object
        sel = [ref]
        self.collect_childs(ref, sel)
        main = self.create_collection(context.scene.collection, "Levels")
        coll = self.create_collection(main, self.collection_name)
        for o in sel:
            coll.objects.link(o)
        return {'FINISHED'}

    def invoke(self, context, event):
        return context.window_manager.invoke_props_dialog(self)


class ARCHIPACK_OT_make_building_collection(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.make_building_collection"
    bl_label = "Create Building Instance"
    bl_description = "Create a building collection instance from multiple selected references"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL'}
    collection_name: StringProperty(
        name="Name",
        default="Building"
    )

    def filter_instance_collection(self, o):
        return o.type == "EMPTY" and o.instance_type == "COLLECTION" and o.instance_collection is not None

    @classmethod
    def poll(cls, context):
        return archipack_reference_point.poll(context.active_object)

    def execute(self, context):
        refs = [o for o in context.selected_objects
                if archipack_reference_point.filter(o) or self.filter_instance_collection(o)
                ]

        main = self.create_collection(context.scene.collection, "Buildings")
        coll = self.create_collection(main, self.collection_name)

        for ref in refs:
            sel = [ref]
            if archipack_reference_point.filter(ref):
                self.collect_childs(ref, sel)
                sub = self.create_collection(coll, ref.name)
                try:
                    coll.children.link(sub)
                except:
                    pass
                for c in sel:
                    sub.objects.link(c)
            else:
                coll.objects.link(ref)

        offset = -context.active_object.matrix_world.translation
        bpy.ops.object.collection_instance_add(
            collection=coll.name,
            align='WORLD',
            location=offset)

        instance = context.active_object
        instance.location = offset

        old = instance.users_collection[:]

        for c in old:
            c.objects.unlink(instance)

        main.objects.link(instance)

        self.select_object(context, instance, True)

        return {'FINISHED'}

    def invoke(self, context, event):
        return context.window_manager.invoke_props_dialog(self)


class ARCHIPACK_OT_move_to_3d(Operator):
    bl_idname = "archipack.move_to_3d"
    bl_label = "Move to 3d"
    bl_description = "Move point to 3d position"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL', 'UNDO'}

    @classmethod
    def poll(cls, context):
        return archipack_reference_point.poll(context.active_object)

    def execute(self, context):
        if context.mode == "OBJECT":
            sel = context.selected_objects[:]
            for o in sel:
                d = archipack_reference_point.datablock(o)
                if d is None:
                    continue
                o.matrix_world.translation = d.location_3d
                o.rotation_euler.z = d.rotation_3d
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_kill_archipack(Operator):
    bl_idname = "archipack.kill_archipack"
    bl_label = "Do you realy want to kill archipack parameters ?"
    bl_description = "Kill archipack parameters, objects will no more be editable via parameters"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL', 'UNDO'}
    selected_only: BoolProperty(default=False)

    @classmethod
    def poll(cls, context):
        return context.mode == "OBJECT"

    def apply(self, context, objects):

        for o in objects:

            keys = o.keys()
            for key in keys:
                if "archipack_" in key:
                    try:
                        # will fail for holes
                        o.property_unset(key)
                    except:
                        pass
                    try:
                        del o[key]
                    except:
                        pass

            if o.data is not None:
                keys = o.data.keys()
                for key in keys:
                    if "archipack_" in key:
                        o.data.property_unset(key)

    def invoke(self, context, event):
        return context.window_manager.invoke_confirm(self, event)

    def execute(self, context):
        if context.mode == "OBJECT":

            bpy.ops.archipack.disable_manipulate()

            if self.selected_only:
                objects = context.selected_objects[:]
            else:
                objects = context.scene.objects[:]

            self.apply(context, objects)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_move_to_2d(Operator):
    bl_idname = "archipack.move_to_2d"
    bl_label = "Move to 2d"
    bl_description = "Move point to 2d position"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL', 'UNDO'}

    @classmethod
    def poll(cls, context):
        return archipack_reference_point.poll(context.active_object)

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            sel = context.selected_objects[:]
            for o in sel:
                d = archipack_reference_point.datablock(o)
                if d is None:
                    continue
                d.location_3d = o.matrix_world.translation
                d.rotation_3d = o.rotation_euler.z
                o.matrix_world.translation = d.location_2d
                o.rotation_euler.z = 0
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_store_2d_reference(Operator):
    bl_idname = "archipack.store_2d_reference"
    bl_label = "Set 2d"
    bl_description = "Set 2d reference position"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL', 'UNDO'}

    @classmethod
    def poll(cls, context):
        return archipack_reference_point.poll(context.active_object)

    def execute(self, context):
        if context.mode == "OBJECT":
            sel = context.selected_objects[:]
            for o in sel:
                d = archipack_reference_point.datablock(o)
                if d is None:
                    continue
                x, y, z = o.matrix_world.translation
                d.location_2d = Vector((x, y, 0))
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_parent_to_reference(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.parent_to_reference"
    bl_label = "Parent"
    bl_description = "Make selected object childs of parent reference point"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    @classmethod
    def poll(cls, context):
        # filter only: reference point doesn't need to be selected
        return archipack_reference_point.filter(context.object)

    def parent_to_reference(self, context, o):
        """parent selected objects to reference point
        :param context:
        :param o: reference point object
        """
        self.unselect_object(context, o)
        bpy.ops.object.parent_clear(type='CLEAR_KEEP_TRANSFORM')
        self.select_object(context, o, True)
        bpy.ops.object.parent_set(type='OBJECT', keep_transform=False)
        self.unselect_object(context, o)
        # update by hand before scene update
        # itM = o.matrix_world.inverted()
        # for c in context.selected_objects:
        #    c.location = itM @ c.matrix_world.translation

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            self.parent_to_reference(context, o)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_merge_references(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.merge_references"
    bl_label = "Merge"
    bl_description = "Merge selected references into active one"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    @classmethod
    def poll(cls, context):
        # filter only: reference point doesn't need to be selected
        return archipack_reference_point.filter(context.object)

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.object
            sel = [c for c in context.selected_objects
                   if archipack_reference_point.filter(c) and c.name != o.name]

            selected_objects = []
            for c in sel:
                selected_objects.extend(c.children)

            with ensure_select_and_restore(context, o, selected_objects) as (ctx, act, _sel):
                ARCHIPACK_OT_parent_to_reference.parent_to_reference(self, ctx, act)

            for c in sel:
                if archipack_reference_point.datablock(c) is not None:
                    self.delete_object(context, c)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_remove_references(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.remove_references"
    bl_label = "Delete all references"
    bl_description = "Remove all references, warning there is no undo !"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    selected_only: BoolProperty(default=False)

    @classmethod
    def poll(cls, context):
        # filter only: reference point doesn't need to be selected
        return True

    def execute(self, context):
        if context.mode == "OBJECT":

            if self.selected_only:
                objects = context.selected_objects[:]
            else:
                objects = context.scene.objects[:]

            refs = [o for o in objects if archipack_reference_point.filter(o)]

            if len(refs) < 1:
                return {'CANCELLED'}

            selected_objects = []
            for o in refs:
                selected_objects.extend(o.children)

            ctx = context.copy()
            ctx['object'] = selected_objects[0]
            ctx['selected_editable_objects'] = selected_objects
            bpy.ops.object.parent_clear(ctx, type='CLEAR_KEEP_TRANSFORM')
            for o in refs:
                self.delete_object(context, o)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_duplicate_level(ArchipackObjectsManager, Operator):
    """Make a copy of this level"""
    bl_idname = "archipack.duplicate_level"
    bl_label = "Duplicate level"
    bl_description = "Duplicate one or more level"
    bl_category = 'Archipack'
    bl_options = {'INTERNAL', 'REGISTER', 'UNDO'}
    collection_name: StringProperty(
        name="Name",
        default="Level"
    )
    copy_method: EnumProperty(
        name="Copy",
        description="Duplication method",
        default='LINKED',
        items=(
            ('COPY', 'Copy', '', 0),
            ('LINKED', 'Linked', '', 1),
            ('COLLECTION', "Collection", "Collection instance", 2)
        )
    )
    spacing: FloatProperty(
        name="Spacing",
        description="Vertical space between levels",
        default=3.0,
        min=1.0
    )
    duplicates: IntProperty(
        name="Levels",
        description="levels to add",
        min=1,
        default=1
    )
    
    @classmethod
    def poll(cls, context):
        return archipack_reference_point.poll(context.active_object)

    def execute(self, context):
        tim = time.time()
        if context.mode == "OBJECT":
            context.window.cursor_set("WAIT")
            o = context.active_object

            if self.copy_method == 'COLLECTION':

                sel = [o]
                self.collect_childs(o, sel)
                main = self.create_collection(context.scene.collection, "Levels")
                coll = self.create_collection(main, o.name)
                for c in sel:
                    coll.objects.link(c)
                for i in range(self.duplicates):

                    bpy.ops.object.collection_instance_add(
                        collection=coll.name,
                        align='WORLD',
                        location=(0, 0, (i + 1) * self.spacing))

                    instance = context.object
                    old = instance.users_collection[:]

                    for c in old:
                        c.objects.unlink(instance)

                    main.objects.link(instance)

            else:

                objs = self.filter_selection(o.children)

                bpy.ops.archipack.disable_manipulate()
                with stop_auto_manipulate(context):

                    bpy.ops.object.select_all(action="DESELECT")

                    for i in range(self.duplicates):
                        t = time.time()
                        ref = self._duplicate_object(context, o, False)
                        walls_openings = {}
                        holes = {}
                        childs = []
                        # TODO: handle custom wall and wall1 here
                        # could be harder as custom wall might be any other kind of object
                        if "archipack_wall2" in objs:
                            for name in objs["archipack_wall2"]:
                                c = self.get_scene_object(context, name)
                                p = self._duplicate_object(context, c, False)
                                # walls.append(p)
                                childs.append(p)
                                # find openings wich belongs to this wall
                                modif = c.modifiers.get("AutoMixedBoolean")
                                if modif is not None and modif.object is not None:
                                    walls_openings[p.name] = set()
                                    p_m = p.modifiers.get("AutoMixedBoolean")
                                    p_m.object = self._duplicate_object(context, modif.object, False)
                                    for m in modif.object.modifiers:
                                        if m.type == 'BOOLEAN' and m.object is not None:
                                            opening = m.object.parent
                                            walls_openings[p.name].add(opening.name)

                        for key, coll in objs.items():
                            if key in {"archipack_wall2"}:
                                continue
                            is_opening = key == "archipack_window" or key == "archipack_door"
                            for o_name in coll:
                                # will dup holes too
                                c = self.get_scene_object(context, o_name)
                                p = self.duplicate_object(context,
                                                          c,
                                                          is_opening and self.copy_method == 'LINKED')

                                if is_opening or "archipack_custom" in c.data:
                                    for child in p.children:
                                        if "archipack_hole" in child or "archipack_custom_hole" in child:
                                            holes[o_name] = child.name
                                            break

                                childs.append(p)

                        t = time.time()

                        itM = ref.matrix_world.inverted()
                        for c in childs:
                            c.parent = ref
                            c.location = itM @ c.matrix_world.translation

                        ref.location.z += (i + 1) * self.spacing

                        for w_name, openings in walls_openings.items():
                            wall = self.get_scene_object(context, w_name)
                            modif = wall.modifiers.get("AutoMixedBoolean")
                            modifs = [m for m in modif.object.modifiers
                                if m.type == 'BOOLEAN' and m.object is not None]

                            for m, o_name in zip(modifs, openings):
                                hole = self.get_scene_object(context, holes[o_name])
                                m.object = hole

            self.select_object(context, o, True)
            context.window.cursor_set("DEFAULT")
            context.area.tag_redraw()
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "copy_method", text_ctxt="archipack")
        layout.prop(self, "spacing", text_ctxt="archipack")
        layout.prop(self, "duplicates", text_ctxt="archipack")
        if self.copy_method == 'COLLECTION':
            layout.prop(self, "collection_name", text_ctxt="archipack")

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)


class ARCHIPACK_OT_delete(ArchipackObjectsManager, Operator):
    bl_label = "Delete object(s)"
    bl_idname = 'archipack.delete'
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    bl_description = "Delete selected archipack object(s)"

    @classmethod
    def poll(cls, context):
        o = context.object
        return cls.archipack_filter(o)

    def execute(self, context):
        # o = context.object
        # delete walls first, will remove windows and holes
        # then others (the first one, on_delete handle multi object)
        sel = self.filter_selection(context.selected_objects)
        # print("ARCHIPACK_OT_delete.execute()", len(sel))
        if len(sel) > 0:
            bpy.ops.archipack.disable_manipulate()
            # with stop_auto_manipulate(context):
            if "archipack_wall2" in sel:
                for name in sel["archipack_wall2"]:
                    # walls doesnt handle multiple objects
                    o = self.get_scene_object(context, name)
                    d = self.archipack_datablock(o)
                    if d is not None:
                        d.on_delete(context, o)
                        self.delete_object(context, o)
                del sel["archipack_wall2"]

            for names in sel.values():
                for name in names:
                    # print(name)
                    # objects does handle multiple objects delete
                    o = self.get_scene_object(context, name)
                    d = self.archipack_datablock(o)
                    if d is not None:
                        d.on_delete(context, o)
                        self.delete_object(context, o)
                    break

            # XXX force outliner update but crash kitops / hardops / boxcutter
            # context.selected_objects access lead to SEG_FAULT
            # context.view_layer.update()

        return {'FINISHED'}

    def invoke(self, context, event):
        return context.window_manager.invoke_confirm(self, event)


def register():
    bpy.utils.register_class(archipack_reference_point)
    Object.archipack_reference_point = CollectionProperty(type=archipack_reference_point)
    bpy.utils.register_class(ARCHIPACK_PT_reference_point)
    bpy.utils.register_class(ARCHIPACK_OT_reference_point)
    bpy.utils.register_class(ARCHIPACK_OT_add_reference_point)
    bpy.utils.register_class(ARCHIPACK_OT_merge_references)
    bpy.utils.register_class(ARCHIPACK_OT_remove_references)
    bpy.utils.register_class(ARCHIPACK_OT_move_to_3d)
    bpy.utils.register_class(ARCHIPACK_OT_move_to_2d)
    bpy.utils.register_class(ARCHIPACK_OT_duplicate_level)
    bpy.utils.register_class(ARCHIPACK_OT_store_2d_reference)
    bpy.utils.register_class(ARCHIPACK_OT_parent_to_reference)
    bpy.utils.register_class(ARCHIPACK_OT_kill_archipack)
    bpy.utils.register_class(ARCHIPACK_OT_delete)
    bpy.utils.register_class(ARCHIPACK_OT_make_level_collection)
    bpy.utils.register_class(ARCHIPACK_OT_make_building_collection)

    bpy.utils.register_class(archipack_building)
    Object.archipack_building = CollectionProperty(type=archipack_building)
    bpy.utils.register_class(ARCHIPACK_PT_building)
    bpy.utils.register_class(ARCHIPACK_OT_building)


def unregister():
    bpy.utils.unregister_class(ARCHIPACK_OT_make_building_collection)
    bpy.utils.unregister_class(ARCHIPACK_OT_make_level_collection)
    bpy.utils.unregister_class(ARCHIPACK_OT_delete)
    bpy.utils.unregister_class(ARCHIPACK_PT_reference_point)
    bpy.utils.unregister_class(ARCHIPACK_OT_reference_point)
    bpy.utils.unregister_class(ARCHIPACK_OT_add_reference_point)
    bpy.utils.unregister_class(ARCHIPACK_OT_merge_references)
    bpy.utils.unregister_class(ARCHIPACK_OT_remove_references)
    bpy.utils.unregister_class(ARCHIPACK_OT_move_to_3d)
    bpy.utils.unregister_class(ARCHIPACK_OT_move_to_2d)
    bpy.utils.unregister_class(ARCHIPACK_OT_duplicate_level)
    bpy.utils.unregister_class(ARCHIPACK_OT_store_2d_reference)
    bpy.utils.unregister_class(ARCHIPACK_OT_parent_to_reference)
    bpy.utils.unregister_class(ARCHIPACK_OT_kill_archipack)
    bpy.utils.unregister_class(archipack_reference_point)
    del Object.archipack_reference_point

    bpy.utils.unregister_class(ARCHIPACK_PT_building)
    bpy.utils.unregister_class(ARCHIPACK_OT_building)
    bpy.utils.unregister_class(archipack_building)
    del Object.archipack_building
