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
from bpy.types import Operator
from bpy.props import BoolProperty
from mathutils import Vector
from .archipack_abstraction import ensure_select_and_restore
from .archipack_i18n import Archipacki18n
from .archipack_object import ArchipackObjectsManager, stop_auto_manipulate
import logging
logger = logging.getLogger("archipack")


class ArchipackBoolManager(ArchipackObjectsManager):
    """
        Handle hybrid methods for booleans
        merge holes with boolean and use result on wall
    """
    def __init__(self):
        """
            mode in 'ROBUST', 'INTERACTIVE', 'HYBRID'
        """
        # internal variables
        self.itM = None
        self.minx = 0
        self.miny = 0
        self.minz = 0
        self.maxx = 0
        self.maxy = 0
        self.maxz = 0

    @staticmethod
    def _world_bounding_box(o):
        tM = o.matrix_world
        x, y, z = zip(*[tM @ Vector(b) for b in o.bound_box])
        x, y, z = list(x), list(y), list(z)
        return min(x), min(y), min(z), max(x), max(y), max(z)

    def _init_bounding_box(self, wall):
        self.minx, self.miny, self.minz, \
            self.maxx, self.maxy, self.maxz = self._world_bounding_box(wall)

        self.center = Vector((
            0.5 * (self.maxx + self.minx),
            0.5 * (self.maxy + self.miny),
            0.5 * (self.maxz + self.minz)))

    def _contains(self, o):
        # check for bounding boxes intersections
        minx, miny, minz, maxx, maxy, maxz = self._world_bounding_box(o)
        if (maxx < self.minx or minx > self.maxx or
                maxy < self.miny or miny > self.maxy or
                maxz < self.minz or minz > self.maxz):
            return False

        return True

    def filter_wall(self, wall):
        d = wall.data
        return (d is not None and (
               'archipack_wall2' in d or 
               'archipack_wall' in d
               )) or 'archipack_custom_wall' in wall

    def datablock(self, o):
        """
            get datablock from windows and doors
            return
                datablock if found
                None when not found
        """
        d = None
        if o.data is not None:
            try:
                if "archipack_window" in o.data:
                    d = o.data.archipack_window[0]
                elif "archipack_door" in o.data:
                    d = o.data.archipack_door[0]
            except:
                pass
        return d

    def prepare_hole(self, context, hole):
        hole.display_type = 'WIRE'
        hole.hide_render = True
        # self.select_object(context, hole)
        if "archipack_custom_hole" not in hole:
            hole.lock_location = (True, True, True)
            hole.lock_rotation = (True, True, True)
            hole.lock_scale = (True, True, True)
            hole.hide_select = True
            hole.hide_set(True)
        # hide hole from cycles
        hole.cycles_visibility.camera = False
        hole.cycles_visibility.diffuse = False
        hole.cycles_visibility.glossy = False
        hole.cycles_visibility.shadow = False
        hole.cycles_visibility.scatter = False
        hole.cycles_visibility.transmission = False

    def get_child_hole(self, o):

        # Handle custom holes : objects tagged with "archipack_custom_hole"
        if "archipack_custom_hole" in o:
            return o

        for hole in o.children:
            if "archipack_hole" in hole or "archipack_custom_hole" in hole:
                return hole
        return None

    def _generate_hole(self, context, o):
        # use existing one
        hole = self.get_child_hole(o)
        if hole is None:
            # generate single hole from archipack primitives
            # regardless of location to allow draw tools to show holes
            d = self.datablock(o)
            if d is not None:
                hole = d.interactive_hole(context, o)
        return hole

    def sort_holes(self, wall, holes):
        """
            sort hole from center to borders by distance from center
            may improve nested booleans
        """
        holes = sorted(
            [(o, (o.matrix_world.translation - self.center).length) for o in holes],
            key=lambda x: x[1])

        return [o[0] for o in holes]

    def difference(self, basis, hole):
        basis.hide_set(False)
        basis.hide_select = False
        m = basis.modifiers.new('AutoBoolean', 'BOOLEAN')
        m.operation = 'DIFFERENCE'
        m.object = hole
        basis.hide_set(True)
        basis.hide_select = True

    def union(self, basis, hole):
        basis.hide_set(False)
        basis.hide_select = False
        m = basis.modifiers.new('AutoMerge', 'BOOLEAN')
        m.operation = 'UNION'
        m.object = hole
        basis.hide_set(True)
        basis.hide_select = True

    def remove_modif_and_object(self, context, o, to_delete):
        for m, h in to_delete:
            if m is not None:
                m.object = None
                o.modifiers.remove(m)
            if h is not None:
                if "archipack_custom_hole" not in h:
                    self.delete_object(context, h)

    def create_merge_basis(self, context, wall):
        """
            Create object to merge all holes using booleans
        """
        m = bpy.data.meshes.new("AutoBoolean")
        hole = bpy.data.objects.new("AutoBoolean", m)
        hole['archipack_hybridhole'] = True
        
        self.link_object_to_scene(context, hole)

        if wall.parent is not None:
            hole.parent = wall.parent
        hole.matrix_world = wall.matrix_world.copy()

        self.link_materials(context, wall, hole)
        return hole

    def update_hybrid(self, context, wall, childs, holes):
        """
            Update all holes modifiers
            remove holes not found in childs
        """
        existing = {}
        to_delete = {}
        hole_names = {h.name:h for h in holes}
        # print(hole_names)
        m = wall.modifiers.get("AutoMixedBoolean")
        if m is None:
            m = wall.modifiers.new('AutoMixedBoolean', 'BOOLEAN')
            m.operation = 'DIFFERENCE'
            # print("Add modifier")

        if m.object is None:
            hole_obj = self.create_merge_basis(context, wall)
            m.object = hole_obj
        else:
            hole_obj = m.object

        self.prepare_hole(context, hole_obj)

        # mixed-> mixed
        for m in hole_obj.modifiers:
            h = m.object
            if h is not None:
                if h.name in hole_names:
                    existing[h.name] = True
                else:
                    to_delete[h.name] = [m, h]
            else:
                to_delete[m.name] = [m, h]
                
        # remove modifier and holes not found in new list
        self.remove_modif_and_object(context, hole_obj, to_delete.values())
        
        # add modifier and holes not found in existing
        for h in holes:
            if h.name not in existing:
                self.union(hole_obj, h)
                existing[h.name] = True

        if len(hole_obj.modifiers) < 1:
            # Clean up hole and AutoBoolean target when not needed
            m = wall.modifiers.get("AutoMixedBoolean")
            if m is not None:
                self.remove_modif_and_object(context, wall, [[m, hole_obj]])
        else:
            # AutoBoolean will be child of reference point
            childs.append(hole_obj)

    def remove_hole(self, context, wall, hole):

        removed = False
        hole_obj = None
        modif = wall.modifiers.get("AutoMixedBoolean")

        if modif is not None and modif.object is not None:
            to_delete = []
            hole_obj = modif.object
            for m in hole_obj.modifiers:
                if m.type == 'BOOLEAN' and m.object is hole:
                    removed = True
                    to_delete.append([m, None])
            self.remove_modif_and_object(context, hole_obj, to_delete)

        if removed and len(hole_obj.modifiers) < 1:
            self.remove_modif_and_object(context, wall, [[modif, hole_obj]])

        return removed

    def autoboolean(self, context, wall, seek_objects=None):
        """
            Entry point for multi-boolean operations like
            in T panel autoBoolean
        """
        self.select_object(context, wall, True)

        io = None
        wall2d = None
        if wall.data is not None and "archipack_wall2" in wall.data:
            # ensure wall modifier is there before any boolean
            # to support "revival" of applied modifiers
            wd = wall.data.archipack_wall2[0]
            try:
                io, wall2d, childs = wd.as_geom(context, wall, 'BOUNDARY', [], [], [])
            except:
                pass
            wd.update(context)

        # bpy.ops.object.select_all(action='DESELECT')
        
        # XXX 2.8 ???
        
        # context.scene.objects.active = None
        childs = []
        holes = []
        # get wall bounds to find what's inside
        self._init_bounding_box(wall)

        if seek_objects is None:
            seek_objects = self.get_scene_objects(context)

        # either generate hole or get existing one
        for o in seek_objects:

            # print("auto_boolean", o.name)

            # filter holes found in wall bounding box
            if o.type == 'MESH' and self._contains(o):
                # print("contains", o.name)
                d = self.datablock(o)
                intersect = True

                # deep check to ensure neighboors walls
                # doesnt interfer with this one
                # using a pygeos based 2d check
                if d is not None and wall2d is not None:
                    coords = d.hole_2d('BOUND')
                    hole2d = io.coords_to_polygon(o.matrix_world, coords)
                    intersect = wall2d.intersects(hole2d)

                if intersect:
                    # print("intersects:", o.name)
                    h = self._generate_hole(context, o)
                    if h is not None:
                        # print("hole", h.name)
                        holes.append(h)
                        # never link custom hole to reference
                        if "archipack_custom_hole" not in o:
                            childs.append(o)

        # sort from center to border
        self.sort_holes(wall, holes)

        # hole(s) are not selected nor active after this one
        for hole in holes:
            # copy wall material to hole
            self.link_materials(context, wall, hole)
            self.prepare_hole(context, hole)

        # update / remove / add  boolean modifier
        self.update_hybrid(context, wall, childs, holes)

        # bpy.ops.object.select_all(action='DESELECT')
        # parenting childs to wall reference point
        with ensure_select_and_restore(context, wall, childs, 'OBJECT') as (ctx, act, sel):
            bpy.ops.archipack.add_reference_point()

    def singleboolean(self, context, wall, o):
        """
            Entry point for single boolean operations
            in use in draw door and windows over wall
            o is either a window or a door
            wall MUST be active_object !!
        """

        # generate holes for crossing window and doors
        hole = self._generate_hole(context, o)
        hole_obj = None

        if hole is None:
            return

        self.link_materials(context, wall, hole)

        self.prepare_hole(context, hole)

        # find or add merge basis to wall
        m = wall.modifiers.get('AutoMixedBoolean')

        if m is None:
            m = wall.modifiers.new('AutoMixedBoolean', 'BOOLEAN')
            m.operation = 'DIFFERENCE'

        if m.object is None:
            hole_obj = self.create_merge_basis(context, wall)
            m.object = hole_obj
        else:
            hole_obj = m.object

        # add hole to merge basis
        self.union(hole_obj, hole)

        bpy.ops.object.select_all(action='DESELECT')
        self.select_object(context, hole_obj)
        self.select_object(context, o)
        self.select_object(context, wall, True)
        bpy.ops.archipack.add_reference_point()

        """ 
        if "archipack_wall2" in wall.data:
            self.select_object(context, wall, True)
            d = wall.data.archipack_wall2[0]
            d.setup_childs(context, wall, openings_only=True)
            if d.dimensions:
                # update manipulators
                d.update(context, manipulable_refresh=True)
            else:
                d.relocate_childs(context, wall)
        """
        if hole_obj is not None:
            self.prepare_hole(context, hole_obj)


class ARCHIPACK_OT_single_boolean(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.single_boolean"
    bl_label = "SingleBoolean"
    bl_description = "Add single boolean for doors and windows"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}

    """
        Wall must be active object
        window or door must be selected
    """

    def execute(self, context):
        if context.mode == "OBJECT":
            wall = context.object
            manager = ArchipackBoolManager()
            for o in context.selected_objects:
                if o.name != wall.name:
                    manager.singleboolean(context, wall, o)
                break
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_remove_hole(Operator):
    bl_idname = "archipack.remove_hole"
    bl_label = "Remove hole Boolean"
    bl_description = "Remove single boolean for doors and windows"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}

    """
        Hole must be active object
        walls must be selected
    """

    def execute(self, context):
        if context.mode == "OBJECT":
            hole = context.object
            sel = context.selected_objects[:]
            manager = ArchipackBoolManager()
            for o in sel:
                # print("remove_hole of", o.name)
                if o.name != hole.name:
                    manager.remove_hole(context, o, hole)

            # print("archipack.remove_hole.execute() success")
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_auto_boolean(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.auto_boolean"
    bl_label = "Auto Boolean"
    bl_description = "Automatic boolean for doors and windows. Select your wall(s) then push"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        if context.mode == "OBJECT":
            # print("ARCHIPACK_OT_auto_boolean.disable_manipulate")
            bpy.ops.archipack.disable_manipulate()
            active = context.object

            with stop_auto_manipulate(context):

                manager = ArchipackBoolManager()
                walls = [wall for wall in context.selected_objects if manager.filter_wall(wall)]
                # print("ARCHIPACK_OT_auto_boolean.autoboolean")

                for wall in walls:
                    manager.autoboolean(context, wall)
                    # print("ARCHIPACK_OT_auto_boolean.execute() done")

            self.select_object(context, active, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_generate_hole(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.generate_hole"
    bl_label = "Generate hole"
    bl_description = "Generate interactive hole for doors and windows"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        if context.mode == "OBJECT":
            manager = ArchipackBoolManager()
            o = context.active_object
            d = manager.datablock(o)
            if d is None:
                self.report({'WARNING'}, "Archipack: active object must be a door or a window")
                return {'CANCELLED'}
            # bpy.ops.object.select_all(action='DESELECT')
            # self.select_object(context, o, True)
            logger.debug("ARCHIPACK_OT_generate_hole manager._generate_hole")
            hole = manager._generate_hole(context, o)
            logger.debug("ARCHIPACK_OT_generate_hole manager.prepare_hole")
            manager.prepare_hole(context, hole)
            # self.unselect_object(context, hole)
            # self.select_object(context, o, True)
            logger.debug("ARCHIPACK_OT_generate_hole.execute() done")
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_custom_hole(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.custom_hole"
    bl_label = "Custom hole"
    bl_description = "Make active object a hole for autoboolean"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    
    @classmethod
    def poll(self, context):
        o = context.active_object
        return (o is not None and
            o.type == 'MESH' and 
            "archipack_custom_hole" not in o and
            not ArchipackBoolManager.filter_wall(None, o))

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            o["archipack_custom_hole"] = 1
            o['archipack_skip_material'] = True
            o.display_type = 'WIRE'
            manager = ArchipackBoolManager()
            manager._init_bounding_box(o)
            walls = [wall for wall in self.get_scene_objects(context)
                if manager.filter_wall(wall) and manager._contains(wall)]
            for wall in walls:
                self.select_object(context, wall)
                
            bpy.ops.archipack.auto_boolean()
            
            for wall in walls: 
                self.unselect_object(context, wall)
            self.select_object(context, o, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}

            
class ARCHIPACK_OT_custom_hole_remove(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.custom_hole_remove"
    bl_label = "Custom hole"
    bl_description = "Remove hole params from active object"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    
    @classmethod
    def poll(self, context):
        o = context.active_object
        return (o is not None and
            o.type == 'MESH' and 
            "archipack_custom_hole" in o)

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            del o["archipack_custom_hole"]
            if "archipack_skip_material" in o:
                del o['archipack_skip_material']
            o.display_type = 'TEXTURED'
            o.hide_render = False
            
            holes = {}
            objs = self.get_scene_objects(context)
            for hole in objs:
                if "archipack_hybridhole" in hole:
                    for m in hole.modifiers:
                        if m.object == o:
                            m.object = None
                            hole.modifiers.remove(m)
                            holes[hole.name] = hole
                            
            manager = ArchipackBoolManager()
            manager._init_bounding_box(o)
            walls = [wall for wall in objs
                if manager.filter_wall(wall) and manager._contains(wall)]
                
            # find target wall(s) and cleanup
            for wall in walls:
                for m in wall.modifiers:
                    if m.type == 'BOOLEAN' and m.object and m.object.name in holes:
                        self.select_object(context, wall, True)
                        bpy.ops.archipack.auto_boolean()
                self.unselect_object(context, wall)
            self.select_object(context, o, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_apply_holes(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.apply_holes"
    bl_label = "Apply holes"
    bl_description = "Apply modifiers and remove holes from scene"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    selected_only: BoolProperty(default=False)

    @classmethod
    def poll(cls, context):
        return context.mode == "OBJECT"

    def modifiers_apply(self, context, o):
        ctx = context.copy()
        ctx['object'] = o
        for mod in o.modifiers[:]:
            ctx['modifier'] = mod
            try:
                bpy.ops.object.modifier_apply(ctx, apply_as='DATA', modifier=mod.name)
            except:
                pass

    def get_boolobjects(self, o, to_remove):
        modifiers = [m for m in o.modifiers if m.type == 'BOOLEAN']
        for m in modifiers:
            if m.object is None:
                o.modifiers.remove(m)
            else:
                if 'archipack_custom_hole' not in m.object:
                    to_remove.add(m.object.name)

    def apply(self, context, objects):
        to_remove = set()
        holes = set()

        for o in objects:
            self.get_boolobjects(o, to_remove)

        for o in objects:
            self.modifiers_apply(context, o)

        # step 1 remove AutoBoolean
        for name in to_remove:
            o = self.get_scene_object(context, name)
            if o is not None:
                self.get_boolobjects(o, holes)
                self.delete_object(context, o)

        # step 2 remove holes
        # print(holes)
        for name in holes:
            o = self.get_scene_object(context, name)
            self.delete_object(context, o)

    def execute(self, context):
        if context.mode == "OBJECT":

            if self.selected_only:
                objects = context.selected_objects[:]
            else:
                objects = self.get_scene_objects(context)

            objects = [o for o in objects
                        if o.type == 'MESH' and (
                        "archipack_wall2" in o.data or
                        "archipack_wall" in o.data or
                        "archipack_custom_wall" in o)
                        ]
            self.apply(context, objects)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


def register():
    bpy.utils.register_class(ARCHIPACK_OT_generate_hole)
    bpy.utils.register_class(ARCHIPACK_OT_single_boolean)
    bpy.utils.register_class(ARCHIPACK_OT_auto_boolean)
    bpy.utils.register_class(ARCHIPACK_OT_remove_hole)
    bpy.utils.register_class(ARCHIPACK_OT_custom_hole)
    bpy.utils.register_class(ARCHIPACK_OT_custom_hole_remove)
    bpy.utils.register_class(ARCHIPACK_OT_apply_holes)


def unregister():
    bpy.utils.unregister_class(ARCHIPACK_OT_generate_hole)
    bpy.utils.unregister_class(ARCHIPACK_OT_single_boolean)
    bpy.utils.unregister_class(ARCHIPACK_OT_auto_boolean)
    bpy.utils.unregister_class(ARCHIPACK_OT_remove_hole)
    bpy.utils.unregister_class(ARCHIPACK_OT_custom_hole)
    bpy.utils.unregister_class(ARCHIPACK_OT_custom_hole_remove)
    bpy.utils.unregister_class(ARCHIPACK_OT_apply_holes)
