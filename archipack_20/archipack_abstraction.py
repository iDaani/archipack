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
# noinspection PyUnresolvedReferences
import bpy
USE_SLOTS = False

"""
 Layer (layer Management) / collection names by class
"""
layer_names = {
    "All":"All",
    "reference_point":"Reference",
    "wall":"Walls",
    "custom_wall":"Walls",
    "wall2":"Walls",
    "slab":"Slabs",
    "floor":"Floors",
    "molding":"Moldidngs",
    "fence":"Furnitures",
    "stair":"Furnitures",
    "truss":"Furnitures",
    "kitchen":"Furnitures",
    "kitchen_module":"Furnitures",
    "handle":"Openings",
    "window":"Openings",
    "window_panel":"Openings",
    "window_shutter":"Openings",
    "blind":"Openings",
    "door":"Openings",
    "door_panel":"Openings",
    "custom":"Openings",
    "custom_part":"Openings",
    "roof":"Roofs",
    "beam":"Beams",
    "slab_cutter":"Slabs",
    "floor_cutter":"Floors",
    "roof_cutter":"Roofs",
    "dimension_auto":"2d",
    "section":"2d",
    "section_target":"2d",
    "area": "2d",
    "area_cutter": "2d",
    "layout":"2d",
    "profile":"Profile",
    "hole":"Holes",
    "hybridhole":"Holes",
    "custom_hole":"Holes",
    "terrain":"Terrain"
}


class ensure_select_and_restore():
    """Context Manager
    For use bpy.ops like transform.translate or any op working on selection

    Usage:

    with ensure_select_and_restore(context, obj, sel, object_mode='EDIT'):
        .. obj is selected and active
        bpy.ops.transform.translate ...

    with ensure_select_and_restore(context, obj, sel, object_mode='OBJECT') as (new_act, new_sel):
        .. new_act is selected and active and in object_mode if set
        .. old_sel objects are no more selected (if different of obj)
        .. old_act is no more active (if different of obj)
        bpy.ops.transform.translate ...

    .. obj is not active nor selected unless it was selected or active before
    .. old_sel are selected
    .. old_act is active
    :param context: blender context
    :param obj: blender object
    :param sel: a new selection
    :param object_mode: object mode to switch in ['EDIT', 'OBJECT', ..]
    """

    def ensure_select(self, o):
        if o is not None:
            if o.hide_get():
                o.hide_set(False)
                self._hide_viewport.add(o.name)
            if o.hide_select:
                o.hide_select = False
                self._hide_select.add(o.name)
            o.select_set(state=True, view_layer=self._ctx.view_layer)

    def get_object(self, name):
        return self._ctx.view_layer.objects.get(name)

    def restore_vis_sel(self):
        for name in self._vis_sel_state:
            o = self.get_object(name)
            if o is not None:
                o.hide_select = name in self._hide_select
                o.hide_set(name in self._hide_viewport)

    def __init__(self, context, obj, sel, object_mode=None):
        act = context.active_object
        self._ctx = context
        self._act_mode = None
        self._act = ""
        self._previous_sel = set([o.name for o in context.selected_objects])
        self._obj = obj
        self._sel = sel

        self._vis_sel_state = set([o.name for o in sel])

        self._vis_sel_state.add(obj.name)
        obj_mode = obj.mode

        self._hide_viewport = set()
        self._hide_select = set()

        # ensure act.mode allow deselect all
        # other objects are all in 'OBJECT' mode
        if act is not None:
            self._act = act.name
            self._act_mode = act.mode
            if act.mode != 'OBJECT':
                bpy.ops.object.mode_set(mode='OBJECT')

        # deselect require object mode
        bpy.ops.object.select_all(action="DESELECT")

        # with stop_auto_manipulate(context):
        # hide_select and hide_viewport prevent selection
        self.ensure_select(obj)
        context.view_layer.objects.active = obj

        for o in sel:
            self.ensure_select(o)

        if object_mode is not None:
            # obj == act -> obj was in _act_mode, exit set obj mode back to _act_mode
            # obj != act -> obj was in 'OBJECT' mode, exit set obj mode back to 'OBJECT'
            if obj_mode != object_mode:
                bpy.ops.object.mode_set(mode=object_mode)

    def __enter__(self):
        return self._ctx, self._obj, self._sel

    def __exit__(self, exc_type, exc_val, exc_tb):

        # deselect require object mode
        if bpy.ops.object.mode_set.poll():
            bpy.ops.object.mode_set(mode='OBJECT')

        bpy.ops.object.select_all(action="DESELECT")

        # restore hide_viewport and hide_select
        self.restore_vis_sel()

        # with stop_auto_manipulate(self._ctx):
        # restore previous selection, use names so deleted objects wont crash blender
        for name in self._previous_sel:
            o = self.get_object(name)
            if o is not None:
                o.select_set(state=True)

        # restore previous active, use names so deleted objects wont crash blender
        act = self.get_object(self._act)
        if act is not None:
            self._ctx.view_layer.objects.active = act

            # restore act mode
            if self._act_mode is not None and act.mode != self._act_mode:
                bpy.ops.object.mode_set(mode=self._act_mode)

        # false let raise exception if any
        return False


class ArchipackLayerManager:
    """
     Layer manager for use in 2.8 series
    """
    def collection_by_name(self, context, collection_name):
        if collection_name in layer_names:
            return layer_names[collection_name].capitalize()
        return collection_name.capitalize()

    def create_collection(self, main, name):
        """ Create nested collections
        :param main:
        :param name:
        :return:
        """
        _name = name.split("/")
        curr = _name.pop(0)

        coll = main.children.get(curr)
        if coll is None:
            coll = bpy.data.collections.new(name=curr)
            main.children.link(coll)

        # create nested collections if any
        if len(_name) > 0:
            if len(_name) > 1:
                nest = "/".join(_name)
            else:
                nest = _name[0]
            return self.create_collection(coll, nest)

        return coll

    def get_collection(self, context, o, layer_name, default_layer):
        if layer_name is not None:
            collection_name = self.collection_by_name(context, layer_name)

        else:
            collection_name = None
            if o.data is not None and "archipack_custom_hole" not in o:
                for key in o.data.keys():
                    if "archipack_" in key:
                        collection_name = self.collection_by_name(context, key[10:])
                        break
            if collection_name is None:
                for key in o.keys():
                    if "archipack_" in key:
                        collection_name = self.collection_by_name(context, key[10:])
                        break

        # print(o.name, collection_name)

        coll_main = context.scene.collection.children.get("Archipack")
        if coll_main is None:
            coll_main = bpy.data.collections.new(name="Archipack")
            context.scene.collection.children.link(coll_main)

        if collection_name is None:
            return context.scene.collection

        coll = self.create_collection(coll_main, collection_name)

        return coll


class ArchipackObjectsManagerBase:

    def _cleanup_datablock(self, d, typ):
        if d and d.users < 1:
            if typ == 'MESH':
                bpy.data.meshes.remove(d)
            elif typ == 'CURVE':
                bpy.data.curves.remove(d)
            elif typ == 'LAMP':
                bpy.data.lamps.remove(d)

    def _delete_object(self, context, o):
        o.select_set(state=False)
        d = o.data
        typ = o.type
        self.unlink_object_from_scene(context, o)
        bpy.data.objects.remove(o)
        self._cleanup_datablock(d, typ)

    def _delete_childs(self, context, o):
        if o and o.children:
            for c in o.children:
                self._delete_childs(context, c)
        self._delete_object(context, o)

    def delete_object(self, context, o):
        """
          Recursively delete object and childs
          Cleanup datablock when needed
          @o: object to delete
        """
        if o is not None:
            self._delete_childs(context, o)

    def _duplicate_object(self, context, o, linked):
        new_o = o.copy()
        if o.data:
            if linked:
                new_o.data = o.data
            else:
                new_o.data = o.data.copy()
        layer_name = o.users_collection[0].name
        self.link_object_to_scene(context, new_o, layer_name=layer_name)
        return new_o

    def _duplicate_childs(self, context, o, linked):
        p = self._duplicate_object(context, o, linked)
        for child in o.children:
            c = self._duplicate_childs(context, child, linked)
            c.parent = p
            # c.location = child.location.copy()
            c.matrix_local = child.matrix_local.copy()
            c.matrix_parent_inverse = child.matrix_parent_inverse.copy()
        return p

    def duplicate_object(self, context, o, linked):
        """
          Recursively duplicate object and childs
          @o: object to duplicate
          @linked : boolean linked duplicate
          return parent on success
        """
        if o is not None:
            return self._duplicate_childs(context, o, linked)
        return None

    def _link_object(self, src, o):
        if src.data:
            d = o.data
            typ = o.type
            o.data = src.data
            self._cleanup_datablock(d, typ)

    def _link_child(self, src, o):
        self._link_object(src, o)
        if len(src.children) == len(o.children):
            for child, o_child in zip(src.children, o.children):
                self._link_child(child, o_child)

    def link_object(self, src, o):
        """
         Recursievely link datablock
         @src: object source
         @o: object destination
         src and o parent child relationship must match
         !!! disable manipulate before !!!
        """
        if src is not None:
            self._link_child(src, o)

    def get_topmost_parent(self, o):
        if o.parent:
            return self.get_topmost_parent(o.parent)
        else:
            return o

    def get_reference_point(self, o):
        if o is None:
            return None
        elif "archipack_reference_point" in o:
            return o
        else:
            p = o.parent
            if p is not None:
                return self.get_reference_point(p)
        return None


class ArchipackObjectsManager(ArchipackLayerManager, ArchipackObjectsManagerBase):
    """
      Provide objects and datablock utility
      Support meshes curves and lamps
      - recursive delete objects and datablocks
      - recursive clone linked
      - recursive copy

     Provide abstraction layer for blender 2.8 series

    """
    def scene_ray_cast(self, context, orig, vec):

        return context.scene.ray_cast(
            view_layer=context.view_layer,
            origin=orig,
            direction=vec)

    def get_cursor_location(self, context):
        try:
            # before 28.02.2019
            res = context.scene.cursor_location.copy()
        except:
            pass

        try:
            # after 28.02.2019
             res = context.scene.cursor.location.copy()
        except:
           pass
        return res

    def set_cursor_location(self, context, location):
        try:
            # after 28.02.2019
            context.scene.cursor.location = location
        except:
            pass
        try:
            # before 28.02.2019
            context.scene.cursor_location = location
        except:
            pass

    def get_linked_objects(self, context, o):
        """Collect linked objects on scene
        Regardless visibility
        :param context: blender context
        :param o: blender object
        :return: list of all linked objects
        """
        _d = o.data
        _objs = context.scene.objects
        if _d is not None:
            return [c for c in _objs if c.data and c.data is _d]
        return []

    def get_scene_object(self, context, name):
        return context.scene.objects.get(name.strip())

    def get_scene_objects(self, context):
        return context.scene.objects

    def select_object(self, context, o, active=False):
        """
         Select object and optionnaly make active
        """
        if o is not None:

            o.select_set(state=True, view_layer=context.view_layer)
            if active:
                context.view_layer.objects.active = o

    def collect_childs(self, o, res):
        res.extend(list(o.children))
        for c in o.children:
            self.collect_childs(c, res)

    def link_materials(self, context, source, dest):
        if USE_SLOTS:
            # adding to slots doesnt work on newly created objects
            # also require object to be visible
            _mats = source.material_slots
            _slots = dest.material_slots
            n_mats = len(_mats)
            n_slots = len(_slots)
            delta = n_mats - n_slots
            if delta != 0:
                with ensure_select_and_restore(context, dest, [dest]):
                    # print("link_materials :", n_mats, n_slots)
                    if delta < 0:
                        for i in range(-delta):
                            bpy.ops.object.material_slot_remove()
                    else:
                        for i in range(delta):
                            bpy.ops.object.material_slot_add()

            dest.material_slots.update()

            _slots = dest.material_slots
            for src, dst in zip(_mats, _slots):
                dst.material = src.material
        else:
            src_mats = source.data.materials
            dest_mats = dest.data.materials
            n_mats = len(src_mats)
            n_dest = len(dest_mats)
            delta = n_mats - n_dest
            for i in range(-delta):
                dest_mats.pop(index=-1)
            for i, mat in enumerate(src_mats):
                if i + 1 > n_dest:
                    dest_mats.append(mat)
                else:
                    dest_mats[i] = mat
            dest_mats.update()

    def unselect_object(self, context, o):
        if o is not None:
            o.select_set(state=False, view_layer=context.view_layer)

    def link_object_to_scene(self, context, o, layer_name=None, default_layer=False):
        coll = self.get_collection(context, o, layer_name, default_layer)
        coll.objects.link(o)

    def unlink_object_from_scene(self, context, o):
        for coll in o.users_collection:
            coll.objects.unlink(o)

    def is_visible(self, o):
        return not o.hide_get()

    def is_selected(self, o):
        return o.select_get()

    def hide_object(self, o):
        o.hide_set(True)

    def show_object(self, o):
        o.hide_set(False)

    def get_transform_orientation(self, context):
        return context.scene.transform_orientation_slots[0].type

    def set_transform_orientation(self, context, _type):
        context.scene.transform_orientation_slots[0].type = _type

    @classmethod
    def archipack_filter(cls, o):
        if o is not None:
            if o.data is not None:
                for key in o.data.keys():
                    if "archipack_" in key:
                        return True
            for key in o.keys():
                if "archipack_" in key:
                    return True
        return False

    @classmethod
    def archipack_datablock(cls, o):
        """
         Return archipack datablock from object
        """
        d = None
        if o is not None:
            if o.data is not None:
                try:
                    for key in o.data.keys():
                        if "archipack_" in key:
                            d = getattr(o.data, key)[0]
                            break
                except:
                    pass
            if d is None:
                try:
                    for key in o.keys():
                        if "archipack_" in key:
                            d = getattr(o, key)[0]
                            break
                except:
                    pass
        return d

    @classmethod
    def filter_selection(cls, sel):
        res = {}
        for o in sel:
            d = cls.archipack_datablock(o)
            if d is not None:
                key = d.__class__.__name__
                if key not in res:
                    res[key] = set()
                res[key].add(o.name)
        return res

    @classmethod
    def filter_selection_loose(cls, sel):
        """Filter archipack datablock and custom props tagged
        :param sel:
        :return:
        """
        res = {}
        for o in sel:
            d = cls.archipack_datablock(o)
            if d is None:
                for key in o.keys():
                    if "archipack_" in key:
                        if key not in res:
                            res[key] = set()
                        res[key].add(o.name)
            else:
                key = d.__class__.__name__
                if key not in res:
                    res[key] = set()
                res[key].add(o.name)
        return res