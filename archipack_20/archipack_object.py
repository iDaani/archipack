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
major, minor, rev = bpy.app.version
# noinspection PyUnresolvedReferences
from bpy.props import BoolProperty, StringProperty, EnumProperty, IntVectorProperty
from mathutils import Vector, Matrix
from mathutils.geometry import (
    intersect_line_plane
    )
from bpy_extras import view3d_utils
from .archipack_iconmanager import icons
from .archipack_abstraction import ArchipackObjectsManager, ensure_select_and_restore
from . import __version__
from .archipack_prefs import get_prefs

objman = ArchipackObjectsManager()  
    
    
import logging
import time
logger = logging.getLogger("archipack")


class stop_auto_manipulate():
    """Context Manager

    Usage:

    with stop_auto_manipulate(context):
        bpy.ops.archipack ...
    :param context: blender context
    """

    def __init__(self, context):
        if hasattr(context.window_manager, "archipack"):
            wm = context.window_manager.archipack
            self._ctx = context
            self._state = wm.auto_manipulate
            wm.auto_manipulate = False

    def __enter__(self):
        return None

    def __exit__(self, exc_type, exc_val, exc_tb):
        if hasattr(self._ctx.window_manager, "archipack"):
         self._ctx.window_manager.archipack.auto_manipulate = self._state

        # false let raise exception if any
        return None


def get_enum(self, context):
    return icons.enum(context, self.__class__.__name__, "json")


def preset_operator(self, context):
    import os
    base_preset, ext = os.path.splitext(self.preset)
    bpy.ops.archipack.preset_loader(filepath="{}.json".format(base_preset))


class ArchipackPanel:
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'UI'
    bl_category = 'Archipack'
    bl_translation_context = "archipack"

    def draw_common(self, context, layout):
        if hasattr(context.window_manager, "archipack"):
            wm = context.window_manager.archipack
            self.draw_translate(context, layout)

            row = layout.row(align=True)
            self.draw_prop(context, layout, row, wm, "auto_manipulate", icon='AUTO', emboss=True)
            if not wm.auto_manipulate:
                self.draw_op(context, layout, row, "archipack.manipulate", icon='VIEW_PAN')
            self.draw_op(context, layout, layout, "archipack.delete", icon="TRASH")


class ArchipackGenericOperator(ArchipackObjectsManager):
    """
     Generic operator working on any archipack object
     Provide generic datablock accessor
     Polling for selected and active archipack objects
    """
    bl_options = {'INTERNAL', 'UNDO'}
    translation_context = "archipack"

    @classmethod
    def poll(cls, context):
        o = context.active_object
        return o and ArchipackObjectsManager.is_selected(cls, o) and cls.filter(o)

    @classmethod
    def filter(cls, o):
        return cls.archipack_filter(o)

    def datablock(self, o):
        """
         Return archipack datablock from object
        """
        return self.archipack_datablock(o)


class ArchipackObject(ArchipackObjectsManager):
    """
        Shared property of archipack's objects PropertyGroup
        provide basic support for copy to selected
        and datablock access / filtering by object
    """
    bl_translation_context = "archipack"

    def __init__(self):
        self.previously_selected = []
        self.previously_active = None
        self.auto_manipulate_state = True

    def iskindof(self, o, typ):
        """
            return true if object contains databloc of typ name
        """
        return o.data is not None and typ in o.data

    @property
    def category(self):
        return self.__class__.__name__[10::]

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
        res = False
        try:
            res = cls.__name__ in o.data and getattr(o.data, cls.__name__)[0]
        except:
            pass
        if not res:
            try:
                res = cls.__name__ in o and getattr(o, cls.__name__)[0]
            except:
                pass
        return res

    @classmethod
    def poll(cls, o):
        """
            Filter object with this class in data
            return
            True when object contains this datablock
            False otherwhise
            usage:
            class_name.filter(object) from outside world
            self.__class__.filter(object) from instance
        """
        res = False
        try:
            res = ArchipackObjectsManager.is_selected(cls, o) and cls.filter(o)
        except:
            pass
        return res

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
        d = None
        try:
            d = getattr(o.data, cls.__name__)[0]
        except:
            pass

        if d is None:
            try:
                d = getattr(o, cls.__name__)[0]
            except:
                pass

        return d

    def on_delete(self, context, obj):
        """ Default on_delete, delete selected objects of same kind
        :param context:
        :param obj:
        :return:
        """
        for o in context.selected_objects:
            if self.filter(o) and o.name != obj.name:
                self.delete_object(context, o)

    def disable_auto_manipulate(self, context):
        if hasattr(context.window_manager, "archipack"):
            wm = context.window_manager.archipack
            # 2nd call on instances ??
            self.auto_manipulate_state = wm.auto_manipulate
            wm.auto_manipulate = False
        context.window.cursor_set("WAIT")

    def restore_auto_manipulate(self, context):
        if hasattr(context.window_manager, "archipack"):
            wm = context.window_manager.archipack
            if self.auto_manipulate_state:
                wm.auto_manipulate = self.auto_manipulate_state
        context.window.cursor_set("DEFAULT")

    def find_in_selection(self, context, auto_update=True):
        """
            find witch selected object this datablock instance belongs to
            store context to be able to restore after oops
            provide support for "copy to selected"
            XXX provide support for "context.object" override and preserve active one
            return
            object or None when instance not found in selected objects
            
            context.active_object is None when object's layer is hidden
            context.object does work in any case
        """
        if auto_update is False:
            return None

        # set by context.view_layer.objects.active
        o = context.object

        # set by user selection
        active = context.active_object
        selected = context.selected_objects[:]

        self.previously_selected = []
        self.previously_active = None

        if self.__class__.datablock(o) == self:
            self.previously_selected = selected
            self.previously_active = active
            self.disable_auto_manipulate(context)
            return o

        for o in selected:
            if self.__class__.datablock(o) == self:
                self.previously_selected = selected
                self.previously_active = active
                self.disable_auto_manipulate(context)
                return o
        
        return None

    def restore_context(self, context):
        """
         restore context
        """
        # context.view_layer.update()
        bpy.ops.object.select_all(action="DESELECT")

        try:
            for o in self.previously_selected:
                self.select_object(context, o)
        except:
            pass

        if self.previously_active is not None:
            logger.debug("restore_context: %s", self.previously_active.name)

        self.select_object(context, self.previously_active, True)
        self.restore_auto_manipulate(context)
        self.previously_selected = None
        self.previously_active = None

    def id_mat(self, index, source=None):
        """ Limit material index to number of materials
        :param index:
        :param source: source obj with .idmat array, allow override on parent from childs
        :return:
        """
        if source is None:
            _source = self
        else:
            _source = source
        _idx = _source.idmat[index]
        if _idx < len(self.id_data.materials):
            return _idx
        return 0

    def shade_smooth(self, context, o, smooth_angle):
        m = o.data
        if not m.use_auto_smooth or m.auto_smooth_angle != smooth_angle:
            m.use_auto_smooth = True
            m.auto_smooth_angle = smooth_angle
        t = time.time()
        with ensure_select_and_restore(context, o, [o]):
            bpy.ops.object.shade_smooth()
        # logger.debug("bpy.ops.object.shade_smooth %.4f" % (time.time() - t))

    def find_modifier_by_type(self, o, mod_type):
        for mod in o.modifiers:
            if mod.type == mod_type:
                return mod
        return None

    def bevel_modifier(self, o, width=0.0005):
        mod = self.find_modifier_by_type(o, "BEVEL")
        if mod is None:
            mod = o.modifiers.new("Bevel", "BEVEL")
        mod.limit_method = 'ANGLE'
        mod.offset_type = 'WIDTH'
        mod.width = width
        mod.harden_normals = True
        mod.segments = 4
        mod.use_clamp_overlap = False


class ArchipackCreateTool(ArchipackObjectsManager):
    """
        Shared property of archipack's create tool Operator
    """
    bl_category = 'Archipack'
    bl_options = {'INTERNAL', 'UNDO'}

    filepath: StringProperty(
        options={'SKIP_SAVE'},
        name="Preset",
        description="Full filename of python preset to load at create time",
        default=""
    )

    act = None

    @property
    def archipack_category(self):
        """
            return target object name from ARCHIPACK_OT_object_name
        """
        return self.bl_idname[13:]

    def load_preset(self, d, auto_update=True):
        """
            Load python preset
            d: archipack object datablock
            preset: full filename.py with path
        """

        d.auto_update = False

        if self.filepath != "":
            try:
                # print("Load preset")
                bpy.ops.archipack.preset_loader(filepath=self.filepath)
                # print("Load preset success")
            except:
                logger.debug("Archipack unable to load preset file : %s" % (self.filepath))
                pass

        d.auto_update = auto_update

    def add_material(self, context, o, material='', category=None):
        # skip if preset allready add material
        if "archipack_material" in o:
            return
        # enable viewport transparency
        o.show_transparent = True
        try:
            if category is None:
                category = self.archipack_category

            if bpy.ops.archipack.material.poll():
                bpy.ops.archipack.material(category=category, material=material)

        except Exception as ex:
            import traceback
            traceback.print_exc()
            logger.debug("Archipack %s materials not found" % (self.archipack_category))
            pass

    def invoke(self, context, event):
        o = context.active_object
        if o is None:
            # fallback to context.object
            o = context.object
        if self.archipack_filter(o):
            self.act = o
        return self.execute(context)

    def add_to_reference(self, context, o):
        # no reference when rendering in background
        if bpy.app.background:
            return

        ref = self.get_reference_point(self.act)
        if ref is None:
            ref = o
        with ensure_select_and_restore(context, ref, [o]):
            bpy.ops.archipack.add_reference_point()


class ArchipackDrawTool(ArchipackObjectsManager):
    """
        Draw tools
    """
    bl_category = 'Archipack'
    bl_options = {'INTERNAL', 'UNDO'}

    disabled_walls = {}
    
    def region_2d_to_orig_and_vect(self, context, event):

        region = context.region
        rv3d = context.region_data
        coord = (event.mouse_region_x, event.mouse_region_y)
        vec = view3d_utils.region_2d_to_vector_3d(region, rv3d, coord)
        orig = view3d_utils.region_2d_to_origin_3d(region, rv3d, coord)

        return rv3d.is_perspective, orig, vec

    def isect_vec_plane(self, p0, vec, p_co, p_no, epsilon=1e-6):
        """
        p0, vec: define the line
        p_co, p_no: define the plane:
            p_co is a point on the plane (plane coordinate).
            p_no is a normal vector defining the plane direction;
                 (does not need to be normalized).
        return a Vector or None (when the intersection can't be found).
        """
        u = vec
        d = p_no.dot(u)

        if abs(d) > epsilon:
            w = p0 - p_co
            t = -p_no.dot(w) / d
            return p0 + t * u
        else:
            # The segment is parallel to plane
            return None

    def mouse_to_plane(self, context, event, origin=Vector((0, 0, 0)), normal=Vector((0, 0, 1))):
        """
            convert mouse pos to 3d point over plane defined by origin and normal
            return None if the point is behind camera view
        """
        is_perspective, orig, vec = self.region_2d_to_orig_and_vect(context, event)
        # pt = intersect_line_plane(orig, orig + vec, origin, normal, False)
        pt = self.isect_vec_plane(orig, vec, origin, normal)

        # fix issue with parallel plane (front or left ortho view)
        if pt is None:
            pt = self.isect_vec_plane(orig, vec, origin, Vector((0, 1, 0)))

        if pt is None:
            pt = self.isect_vec_plane(orig, vec, origin, Vector((1, 0, 0)))
            # pt = intersect_line_plane(orig, orig + vec, origin, vec, False)

        if pt is None:
            return None

        if is_perspective:
            # Check if point is behind point of view (mouse over horizon)
            y = Vector((0, 0, 1))
            # x = vec.cross(y)
            x = y.cross(vec)
            itM = Matrix([
                [x.x, y.x, vec.x, orig.x],
                [x.y, y.y, vec.y, orig.y],
                [x.z, y.z, vec.z, orig.z],
                [0, 0, 0, 1]
                ]).inverted()
            res = itM @ pt

            if res.z < 0:
                return None

        return pt

    def is_snapping(self, context, event, pt):
        """
        :param context:
        :param event:
        :param pt: point in world space
        :return:
        """
        is_perspective, orig, vec = self.region_2d_to_orig_and_vect(context, event)
        if vec.x != 0 and vec.y != 0:
            x = vec.cross(Vector((0, 0, 1)))
        else:
            x = vec.cross(Vector((1, 0, 0)))
        y = x.cross(vec)
        sp = Matrix([
            [x.x, y.x, vec.x, orig.x],
            [x.y, y.y, vec.y, orig.y],
            [x.z, y.z, vec.z, orig.z],
            [0, 0, 0, 1]
        ]).inverted() @ pt
        return abs(sp.x) > 0.001 or abs(sp.y) > 0.001


    def mouse_to_scene_raycast(self, context, event):
        """
            convert mouse pos to 3d point over plane defined by origin and normal
        """
        is_perspective, orig, vec = self.region_2d_to_orig_and_vect(context, event)
        res, pos, normal, face_index, obj, matrix_world = self.scene_ray_cast(
            context,
            orig,
            vec)

        return res, pos, normal, face_index, obj, matrix_world
    
    def restore_walls(self, context):
        """
         Enable finish on wall
        """
        last = context.active_object
        for name, o in self.disabled_walls.items():
            # print("restore_walls ", name)
            d = o.data.archipack_wall2[0]
            self.select_object(context, o, True)
            d.finish_enable = True
            self.unselect_object(context, o)
        self.select_object(context, last, True)
        self.disabled_walls.clear()

    def mouse_hover_wall(self, context, event):
        """
            convert mouse pos to matrix at bottom of surrounded wall, y oriented outside wall
        """
        res, pt, y, i, o, tM = self.mouse_to_scene_raycast(context, event)

        if res and o.data is not None:

            z = Vector((0, 0, 1))
            y = -y
            x = y.cross(z)
            width = 0
            z_offset = 0
            res = False
            if 'archipack_wall2' in o.data:

                res = True

                d = o.data.archipack_wall2[0]
                width = d.width
                z_offset = d.z_offset

                # TODO: identify wall segment and pass finishing overflow_out
                
                if d.finish_enable and (len(d.finish) > 0 or len(d.finish2) > 0):
                    # print("Disable finish for %s" % o.name)
                    with ensure_select_and_restore(context, o, [o]):
                        d.finish_enable = False
                    self.disabled_walls[o.name] = o

            elif ('archipack_wall' in o.data or 
                'archipack_custom_wall' in o):

                # one point on the opposite to raycast side (1 unit (meter) inside)
                p0 = pt + y.normalized()
                # direction
                dp = -y.normalized()
                # cast another ray to find wall depth
                attempt = 0
                while res is False and attempt < 5:
                    res, pos, normal, face_index, obj, matrix_world = self.scene_ray_cast(context, p0, dp)
                    # must hit another face or another object
                    res = res and (i != face_index or obj.name != o.name)
                    p0 -= dp
                    attempt += 1

                if res:
                    width = (pt - pos).to_2d().length

            if res:
                pt += (0.5 * width) * y.normalized()
                return True, Matrix([
                    [x.x, y.x, z.x, pt.x],
                    [x.y, y.y, z.y, pt.y],
                    [x.z, y.z, z.z, o.matrix_world.translation.z],
                    [0, 0, 0, 1]
                ]), o, width, y, z_offset

        return False, Matrix(), None, 0, Vector(), 0


    """
    def mouse_paste_material(self, context, event):
        # Paste a material index over wall
        res, pt, y, i, o, tM = self.mouse_to_scene_raycast(context, event)

        if res and o.data is not None:

            # When pasting over other object, use face material index to identify slot

            if 'archipack_wall2' in o.data:

                d = o.data.archipack_wall2[0]
                g = d.get_generator(o)
                last = 1e32
                _seg = None
                id_mat = -1
                _part_ext = None
                _part_int = None

                for _k, part in zip(g.outside.valid_segs, d.parts):
                    res, dist, t = _k.point_sur_segment(pt)
                    if res and abs(dist) < last:
                        last = abs(dist)
                        _part_ext = part

                if last > 0.001:
                    for _k, part in zip(g.outside.valid_segs, d.parts):
                        res, dist, t = _k.point_sur_segment(pt)
                        if res and abs(dist) < last:
                            last = abs(dist)
                            _seg = _k
                            _part_int = part

                if _part_ext is not None:
                    if not _part_ext.override_material:
                        _part_ext.idmat[MAT_PART_INSIDE] =_part_ext.idmat[MAT_INSIDE]
                    _part_ext.idmat[MAT_PART_OUTSIDE] = id_mat
                    _part_ext.override_material = True

                if _part_int is not None:
                    if not _part_int.override_material:
                        _part_int.idmat[MAT_PART_OUTSIDE] =_part_int.idmat[MAT_OUTSIDE]
                    _part_int.idmat[MAT_PART_INSIDE] = id_mat
                    _part_int.override_material = True

            elif ('archipack_wall' in o.data or
                  'archipack_custom_wall' in o):
    """