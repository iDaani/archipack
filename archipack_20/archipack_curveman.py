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
import os
import json
import subprocess
from math import atan2, pi
from mathutils.geometry import interpolate_bezier
from mathutils import Matrix, Vector
from bpy.types import Operator
from bpy.props import (
    StringProperty, EnumProperty, BoolProperty,
    FloatVectorProperty, IntProperty, FloatProperty
    )
from .archipack_viewmanager import ViewManager
from .archipack_iconmanager import icons
from .archipack_i18n import Archipacki18n
from .archipack_object import ArchipackGenericOperator, ArchipackObjectsManager
from .archipack_generator import Generator, Line



import logging

logger = logging.getLogger("archipack")

precision = 6


"""Generate presets from selected curves in a files

sel = C.selected_objects
for o in sel:
    bpy.ops.archipack.profile_save(
        'INVOKE_DEFAULT',
        object_name=o.name,
        preset_name="Molding_{}".format(o.name.capitalize())
        )
"""


def get_enum(self, context):
    return icons.enum(context, "archipack_curves", "json")


def preset_operator(self, context):

    if self.auto_update and self.profil == 'USER':
        act = context.active_object
        sel = context.selected_objects
        # print("preset_operator:", act.name)

        self.user_profile_savename = bpy.path.display_name(self.user_profile_filename)
        self.load_profile(context, update=True)

        cls = self.__class__.__name__
        x, y, z = self.user_profile_dimension
        for o in sel:
            if o.data and o.name != act.name:
                if cls in o.data:
                    self.select_object(context, o, True)
                    d = getattr(o.data, cls)[0]
                    d.auto_update = False
                    # enable user profile
                    d.profil = self.profil
                    d.user_profile_dimension = self.user_profile_dimension.copy()
                    d.user_profile_savename = self.user_profile_savename
                    d.user_profile_filename = self.user_profile_filename
                    # profile curve object name
                    d.user_profile = self.user_profile
                    d.refresh_profile_size(context, x, y)
                    d.auto_update = True

        self.select_object(context, act, True)

    return None


class ArchipackCurveManager:
    """
     Provide utility to manage curve input
    """
    def is_cw(self, pts):
        p0 = pts[-1]
        d = 0
        for p in pts:
            d += (p.x * p0.y - p.y * p0.x)
            p0 = p
        return d > 0

    def interpolate_bezier(self, pts, wM, p0, p1, resolution):
        """
         Bezier curve approximation
        """
        if (resolution == 0 or
                (p0.handle_right_type == 'VECTOR' and
                p1.handle_left_type == 'VECTOR')):
            pts.append(wM @ p0.co.to_3d())
        else:
            v = (p1.co - p0.co).normalized()
            d1 = (p0.handle_right - p0.co).normalized()
            d2 = (p1.co - p1.handle_left).normalized()
            if d1 == v and d2 == v:
                pts.append(wM @ p0.co.to_3d())
            else:
                seg = interpolate_bezier(wM @ p0.co,
                    wM @ p0.handle_right,
                    wM @ p1.handle_left,
                    wM @ p1.co,
                    resolution + 1)
                for i in range(resolution):
                    pts.append(seg[i].to_3d())

    def coords_from_spline(self, spline, itM, resolution, ccw=False, cw=False, close=False):
        """
        :param spline: Blender spline (bezier or poly)
        :param itM: Matrix to make points absolute world
        :param resolution: bezier resolution
        :param ccw: return points in ccw order
        :param cw: return points in cw order
        :param close: force closed spline
        :return:
        """
        pts = []
        closed = close or spline.use_cyclic_u

        if spline.type == 'POLY':

            pts = [itM @ p.co.to_3d() for p in spline.points]

        elif spline.type == 'BEZIER':

            points = spline.bezier_points

            for i in range(1, len(points)):
                p0 = points[i - 1]
                p1 = points[i]
                self.interpolate_bezier(pts, itM, p0, p1, resolution)

            if closed:
                p0 = points[-1]
                p1 = points[0]
                self.interpolate_bezier(pts, itM, p0, p1, resolution)

            else:
                pts.append(itM @ points[-1].co.to_3d())

        if ccw or cw:
            is_cw = self.is_cw(pts)
            if (ccw and is_cw) or (cw and not is_cw):
                pts = list(reversed(pts))
                if closed:
                    # keep first point at same location
                    pts.insert(0, pts.pop())

        if len(pts) < 1:
            return pts

        # skip duplicate points using arbitrary set distance threshold
        # to handle precision issues
        new_pts = []
        p1 = pts[-1]
        for p in pts:
            if (p - p1).length < 0.0001:
                continue
            new_pts.append(p)
            p1 = p
        return new_pts


def update_path(self, context):
    self.manipulable_refresh = True
    self.update_path(context)


def update_manipulators(self, context):
    self.manipulable_refresh = True
    self.update(context, manipulable_refresh=True)


def update_closed(self, context):
    if len(self.parts) > 0:
        g = self.get_generator()
        g.update_parts(self)
        self.manipulable_refresh = True
        self.update(context, manipulable_refresh=True)


# parts display in ui
# when closed ui = parts
# when open ui = parts - 1
def get_parts(self):
    return self.num_valid_parts


def set_parts(self, value):
    self.set_parts(value)

    return None


class ArchipackUserDefinedPath:
    """
      Archipack path based base class
      base for wall, slab, floor, fence, molding and cutters
      provide "from curve" and parts management (insert/remove)
    """
    user_defined_path: StringProperty(
        description="Use a curve to define shape",
        name="User defined",
        update=update_path
    )

    user_defined_reverse: BoolProperty(
        name="Reverse",
        default=False,
        update=update_path
    )

    user_defined_spline: IntProperty(
        name="Spline index",
        min=0,
        default=0,
        update=update_path
    )

    user_defined_resolution: IntProperty(
        name="Resolution",
        min=1,
        max=128,
        default=12,
        update=update_path
    )

    # apparent parts number, len(parts) - 1 when open
    n_parts: IntProperty(
        options={'SKIP_SAVE'},
        name="Segments",
        description="Number of segments",
        min=1,
        default=1,
        get=get_parts,
        set=set_parts,
        update=update_manipulators
    )

    user_defined_path_expand: BoolProperty(
        description="Expand from curve panel",
        name="From curve",
        options={'SKIP_SAVE'},
        default=False
    )
    closed: BoolProperty(
        default=False,
        name="Close",
        description="Close line",
        update=update_closed
    )
    user_defined_adaptive: BoolProperty(
        default=False,
        name="Adaptive",
        description="Adaptive interpolation",
        update=update_path
    )
    user_defined_deviation: FloatProperty(
        name="Deviation",
        min=0.01,
        default=0.2,
        precision=5,
        unit='LENGTH', subtype='DISTANCE',
        update=update_path
    )
    user_defined_angle: FloatProperty(
        name="Angle",
        min=0,
        default=0.13962634015954636,
        precision=5,
        subtype='ANGLE', unit='ROTATION',
        update=update_path
    )

    @property
    def is_closed(self):
        return self.always_closed or self.closed

    @property
    def num_parts(self):
        return len(self.parts)

    @property
    def num_valid_parts(self):
        i = self.num_parts
        if not self.is_closed:
           i -= 1
        return i

    @property
    def valid_parts(self):
        parts = self.parts[:]
        if not self.is_closed:
            parts.pop()
        return parts

    def move_object(self, o, p, axis=2):
        """Move object when first point of generator is moving
        :param o: blender object
        :param p: new location in world coordsys
        :param axis: [2:3] 2 for 2d location on xy plane only, 3 for 3d location
        :return:
        """

        # Keep track of child location in world coordsys
        _cp = [child.matrix_world.translation.to_3d() for child in o.children]

        if o.parent:
            _p = o.parent.matrix_world.inverted() @ p
        else:
            _p = p

        o.location[0:axis] = _p[0:axis]
        o.matrix_world.translation[0:axis] = p[0:axis]

        # Update child location so absolute location wont change
        if len(_cp) > 0:
            itM = o.matrix_world.inverted()
            for _p, child in zip(_cp, o.children):
                child.location[0:axis] = (itM @ _p)[0:axis]
                child.matrix_world.translation[0:axis] = _p[0:axis]

        # logger.debug("move_object p:%s old:%s new:%s", p, old, o.matrix_world.translation)

    def from_spline(self, context, o, curve, ccw=False, cw=False):

        auto_update = self.auto_update
        spline = curve.data.splines[self.user_defined_spline]

        closed = spline.use_cyclic_u
        # ensure there is enough points
        pts = []
        if spline.type == 'POLY':
            pts = spline.points
        elif spline.type == 'BEZIER':
            pts = spline.bezier_points

        n_pts = 2
        if closed:
            n_pts = 3

        if len(pts) < n_pts:
            return

        self.auto_update = False
        self.closed = closed

        idx = 0
        if not closed and self.user_defined_reverse:
            idx = -1

        p = curve.matrix_world @ pts[idx].co.to_3d()
        self.move_object(o, p, axis=3)

        g = Generator(o)
        g.from_spline(curve, spline, self.user_defined_resolution, ccw, cw,
                      self.user_defined_reverse,
                      self.is_closed,
                      adaptive=self.user_defined_adaptive,
                      deviation=self.user_defined_deviation,
                      angle_max=self.user_defined_angle)

        # print(g)

        # XXX wrong on open curves
        # self.n_parts = g.numknots
        self.n_parts = g.numsegs

        # self.update_parts()
        # print("n_parts:%s len(parts):%s" % (self.n_parts, len(self.parts)))

        g.update_parts(self)

        self.auto_update = auto_update
        return g

    def relocate_childs(self, context, o):
        """
         Override this method to synch childs when changes are done
        """
        return

    def after_reverse(self, context, o):
        self.auto_update = True

    def reverse(self, context, o):

        g = self.get_generator(o)

        g.reverse()

        self.auto_update = False
        g.update_parts(self)

        # location wont change when closed
        if not self.is_closed:
            self.move_object(o, g.segs[0].p0)

        self.after_reverse(context, o)

    def update_path(self, context):
        """
         Handle curve Io
         including multiple splines
        """
        o = self.find_in_selection(context)
        curve = self.get_scene_object(context, self.user_defined_path)

        if o is None or curve is None or curve.type != 'CURVE':
            self.restore_auto_manipulate(context)
            return

        splines = curve.data.splines
        if len(splines) > self.user_defined_spline:
            self.from_spline(context, o, curve)

        self.restore_auto_manipulate(context)

    def setup_parts_manipulators(self, z_prop='z', size=1, flip=False):
        """
         z_prop: prop name of dumb or real z size for placeholder
                 when drawing wall snap manipulator
        """
        for i, p in enumerate(self.parts):
            n_manips = len(p.manipulators)

            # angle between parts
            if n_manips < 1:
                s = p.manipulators.add()
            else:
                s = p.manipulators[0]

            if i == 0:
                s.type_key = "DUMB_ANGLE"
            else:
                s.type_key = "DUAL_ANGLE"
                s.prop1_name = json.dumps({"angle": "a_ui", "dir": "change_side", "lock": "lock"})
                s.prop2_name = json.dumps({"flip": flip})

            # size / radius
            if n_manips < 2:
                s = p.manipulators.add()
            else:
                s = p.manipulators[1]

            if p.type == 0:
                s.type_key = "DUAL_SIZE"
                s.prop1_name = json.dumps({"length": "l_ui", "dir": "change_side", "lock": "lock"})
                s.prop2_name = ""
            else:
                s.type_key = 'ARC_ANGLE_RADIUS'
                s.prop1_name = "da_ui"
                s.prop2_name = "r_ui"

            # start point snap
            if n_manips < 3:
                s = p.manipulators.add()
            else:
                s = p.manipulators[2]

            s.type_key = 'WALL_SNAP'
            s.prop1_name = str(i)
            s.prop2_name = z_prop

            # part number
            if n_manips < 4:
                s = p.manipulators.add()
            else:
                s = p.manipulators[3]

            s.type_key = 'DUMB_STRING'
            s.prop1_name = str(i + 1)

    def add_part(self, o, length):
        self.manipulable_disable(o)
        self.auto_update = False
        # self.parts.add()
        self.n_parts += 1
        # self.update_parts()
        last = -1
        if not self.is_closed:
            last -= 1
        p = self.parts[last]
        p.length = length
        # self.update_parts()
        # self.setup_manipulators()
        self.auto_update = True
        return p

    def before_insert_part(self, context, o, g):
        return

    def after_insert_part(self, context, o, where, distance):
        """
         Rebuild childs index and location
         When removing a part
         Override in objects synch childs
        """
        return

    def insert_part(self, context, o, where):

        # disable manipulate as we are changing structure
        self.manipulable_disable(o)

        self.auto_update = False

        g = self.get_generator()
        # detect childs location
        self.before_insert_part(context, o, g)

        _last = g.segs[where]
        p0 = _last.lerp(0.5)
        part_0 = self.parts[where]

        _k = g.create_segment(self, part_0, p0, _last, "None")

        if part_0.type == 1:
            _k._da /= 2

        if where + 1 < len(g.segs):
            _next = g.segs[where + 1]
            _k._next = _next
            _next._last = _k

        g.segs.insert(where + 1, _k)

        # self.parts.add()
        self.n_parts += 1

        g.update_parts(self)

        self.after_insert_part(context, o, where, _k.v_length)
        # self.update_parts()
        # self.manipulable_refresh = True
        # self.setup_manipulators()
        self.auto_update = True

    def before_make_first(self, context, o, g):
        return

    def after_make_first(self, context, o, where):
        return

    def make_first(self, context, o, where):
        if self.is_closed:
            self.manipulable_disable(o)
            self.auto_update = False
            g = self.get_generator()
            self.before_make_first(context, o, g)

            g.make_first(where)
            g.update_parts(self)
            # Reset origin is in local coordsys, use only rotation part to ensure stability
            # over loops as setting location does not update matrix_world in the between
            p = o.matrix_world @ g.reset_origin()
            self.move_object(o, p)
            # self.manipulable_refresh = True
            # self.setup_manipulators()
            self.auto_update = True
            self.after_make_first(context, o, where)

    def before_remove_part(self, context, o, g):
        return

    def after_remove_part(self, context, o, where):
        return

    def remove_part(self, context, o, where):

        if self.num_parts < 2:
            return

        self.manipulable_disable(o)
        # disable manipulate as we are changing structure

        self.auto_update = False

        g = self.get_generator()

        # detect childs location
        self.before_remove_part(context, o, g)

        # self.parts.remove(0)
        self.n_parts -= 1

        # preserve shape
        # using generator
        last = None

        if where > 0 or self.is_closed:
            last = g.segs[where - 1]

        g.segs.pop(where)

        w = None

        if where < len(g.segs):
            w = g.segs[where]

        elif self.is_closed:
            w = g.segs[0]

        if w is not None and last is not None:
            w._last = last
            last._next = w

        g.update_parts(self)

        if where < 1:
            # Reset origin is in local coordsys, use only rotation part to ensure stability
            # over loops as setting location does not update matrix_world in the between
            p = o.matrix_world @ g.reset_origin()
            self.move_object(o, p)

        self.after_remove_part(context, o, where)

        # fix snap manipulators index
        # self.update_parts()
        # self.manipulable_refresh = True

        # self.setup_manipulators()
        self.auto_update = True

    def set_parts(self, value):

        if self.is_closed:
            n_parts = max(3, value)
        else:
            n_parts = max(2, value + 1)

        delta = n_parts - self.num_parts

        if delta < 0:
            # remove parts
            for i in range(self.num_parts, n_parts, -1):
                self.parts.remove(i - 1)
        elif delta > 0:
            # add missing parts
            for i in range(self.num_parts, n_parts):
                self.parts.add()

        if delta != 0:
            self.update_parts()
            self.manipulable_refresh = True

    def update_parts(self):

        for p in self.parts:
            self.create_uid(p)

        self.setup_manipulators()

    def template_user_path(self, context, layout, focus=True):
        """
         Draw from curve in ui
        """
        icon = "TRIA_RIGHT"
        if self.user_defined_path_expand:
            icon = "TRIA_DOWN"

        row = layout.row(align=True)
        self.draw_prop(context, layout, row, self, 'user_defined_path_expand', icon=icon, emboss=True)

        if self.user_defined_path_expand:
            row = layout.row(align=True)
            if self.user_defined_path != "":
                op = self.draw_op(context, layout, row, "archipack.object_edit", text="", icon="EDITMODE_HLT")
                op.object_name = self.user_defined_path
                op.update_func_name = "update_path"
                op.focus = focus
                self.draw_op(context, layout, row,
                            "archipack.select",
                            text = "",
                            icon="RESTRICT_SELECT_OFF"
                            ).name = self.user_defined_path

            row.prop_search(self, "user_defined_path", context.scene, "objects", text="", icon='OUTLINER_OB_CURVE')

            if self.user_defined_path != "":
                self.draw_op(context, layout, row,
                         "archipack.object_update",
                         text="",
                         icon='FILE_REFRESH').update_func_name = "update_path"
                self.draw_prop(context, layout, layout, self, 'user_defined_spline')
                self.draw_prop(context, layout, layout, self, 'user_defined_adaptive')
                if self.user_defined_adaptive:
                    self.draw_prop(context, layout, layout, self, 'user_defined_deviation')
                    self.draw_prop(context, layout, layout, self, 'user_defined_angle')
                else:
                    self.draw_prop(context, layout, layout, self, 'user_defined_resolution')

        return self.user_defined_path_expand

    def template_parts(self, context, layout, draw_type=False, draw_reverse=True):
        """
         Draw parts in ui
        """
        box = layout.box()
        row = box.row()
        self.draw_prop(context, layout, row, self, "n_parts")

        if not self.always_closed:
            self.draw_prop(context, layout, row, self, "closed")
            if draw_reverse:
                self.draw_op(context, layout, box, "archipack.path_reverse", icon='FILE_REFRESH')

        for i, part in enumerate(self.valid_parts):
            box = layout.box()
            part.draw(context, box, i, draw_type=draw_type, closed=self.is_closed)

    def manipulable_setup_parts(self, context, o):
        """
            NOTE:
            this one assume context.active_object is the instance this
            data belongs to, failing to do so will result in wrong
            manipulators set on active object
        """

        n_parts = self.num_valid_parts

        for i, part in enumerate(self.parts):
            if i < n_parts:
                if i > 0 or self.is_closed:
                    # start angle
                    self.manip_stack.append(part.manipulators[0].setup(context, o, part))

                # length / radius + angle
                self.manip_stack.append(part.manipulators[1].setup(context, o, part))

                # index
                self.manip_stack.append(part.manipulators[3].setup(context, o, self))

            # snap point
            self.manip_stack.append(part.manipulators[2].setup(context, o, self))

    def manipulable_setup(self, context, o):

        self.setup_manipulators()
        self.manipulable_setup_parts(context, o)


class ARCHIPACK_OT_path_reverse(ArchipackGenericOperator, Operator):
    bl_idname = "archipack.path_reverse"
    bl_label = "Reverse"
    bl_description = "Reverse parts order"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'INTERNAL', 'UNDO'}

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            d = self.datablock(o)
            if d is None:
                return {'CANCELLED'}
            d.reverse(context, o)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ArchipackProfileManager(ArchipackCurveManager):
    """
     IO thumbs and .json files for user defined profiles
    """
    def _round_co(self, co, prec):
        return (round(co.x, prec), round(co.y, prec), round(co.z, prec))

    def _from_json(self, curve, json_str):
        """
          Adds a spline to a curve
        """
        js = json.loads(json_str)
        curve.resolution_u = js['resolution_u']
        curve.fill_mode = js['fill_mode']

        for spl in js['splines']:

            spline = curve.splines.new(spl['type'])
            spline.use_cyclic_u = spl['use_cyclic_u']
            pts = spl['points']

            if spl['type'] == 'BEZIER':
                handle_left = spl['handle_left']
                handle_right = spl['handle_right']
                spline.bezier_points.add(count=len(pts) - 1)
                for i, p in enumerate(pts):
                    spline.bezier_points[i].co = Vector(p).to_3d()
                    spline.bezier_points[i].handle_left = Vector(handle_left[i]).to_3d()
                    spline.bezier_points[i].handle_right = Vector(handle_right[i]).to_3d()
            elif spl['type'] == 'POLY':
                spline.points.add(count=len(pts) - 1)
                for i, p in enumerate(pts):
                    spline.points[i].co = Vector(p).to_4d()

        return js['x'], js['y']

    def _to_json(self, curve):
        curve.dimensions = '2D'
        js = {
            'resolution_u': curve.resolution_u,
            'fill_mode': curve.fill_mode,
            'splines': []
            }

        for spline in curve.splines:
            spl = {
                'type': spline.type,
                'use_cyclic_u': spline.use_cyclic_u,
            }
            if spline.type == 'BEZIER':
                pts = spline.bezier_points
                spl['handle_left'] = [self._round_co(p.handle_left, precision) for p in pts]
                spl['handle_right'] = [self._round_co(p.handle_right, precision) for p in pts]

            elif spline.type == 'POLY':
                pts = spline.points

            spl['points'] = [self._round_co(p.co, precision) for p in pts]
            js['splines'].append(spl)

        js['x'], js['y'] = self._curve_bound_box(curve)

        json_str = json.dumps(js, sort_keys=True)
        return json_str

    def _find_preset_full_path(self, preset_name):
        """
         @preset_name: preset clean file name without extension
         Return preset full path with .json extension
        """
        filename = "{}.json".format(preset_name)
        folders = bpy.utils.script_paths("presets")
        # append after user script so user override factory
        folders.append(os.path.join(
                os.path.dirname(os.path.realpath(__file__)),
                "presets"))
        for folder in folders:
            user_preset = os.path.join(
                folder,
                "archipack_curves",
                filename)
            if os.path.isfile(user_preset):
                return user_preset
        return None

    def _user_preset_full_path(self, preset_name):
        """
         @preset_name: preset clean file name without extension
         Return absolute full path of preset in user presets folder
        """
        preset_file = "{}.json".format(preset_name)
        presets_path = bpy.utils.user_resource('SCRIPTS',
                          os.path.join("presets", "archipack_curves"),
                          create=True)
        return os.path.join(presets_path, preset_file)

    def load_curve(self, preset_name):
        """
          Load a json spline definition
          return curve data not linked to scene
        """
        curve = None
        x, y = 0, 0
        # load from current directory
        full_path = self._find_preset_full_path(preset_name)

        if full_path is None:
            # print("Archipack: Error preset file not found")
            return curve, x, y

        try:
            with open(full_path) as fh:
                json_str = ''.join(fh.readlines())
            curve = bpy.data.curves.new(name=preset_name, type='CURVE')
            curve.dimensions = '2D'
            x, y = self._from_json(curve, json_str)
        except Exception as ex:
            import traceback
            traceback.print_exc()
            print("Archipack: Error while loading {}".format(preset_name))
            pass
        return curve, x, y

    def _curve_bound_box(self, curve):
        # estimate curve size
        pts = []
        for spline in curve.splines:
            pts.extend(self.coords_from_spline(spline, Matrix(), 12))
        x = [co.x for co in pts]
        y = [co.y for co in pts]
        sx = round(max(0.0001, max(x) - min(x)), precision)
        sy = round(max(0.0001, max(y) - min(y)), precision)
        return sx, sy

    def save_curve(self, o, preset_name):
        """
         Save a json curve spline definition
        """
        curve = o.data
        if curve:
            file_name = bpy.path.clean_name(preset_name)
            # always save in user presets
            full_path = self._user_preset_full_path(file_name)
            json_str = self._to_json(curve)
            with open(full_path, 'w') as fh:
                fh.write(json_str)
            # clear file when present
            base_path, ext = os.path.splitext(full_path)
            thumb = base_path + '.png'
            if os.path.isfile(thumb):
                os.remove(thumb)
            # update profiles enum
            icons.add_preset("archipack_curves", file_name, "json")
            self.background_render(thumb)

    def background_render(self, preset):
        generator = os.path.dirname(os.path.realpath(__file__)) + os.path.sep + "archipack_thumbs.py"
        # Run external instance of blender like the original thumbnail generator.
        cmd = [
            bpy.app.binary_path,
            "--background",
            "--factory-startup",
            "-noaudio",
            "--python", generator,
            "--",
            "addon:" + __package__,
            "matlib:none",
            "cls:archipack_curves",
            "preset:" + preset
            ]
        subprocess.Popen(cmd)


def update(self, context):

    if self.auto_update:
        o = self.get_scene_object(context, self.user_profile)
        if o and o.type == 'CURVE':
            self.auto_update = False
            sx, sy = self._curve_bound_box(o.data)
            self.user_profile_dimension.x = sx
            self.user_profile_dimension.y = sy
            self.refresh_profile_size(context, sx, sy)
            self.auto_update = True
            self.update(context)
        elif self.user_profile != "":
            self.user_profile = ""


class ArchipackProfile(ArchipackProfileManager, ArchipackObjectsManager):
    """
      Propertygroup to manage user profiles
      Must inherit ArchipackObjectsManager
    """
    user_profile_filename: EnumProperty(
            name="Profiles",
            description="Available profiles presets",
            items=get_enum,
            update=preset_operator
            )
    user_profile_savename: StringProperty(
            options={'SKIP_SAVE'},
            default="",
            description="Profile filename without extension",
            name="Save"
            )
    user_profile: StringProperty(
            name="Profile",
            description="Use curve as profile",
            default="",
            update=update
            )
    user_profile_dimension: FloatVectorProperty(subtype='XYZ')
    profile_expand: BoolProperty(
        options={'SKIP_SAVE'},
        default=False,
        description="Expand panel display",
        name="Expand"
        )

    def refresh_profile_size(self, context, x, y):
        """
         Todo: override this method
         to update profile size ui
         eg:
         self.x = x
         self.z = y

        """
        return

    def load_profile(self, context, update=False):

        # print("load_profile", self.user_profile_filename)

        if self.user_profile_filename is None:
            return None

        o = self.get_scene_object(context, self.user_profile)
        curve, x, y = self.load_curve(self.user_profile_filename)
        if curve:
            if o is None:
                o = bpy.data.objects.new(curve.name, curve)
                o.show_name = True
                self.link_object_to_scene(context, o, layer_name="profile")
                self.user_profile_savename = o.name
            else:
                d = o.data
                o.name = curve.name
                o.data = curve
                if d.users < 1:
                    bpy.data.curves.remove(d)

            self.auto_update = False
            self.user_profile_dimension = Vector((x, y, 0))
            self.user_profile = o.name
            # Sub objects without auto_update must update parent using this call
            self.refresh_profile_size(context, x, y)
            self.auto_update = True
        return o

    def update_profile(self, context):
        if self.user_profile == "":
            return
        o = self.get_scene_object(context, self.user_profile)
        if o is None:
            o = self.load_profile(context)
        return o

    def draw_user_profile(self, context, layout, update_func_name="update", focus=True):
        # self.draw_prop(context, layout, self, 'user_profile_filename')

        layout.template_icon_view(self, "user_profile_filename", show_labels=True, scale=5)
        row = layout.row(align=True)
        if self.user_profile != "":
            op = self.draw_op(context, layout, row, "archipack.object_edit", text="", icon="EDITMODE_HLT")
            op.focus = focus
            op.object_name = self.user_profile
            op.update_func_name = update_func_name
            self.draw_op(context, layout, row,
                         "archipack.select",
                         text="",
                         icon="RESTRICT_SELECT_OFF").name = self.user_profile
            self.draw_op(context, layout, row, "archipack.object_update", text="", icon="FILE_REFRESH")

        row.prop_search(self, "user_profile", context.scene, "objects", text="", icon='OUTLINER_OB_CURVE')

        if self.user_profile != "":
            row = layout.row(align=True)
            self.draw_prop(context, layout, row, self,'user_profile_savename', text="")
            op = self.draw_op(context, layout, row, "archipack.profile_save", text="Save", icon="NONE")
            op.object_name = self.user_profile
            op.preset_name = self.user_profile_savename


class ARCHIPACK_OT_object_update(ArchipackGenericOperator, Operator):
    bl_idname = "archipack.object_update"
    bl_label = "Update"
    bl_description = "Update object"
    bl_category = 'Archipack'

    update_func_name : StringProperty(
        description="Update function name",
        default="update"
        )

    def update(self, context, o):
        last_state = self.is_selected(o)
        self.select_object(context, o, True)
        d = self.datablock(o)
        try:
            getattr(d, self.update_func_name)(context)
        except:
            pass
        if last_state:
            self.select_object(context, o)
        else:
            self.unselect_object(context, o)

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            sel = context.selected_objects
            for c in sel:
                self.update(context, c)
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_object_edit(ArchipackGenericOperator, Operator):
    bl_idname = "archipack.object_edit"
    bl_label = "Edit"
    bl_description = "Edit an object"
    bl_options = {'REGISTER', 'UNDO', 'INTERNAL'}
    focus: BoolProperty(default=True)

    update_func_name : StringProperty(
        description="Update function name",
        default="update"
        )
    object_name : StringProperty(
        description="Curve name",
        default=""
        )

    wm = None
    act_name = None

    def modal(self, context, event):

        if event.type == 'ESC':
            bpy.ops.object.mode_set(mode="OBJECT")

        if 'EDIT' not in context.mode:
            o = self.get_scene_object(context, self.act_name)
            if o and self.update_func_name != "":
                d = self.datablock(o)
                if d:
                    try:
                        self.select_object(context, o, True)
                        getattr(d, self.update_func_name)(context)
                    except:
                        pass
            if self.focus:
                self.wm.restore()
            return {'FINISHED'}

        return {'PASS_THROUGH'}

    def invoke(self, context, event):
        if context.space_data is not None and context.space_data.type == 'VIEW_3D':

            self.act_name = context.active_object.name

            o = self.get_scene_object(context, self.object_name)
            if o and o.type in {'CURVE', 'MESH'}:
                if self.focus:
                    self.wm = ViewManager(context)
                    self.wm.save()
                    self.wm.safe_ortho_view(max(o.dimensions.x, o.dimensions.y), o.matrix_world.translation)
                bpy.ops.object.select_all(action="DESELECT")
                self.select_object(context, o, True)
                bpy.ops.object.mode_set(mode="EDIT")
                context.window_manager.modal_handler_add(self)
                return {'RUNNING_MODAL'}
            else:
                self.report({'WARNING'}, "Object not found")
                return {'CANCELLED'}
        else:
            self.report({'WARNING'}, "Active space must be a View3d")
            return {'CANCELLED'}


class ARCHIPACK_OT_profile_save(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.profile_save"
    bl_label = "Save"
    bl_description = "Save a profile curve"
    bl_options = {'REGISTER', 'UNDO', 'INTERNAL'}

    object_name : StringProperty(
        description="Curve object name",
        default=""
        )
    preset_name : StringProperty(
        description="Preset file name without extension",
        default=""
        )

    @classmethod
    def poll(self, context):
        return True

    def invoke(self, context, event):
        # if context.space_data is not None and context.space_data.type == 'VIEW_3D':
        o = self.get_scene_object(context, self.object_name)
        if o and o.type == 'CURVE':
            manager = ArchipackProfileManager()
            manager.save_curve(o, self.preset_name)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Invalid curve")
            return {'CANCELLED'}


def register():
    bpy.utils.register_class(ARCHIPACK_OT_path_reverse)
    bpy.utils.register_class(ARCHIPACK_OT_object_edit)
    bpy.utils.register_class(ARCHIPACK_OT_object_update)
    bpy.utils.register_class(ARCHIPACK_OT_profile_save)


def unregister():
    bpy.utils.unregister_class(ARCHIPACK_OT_path_reverse)
    bpy.utils.unregister_class(ARCHIPACK_OT_object_edit)
    bpy.utils.unregister_class(ARCHIPACK_OT_object_update)
    bpy.utils.unregister_class(ARCHIPACK_OT_profile_save)


"""Test load/save
from archipack.archipack_curveman import ArchipackCurveManager, ArchipackProfileManager

cman = ArchipackCurveManager()

curve = C.object.data
cman.save_curve(curve, "test")

curve = cman.load_curve("test")
o = bpy.data.objects.new(curve.name, curve)
C.scene.objects.link(o)

"""
