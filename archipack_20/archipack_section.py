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
from bpy.types import Operator, PropertyGroup, Curve, Object, Camera, Panel
from bpy.props import (
    FloatProperty, EnumProperty, BoolProperty,
    CollectionProperty, StringProperty
    )
from mathutils import Vector, Matrix
from math import pi, tan
from .archipack_manipulator import Manipulable
from .archipack_preset import ArchipackPreset, PresetMenuOperator
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackPanel,
    ArchipackCreateTool, ArchipackObject, ArchipackObjectsManager
    )
from .archipack_dimension import DimensionProvider
from .archipack_autoboolean import ArchipackBoolManager


def update(self, context):
    self.update(context)


class ArchipackSectionManager(ArchipackObjectsManager):
    """
     A class to manage sections
     as solid surfaces or curves
     from camera or archipack_section
    """
    def as_mesh(self, context, src, as_tri=False):
        """
         Get temporary meshes to bisect
        """
        m = src.to_mesh(
            depsgraph=context.depsgraph,
            # scene=context.scene,
            apply_modifiers=True,
            # settings='RENDER',
            # calc_tessface=True,
            calc_undeformed=False)

        o = bpy.data.objects.new("Temp", m)
        o.matrix_world = src.matrix_world.copy()
        # Link object into scene
        self.link_object_to_scene(context, o)
        
        if as_tri:        
            self.select_object(context, o, True)
            bpy.ops.object.mode_set(mode='EDIT')
            bpy.ops.mesh.select_all(action='SELECT')
            bpy.ops.mesh.quads_convert_to_tris(quad_method='BEAUTY', ngon_method='BEAUTY')
            bpy.ops.object.mode_set(mode='OBJECT')
            self.unselect_object(context, o)

        return o

    def clip_section(self, o, plane_co, plane_cr, clip):
        """
         Clear sides
        """
        if clip > 0 and o.data and len(o.data.edges) > 0:

            d = clip * plane_cr

            bpy.ops.object.mode_set(mode='EDIT')
            bpy.ops.mesh.select_all(action='SELECT')
            # clear sides
            if bpy.ops.mesh.bisect.poll():
                try:
                    bpy.ops.mesh.bisect(
                        plane_co=plane_co + d,
                        plane_no=plane_cr,
                        use_fill=False,
                        clear_inner=False,
                        clear_outer=True)
                except:
                    pass

            bpy.ops.mesh.select_all(action='SELECT')
            if bpy.ops.mesh.bisect.poll():
                try:
                    bpy.ops.mesh.bisect(
                    plane_co=plane_co - d,
                    plane_no=plane_cr,
                    use_fill=False,
                    clear_inner=True,
                    clear_outer=False)
                except:
                    pass

            bpy.ops.object.mode_set(mode='OBJECT')

    def generate_section(self, context, sel, tM, clip_x, clip_y):
        """
          Bisect meshes
          sel: array of temp mesh to bissect
          tM: matrix of main bissect plane
          clip_x: width of bissect in the tM plane
          clip_y: width of bissect in the tM plane
            when 0 doesn't clip
          return mesh object
        """
        bpy.ops.object.select_all(action='DESELECT')
        rM = tM.to_3x3().transposed()
        plane_co = tM.translation
        plane_x = rM[0]
        plane_y = rM[1]
        plane_no = rM[2]

        objs = [self.as_mesh(context, o) for o in sel]

        for o in objs:
        
            # select and make active
            self.select_object(context, o, True)

            if o.data and len(o.data.edges) > 0:
                bpy.ops.object.mode_set(mode='EDIT')
                bpy.ops.mesh.select_all(action='SELECT')
                if bpy.ops.mesh.bisect.poll():
                    bpy.ops.mesh.bisect(
                        'INVOKE_DEFAULT',
                        plane_co=plane_co,
                        plane_no=plane_no,
                        use_fill=True,
                        clear_inner=True,
                        clear_outer=True)
                bpy.ops.object.mode_set(mode='OBJECT')

        for o in objs:
            # select and make active
            self.select_object(context, o)
            
        # create a placeholder mesh to join other into
        m = bpy.data.meshes.new("Section")
        o = bpy.data.objects.new("Section", m)
        # Link object into scene
        self.link_object_to_scene(context, o, layer_name="2d")
        
        # select and make active
        self.select_object(context, o, True)
        
        bpy.ops.object.join()

        o = context.active_object
        self.clip_section(o, plane_co, plane_x, clip_x)
        self.clip_section(o, plane_co, plane_y, clip_y)
        return o

    def as_curves(self, context, o, itM, loc, src_name, section_name):
        """
         o: source mesh
         tM: source section object matrix world
         loc: location for section at create time (wont change on update)
         src_name: source section object name
         section_name: old curve name if any

         return curve object or None
        """
        if len(o.data.vertices) > 0:
            scene = context.scene

            for v in o.data.vertices:
                v.co = itM @ v.co

            # fix 2.8 issue with faces
            bpy.ops.object.mode_set(mode='EDIT')
            bpy.ops.mesh.select_all(action='SELECT')
            bpy.ops.mesh.delete(type='ONLY_FACE')
            bpy.ops.object.mode_set(mode='OBJECT')

            bpy.ops.object.convert(target='CURVE', keep_original=False)

            o = context.active_object
            old = scene.objects.get(section_name)

            if old is not None and o.name != section_name:
                # replace old.data by new one
                rem = old.data
                old.data = o.data
                # remove newly created object
                self.unlink_object_from_scene(context, o)
                bpy.data.curves.remove(rem)
                o = old
            else:
                x, y, z = loc
                o.location = Vector((x, y, 0))

            # select and make active
            self.select_object(context, o, True)
        
            d = archipack_section_target.datablock(o)

            if d is None:
                d = o.archipack_section_target.add()
                d.source_name = src_name
                # self.unlink_object_from_scene(context, o)
                # self.link_object_to_scene(context, o, layer_name="2d")

            d.update(context)
            return o

        return None

    def get_objects(self, context, o, sel):
        """
          Get scene objects found in o bounding box
          return plane matrix_world (source matrix_world or camera plane)
        """
        # use manager to find objects in bounding box
        manager = ArchipackBoolManager()

        # clear scale TODO: check with scaled camera
        loc, rot, sca = o.matrix_world.decompose()
        tM = Matrix.Translation(loc) @ rot.to_matrix().to_4x4()

        if o.type == 'CAMERA':
            d = o.data
            c = d.clip_start
            w = 0.5 * tan(d.angle_x) * c
            h = 0.5 * tan(d.angle_y) * c
            p0 = tM @ Vector((w, h, -c))
            p1 = tM @ Vector((-w, h, -c))
            p2 = tM @ Vector((-w, -h, -c))
            p3 = tM @ Vector((w, -h, -c))
            x = [p.x for p in [p0, p1, p2, p3]]
            y = [p.y for p in [p0, p1, p2, p3]]
            z = [p.z for p in [p0, p1, p2, p3]]
            manager.minx = min(x)
            manager.maxx = max(x)
            manager.miny = min(y)
            manager.maxy = max(y)
            manager.minz = min(z)
            manager.maxz = max(z)
            tM = tM @ Matrix.Translation(Vector((0, 0, - c - 0.001)))
            # Matrix.Rotation(pi / 2, 4, Vector((1, 0, 0))))
        else:
            manager._init_bounding_box(o)
            # exclude z filtering
            # manager.minz = -1e32
            # manager.maxz = 1e32
            # manager.center.z = 0

        sel.extend([
            c for c in context.scene.objects
            if c.type == 'MESH' and
            self.is_visible(c) and
            not c.hide_render and
            c.name != o.name and
            not archipack_section_target.filter(c) and
            manager._contains(c)
            ])
        return tM


class archipack_section_target(ArchipackObject, DimensionProvider, PropertyGroup):
    source_name: StringProperty(default="")
    text_x: FloatProperty(
        name="x",
        description="Text Offset",
        unit='LENGTH', subtype='DISTANCE',
        default=0,
        update=update
    )
    text_y: FloatProperty(
        name="y",
        description="Text Offset",
        unit='LENGTH', subtype='DISTANCE',
        default=0,
        update=update
    )
    text_angle: FloatProperty(
        name="Angle",
        description="Text Angle",
        subtype='ANGLE', unit='ROTATION',
        default=0,
        update=update
    )
    text_size: FloatProperty(
        name="Text size",
        description="Text size",
        unit='LENGTH', subtype='DISTANCE',
        default=0.25, min=0.01,
        update=update
    )
    text: StringProperty(
        name="Label",
        default="",
        update=update
    )

    def update_child(self, context, o, center):
        t_o = None
        text = None
        # find text if any
        for child in o.children:
            if child.type == 'FONT':
                t_o = child
                text = t_o.data
                break

        if t_o is None:
            text = bpy.data.curves.new(o.name + "_label", type='FONT')
            text.dimensions = '2D'
            t_o = bpy.data.objects.new(o.name + "_label", text)
            # Link object into scene
            self.link_object_to_scene(context, t_o, layer_name="2d")
            t_o.parent = o

        self.link_materials(context, o, t_o)
        t_o.location = center
        t_o.rotation_euler.z = self.text_angle
        text.body = self.text
        text.size = self.text_size
        text.align_x = 'CENTER'
        return t_o

    def update(self, context):

        o = context.active_object

        if archipack_section_target.datablock(o) != self:
            return

        curve = o.data
        self.dimension_points.clear()
        j = 0
        for spline in curve.splines:
            for i, v in enumerate(spline.points):
                self.add_dimension_point(j, v.co.to_3d())
                j += 1

        center = Vector(o.bound_box[0]) + Vector((self.text_x, self.text_y, 0))
        self.update_child(context, o, center)

        self.update_dimensions(context, o)


class archipack_section(ArchipackObject, ArchipackSectionManager, Manipulable, PropertyGroup):
    """ Archipack section"""
    size: FloatProperty(
        description="Mesured section",
        name="Width",
        default=10,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    height: FloatProperty(
        description="Mesured section",
        name="Height",
        default=10,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    symbol_size: FloatProperty(
        name="Symbol Size",
        default=1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    text_x: FloatProperty(
        name="x",
        description="Text Offset",
        unit='LENGTH', subtype='DISTANCE',
        default=0,
        update=update
    )
    text_y: FloatProperty(
        name="y",
        description="Text Offset",
        unit='LENGTH', subtype='DISTANCE',
        default=0,
        update=update
    )
    text_angle: FloatProperty(
        name="Angle",
        description="Text Angle",
        subtype='ANGLE', unit='ROTATION',
        default=0,
        update=update
    )
    text_size: FloatProperty(
        name="Text size",
        description="Text size",
        unit='LENGTH', subtype='DISTANCE',
        default=0.25, min=0.01,
        update=update
    )
    text: StringProperty(
        name="Label",
        default="",
        update=update
    )
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update
    )
    section_name: StringProperty(default="")

    def setup_manipulators(self):
        if len(self.manipulators) < 1:
            # add manipulator for x property
            s = self.manipulators.add()
            s.prop1_name = "size"
            s.prop2_name = "x"
            s.type_key = "SNAP_SIZE_LOC"
        if len(self.manipulators) < 2:
            # add manipulator for x property
            s = self.manipulators.add()
            s.prop1_name = "height"
            s.type_key = "SIZE"

    def _add_spline(self, curve, closed, coords):
        spline = curve.splines.new('POLY')
        spline.use_endpoint_u = False
        spline.use_cyclic_u = closed
        spline.points.add(len(coords) - 1)
        for i, coord in enumerate(coords):
            x, y, z = coord
            spline.points[i].co = (x, y, z, 1)

    def update_child(self, context, o, center):
        t_o = None
        text = None
        # find text if any
        for child in o.children:
            if child.type == 'FONT':
                t_o = child
                text = t_o.data
                break

        if t_o is None:
            text = bpy.data.curves.new(o.name + "_label", type='FONT')
            text.dimensions = '2D'
            t_o = bpy.data.objects.new(o.name + "_label", text)
            # Link object into scene
            self.link_object_to_scene(context, t_o, layer_name="2d")
            t_o.parent = o

        self.link_materials(context, o, t_o)
        t_o.location = center
        t_o.rotation_euler.z = self.text_angle
        text.body = self.text
        text.size = self.text_size
        text.align_x = 'CENTER'
        return t_o

    def update_section(self, context):
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return
        src_name = o.name
        loc = o.matrix_world.translation
        clip_x = 0.5 * self.size
        clip_y = 0.5 * self.height
        sel = []
        tM = self.get_objects(context, o, sel)

        # rotate section plane normal 90 deg on x axis
        pM = tM @ Matrix.Rotation(pi / 2, 4, "X")
        s = self.generate_section(context, sel, pM, clip_x, clip_y)

        # get points in plane matrix coordsys so they are in 2d
        itM = pM.inverted()
        c = self.as_curves(context, s, itM, loc, src_name, self.section_name)
        if c is None:
            self.delete_object(context, s)
        else:
            self.section_name = c.name
            self.link_materials(context, o, c)
        self.restore_context(context)
        return c

    def make_section_symbol(self, curve, w, h, z):

        p0 = Vector((-w, h, z))
        p1 = Vector((-w, 0, z))
        p2 = Vector((w, 0, z))
        p3 = Vector((w, h, z))

        self._add_spline(curve, False, [p0, p1, p2, p3])

        x = 0.2 * h
        y = 0.2 * h
        p1 = Vector((-w - x, h - y, z))
        p2 = Vector((-w + x, h - y, z))
        self._add_spline(curve, False, [p1, p0, p2])
        p1 = Vector((w - x, h - y, z))
        p2 = Vector((w + x, h - y, z))
        self._add_spline(curve, False, [p1, p3, p2])

    def update(self, context):

        # provide support for "copy to selected"
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        curve = o.data
        curve.splines.clear()

        w = 0.5 * self.size
        h = self.symbol_size
        z = 0.5 * self.height
        self.make_section_symbol(curve, w, h, -z)
        self.make_section_symbol(curve, w, h, z)

        self.setup_manipulators()
        self.manipulators[0].set_pts([(-w, 0, -z), (w, 0, -z), (0.5, 0, 0)])
        self.manipulators[1].set_pts([(w, 0, -z), (w, 0, z), (-0.5, 0, 0)], normal=o.matrix_world.row[0].to_3d())

        center = Vector((w + self.text_x, self.text_y, 0))
        self.update_child(context, o, center)
        # always restore context
        self.restore_context(context)

    def on_delete(self, context, o):
        s = self.get_scene_object(context, self.section_name)
        self.delete_object(context, s)


class archipack_section_camera(ArchipackObject, ArchipackSectionManager, PropertyGroup):
    """ Archipack camera section"""
    auto_update: BoolProperty(
        # Wont save auto_update state in any case
        options={'SKIP_SAVE'},
        default=True,
        update=update
    )
    section_name: StringProperty(default="")

    def update_section(self, context):
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return
        src_name = o.name

        d = o.data
        c = d.clip_start
        clip_x = tan(0.5 * d.angle_x) * c
        clip_y = tan(0.5 * d.angle_y) * c

        sel = []
        tM = self.get_objects(context, o, sel)
        last_o =self.get_scene_object(context, self.section_name)
        new_o = self.generate_section(context, sel, tM, clip_x, clip_y)
        
        # select and make active
        self.select_object(context, new_o, True)
        
        d = new_o.archipack_section_target.add()
        d.source_name = src_name
        self.section_name = new_o.name

        if last_o:
            self.delete_object(context, last_o)

        self.restore_context(context)
        return new_o

    def update(self, context):

        # provide support for "copy to selected"
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        # always restore context
        self.restore_context(context)

    def on_delete(self, context, o):
        s = self.get_scene_object(context, self.section_name)
        self.delete_object(context, s)


class ARCHIPACK_PT_section_camera(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_section_camera"
    bl_label = "Section"

    @classmethod
    def poll(cls, context):
        return archipack_section_camera.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        if not archipack_section_camera.filter(o):
            return
        layout = self.layout
        self.draw_op(context, layout, layout, 'archipack.delete', icon='TRASH')
        self.draw_op(context, layout, layout, 'archipack.section_select', text="Select target")
        self.draw_op(context, layout, layout, 'archipack.section_update', icon='FILE_REFRESH')


class ARCHIPACK_PT_section(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_section"
    bl_label = "Section"

    @classmethod
    def poll(cls, context):
        # ensure your object panel only show when active object is the right one
        return archipack_section.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_section.datablock(o)
        if d is None:
            return

        layout = self.layout

        self.draw_common(context, layout)

        self.draw_op(context, layout, layout, 'archipack.section_select', text="Select target")
        self.draw_op(context, layout, layout, 'archipack.section_update', icon='FILE_REFRESH')

        box = layout.box()
        row = box.row(align=True)

        # Presets operators
        self.draw_op(context, layout, row, "archipack.section_preset_menu",
                     text=bpy.types.ARCHIPACK_OT_section_preset_menu.bl_label, icon="PRESET")
        self.draw_op(context, layout, row, "archipack.section_preset", icon='ADD', text="")
        self.draw_op(context, layout, row, "archipack.section_preset", icon='REMOVE', text="").remove_active = True

        box = layout.box()
        self.draw_prop(context, layout, box, d, 'size')
        self.draw_prop(context, layout, box, d, 'height')
        self.draw_prop(context, layout, box, d, 'symbol_size')
        box = layout.box()
        self.draw_prop(context, layout, box, d, 'text')
        self.draw_prop(context, layout, box, d, 'text_size')
        self.draw_prop(context, layout, box, d, 'text_x')
        self.draw_prop(context, layout, box, d, 'text_y')
        self.draw_prop(context, layout, box, d, 'text_angle')


class ARCHIPACK_PT_section_target(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_section_target"
    bl_label = "Section"

    @classmethod
    def poll(cls, context):
        return archipack_section_target.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        layout = self.layout
        # retrieve datablock of your object
        d = archipack_section_target.datablock(o)
        self.draw_op(context, layout, layout, 'archipack.delete', icon='TRASH')
        self.draw_op(context, layout, layout, 'archipack.section_select', text="Select source")
        self.draw_op(context, layout, layout, 'archipack.section_update', icon='FILE_REFRESH')
        src = context.scene.objects.get(d.source_name)
        if archipack_section.filter(src):
            box = layout.box()
            self.draw_prop(context, layout, box, d, 'text')
            self.draw_prop(context, layout, box, d, 'text_size')
            self.draw_prop(context, layout, box, d, 'text_x')
            self.draw_prop(context, layout, box, d, 'text_y')
            self.draw_prop(context, layout, box, d, 'text_angle')


class ARCHIPACK_OT_section(ArchipackCreateTool, Operator):
    bl_idname = "archipack.section"
    bl_label = "Section"
    bl_description = "Create Section"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    def create(self, context):

        # Create an empty curve datablock
        c = bpy.data.curves.new("Section", type='CURVE')
        c.dimensions = '2D'
        o = bpy.data.objects.new("Section", c)

        # Add your properties on curve datablock
        d = c.archipack_section.add()

        # Link object into scene
        self.link_object_to_scene(context, o, layer_name="2d")
        # select and make active
        self.select_object(context, o, True)
        # add a material
        self.add_material(context, o)

        # Load preset into datablock
        self.load_preset(d)


        for c in o.children:
            self.link_materials(context, o, c)
        return o

    def execute(self, context):
        if context.mode == "OBJECT":
            act = context.active_object
            bpy.ops.object.select_all(action="DESELECT")
            o = self.create(context)
            if act:
                o.parent = act
                o.matrix_world = act.matrix_world.copy()
            else:
                o.location = self.get_cursor_location(context)

            # select and make active
            self.select_object(context, o, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_section_camera(ArchipackCreateTool, Operator):
    bl_idname = "archipack.section_camera"
    bl_label = "Section"
    bl_description = "Create Section camera"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    def create(self, context):

        d = bpy.data.cameras.new("Camera Section")
        o = bpy.data.objects.new("Camera Section", d)

        d.archipack_section_camera.add()

        # Link object into scene
        self.link_object_to_scene(context, o)
        
        # select and make active
        self.select_object(context, o, True)
        return o

    def execute(self, context):
        if context.mode == "OBJECT":
            o = self.create(context)
            o.location = self.get_cursor_location(context)
            # select and make active
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_section_update(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.section_update"
    bl_label = "Update"
    bl_description = "Create / update Section"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        if context.mode == "OBJECT":
            act = context.active_object
            src = act
            # wich object was previously selected
            is_target = False
            select_new = False
            if archipack_section_target.filter(act):
                d = archipack_section_target.datablock(act)
                is_target = True
                o = self.get_scene_object(context, d.source_name)
                if o:
                    src = o
                    # select and make active
                    self.select_object(context, src, True)
                    
            if archipack_section.filter(src):
                d = archipack_section.datablock(src)

            elif archipack_section_camera.filter(src):
                select_new = is_target
                d = archipack_section_camera.datablock(src)

            if d:
                self.select_object(context, src, True)
                dst = d.update_section(context)

            self.unselect_object(context, src)
            if dst and select_new:
                # select and make active
                self.select_object(context, dst, True)
        
            else:
                if dst:
                    self.unselect_object(context, dst)
                # select and make active
                self.select_object(context, act, True)
                

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_section_select(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.section_select"
    bl_label = "Select section"
    bl_description = "Select section"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            obj_name = ""
            if archipack_section.filter(o):
                d = archipack_section.datablock(o)
                obj_name = d.section_name

            elif archipack_section_camera.filter(o):
                d = archipack_section_camera.datablock(o)
                obj_name = d.section_name

            elif archipack_section_target.filter(o):
                d = archipack_section_target.datablock(o)
                obj_name = d.source_name

            o = self.get_scene_object(context, obj_name)
            if o:
                bpy.ops.object.select_all(action='DESELECT')
                
                # select and make active
                self.select_object(context, o, True)
        
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_cross_section(ArchipackObjectsManager, Operator):
    """
      Create a cross section using a plane as bisector
      Plane size on x axis define limits for section on the sides
    """
    bl_idname = "archipack.cross_section"
    bl_label = "Cross section"
    bl_description = "Create a cross section using a plane as bisector"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    mode : EnumProperty(
        name="Mode",
        description="Output type",
        items=(
            ('MESH', 'Mesh', 'Output cross section as mesh'),
            ('CURVE', 'Curve', 'Output cross section as curve')
            ),
        default='CURVE'
        )
    clip : BoolProperty(
        name="clip",
        description="Clip sides at plane boundary",
        default=False
        )

    @classmethod
    def poll(cls, context):
        return context.active_object is not None

    def create(self, context, o):
        src_name = o.name
        loc = o.matrix_world.translation
        if self.clip:
            clip_x = 0.5 * o.dimensions.x
            clip_y = 0.5 * o.dimensions.y
        else:
            clip_x, clip_y = 0, 0

        sel = []
        manager = ArchipackSectionManager()
        tM = manager.get_objects(context, o, sel)
        s = manager.generate_section(context, sel, tM, clip_x, clip_y)

        if self.mode == 'CURVE':
            itM = tM.inverted()
            c = manager.as_curves(context, s, itM, loc, src_name, "")
            if c is None:
                self.delete_object(context, s)
            else:
                c.matrix_world = tM
            return c
        else:
            return s

    def execute(self, context):
        if context.mode == "OBJECT":
            act = context.active_object
            bpy.ops.object.select_all(action="DESELECT")
            o = self.create(context, act)
            # select and make active
            self.select_object(context, o, True)            
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_section_preset_menu(PresetMenuOperator, Operator):
    bl_idname = "archipack.section_preset_menu"
    bl_label = "Section preset"
    preset_subdir = "archipack_section"


class ARCHIPACK_OT_section_preset(ArchipackPreset, Operator):
    bl_description = "Add / removea Section Preset"
    bl_idname = "archipack.section_preset"
    bl_label = "Section preset"
    preset_menu = "ARCHIPACK_OT_section_preset_menu"

    @property
    def blacklist(self):
        return ['manipulators']


def register():
    bpy.utils.register_class(archipack_section)
    Curve.archipack_section = CollectionProperty(type=archipack_section)
    bpy.utils.register_class(archipack_section_target)
    Object.archipack_section_target = CollectionProperty(type=archipack_section_target)
    bpy.utils.register_class(archipack_section_camera)
    Camera.archipack_section_camera = CollectionProperty(type=archipack_section_camera)
    bpy.utils.register_class(ARCHIPACK_PT_section)
    bpy.utils.register_class(ARCHIPACK_PT_section_camera)
    bpy.utils.register_class(ARCHIPACK_PT_section_target)
    bpy.utils.register_class(ARCHIPACK_OT_section)
    bpy.utils.register_class(ARCHIPACK_OT_cross_section)
    bpy.utils.register_class(ARCHIPACK_OT_section_camera)
    bpy.utils.register_class(ARCHIPACK_OT_section_select)
    bpy.utils.register_class(ARCHIPACK_OT_section_update)
    bpy.utils.register_class(ARCHIPACK_OT_section_preset_menu)
    bpy.utils.register_class(ARCHIPACK_OT_section_preset)


def unregister():
    bpy.utils.unregister_class(archipack_section)
    bpy.utils.unregister_class(archipack_section_target)
    bpy.utils.unregister_class(archipack_section_camera)
    del Curve.archipack_section
    del Camera.archipack_section_camera
    del Object.archipack_section_target
    bpy.utils.unregister_class(ARCHIPACK_PT_section)
    bpy.utils.unregister_class(ARCHIPACK_PT_section_camera)
    bpy.utils.unregister_class(ARCHIPACK_PT_section_target)
    bpy.utils.unregister_class(ARCHIPACK_OT_section)
    bpy.utils.unregister_class(ARCHIPACK_OT_cross_section)
    bpy.utils.unregister_class(ARCHIPACK_OT_section_camera)
    bpy.utils.unregister_class(ARCHIPACK_OT_section_select)
    bpy.utils.unregister_class(ARCHIPACK_OT_section_update)
    bpy.utils.unregister_class(ARCHIPACK_OT_section_preset_menu)
    bpy.utils.unregister_class(ARCHIPACK_OT_section_preset)
