# -*- coding:utf-8 -*-

# #
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
# 
# <pep8 compliant>

# ----------------------------------------------------------
# Author: Stephen Leger (s-leger)
#
# ----------------------------------------------------------

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
import time
# noinspection PyUnresolvedReferences
import bpy
# noinspection PyUnresolvedReferences
from bpy.types import Operator, PropertyGroup, Mesh, Panel
from bpy.props import (
    FloatProperty, BoolProperty, IntProperty, IntVectorProperty,
    StringProperty, EnumProperty,
    CollectionProperty
)
import bmesh
from .bmesh_utils import BmeshEdit as bmed
from mathutils import Vector, Matrix
from math import pi
from .archipack_manipulator import Manipulable, archipack_manipulator
from .archipack_abstraction import ensure_select_and_restore
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackPanel,
    ArchipackCreateTool,
    ArchipackObject,
    stop_auto_manipulate
)
from .archipack_cutter import (
    CutAblePolygon, CutAbleGenerator,
    ArchipackCutter,
    ArchipackCutterPart,
    update_operation
)
from .archipack_dimension import DimensionProvider
from .archipack_curveman import ArchipackUserDefinedPath
from .archipack_segments2 import ArchipackSegment
from .archipack_throttle import throttle
from .archipack_gl import GlText

import logging

logger = logging.getLogger("archipack")


class AreaGenerator(CutAblePolygon, CutAbleGenerator):

    def __init__(self, o=None):
        CutAbleGenerator.__init__(self, o)

    def get_verts(self, verts):
        verts.extend([s.p0.to_3d() for s in self.segs])

    def cut(self, o, realtime):
        """
            either external or holes cuts
        """
        # use offset segs (self.segs = self.line.segs) as base
        self.as_lines(step_angle=0.0502)

        self.limits()
        if not realtime:
            itM = o.matrix_world.inverted()
            for b in o.children:
                d = archipack_area_cutter.datablock(b)
                if d is not None:
                    tM = itM @ b.matrix_world
                    g = d.ensure_direction(tM)
                    self.slice(g)

    def buildmesh(self, o, d):

        verts = []
        self.get_verts(verts)
        if len(verts) > 2:

            # ensure verts are CCW
            if self.is_cw:
                verts = list(reversed(verts))

            bm = bmesh.new()

            for v in verts:
                bm.verts.new(v)

            bm.verts.ensure_lookup_table()

            for i in range(1, len(verts)):
                bm.edges.new((bm.verts[i - 1], bm.verts[i]))

            bm.edges.new((bm.verts[-1], bm.verts[0]))

            for hole in self.holes:
                i0 = len(bm.verts)
                for seg in hole.segs:
                    bm.verts.new(seg.p0.to_3d())
                bm.verts.ensure_lookup_table()

                for i in range(i0 + 1, len(bm.verts)):
                    bm.edges.new((bm.verts[i - 1], bm.verts[i]))
                bm.edges.new((bm.verts[-1], bm.verts[i0]))

            bm.edges.ensure_lookup_table()

            bmed._end(bm, o)


def update(self, context):
    self.update(context)


def update_manipulators(self, context):
    self.manipulable_refresh = True
    self.update(context)


def update_path(self, context):
    self.update_path(context)


class archipack_area_part(Archipacki18n, ArchipackSegment, PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_area[0]


class archipack_area(Archipacki18n, ArchipackUserDefinedPath, ArchipackObject, Manipulable, DimensionProvider,
                     PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        name='Ui tabs',
        description="Display settings",
        items=(
            ('MAIN', 'Main', 'Display main settings', 'NONE', 0),
            ('PARTS', 'Shape', 'Display area segments settings', 'NONE', 1)
        ),
        default='MAIN',
    )

    # boundary
    parts: CollectionProperty(type=archipack_area_part)

    # Flag to prevent mesh update while making bulk changes over variables
    auto_update: BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update_manipulators
    )
    height: FloatProperty(
        name="Height",
        description="Volume Height",
        default=0.1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    text_size: FloatProperty(
        name="Text size",
        default=0.1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    text_angle: FloatProperty(
        name="Text Angle",
        default=0.0,
        subtype='ANGLE', unit='ROTATION',
        update=update
    )
    text_offset_x: FloatProperty(
        name="Text Offset x",
        default=0.0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    text_offset_y: FloatProperty(
        name="Text Offset y",
        default=0.0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )

    unit_mode: EnumProperty(
        name="Unit",
        items=(
            ('AUTO', 'Scene', 'Use scene units'),
            ('ADAPTIVE', 'Auto', 'Adaptive'),
            ('METERS', 'Meters', 'Meters'),
            ('CENTIMETERS', 'Centimeters', 'Centimeters'),
            ('MILLIMETERS', 'Millimeters', 'Millimeters'),
            ('FEET', 'Feet', 'Feet'),
            ('INCHES', 'Inch', 'Inch'),
            ('NONE', 'None', 'No unit')
        ),
        default="AUTO",
        update=update
    )
    precision: IntProperty(
        name="Precision",
        min=0,
        default=2,
        update=update
    )
    type: EnumProperty(
        name="Type",
        items=(
            ("AREA", "Area", "Area", 0),
            ("VOLUME", "Volume", "Volume", 1),
            ("BOTH", "Area and Volume", "Area and Volume", 2)
        ),
        default="AREA",
        update=update
    )
    always_closed = True

    def text(self, context, value, typ):

        dimension = 1
        val = value

        if typ == 'AREA':
            dimension = 2

        elif typ == 'VOLUME':
            dimension = 3
            val = value * self.height

        unit_mode = self.unit_mode
        unit_type = 'SIZE'

        label = GlText(
            label="",
            value=val,
            precision=self.precision,
            unit_mode=unit_mode,
            unit_type=unit_type,
            dimension=dimension
            )
        return label.add_units(context)

    def update_child(self, context, o, child, center, area):

        typ = self.type

        if typ == "BOTH":
            body = "{}\n{}".format(
                            self.text(context, area, "AREA"),
                            self.text(context, area, "VOLUME")
                            )
        else:
            body = self.text(context, area, typ)

        if child is not None:
            t_o = child
            text = t_o.data
        else:
            text = bpy.data.curves.new("{}_label".format(o.name), type='FONT')
            text.dimensions = '2D'
            t_o = bpy.data.objects.new("{}_label".format(o.name), text)
            # Link object into scene
            self.link_object_to_scene(context, t_o, layer_name="2d")
            self.link_materials(context, o, t_o)
            t_o.color = (0, 0, 0, 1)
            t_o.parent = o

        t_o.location = center + Vector((self.text_offset_x, self.text_offset_y, 0))
        t_o.rotation_euler.z = self.text_angle

        text.body = body
        text.size = self.text_size
        text.align_y = 'CENTER'
        text.align_x = 'CENTER'
        return t_o

    def get_generator(self, o=None):
        g = AreaGenerator(o)
        g.add_parts(self)
        g.line = g.make_offset(0)
        return g

    def setup_manipulators(self):
        self.setup_parts_manipulators('height')

    def from_spline(self, context, o, curve, ccw=False, cw=False):
        ArchipackUserDefinedPath.from_spline(self, context, o, curve, ccw=True)

    def update(self, context, manipulable_refresh=False, update_childs=False):

        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        tim = time.time()
        # clean up manipulators before any data model change
        if self.manipulable_refresh:
            self.manipulable_disable(o)

        if self.num_parts < 3:
            self.restore_context(context)
            return

        # changed = self.update_parts()
        g = self.get_generator()
        g.locate_manipulators(self)

        self.select_object(context, o, True)

        # cut transfer .line with offset into g.segs

        # throttle.add(context, o, self)
        realtime = throttle.is_active(o.name)

        g.cut(o, realtime)
        g.buildmesh(o, self)
        center, area = g.center, g.area

        for hole in g.holes:
            area -= hole.area

        child = None
        for c in o.children:
            if c.type == 'FONT':
                child = c
                break

        self.update_child(context, o, child, center, area)

        logger.debug("Area.update():%.4f seconds", time.time() - tim)
        # restore context
        self.restore_context(context)

    def manipulable_setup(self, context, o):
        """
            NOTE:
            this one assume context.active_object is the instance this
            data belongs to, failing to do so will result in wrong
            manipulators set on active object
        """
        # generator does update manipulators location
        self.get_generator()
        # setup childs manipulators
        self.setup_manipulators()
        self.manipulable_setup_parts(context, o)

    def as_2d(self, context, o):
        curves = []
        g = self.get_generator(o)
        curve = g.line.as_curve(name=o.name)
        self.link_object_to_scene(context, curve, layer_name="2d")
        curves.append(curve)
        for c in o.children:
            d = archipack_area_cutter.datablock(c)
            if d is not None:
                g = d.get_generator(c)
                curve = g.line.as_curve(name=c.name)
                self.link_object_to_scene(context, curve, layer_name="2d")
                curves.append(curve)
        return curves


def update_hole(self, context):
    # update parent only when manipulated
    self.update(context, update_parent=True)


class archipack_area_cutter_segment(ArchipackCutterPart, PropertyGroup):
    manipulators: CollectionProperty(type=archipack_manipulator)

    @property
    def parent_data(self):
        return self.id_data.archipack_area_cutter[0]


class archipack_area_cutter(ArchipackCutter, ArchipackObject, Manipulable, DimensionProvider, PropertyGroup):
    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('MAIN', 'Main', 'Display cutter settings', 'NONE', 0),
            ('PARTS', 'Shape', 'Display cutter segments settings', 'NONE', 1)
        ),
        default='MAIN',
    )
    parts: CollectionProperty(type=archipack_area_cutter_segment)

    def update_parent(self, context, o):
        if o is not None:

            cutables = []
            area = o.parent

            d = archipack_area.datablock(area)
            if d is not None:
                cutables.append(area)

            # filter cutables in bounding box, and add old ones so we update cutables when cutter goes out
            self.filter_cutables(context, o, cutables)
            store = []
            for c in cutables:
                d = archipack_area.datablock(c)
                if d is not None:
                    store.append(c)
                    with ensure_select_and_restore(context, c, [c]):
                        d.update(context)

            # store cutables found in bound box (realy cut)
            self.store_cutables(o, store)

    def get_generator(self, o=None):
        g = ArchipackCutter.get_generator(self, o)
        # print("archipack_area_cutter.get_generator()", [_k.idx for _k in g.line.segs])

        return g

    def draw(self, context, layout, draw_offset=False, draw_type=False):

        self.draw_prop(context, layout, layout, self, "tabs", expand=True)

        box = layout.box()
        if self.tabs == 'MAIN':
            self.draw_prop(context, layout, box, self, 'operation', text="")
            if draw_offset:
                self.draw_prop(context, layout, box, self, 'offset')

        elif self.tabs == 'PARTS':
            self.template_user_path(context, box, focus=False)
            self.template_parts(context, layout, draw_type=draw_type)


class ARCHIPACK_PT_area(ArchipackPanel, Archipacki18n, Panel):
    bl_description = "Archipack Area / Volume"
    bl_idname = "ARCHIPACK_PT_area"
    bl_label = "Area / Volume"

    @classmethod
    def poll(cls, context):
        return archipack_area.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_area.datablock(o)
        if d is None:
            return
        layout = self.layout
        # row = layout.row(align=True)
        self.draw_common(context, layout)
        # self.draw_op(context, layout, "archipack.area", text="Delete", icon='ERROR').mode = 'DELETE'
        self.draw_op(context, layout, layout, 'archipack.area_cutter', icon="MOD_BOOLEAN").parent = o.name

        self.draw_prop(context, layout, layout, d, "tabs", expand=True)
        box = layout.box()
        if d.tabs == 'MAIN':
            self.draw_prop(context, layout, box, d, 'type')
            self.draw_prop(context, layout, box, d, 'unit_mode')
            self.draw_prop(context, layout, box, d, 'precision')
            if d.type != 'AREA':
                self.draw_prop(context, layout, box, d, 'height')

            box=layout.box()
            self.draw_prop(context, layout, box, d, 'text_size')
            self.draw_prop(context, layout, box, d, 'text_offset_x')
            self.draw_prop(context, layout, box, d, 'text_offset_y')
            self.draw_prop(context, layout, box, d, 'text_angle')

        elif d.tabs == 'PARTS':
            d.template_user_path(context, box, focus=False)
            d.template_parts(context, layout, draw_type=True)


class ARCHIPACK_PT_area_cutter(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_area_cutter"
    bl_label = "Area Cutter"

    @classmethod
    def poll(cls, context):
        return archipack_area_cutter.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        d = archipack_area_cutter.datablock(o)
        if d is None:
            return
        layout = self.layout

        self.draw_common(context, layout)

        d.draw(context, layout, draw_offset=True)


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


# ------------------------------------------------------------------
# Define operator class to create object
# ------------------------------------------------------------------


class ARCHIPACK_OT_area(ArchipackCreateTool, Operator):
    bl_idname = "archipack.area"
    bl_label = "Area / Volume"
    bl_description = "Create Area / Volume"

    def create(self, context):
        m = bpy.data.meshes.new("Area")
        o = bpy.data.objects.new("Area", m)
        d = m.archipack_area.add()
        angle_90 = pi / 2
        x = 4
        # make manipulators selectable
        d.manipulable_selectable = True
        d.set_parts(4)
        for i, p in enumerate(d.parts):
            p.a0 = angle_90
            p.length = x
        d.parts[0].a0 = - angle_90
        # Link object into scene
        self.link_object_to_scene(context, o, layer_name="2d")
        o.color = (1, 0, 0, 1)

        # select and make active
        self.select_object(context, o, True)
        self.add_material(context, o, material="DEFAULT", category="dimension_auto")
        self.load_preset(d)
        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            with stop_auto_manipulate(context):
                o = self.create(context)
                o.location = self.get_cursor_location(context)

            self.add_to_reference(context, o)

            # select and make active
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_area_from_curve(Operator):
    bl_idname = "archipack.area_from_curve"
    bl_label = "Area curve"
    bl_description = "Create a area from a curve"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    @classmethod
    def poll(self, context):
        return context.active_object is not None and context.active_object.type == 'CURVE'

    def create(self, context):
        curve = context.active_object
        bpy.ops.archipack.area()
        o = context.active_object
        d = archipack_area.datablock(o)
        d.from_spline(context, o, curve)
        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            self.create(context)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_area_from_wall(ArchipackCreateTool, Operator):
    bl_idname = "archipack.area_from_wall"
    bl_label = "Area / Volume"
    bl_description = "Create Area / Volume from a wall"

    @classmethod
    def poll(self, context):
        o = context.active_object
        return o is not None and o.data is not None and 'archipack_wall2' in o.data

    def create(self, context):
        m = bpy.data.meshes.new("Area")
        o = bpy.data.objects.new("Area", m)
        d = m.archipack_area.add()
        d.manipulable_selectable = True
        d.auto_update = False
        self.link_object_to_scene(context, o, layer_name="2d")
        o.color = (1, 0.25, 0, 1)

        self.select_object(context, o, True)

        self.add_material(context, o, material="DEFAULT", category="dimension_auto")
        self.load_preset(d)
        d.auto_update = False
        return o

    def area_from_wall(self, context, w, wd):
        """
         Create area from surrounding wall
         Use slab cutters, windows and doors, T childs walls
        """
        tim = time.time()
        # wall is either a single or collection of polygons
        try:
            io, wall, childs = wd.as_geom(context, w, 'AREA', [], [], [])

        except RecursionError as ex:
            self.report({"ERROR"}, "RecursionError while building geometry: %s" % ex)
            import traceback
            traceback.print_exc()
            return None

        except Exception as ex:
            self.report({"ERROR"}, "Error while building geometry: %s" % ex)
            import traceback
            traceback.print_exc()
            return None

        o = None
        # Multipolygon
        if wall.type_id == 6:
            polys = wall.geoms
        else:
            polys = [wall]
        sel = []
        logger.debug("area_from_wall() curves :%.4f seconds", time.time() - tim)

        for poly in polys:

            boundary = io._to_curve(poly.exterior, "{}-boundary".format(w.name), '2D')
            # boundary.location.z -= wd.z_offset

            logger.debug("area_from_wall() boundary :%.4f seconds", time.time() - tim)

            o = self.create(context)
            sel.append(o)
            # o.matrix_world = w.matrix_world.copy()
            d = archipack_area.datablock(o)
            d.height = wd.z + wd.z_offset
            logger.debug("area_from_wall() create :%.4f seconds", time.time() - tim)
            d.user_defined_path = boundary.name
            logger.debug("area_from_wall() user_defined_path :%.4f seconds", time.time() - tim)
            self.delete_object(context, boundary)
            logger.debug("area_from_wall() delete_object :%.4f seconds", time.time() - tim)
            d.user_defined_path = ""
            logger.debug("area_from_wall() area :%.4f seconds", time.time() - tim)
            for hole in poly.interiors:
                curve = io._to_curve(hole, "{}-cut".format(o.name), '3D')
                bpy.ops.archipack.area_cutter(parent=o.name, curve=curve.name)
                c = context.active_object
                cd = archipack_area_cutter.datablock(c)
                cd.user_defined_path = ""
                self.delete_object(context, curve)
                self.unselect_object(context, c)

            logger.debug("area_from_wall() cutters :%.4f seconds", time.time() - tim)

            # link to reference point here to retrieve slabs holes too when updating
            self.select_object(context, o, True)
            self.select_object(context, w, True)
            bpy.ops.archipack.add_reference_point()
            self.unselect_object(context, w)

            # select and make active
            self.select_object(context, o, True)
            d.auto_update = True
            self.unselect_object(context, o)
            logger.debug("area_from_wall() %s :%.4f seconds", o.name, time.time() - tim)

        for obj in sel:
            self.select_object(context, obj)

        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            wall = context.active_object
            bpy.ops.object.select_all(action="DESELECT")
            wd = wall.data.archipack_wall2[0]
            bpy.ops.archipack.disable_manipulate()
            with stop_auto_manipulate(context):
                o = self.area_from_wall(context, wall, wd)
            self.select_object(context, wall, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_area_cutter(ArchipackCreateTool, Operator):
    bl_idname = "archipack.area_cutter"
    bl_label = "Area Cutter"
    bl_description = "Area Cutter"

    parent: StringProperty("")
    curve: StringProperty("")

    def create(self, context):
        m = bpy.data.meshes.new("Area Cutter")
        o = bpy.data.objects.new("Area Cutter", m)
        d = m.archipack_area_cutter.add()
        parent = self.get_scene_object(context, self.parent)
        curve = self.get_scene_object(context, self.curve)

        if parent is not None:
            o.parent = parent
            bbox = parent.bound_box
            angle_90 = pi / 2
            x0, y0, z = bbox[0]
            x1, y1, z = bbox[6]
            x = max(1, 0.2 * (x1 - x0))
            o.matrix_world = parent.matrix_world @ Matrix.Translation(Vector((-3 * x, 0, 0)))
            # d.auto_update = False
            d.set_parts(4)
            for i, p in enumerate(d.parts):
                p.a0 = angle_90
                p.length = x
            d.parts[0].a0 = - angle_90

            # d.auto_update = True
        else:
            o.location = self.get_cursor_location(context)

        # make manipulators selectable
        d.manipulable_selectable = True
        self.link_object_to_scene(context, o, layer_name="2d")
        o.color = (1, 0, 0, 1)

        if parent is not None:
            self.link_materials(context, o.parent, o)
        self.select_object(context, o, True)
        self.load_preset(d)
        update_operation(d, context)

        if curve is not None:
            d.user_defined_path = curve.name

        return o

    # -----------------------------------------------------
    # Execute
    # -----------------------------------------------------
    def execute(self, context):
        if context.mode == "OBJECT":
            bpy.ops.object.select_all(action="DESELECT")
            with stop_auto_manipulate(context):
                o = self.create(context)
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


def register():
    bpy.utils.register_class(archipack_area_cutter_segment)
    bpy.utils.register_class(archipack_area_cutter)
    Mesh.archipack_area_cutter = CollectionProperty(type=archipack_area_cutter)
    bpy.utils.register_class(ARCHIPACK_OT_area_cutter)
    bpy.utils.register_class(ARCHIPACK_PT_area_cutter)
    bpy.utils.register_class(archipack_area_part)
    bpy.utils.register_class(archipack_area)
    Mesh.archipack_area = CollectionProperty(type=archipack_area)
    bpy.utils.register_class(ARCHIPACK_PT_area)
    bpy.utils.register_class(ARCHIPACK_OT_area)
    bpy.utils.register_class(ARCHIPACK_OT_area_from_curve)
    bpy.utils.register_class(ARCHIPACK_OT_area_from_wall)


def unregister():
    bpy.utils.unregister_class(archipack_area_part)
    bpy.utils.unregister_class(archipack_area)
    del Mesh.archipack_area
    bpy.utils.unregister_class(ARCHIPACK_PT_area)
    bpy.utils.unregister_class(ARCHIPACK_OT_area)
    bpy.utils.unregister_class(ARCHIPACK_OT_area_from_curve)
    del Mesh.archipack_area_cutter
    bpy.utils.unregister_class(archipack_area_cutter_segment)
    bpy.utils.unregister_class(archipack_area_cutter)
    bpy.utils.unregister_class(ARCHIPACK_OT_area_cutter)
    bpy.utils.unregister_class(ARCHIPACK_PT_area_cutter)
    bpy.utils.unregister_class(ARCHIPACK_OT_area_from_wall)
