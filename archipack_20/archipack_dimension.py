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
import json
from bpy.types import Operator, PropertyGroup, Curve, Panel
from bpy.props import (
    FloatProperty, EnumProperty, BoolProperty, IntProperty,
    CollectionProperty, FloatVectorProperty, StringProperty
    )
from mathutils import Vector, Matrix
from math import cos, sin, pi, atan2
from .archipack_manipulator import Manipulable, archipack_manipulator
from .archipack_preset import ArchipackPreset, PresetMenuOperator
from .archipack_i18n import Archipacki18n
from .archipack_object import (
    ArchipackPanel,
    ArchipackCreateTool,
    ArchipackObject,
    ArchipackObjectsManager,
    stop_auto_manipulate
    )
from .archipack_gl import GlText


def update(self, context):
    self.update(context)


class archipack_dimension_point(PropertyGroup):
    """
     Store dimension provider points
     uid has to be unique for object
     location is in object coordsys
     used as sub group for DimensionProvider
    """
    index: IntProperty(
        name="point index",
        description="Mesured point index",
        default=-1
    )
    location: FloatVectorProperty(
        name="Location",
        description="Location of mesured point in object space",
        subtype='XYZ'
    )


class DimensionProvider():
    """
      A class to add dimension provider ability to archipack objects
      objects must store a unique id as per object instance
      for mesured points so uid remains stable
    """
    dimension_points: CollectionProperty(
        type=archipack_dimension_point,
        description="store 3d points to provide measurable points"
    )
    dimension_uid: IntProperty(default=0)

    def create_uid(self, part, increment=1):
        """
         Generate an unique index number
         increment > 1 allow to create stable
         uid to store more than one measurable
         for each object parts
        """
        if part.uid < 0:
            part.uid = self.dimension_uid
            self.dimension_uid += increment

    def _add_dimension_point(self, uid, location):
        """
         Sub to realy add missing point if any
         uid: point unique id
         location: location in object coordsys
        """
        d = self.dimension_points.add()
        d.location = location
        d.index = uid
        self.dimension_uid += 1

    def remove_dimension_point(self, uid):
        """
         Remove point if found
         uid: point unique id
        """
        to_remove = -1
        for i, p in enumerate(self.dimension_points):
            if p.index == uid:
                to_remove = i
                break
        if to_remove > -1:
            self.dimension_points.remove(to_remove)

    def add_dimension_point(self, uid, location):
        """
         Add / update point location
         uid: point unique id
         location: location in object coordsys
        """
        for i, p in enumerate(self.dimension_points):
            if p.index == uid:
                p.location = location
                return
        self._add_dimension_point(uid, location)

    def dimension_point(self, o, uid):
        """
         o: this object
         uid: point unique id
         Return dimension point location in world coordsys or None
        """
        for i, p in enumerate(self.dimension_points):
            if p.index == uid:
                return o.matrix_world @ p.location
        return None

    def _update_dimensions(self, context, o):
        d = archipack_dimension_auto.datablock(o)
        if d:
            self.select_object(context, o, True)
            d.update(context)
            self.unselect_object(context, o)

    def _get_topmost_parent(self, o):
        if o.parent:
            return self._get_topmost_parent(o.parent)
        else:
            return o

    def _update_child_dimensions(self, context, o):
        self._update_dimensions(context, o)
        for c in o.children:
            self._update_child_dimensions(context, c)

    def update_dimensions(self, context, o):
        p = self._get_topmost_parent(o)
        self._update_child_dimensions(context, p)
        self.select_object(context, o, True)


class archipack_dimension_source(ArchipackObjectsManager, PropertyGroup):
    """
      Store dimension provider source path
      used as sub group for multi-dimension
      work with archipack objects, curves and meshes
      Use "weak pointer" to object by name
    """
    obj: StringProperty(
        name="Object name",
        description="Provider object name",
        default=""
    )
    index: IntProperty(
        name="index",
        description="Provider location index",
        default=0
    )
    loc: FloatVectorProperty(
        subtype="XYZ",
        update=update
    )
    provider_type: StringProperty(
        name="Provider type",
        default="USER"
    )
    manipulators: CollectionProperty(type=archipack_manipulator)
    auto_update: BoolProperty(
        default=True,
        options={'SKIP_SAVE'}
    )

    @property
    def parent_data(self):
        return self.id_data.archipack_dimension_auto[0]

    def update(self, context):
        if self.auto_update:
            self.parent_data.update(context)

    def location(self, context, itM):
        """
         return
         bool: valid state
         vector: measured point absolute world location
        """
        o = self.get_scene_object(context, self.obj)
        pos = None
        if o is not None:
            if self.provider_type == 'MESH':
                pts = o.data.vertices
                if self.index < len(pts):
                    pos = itM @ o.matrix_world @ pts[self.index].co
            elif self.provider_type == 'CURVE':
                d = o.data
                if d.type == 'POLY':
                    pts = d.points
                elif d.type == 'BEZIER':
                    pts = d.bezier_points
                if self.index < len(pts):
                    pos = itM @ o.matrix_world @ pts[self.index].co
            else:
                d = None
                try:
                    d = getattr(o.data, "archipack_{}".format(self.provider_type.lower()))[0]
                except:
                    pass
                if d is None:
                    try:
                        d = getattr(o, "archipack_{}".format(self.provider_type.lower()))[0]
                    except:
                        pass
                if d:
                    pt = d.dimension_point(o, self.index)
                if pt is not None:
                    pos = itM @ pt

        elif self.provider_type == 'USER':
            pos = Vector((self.loc.x, self.loc.y, 0))

        return pos is not None, pos


class archipack_dimension_auto(ArchipackObject, Manipulable, PropertyGroup):
    """ Archipack Automatic Dimension curve"""
    distance: FloatProperty(
        description="Size",
        name="Size",
        default=1.0,
        min=0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    flip_side: BoolProperty(
        name="Flip side",
        description="Flip line side",
        default=False,
        update=update
    )
    sum_bar: BoolProperty(
        name="Sum",
        description="Generate sum line",
        default=True,
        update=update
    )
    distance_sum: FloatProperty(
        description="Sum line distance",
        name="Sum Distance",
        default=0.5,
        min=0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    offset: FloatProperty(
        description="Symbol distance from measured points",
        name="Offset",
        default=0.0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    relative_offset: BoolProperty(
        name="Relative offset",
        description="Align to measured points",
        default=False,
        update=update
    )
    type: EnumProperty(
        name="Type",
        items=(
            ("SIZE", "Size", "Size"),
        ),
        default="SIZE",
        update=update
    )
    symbol_size: FloatProperty(
        name="Symbol Size",
        default=0.1,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    symbol_thickness: FloatProperty(
        name="Thickness",
        description="Arrow thickness factor",
        default=0.25, min=0.01,
        update=update
    )
    symbol_type: EnumProperty(
        name="Symbol shape",
        items=(
            ("ARROW", "Arrow", "Arrow"),
            ("TICK", "Architectural tick", "Architectural tick"),
            ("CROSS", "Cross", "Cross"),
            ("DOT", "Dot", "Dot")
        ),
        default="TICK",
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
    text_offset: FloatProperty(
        name="Text Offset",
        default=0.0,
        unit='LENGTH', subtype='DISTANCE',
        update=update
    )
    text_location: EnumProperty(
        items=(
            ("TOP_CENTER", "Top center", "Top center"),
            ("BOTTOM_CENTER", "Bottom center", "Bottom center")
        ),
        default="TOP_CENTER",
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
    sources: CollectionProperty(
        type=archipack_dimension_source
    )
    auto_update: BoolProperty(
        # Wont save auto_update state in any case
        options={'SKIP_SAVE'},
        default=True,
        update=update
    )

    parent_uid: IntProperty(
        description="Unique index of part in parent",
        default=0
    )

    # source selection manipulators
    source_selector: CollectionProperty(type=archipack_manipulator)

    def add_source(self, obj, index, provider_type):
        if provider_type == 'USER':
            s = self.sources.add()
            s.provider_type = provider_type
        else:
            name = obj.name
            if not any([s.index == index for s in self.sources if s.obj == name]):
                s = self.sources.add()
                s.obj = name
                s.index = index
                s.provider_type = provider_type

    def remove_source(self, index):
        self.sources.remove(index)

    def clear_sources(self):
        self.sources.clear()

    def text(self, context, value):

        dimension = 1

        if self.type == 'AREA':
            dimension = 2
        elif self.type == 'VOLUME':
            dimension = 3

        # Either 'ANGLE' or 'SIZE'
        unit_type = self.type
        unit_mode = self.unit_mode

        if self.type in {'AREA', 'VOLUME', 'ALTITUDE'}:
            unit_type = 'SIZE'

        if self.type == 'ANGLE':
            unit_mode = 'ADAPTIVE'

        label = GlText(
            label="",
            value=value,
            precision=self.precision,
            unit_mode=unit_mode,
            unit_type=unit_type,
            dimension=dimension
            )
        return label.add_units(context)

    def setup_manipulators(self):
        if len(self.manipulators) == 0:
            # add manipulator for x property
            s = self.manipulators.add()
            s.prop1_name = "distance"
            s.type_key = 'SIZE'

            s = self.manipulators.add()
            s.prop1_name = "offset"
            s.type_key = 'SIZE'

            s = self.manipulators.add()
            s.type_key = "OP_ADD"
            s.prop1_name = "archipack.dimension_auto_add"
            s.prop2_name = "{}"

        for i, source in enumerate(self.sources):
            if len(source.manipulators) < 1:
                s = source.manipulators.add()
                s.type_key = "OP_REM"
                s.prop1_name = "archipack.dimension_auto_remove"
                if source.provider_type == 'USER':
                    s = source.manipulators.add()
                    s.type_key = "SNAP_VEC"
                    s.prop1_name = "loc"
            source.manipulators[0].prop2_name = json.dumps({'index': i})

    def _add_selector(self, o, itM):
        d = self.archipack_datablock(o)
        if d is not None and hasattr(d, 'dimension_points'):
            for p in d.dimension_points:
                pos = itM @ o.matrix_world @ p.location
                m = self.source_selector.add()
                m.type_key = "OP_ADD"
                m.prop1_name = "archipack.dimension_auto_add"
                m.prop2_name = json.dumps({
                    'obj': o.name,
                    'index': p.index,
                    'provider_type': d.category.upper()
                })
                m.set_pts([pos, pos + Vector((1, 0, 0)), Vector((1, 0, 0))])

    def _add_child_selector(self, o, itM):
        self._add_selector(o, itM)
        for c in o.children:
            self._add_child_selector(c, itM)

    def update_source_selector(self, o):
        itM = o.matrix_world.inverted()
        p = self.get_topmost_parent(o)
        self.source_selector.clear()
        self._add_child_selector(p, itM)

    def manipulable_setup(self, context, o):

        # update manipulators on version change
        self.setup_manipulators()
        self.update_source_selector(o)

        for i, source in enumerate(self.sources):
            # call to remove operator
            self.manip_stack.append(source.manipulators[0].setup(context, o, source))
            if source.provider_type == 'USER':
                self.manip_stack.append(source.manipulators[1].setup(context, o, source))

        for m in self.manipulators:
            self.manip_stack.append(m.setup(context, o, self))

        for m in self.source_selector:
            self.manip_stack.append(m.setup(context, o, self))

    def manipulable_exit(self, o):
        """
            Override with action to do when modal exit
        """
        return

    def _add_spline(self, curve, closed, coords):
        spline = curve.splines.new('POLY')
        spline.use_endpoint_u = False
        spline.use_cyclic_u = closed
        spline.points.add(count=len(coords) - 1)
        for i, coord in enumerate(coords):
            x, y, z = coord
            spline.points[i].co = (x, y, z, 1)

    def text_frame(self, context, curve, p0, t_o):
        # need scene update to retrieve
        # current text dimension
        context.view_layer.update()
        d = t_o.dimensions
        a = -self.text_angle
        rM = Matrix([
            [cos(a), sin(a), 0, 0],
            [-sin(a), cos(a), 0, 0],
            [0, 0, 1, 0],
            [0, 0, 0, 1]
        ])
        p0 += rM @ Vector((0, 0.5 * d.y, 0))
        x = rM @ Vector((0.5 * (d.x + d.y), 0, 0))
        y = rM @ Vector((0, d.y, 0))
        self._add_spline(curve, True, [p0 - x - y, p0 + x - y, p0 + x + y, p0 - x + y])

    def update_child(self, context, o, childs, i, center, value):

        if i < len(childs):
            t_o = childs[i]
            text = t_o.data
        else:
            text = bpy.data.curves.new(o.name, type='FONT')
            text.dimensions = '2D'
            t_o = bpy.data.objects.new(o.name, text)
            # Link object into scene
            self.link_object_to_scene(context, t_o, layer_name="2d")
            t_o.parent = o

        self.link_materials(context, o, t_o)
        t_o.location = center
        t_o.rotation_euler.z = self.text_angle

        text.body = self.text(context, value)
        text.size = self.text_size
        text.align_y = 'BOTTOM'
        text.align_x = 'CENTER'
        return t_o

    def symbol(self, curve, p0, side=1, orientation='X'):
        """
         p0: location of tip
         side: side factor in [-1, 1]
        """
        symbol_type = self.symbol_type

        s = side * self.symbol_size

        if symbol_type == 'ARROW':

            if orientation == 'X':
                x = Vector((s, 0, 0))
                y = Vector((0, s * self.symbol_thickness, 0))
            else:
                x = Vector((0, s, 0))
                y = Vector((s * self.symbol_thickness, 0, 0))

            self._add_spline(curve, True, [p0 - x + y, p0 - x - y, p0])

        elif symbol_type == 'DOT':
            seg = 16
            da = 2 * pi / seg
            r = 0.5 * s
            self._add_spline(curve, True, [
                p0 + r * Vector((cos(da * i), sin(da * i), 0))
                for i in range(seg)
                ])

        elif symbol_type == 'TICK':
            r = s * 0.25 * (2 ** 0.5)
            x = Vector((r, r, 0))
            self._add_spline(curve, False, [p0 - x, p0 + x])

        elif symbol_type == 'CROSS':
            r = s * 0.25 * (2 ** 0.5)
            x = Vector((r, r, 0))
            x2 = Vector((r, -r, 0))
            self._add_spline(curve, False, [p0 - x, p0 + x])
            self._add_spline(curve, False, [p0 - x2, p0 + x2])

    def update(self, context, remove_object=None):

        # provide support for "copy to selected"
        o = self.find_in_selection(context, self.auto_update)

        if o is None:
            return

        # dynamically create manipulators when needed
        self.setup_manipulators()

        curve = o.data
        curve.splines.clear()

        s = self.symbol_size
        dist = self.distance
        if self.flip_side:
            side = -1
        else:
            side = 1

        if self.text_location == 'TOP_CENTER':
            text_location = 0.5 * self.text_size + self.text_offset
        else:
            text_location = -1.2 * self.text_size - self.text_offset

        p0 = Vector((0, 0, 0))

        if self.type == 'SIZE':
            itM = o.matrix_world.inverted()
            pts = []
            to_remove = []
            for i, source in enumerate(self.sources):
                valid, pos = source.location(context, itM)
                if not valid or source.obj.strip() == remove_object:
                    to_remove.append(i)
                else:
                    pts.append(pos)
                    # remove manipulator
                    loc = Vector((pos.x, side * (self.offset + self.distance), 0))
                    source.manipulators[0].set_pts([loc, loc + Vector((1, 0, 0)), (-1, 0, 0)])
                    if source.provider_type == 'USER':
                        x, y, z = source.loc
                        loc = Vector((x, y, 0))
                        source.manipulators[1].set_pts([loc, loc + Vector((1, 0, 0)), (-1, 0, 0)])

            # remove invalid source
            for i in reversed(to_remove):
                self.remove_source(i)

            # sort and filter too close points
            pts.sort(key=lambda p: p.x)
            pts = [p for i, p in enumerate(pts) if abs(pts[i - 1].x - p.x) > 0.0001]
            n_pts = len(pts)

            childs = [child for child in o.children if child.type == 'FONT']
            n_childs = len(childs)

            has_sum_bar = self.sum_bar and n_pts > 2

            # remove text
            if has_sum_bar:
                n_text = n_pts
            else:
                n_text = n_pts - 1

            for i in range(max(0, n_text), n_childs):
                c = childs.pop()
                self.delete_object(context, c)

            # size
            self.manipulators[0].set_pts([
                Vector((0, side * self.offset, 0)),
                Vector((0, side * (self.offset + self.distance), 0)),
                (-1, 0, 0)])

            # offset
            self.manipulators[1].set_pts([
                Vector((0, 0, 0)),
                Vector((0, side * self.offset, 0)),
                (-1, 0, 0)])

            # add user source
            self.manipulators[2].set_pts([
                Vector((0, side * (self.offset + 0.5 * self.distance), 0)),
                Vector((0, side * (self.offset + self.distance), 0)),
                (-2, 0, 0)])

            if n_pts > 0:
                s = self.symbol_size
                x = Vector((s, 0, 0))
                y = Vector((0, s, 0))
                if has_sum_bar:
                    # sum bar
                    dy = side * (self.offset + self.distance_sum + self.distance)
                    if self.symbol_type == 'ARROW':
                        p0 = Vector((pts[0].x + s, dy, 0))
                        p1 = Vector((pts[-1].x - s, dy, 0))
                    else:
                        p0 = Vector((pts[0].x - s, dy, 0))
                        p1 = Vector((pts[-1].x + s, dy, 0))
                    self._add_spline(curve, False, [p0, p1])

                    p0 = Vector((pts[0].x, dy, 0))
                    p1 = Vector((pts[-1].x, dy, 0))
                    self.symbol(curve, p0, -1)
                    self.symbol(curve, p1, 1)
                    center = 0.5 * (p0 + p1) + Vector((0, text_location, 0))
                    t_o = self.update_child(context, o, childs, n_text - 1, center, pts[-1].x - pts[0].x)

                # regular bar
                if self.symbol_type != 'ARROW':
                    p0 = Vector((pts[0].x - s, side * (self.offset + self.distance), 0))
                    p1 = Vector((pts[-1].x + s, side * (self.offset + self.distance), 0))
                    self._add_spline(curve, False, [p0, p1])

                n_pts -= 1
                dist = 0

                for i, p in enumerate(pts):
                    if self.relative_offset:
                        p0 = Vector((p.x, p.y + side * self.offset, 0))
                    else:
                        p0 = Vector((p.x, side * self.offset, 0))
                    # sum bar
                    if self.sum_bar and (i == 0 or i == n_pts):
                        dy = side * (self.offset + self.distance_sum + self.distance + s)
                    else:
                        dy = side * (self.offset + self.distance + s)

                    p1 = Vector((p.x, dy, 0))

                    # side
                    self._add_spline(curve, False, [p0, p1])

                    p1 = Vector((p.x, side * (self.offset + self.distance), 0))
                    if 'ARROW' in self.symbol_type:
                        if i > 0:
                            self.symbol(curve, p1, 1)
                        if i < n_pts:
                            self.symbol(curve, p1, -1)
                    else:
                        self.symbol(curve, p1, 1)

                    if i > 0:
                        p2 = Vector((dist - p.x, 0, 0))
                        # bar
                        if self.symbol_type == 'ARROW':
                            self._add_spline(curve, False, [p1 - x, p1 + p2 + x])

                        center = p1 + 0.5 * p2 + Vector((0, text_location, 0))
                        t_o = self.update_child(context, o, childs, i - 1, center, p.x - dist)
                    dist = p.x

            else:
                tM = Matrix()
                verts = [[tM @ Vector(p) for p in d] for d in [
                    [(0.226, 0.516, 0.0), (0.439, 0.516, 0.0)],
                    [(0.226, -0.122, 0.0), (0.439, -0.122, 0.0)],
                    [(0.387, 0.388, 0.0), (0.387, 0.006, 0.0)],
                    [(0.146, -0.191, 0.0), (0.146, -0.422, 0.0)],
                    [(-0.496, -0.191, 0.0), (-0.496, -0.422, 0.0)],
                    [(0.017, -0.352, 0.0), (-0.368, -0.352, 0.0)],
                    [(0.419, 0.388, 0.0), (0.355, 0.388, 0.0), (0.387, 0.516, 0.0), (0.419, 0.388, 0.0)],
                    [(0.355, 0.006, 0.0), (0.419, 0.006, 0.0), (0.387, -0.122, 0.0), (0.355, 0.006, 0.0)],
                    [(0.017, -0.384, 0.0), (0.017, -0.32, 0.0), (0.146, -0.352, 0.0), (0.017, -0.384, 0.0)],
                    [(-0.368, -0.32, 0.0), (-0.368, -0.384, 0.0), (-0.496, -0.352, 0.0), (-0.368, -0.32, 0.0)],
                    [(-0.496, -0.122, 0.0), (-0.096, -0.122, 0.0), (-0.096, 0.139, 0.0),
                    (0.063, 0.139, 0.0), (0.063, -0.122, 0.0), (0.146, -0.122, 0.0),
                    (0.146, 0.266, 0.0), (0.21, 0.266, 0.0), (-0.175, 0.516, 0.0),
                    (-0.561, 0.266, 0.0), (-0.496, 0.266, 0.0), (-0.496, -0.122, 0.0)],
                    [(-0.337, 0.015, 0.0), (-0.177, 0.015, 0.0), (-0.177, 0.139, 0.0),
                    (-0.337, 0.139, 0.0), (-0.337, 0.015, 0.0)]
                    ]]
                for coords in verts:
                    self._add_spline(curve, coords[0] == coords[-1], coords)

        elif self.type == 'ALTITUDE':
            p1 = Vector((0, self.size, 0))
            p2 = Vector((dist, 0, 0))
            p3 = Vector((dist, self.size, 0))
            s = self.symbol_size
            y = Vector((side * s, 0, 0))
            # sides
            self._add_spline(curve, False, [p1, p3 + y])

            # arrows
            self.symbol(curve, p3, -1, 'Y')
            center = p3 + 0.5 * Vector((0, self.text_size, 0))
            t_o = self.update_child(context, o, center, self.size + self.origin)
            self.manipulators[0].set_pts([p0, p1, (dist, 0, 0)], normal=Vector((0, 0, 1)))

        elif self.type == 'ANGLE':
            t_o = self.update_child(context, o, p0, self.angle)

        elif self.type == 'AREA':
            t_o = self.update_child(context, o, p0, self.area)
            self.text_frame(context, curve, p0, t_o)

        elif self.type == 'VOLUME':
            t_o = self.update_child(context, o, p0, self.volume)
            self.text_frame(context, curve, p0, t_o)

        # always restore context
        self.restore_context(context)


class ARCHIPACK_PT_dimension_auto(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_dimension_auto"
    bl_label = "Multi Dimension"

    @classmethod
    def poll(cls, context):
        return archipack_dimension_auto.poll(context.active_object)

    def draw(self, context):
        o = context.active_object
        if not archipack_dimension_auto.filter(o):
            return
        layout = self.layout

        # retrieve datablock of your object
        d = archipack_dimension_auto.datablock(o)

        # Manipulate mode operator
        self.draw_common(context, layout)

        row = layout.row(align=True)
        self.draw_op(context, layout, row, 'archipack.dimension_auto_orient', text="Orient")
        self.draw_op(context, layout, row, 'archipack.dimension_auto_update', text="Update")

        box = layout.box()
        row = box.row(align=True)

        # Presets operators
        self.draw_op(context, layout, row, "archipack.dimension_auto_preset_menu",
                     text=bpy.types.ARCHIPACK_OT_dimension_auto_preset_menu.bl_label, icon="PRESET")
        self.draw_op(context, layout, row, "archipack.dimension_auto_preset", icon='ADD', text="")
        self.draw_op(context, layout, row, "archipack.dimension_auto_preset", icon='REMOVE', text="").remove_active = True

        box = layout.box()
        self.draw_prop(context, layout, box, d, 'type')
        self.draw_prop(context, layout, box, d, 'unit_mode')
        self.draw_prop(context, layout, box, d, 'precision')
        self.draw_prop(context, layout, box, d, 'distance')
        self.draw_prop(context, layout, box, d, 'flip_side')
        self.draw_prop(context, layout, box, d, 'sum_bar')
        if d.sum_bar:
            self.draw_prop(context, layout, box, d, 'distance_sum')
        self.draw_prop(context, layout, box, d, 'offset')
        self.draw_prop(context, layout, box, d, 'relative_offset')
        box = layout.box()
        self.draw_prop(context, layout, box, d, 'symbol_type')
        self.draw_prop(context, layout, box, d, 'symbol_size')
        if "ARROW" in d.symbol_type:
            self.draw_prop(context, layout, box, d, 'symbol_thickness')
        self.draw_prop(context, layout, box, d, 'text_location')
        self.draw_prop(context, layout, box, d, 'text_size')
        self.draw_prop(context, layout, box, d, 'text_offset')
        self.draw_prop(context, layout, box, d, 'text_angle')


"""
s = C.active_object
bpy.ops.archipack.dimension_auto()
o = C.active_object
d = o.data.archipack_dimension_auto[0]


for i in range(3):
    d.add_source(s, i, 'MESH')

d.add_source(None, 0, 'USER')

o.select = True
C.scene.objects.active = o
d = o.data.archipack_dimension_auto[0]
d.update(C)

"""


class ARCHIPACK_OT_dimension_auto_orient(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.dimension_auto_orient"
    bl_label = "Orient"
    bl_description = "Orient according measured point"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    @classmethod
    def poll(cls, context):
        d = archipack_dimension_auto.datablock(context.active_object)
        return d and len(d.sources) > 1

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            bpy.ops.archipack.disable_manipulate()
            d = archipack_dimension_auto.datablock(o)
            itM = o.matrix_world.inverted()
            # world location
            res, p0 = d.sources[0].location(context, itM)
            if not res:
                return {'CANCELLED'}
            res, p1 = d.sources[-1].location(context, itM)
            if not res:
                return {'CANCELLED'}
            dl = (p1 - p0)
            if o.parent is not None:
                o.location = o.parent.matrix_world.inverted() @ o.matrix_world @ p0
            else:
                o.location = o.matrix_world @ p0
            # o.matrix_world.translation = o.matrix_world @ p0
            o.rotation_euler.z += atan2(dl.y, dl.x)
            d.update(context)
            self.select_object(context, o, True)

            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_dimension_auto_update(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.dimension_auto_update"
    bl_label = "Update"
    bl_description = "Update dimensions"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    @classmethod
    def poll(cls, context):
        return archipack_dimension_auto.poll(context.active_object)

    def _update_dimensions(self, context, o):
        d = archipack_dimension_auto.datablock(o)
        if d:
            self.select_object(context, o, True)
            d.update(context)
            self.unselect_object(context, o)

    def _get_topmost_parent(self, o):
        if o.parent:
            return self._get_topmost_parent(o.parent)
        else:
            return o

    def _update_child_dimensions(self, context, o):
        self._update_dimensions(context, o)
        for c in o.children:
            self._update_child_dimensions(context, c)

    def update_dimensions(self, context, o):
        p = self._get_topmost_parent(o)
        self._update_child_dimensions(context, p)

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            with stop_auto_manipulate(context):
                self.update_dimensions(context, o)
            self.select_object(context, o, True)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_dimension_auto_add(ArchipackObjectsManager, Operator):
    bl_idname = "archipack.dimension_auto_add"
    bl_label = "Add"
    bl_description = "Add user defined measured point"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}

    obj: StringProperty(default="")
    index: IntProperty(default=0)
    provider_type: StringProperty(
        name="Provider type",
        default="USER"
        )

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            bpy.ops.archipack.disable_manipulate()
            d = archipack_dimension_auto.datablock(o)
            obj = self.get_scene_object(context, self.obj)
            d.add_source(obj, self.index, self.provider_type)
            d.update(context)
            d.manipulable_invoke(context, o)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_dimension_auto_remove(Operator):
    bl_idname = "archipack.dimension_auto_remove"
    bl_label = "Remove"
    bl_description = "Remove measured point"
    bl_category = 'Archipack'
    bl_options = {'REGISTER', 'UNDO'}
    index: IntProperty(default=0)

    def execute(self, context):
        if context.mode == "OBJECT":
            o = context.active_object
            bpy.ops.archipack.disable_manipulate()
            d = archipack_dimension_auto.datablock(o)
            d.remove_source(self.index)
            d.update(context)
            # d.manipulate_mode = False
            d.manipulable_invoke(context, o)
            return {'FINISHED'}
        else:
            self.report({'WARNING'}, "Archipack: Option only valid in Object mode")
            return {'CANCELLED'}


class ARCHIPACK_OT_dimension_auto(ArchipackCreateTool, Operator):
    bl_idname = "archipack.dimension_auto"
    bl_label = "Multi Dimension"
    bl_description = "Create Multi Dimension"

    mode: EnumProperty(
            items=(
            ('CREATE', 'Create', '', 0),
            ('DELETE', 'Delete', '', 1)
            ),
            default='CREATE'
            )
    distance: FloatProperty(default=1)
    flip_side: BoolProperty(default=False)
    auto_parent: BoolProperty(default=True)

    def create(self, context, distance, flip_side):
        with stop_auto_manipulate(context):

            # Create an empty curve datablock
            c = bpy.data.curves.new("Dimension", type='CURVE')
            c.dimensions = '2D'
            o = bpy.data.objects.new("Dimension", c)

            # Add your properties on curve datablock
            d = c.archipack_dimension_auto.add()
            d.distance = distance
            d.flip_side = flip_side

            self.link_object_to_scene(context, o)
            self.select_object(context, o, True)

            self.add_material(context, o, category="dimension_auto", material="DEFAULT")
            if hasattr(self, "filepath"):
                self.load_preset(d)
            else:
                d.auto_update = True

        return o

    def delete(self, context, o):
        if archipack_dimension_auto.filter(o):
            self.delete_object(context, o)

    def execute(self, context):
        if context.mode == "OBJECT":
            act = context.active_object
            bpy.ops.object.select_all(action="DESELECT")
            o = self.create(context, self.distance, self.flip_side)
            if act and self.auto_parent:
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


class ARCHIPACK_OT_dimension_auto_preset_menu(PresetMenuOperator, Operator):
    bl_idname = "archipack.dimension_auto_preset_menu"
    bl_label = "Dimension preset"
    preset_subdir = "archipack_dimension_auto"


class ARCHIPACK_OT_dimension_auto_preset(ArchipackPreset, Operator):
    bl_description = "Add / remove a Dimension Preset"
    bl_idname = "archipack.dimension_auto_preset"
    bl_label = "Dimension preset"
    preset_menu = "ARCHIPACK_OT_dimension_auto_preset_menu"

    @property
    def blacklist(self):
        return ['manipulators']


def register():
    bpy.utils.register_class(archipack_dimension_source)

    bpy.utils.register_class(archipack_dimension_auto)
    Curve.archipack_dimension_auto = CollectionProperty(type=archipack_dimension_auto)
    bpy.utils.register_class(ARCHIPACK_PT_dimension_auto)
    bpy.utils.register_class(ARCHIPACK_OT_dimension_auto)
    bpy.utils.register_class(ARCHIPACK_OT_dimension_auto_preset_menu)
    bpy.utils.register_class(ARCHIPACK_OT_dimension_auto_preset)
    bpy.utils.register_class(ARCHIPACK_OT_dimension_auto_add)
    bpy.utils.register_class(ARCHIPACK_OT_dimension_auto_remove)
    bpy.utils.register_class(ARCHIPACK_OT_dimension_auto_orient)
    bpy.utils.register_class(ARCHIPACK_OT_dimension_auto_update)

    bpy.utils.register_class(archipack_dimension_point)


def unregister():
    bpy.utils.unregister_class(archipack_dimension_point)
    bpy.utils.unregister_class(archipack_dimension_source)

    bpy.utils.unregister_class(archipack_dimension_auto)
    del Curve.archipack_dimension_auto
    bpy.utils.unregister_class(ARCHIPACK_PT_dimension_auto)
    bpy.utils.unregister_class(ARCHIPACK_OT_dimension_auto)
    bpy.utils.unregister_class(ARCHIPACK_OT_dimension_auto_preset_menu)
    bpy.utils.unregister_class(ARCHIPACK_OT_dimension_auto_preset)
    bpy.utils.unregister_class(ARCHIPACK_OT_dimension_auto_add)
    bpy.utils.unregister_class(ARCHIPACK_OT_dimension_auto_remove)
    bpy.utils.unregister_class(ARCHIPACK_OT_dimension_auto_orient)
    bpy.utils.unregister_class(ARCHIPACK_OT_dimension_auto_update)
