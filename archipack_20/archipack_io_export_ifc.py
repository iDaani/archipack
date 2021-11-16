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
import bpy
import uuid
import string
from functools import reduce
import time
import os
import datetime
from getpass import getuser
from mathutils import Vector, Matrix
import bmesh
from bpy.types import Operator
from bpy.props import StringProperty, EnumProperty, BoolProperty
from bpy_extras.io_utils import ImportHelper, ExportHelper
from .archipack_prefs import get_prefs


ifctemplate = """ISO-10303-21;
HEADER;
FILE_DESCRIPTION(('ViewDefinition [CoordinationView]'),'2;1');
FILE_NAME('%(filename)s','%(timestamp)s',('%(user)s','%(email)s'),('%(company)s'),'blender-archipack','blender-archipack','');
FILE_SCHEMA(('%(ifcschema)s'));
ENDSEC;
DATA;
%(content)s;
ENDSEC;
END-ISO-10303-21;
"""

X_AXIS = Vector((1, 0, 0))
Z_AXIS = Vector((0, 0, 1))
# template line index is 1 based
stack = []
# associate mesh to data in order to find and return instances as single product definition
mesh_assoc = {}
# shaders
shaders = {}
# shape representation for sub materials
shape_mat_reps = {}
export_materials = True


class StackEntity:
    # min schema compatibility
    _compat_version = 23

    def __init__(self, schema):
        global stack
        stack.append(self)
        # index is 1 based -> set line after append
        self.line = len(stack)
        self.schema = schema

    def opt_string(self, value):
        if value is None or value == "":
            return "$"
        return "'%s'" % value

    def get_line(self, value):
        if hasattr(value, 'line'):
            return "#%s" % value.line
        elif type(value).__name__ in {"list", "tuple"}:
            if len(value) > 0:
                return "(%s)" % ",".join([self.get_line(v) for v in value])
            else:
                return "$"
        elif value is None:
            return "$"
        return "#%s" % value

    def get_list(self, value):
        if type(value).__name__ in {"list", "tuple"}:
            if len(value) > 0:
                return "(%s)" % ",".join(["%s" % v for v in value])
        return "$"

    def get_list_3float(self, value):
        if value is None:
            return "$"
        return self.get_list(["(%.4f,%.4f,%.4f)" % tuple(x) for x in value])

    def get_list_3int(self, value):
        if value is None:
            return "$"
        return self.get_list(["(%s,%s,%s)" % tuple(x) for x in value])

    @property
    def header_str(self):
        return "%s=%s" % (self.line, self.__class__.__name__.upper())

    @property
    def compatible(self):
        return self.schema >= self._compat_version


class ifcGuid:
    chars = string.digits + string.ascii_uppercase + string.ascii_lowercase + '_$'

    @staticmethod
    def compress(g):
        bs = [int(g[i:i + 2], 16) for i in range(0, len(g), 2)]

        def b64(v, l=4):
            return ''.join([ifcGuid.chars[(v // (64 ** i)) % 64] for i in range(l)][::-1])

        return ''.join([b64(bs[0], 2)] + [b64((bs[i] << 16) + (bs[i + 1] << 8) + bs[i + 2]) for i in range(1, 16, 3)])

    @staticmethod
    def expand(g):
        def b64(v):
            return reduce(lambda a, b: a * 64 + b, map(lambda c: ifcGuid.chars.index(c), v))

        bs = [b64(g[0:2])]
        for i in range(5):
            d = b64(g[2 + 4 * i:6 + 4 * i])
            bs += [(d >> (8 * (2 - j))) % 256 for j in range(3)]
        return ''.join(['%02x' % b for b in bs])

    @staticmethod
    def split(g):
        return '{%s-%s-%s-%s-%s}' % (g[:8], g[8:12], g[12:16], g[16:20], g[20:])

    @staticmethod
    def short():
        ifcGuid.split(uuid.uuid1().hex)[1:23].replace("-", "_")

    @staticmethod
    def new():
        return ifcGuid.compress(uuid.uuid1().hex)


class ifcStringEntity(StackEntity):

    def __init__(self, schema, entity, default):
        StackEntity.__init__(self, schema)
        self.entity = entity % default

    def __str__(self):
        return self.entity


class ifcCartesianPoint(StackEntity):
    def __init__(self, schema, location):
        StackEntity.__init__(self, schema)
        self.location = location

    def __str__(self):
        dimensions = len(self.location)
        if dimensions > 2:
            x, y, z = self.location[0:3]
            return "%s((%.4f,%.4f,%.4f))" % (self.header_str, x, y, z)
        elif dimensions == 2:
            x, y = self.location
            return "%s((%.4f,%.4f))" % (self.header_str, x, y)
        return ""


class ifcDirection(StackEntity):
    def __init__(self, schema, axis):
        StackEntity.__init__(self, schema)
        self.axis = axis

    def __str__(self):
        x, y, z = self.axis
        return "%s((%.4f,%.4f,%.4f))" % (self.header_str, x, y, z)


class ifcQuantityLength(StackEntity):
    def __init__(self, schema, name, description, value):
        StackEntity.__init__(self, schema)
        self.name = name
        self.description = description
        self.value = value

    def __str__(self):
        return "%s('%s','%s',%.4f)" % (self.header_str,
                                       self.name,
                                       self.description,
                                       self.value)


class ifcElementQuantity(StackEntity):
    def __init__(self, schema, owner, name="BaseQuantities", elements=None):
        StackEntity.__init__(self, schema)
        self.uid = ifcGuid.new()
        self.owner = owner
        self.name = name
        self.elements = elements

    def __str__(self):
        return "%s('%s',%s,'%s',$,$,%s)" % (self.header_str,
                                            self.uid,
                                            self.get_line(self.owner),
                                            self.name,
                                            self.get_line(self.elements)
                                            )


class ifcRelDefinesByProperties(StackEntity):
    def __init__(self, schema, owner, elements=None, element_quantity=None):
        StackEntity.__init__(self, schema)
        self.uid = ifcGuid.new()
        self.owner = owner
        self.element_quantity = element_quantity
        self.elements = elements

    def __str__(self):
        return "%s('%s',%s,$,%s,%s)" % (self.header_str,
                                        self.uid,
                                        self.get_line(self.owner),
                                        self.get_line(self.elements),
                                        self.get_line(self.element_quantity)
                                        )


class ifcAxis2Placement3d(StackEntity):
    def __init__(self, schema, point, dir_z=None, dir_x=None):
        StackEntity.__init__(self, schema)
        # pointers
        if type(point).__name__ == "Matrix":
            self.point = ifcCartesianPoint(schema, point.translation.to_3d())
            self.dir_z = ifcDirection(schema, point.col[2].to_3d())
            self.dir_x = ifcDirection(schema, point.col[0].to_3d())
        else:
            self.point = point
            self.dir_z = dir_z
            self.dir_x = dir_x

    def __str__(self):
        return "%s(%s,%s,%s)" % (self.header_str,
                                 self.get_line(self.point),
                                 self.get_line(self.dir_z),
                                 self.get_line(self.dir_x))


class ifcCartesianTransformationOperator3D(StackEntity):
    def __init__(self, schema, axis_x, axis_y=None, origin=None, scale=1.0, axis_z=None):
        StackEntity.__init__(self, schema)
        # pointers
        if type(axis_x).__name__ == "Matrix":
            self.origin = ifcCartesianPoint(schema, axis_x.translation.to_3d())
            self.axis_x = ifcDirection(schema, axis_x.col[0].to_3d().normalized())
            self.axis_y = ifcDirection(schema, axis_x.col[1].to_3d().normalized())
            self.axis_z = ifcDirection(schema, axis_x.col[2].to_3d().normalized())
            self.scale = axis_x.col[2].to_3d().length
        else:
            self.axis_x = axis_x
            self.axis_y = axis_y
            self.axis_z = axis_z
            self.scale = scale
            self.origin = origin

    def __str__(self):
        return "%s(%s,%s,%s,%s,%s)" % (self.header_str,
                                       self.get_line(self.axis_x),
                                       self.get_line(self.axis_y),
                                       self.get_line(self.origin),
                                       self.scale,
                                       self.get_line(self.axis_z))


class ifcLocalPlacement(StackEntity):
    def __init__(self, schema, context=None, axis_placement=None):
        StackEntity.__init__(self, schema)

        self.context = context

        if type(axis_placement).__name__ == "Matrix":
            axis_placement = ifcAxis2Placement3d(schema, axis_placement)

        self.axis_placement = axis_placement

    def __str__(self):
        return "%s(%s,%s)" % (self.header_str,
                              self.get_line(self.context),
                              self.get_line(self.axis_placement))


class ifcRoot(StackEntity):
    def __init__(self, schema, owner, name, description):
        StackEntity.__init__(self, schema)
        self.uid = ifcGuid.new()
        self.owner = owner
        self.name = name
        self.description = description

    def __str__(self):
        return "'%s',%s,'%s','%s'" % (
            self.uid,
            self.get_line(self.owner),
            self.name,
            self.description
        )


class ifcProduct(ifcRoot):
    def __init__(self, schema, owner, name, description, object_placement, representation=None):
        ifcRoot.__init__(self, schema, owner, name, description)
        self.object_placement = object_placement
        self.representation = representation

    def __str__(self):
        return "%s,$,%s,%s" % (
            ifcRoot.__str__(self),
            self.get_line(self.object_placement),
            self.get_line(self.representation)
        )


class ifcSite(ifcProduct):
    def __init__(self, schema, owner, name="Site", description="", object_placement=None, lon=(0, 0, 0), lat=(0, 0, 0)):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement)
        self.lon = lon
        self.lat = lat

    def __str__(self):
        lon = "(%s,%s,%s)" % tuple(self.lon)
        lat = "(%s,%s,%s)" % tuple(self.lat)
        return "%s(%s,$,.ELEMENT.,%s,%s,$,$,$)" % (
            self.header_str,
            ifcProduct.__str__(self),
            lon,
            lat
        )


class ifcBuilding(ifcProduct):
    def __init__(self, schema, owner, name="Building", description="", object_placement=None):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement)

    def __str__(self):
        return "%s(%s,$,.ELEMENT.,$,$,$)" % (
            self.header_str,
            ifcProduct.__str__(self)
        )


class ifcWallStandardCase(ifcProduct):
    def __init__(self, schema, owner, name="Wall", description="", object_placement=None, representation=None):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)

    def __str__(self):
        return "%s(%s,$)" % (
            self.header_str,
            ifcProduct.__str__(self)
        )


class ifcWall(ifcProduct):
    def __init__(self, schema, owner, name="Wall", description="", object_placement=None, representation=None):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)

    def __str__(self):
        return "%s(%s,$)" % (
            self.header_str,
            ifcProduct.__str__(self)
        )


class ifcRailing(ifcProduct):
    def __init__(self, schema, owner, name="Railing", description="", object_placement=None,
                 representation=None,
                 predefined_type='RAILING'):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)
        self.predefined_type = predefined_type

    def __str__(self):
        return "%s(%s,$,.%s.)" % (
            self.header_str,
            ifcProduct.__str__(self),
            self.predefined_type
        )


class ifcFurnishingElement(ifcProduct):
    def __init__(self, schema, owner, name="Furnishing", description="", object_placement=None, representation=None):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)

    def __str__(self):
        return "%s(%s,$)" % (
            self.header_str,
            ifcProduct.__str__(self)
        )


class ifcStair(ifcProduct):
    def __init__(self, schema, owner, name="Stair", description="", object_placement=None,
                 representation=None, shape_type="USERDEFINED"):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)
        self.shape_type = shape_type

    def __str__(self):
        return "%s(%s,$,.%s.)" % (
            self.header_str,
            ifcProduct.__str__(self),
            self.shape_type
        )


class ifcOpeningElement(ifcProduct):
    def __init__(self, schema, owner, name="Opening", description="", object_placement=None, representation=None):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)

    def __str__(self):
        return "%s(%s,$)" % (
            self.header_str,
            ifcProduct.__str__(self)
        )


class ifcRelVoidsElement(StackEntity):
    def __init__(self, schema, owner, element=None, cutter=None):
        StackEntity.__init__(self, schema)
        self.uid = ifcGuid.new()
        self.owner = owner
        self.element = element
        self.cutter = cutter

    def __str__(self):
        return "%s('%s',%s,$,$,%s,%s)" % (
            self.header_str,
            self.uid,
            self.get_line(self.owner),
            self.get_line(self.element),
            self.get_line(self.cutter)
        )


class ifcRelFillsElement(StackEntity):
    def __init__(self, schema, owner, element=None, fill=None):
        """
        :param owner:
        :param element: opening
        :param fill: window
        """
        StackEntity.__init__(self, schema)
        self.uid = ifcGuid.new()
        self.owner = owner
        self.element = element
        self.fill = fill

    def __str__(self):
        return "%s('%s',%s,$,$,%s,%s)" % (
            self.header_str,
            self.uid,
            self.get_line(self.owner),
            self.get_line(self.element),
            self.get_line(self.fill)
        )


class ifcWindow(ifcProduct):
    def __init__(self, schema, owner, name="Window", description="", object_placement=None, representation=None,
                 width=None,
                 height=None,
                 predefined_type='WINDOW',
                 partitioning_type='NOTDEFINED'):

        # 1017= IFCWINDOWSTYLE('2JlHbdZSB8wU69mViBoJpV',#30,'Fen\X2\00EA\X0\tre 22',$,$,(#1018,#1021),$,'93BD1967-8DC2-C8E9-E189-C1FB0BC93CDF',.NOTDEFINED.,.SINGLE_PANEL.,.F.,.F.);
        # 1018= IFCWINDOWLININGPROPERTIES('0n2BNDdQaPVU0HOKnoqFSh',#30,'Window Lining Properties',$,0.05,0.05,$,$,$,$,$,$,$);
        # 1021= IFCWINDOWPANELPROPERTIES('1ZKShX2_r80I4kQOkBWlye',#30,'Window Panel Properties - Panel 1',$,.FIXEDCASEMENT.,.MIDDLE.,$,$,$);
        # 1023= IFCRELDEFINESBYTYPE('1NsPS2nuYTxjfzTDa7F2NF',#30,$,$,(#958),#1017);

        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)
        self.width = width
        self.height = height
        # [WINDOW, SKYLIGHT, LIGHTDOME, USERDEFINED, NOTDEFINED]
        self.predefined_type = predefined_type
        # [SINGLE_PANEL,
        # DOUBLE_PANEL_VERTICAL,
        # DOUBLE_PANEL_HORIZONTAL,
        # TRIPLE_PANEL_VERTICAL,
        # TRIPLE_PANEL_BOTTOM,
        # TRIPLE_PANEL_TOP,
        # TRIPLE_PANEL_LEFT,
        # TRIPLE_PANEL_RIGHT,
        # TRIPLE_PANEL_HORIZONTAL,
        # USERDEFINED,
        # NOTDEFINED]
        self.partitioning_type = partitioning_type

    def __str__(self):
        if self.schema >= 42:
            return "%s(%s,$,%.4f,%.4f,.%s.,.%s.)" % (
                self.header_str,
                ifcProduct.__str__(self),
                self.height,
                self.width,
                self.predefined_type,
                self.partitioning_type
            )
        else:
            return "%s(%s,$,%.4f,%.4f)" % (
                self.header_str,
                ifcProduct.__str__(self),
                self.height,
                self.width
            )


class ifcDoor(ifcProduct):
    def __init__(self, schema, owner, name="Door", description="", object_placement=None, representation=None,
                 width=None,
                 height=None):
        # 1345= IFCDOOR('0bwqHNIbH2X8tr7FcN$16v',#30,'PORTE-002',$,$,#1033,#1336,'25EB4457-4A54-4284-8DF5-1CF997FC11B9',2.14,0.98);
        # 1404= IFCDOORSTYLE('2WZZZmeEfhRTBJpv8m2Vui',#30,'Porte 22',$,$,(#1405,#1408),$,'A08E38F0-A0EA-6B6D-D2D3-CF923009FE2C',.SINGLE_SWING_RIGHT.,.NOTDEFINED.,.F.,.F.);
        # 1405= IFCDOORLININGPROPERTIES('1aplvplLVQmAbvKrpJ3vvw',#30,'Door Lining Properties',$,$,0.04,$,$,$,$,0.4,$,$,$,$);
        # 1408= IFCDOORPANELPROPERTIES('3599UTe4A5ug11ZGeNg6SJ',#30,'Door Panel Properties - Panel 1',$,0.04,.SWINGING.,1.,.MIDDLE.,$);
        # 1410= IFCRELDEFINESBYTYPE('26T0bQP$k2nxc$3w0P2d_D',#30,$,$,(#1345),#1404);

        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)
        self.width = width
        self.height = height

    def __str__(self):
        return "%s(%s,$,%.4f,%.4f)" % (
            self.header_str,
            ifcProduct.__str__(self),
            self.height,
            self.width
        )


class ifcBeam(ifcProduct):
    def __init__(self, schema, owner, name="Beam", description="", object_placement=None, representation=None):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)

    def __str__(self):
        return "%s(%s,$)" % (
            self.header_str,
            ifcProduct.__str__(self)
        )


class ifcSlab(ifcProduct):
    def __init__(self, schema, owner, name="Slab", description="", object_placement=None, representation=None,
                 predefined_type="FLOOR"):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)
        # predefined_type in [FLOOR, ROOF, LANDING, BASESLAB, USERDEFINED, NOTDEFINED]
        self.predefined_type = predefined_type

    def __str__(self):
        #                                    objectType   Tag
        return "%s(%s,$,.%s.)" % (
            self.header_str,
            ifcProduct.__str__(self),
            self.predefined_type
        )


class ifcCovering(ifcProduct):
    def __init__(self, schema, owner, name="Covering", description="", object_placement=None, representation=None,
                 predefined_type="FLOORING"):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)
        # predefined_type in ["CEILING", "FLOORING", "CLADDING", "ROOFING", "MOLDING", "SKIRTINGBOARD", "INSULATION",
        # "MEMBRANE", "SLEEVING", "WRAPPING", "USERDEFINED", "NOTDEFINED"]
        self.predefined_type = predefined_type

    def __str__(self):
        #                                     objectType   representation
        return "%s(%s,$,.%s.)" % (
            self.header_str,
            ifcProduct.__str__(self),
            self.predefined_type
        )


class ifcRoof(ifcProduct):
    def __init__(self, schema, owner, name="Roof", description="", object_placement=None, representation=None,
                 shape_type="NOTDEFINED"):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)
        # FLAT_ROOF,
        # SHED_ROOF,
        # GABLE_ROOF,
        # HIP_ROOF,
        # HIPPED_GABLE_ROOF,
        # GAMBREL_ROOF,
        # MANSARD_ROOF,
        # BARREL_ROOF,
        # RAINBOW_ROOF,
        # BUTTERFLY_ROOF,
        # PAVILION_ROOF,
        # DOME_ROOF,
        # FREEFORM,
        # NOTDEFINED
        self.shape_type = shape_type

    def __str__(self):
        #                                     objectType   representation
        return "%s(%s,$,.%s.)" % (
            self.header_str,
            ifcProduct.__str__(self),
            self.shape_type
        )


class ifcBuildingElementProxy(ifcProduct):
    def __init__(self, schema, owner, name="Proxy", description="", object_placement=None, representation=None,
                 composition_type="ELEMENT"):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement, representation)
        self.composition_type = composition_type

    def __str__(self):
        #                                     objectType   representation
        return "%s(%s,$,.%s.)" % (
            self.header_str,
            ifcProduct.__str__(self),
            self.composition_type
        )


class ifcBuildingStorey(ifcProduct):
    def __init__(self, schema, owner, name="Storey", description="", object_placement=None, elevation=0.0):
        ifcProduct.__init__(self, schema, owner, name, description, object_placement)
        self.elevation = elevation

    def __str__(self):
        #                            objectType, representation, long name,  elevation
        return "%s(%s,$,.ELEMENT.,%.4f)" % (
            self.header_str,
            ifcProduct.__str__(self),
            self.elevation
        )


class ifcRepresentationMap(StackEntity):
    def __init__(self, schema, origin=None, representation=None):
        StackEntity.__init__(self, schema)
        self.origin = origin
        self.representation = representation

    def __str__(self):
        #                            objectType, representation, long name,  elevation
        return "%s(%s,%s)" % (
            self.header_str,
            self.get_line(self.origin),
            self.get_line(self.representation)
        )


class ifcRelAggregates(ifcRoot):
    def __init__(self, schema, owner, name="Container", description="", container=None, components=None):
        ifcRoot.__init__(self, schema, owner, name, description)
        self.container = container
        self.components = components

    def __str__(self):
        return "%s(%s,%s,%s)" % (
            self.header_str,
            ifcRoot.__str__(self),
            self.get_line(self.container),
            self.get_line(self.components)
        )


class ifcRelContainedInSpatialStructure(ifcRoot):
    def __init__(self, schema, owner, name="Container", description="", container=None, components=None):
        ifcRoot.__init__(self, schema, owner, name, description)
        self.container = container
        self.components = components

    def __str__(self):
        return "%s(%s,%s,%s)" % (
            self.header_str,
            ifcRoot.__str__(self),
            self.get_line(self.components),
            self.get_line(self.container)
        )


class ifcProductDefinitionShape(StackEntity):

    def __init__(self, schema, components):
        """Define shapes as component of an object eg axis and body
        """
        StackEntity.__init__(self, schema)
        self.components = components

    def __str__(self):
        return "%s($,$,%s)" % (
            self.header_str,
            self.get_line(self.components)
        )


class ifcRepresentation(StackEntity):
    def __init__(self, schema, context, rep_id=None, rep_type=None, items=None):
        """
        :param context: IfcGeometricRepresentationContext (line number)
        :param rep_id: Representation id arbitrary ['Body', 'Axis']
        :param rep_type: Representation type
        [Curve2D, GeometricSet, GeometricCurveSet, Annotations2D, SurfaceModel]
        [SolidModel, SweptSolid, Brep, CSG, Clipping, AdvancedSweptSolid]
        [BoundingBox, SectionedSpline, MappedRepresentation]
        :param items:
        """
        StackEntity.__init__(self, schema)
        self.context = context
        self.representation_id = rep_id
        self.representation_type = rep_type
        self.items = items

    def __str__(self):
        return "%s(%s,%s,%s,%s)" % (
            self.header_str,
            self.get_line(self.context),
            self.opt_string(self.representation_id),
            self.opt_string(self.representation_type),
            self.get_line(self.items)
        )


class ifcShapeRepresentation(ifcRepresentation):
    def __init__(self, schema, context, rep_id=None, rep_type=None, items=None):
        ifcRepresentation.__init__(self, schema, context, rep_id, rep_type, items)


class ifcRepresentationItem(StackEntity):
    def __init__(self, schema):
        StackEntity.__init__(self, schema)


class ifcMappedItem(ifcRepresentationItem):
    def __init__(self, schema, representation_map, transform_operator=None):
        ifcRepresentationItem.__init__(self, schema)
        self.representation_map = representation_map
        self.transform_operator = transform_operator

    def __str__(self):
        return "%s(%s,%s)" % (
            self.header_str,
            self.get_line(self.representation_map),
            self.get_line(self.transform_operator)
        )


class ifcPolyline(StackEntity):
    def __init__(self, schema, points):
        StackEntity.__init__(self, schema)
        self.points = points

    def __str__(self):
        return "%s(%s)" % (
            self.header_str,
            self.get_line(self.points)
        )


class ifcExtrudedAreaSolid(StackEntity):
    def __init__(self, schema, closed_profile_def=None, placement=None, direction=None, amount=0.0):
        StackEntity.__init__(self, schema)
        self.profile_def = closed_profile_def
        self.placement = placement
        self.direction = direction
        self.amount = amount

    def __str__(self):
        return "%s(%s,%s,%s,%.4f)" % (
            self.header_str,
            self.get_line(self.profile_def),
            self.get_line(self.placement),
            self.get_line(self.direction),
            self.amount
        )


class ifcArbitraryClosedProfileDef(StackEntity):
    def __init__(self, schema, polyline, profil_type="AREA"):
        """
        :param polyline:
        :param profil_type: in [AREA, CURVE]
        """
        StackEntity.__init__(self, schema)
        self.polyline = polyline
        self.profil_type = profil_type

    def __str__(self):
        # name
        return "%s(.%s.,$,%s)" % (
            self.header_str,
            self.profil_type,
            self.get_line(self.polyline)
        )


class ifcArbitraryProfileDefWithVoids(StackEntity):
    def __init__(self, schema, outer_curve, inner_curves=None, profil_type="AREA"):
        StackEntity.__init__(self, schema)
        self.outer_curve = outer_curve
        self.inner_curves = inner_curves
        self.profil_type = profil_type

    def __str__(self):  # name
        return "%s(.%s.,$,%s,%s)" % (
            self.header_str,
            self.profil_type,
            self.get_line(self.outer_curve),
            self.get_line(self.inner_curves)
        )


class ifcClosedShell(StackEntity):
    def __init__(self, schema, faces):
        StackEntity.__init__(self, schema)
        self.faces = faces

    def __str__(self):
        return "%s(%s)" % (
            self.header_str,
            self.get_line(self.faces)
        )


class ifcOpenShell(StackEntity):
    def __init__(self, schema, faces):
        StackEntity.__init__(self, schema)
        self.faces = faces

    def __str__(self):
        return "%s(%s)" % (
            self.header_str,
            self.get_line(self.faces)
        )


class ifcFacetedBrep(StackEntity):
    def __init__(self, schema, outer):
        StackEntity.__init__(self, schema)
        self.outer = outer

    def __str__(self):
        return "%s(%s)" % (
            self.header_str,
            self.get_line(self.outer)
        )


class ifcShellBasedSurfaceModel(StackEntity):
    def __init__(self, schema, outer):
        StackEntity.__init__(self, schema)
        self.outer = outer

    def __str__(self):
        return "%s(%s)" % (
            self.header_str,
            self.get_line(self.outer)
        )


class ifcCartesianPointList3D(StackEntity):
    _compat_version = 40

    def __init__(self, schema, points):
        StackEntity.__init__(self, schema)
        self.points = points

    def __str__(self):
        return "%s(%s)" % (
            self.header_str,
            self.get_list_3float(self.points)
        )


class ifcTriangulatedFaceSet(StackEntity):
    _compat_version = 42

    def __init__(self, schema, coordinates=None, normals=None, closed=1, coord_index=None, pn_index=None):
        StackEntity.__init__(self, schema)
        self.coordinates = coordinates
        self.normals = normals
        self.closed = ("F", "T")[int(closed)]
        self.coord_index = coord_index
        self.pn_index = pn_index

    def __str__(self):
        return "%s(%s,%s,.%s.,%s,%s)" % (
            self.header_str,
            self.get_line(self.coordinates),
            self.get_list_3float(self.normals),
            self.closed,
            self.get_list_3int(self.coord_index),
            self.get_list_3int(self.pn_index)
        )


class ifcPolyLoop(StackEntity):
    def __init__(self, schema, points):
        StackEntity.__init__(self, schema)
        self.points = points

    def __str__(self):
        return "%s(%s)" % (
            self.header_str,
            self.get_line(self.points)
        )


class ifcFaceBound(StackEntity):

    def __init__(self, schema, bound, orientation=True):
        StackEntity.__init__(self, schema)
        self.bound = bound
        self.orientation = ("F", "T")[int(orientation)]

    def __str__(self):
        return "%s(%s,.%s.)" % (
            self.header_str,
            self.get_line(self.bound),
            self.orientation
        )


class ifcFaceOuterBound(ifcFaceBound):

    def __init__(self, schema, bound, orientation=True):
        ifcFaceBound.__init__(self, schema, bound, orientation)


class ifcFace(StackEntity):

    def __init__(self, schema, bounds, texture_map=None):
        StackEntity.__init__(self, schema)
        # where outerBound is bounds[0]
        self.bounds = bounds
        self.texture_map = texture_map

    def __str__(self):
        if self.schema >= 40:
            return "%s(%s,%s)" % (
                self.header_str,
                self.get_line(self.bounds),
                self.get_line(self.texture_map)
            )
        else:
            return "%s(%s)" % (
                self.header_str,
                self.get_line(self.bounds)
            )


class ifcColourRgb(StackEntity):
    def __init__(self, schema, colour, name=""):
        """
        :param schema: int schema compatibility
        :param colour: array of r, g, b in range[0-1]
        :param name: such as industry name like RAL
        """
        StackEntity.__init__(self, schema)
        self.colour = colour
        self.name = name

    def __str__(self):
        return "%s('%s',%s)" % (
            self.header_str,
            self.name,
            "%.4f,%.4f,%.4f" % tuple(self.colour))


class ifcColourRgbList(StackEntity):
    def __init__(self, schema, colours):
        """
        :param schema: int schema compatibility
        :param colours: array of (r, g, b) in range[0-1]
        :param name: such as industry name like RAL
        """
        StackEntity.__init__(self, schema)
        self.colours = colours

    def __str__(self):
        return "%s(%s)" % (
            self.header_str,
            self.get_list_3float(self.colours)
        )


class ifcMaterialDefinition(StackEntity):
    def __init__(self, schema, name, description="", category=None):
        StackEntity.__init__(self, schema)
        self.name = name
        self.description = description
        self.category = category

    def __str__(self):
        if self.schema >= 40:
            return "%s('%s','%s',%s)" % (
                self.header_str,
                self.name,
                self.description,
                self.opt_string(self.category)
            )
        else:
            return "%s('%s')" % (
                self.header_str,
                self.name
            )


class ifcShapeAspect(StackEntity):
    def __init__(self, schema, name, description="", representations=None, product_definition='U',
                 part_of_product=None):
        StackEntity.__init__(self, schema)
        self.name = name
        self.description = description
        self.representations = representations
        self.product_definition = product_definition
        self.part_of_product = part_of_product

    def __str__(self):
        return "%s(%s,'%s','%s',.%s.,%s)" % (
            self.header_str,
            self.get_line(self.representations),
            self.name,
            self.description,
            self.product_definition,
            self.get_line(self.part_of_product)
        )


class ifcMaterial(ifcMaterialDefinition):
    def __init__(self, schema, name, description="", category=None):
        ifcMaterialDefinition.__init__(self, schema, name, description, category)


class ifcStyleModel(ifcRepresentation):
    def __init__(self, schema, context, rep_id=None, rep_type=None, items=None):
        ifcRepresentation.__init__(self, schema, context, rep_id, rep_type, items)


class ifcStyledRepresentation(ifcStyleModel):
    def __init__(self, schema, context, rep_id="Style", rep_type="Material", items=None):
        ifcStyleModel.__init__(self, schema, context, rep_id, rep_type, items)


class ifcStyledItem(ifcRepresentationItem):
    def __init__(self, schema, name="", item=None, styles=None):
        """
        :param schema:
        :param name:
        :param item: IfcRepresentationItem
        :param styles: IfcPresentationStyle
        """
        ifcRepresentationItem.__init__(self, schema)
        self.item = item
        self.styles = styles
        self.name = name

    def __str__(self):
        return "%s(%s,%s,%s)" % (
            self.header_str,
            self.get_line(self.item),
            self.get_line(self.styles),
            self.opt_string(self.name)
        )


class ifcProductRepresentation(StackEntity):
    def __init__(self, schema, name, description="", representations=None):
        StackEntity.__init__(self, schema)
        self.name = name
        self.description = description
        self.representations = representations  # IfcRepresentation

    def __str__(self):
        return "%s,%s,%s" % (
            self.opt_string(self.name),
            self.opt_string(self.description),
            self.get_line(self.representations)
        )


class ifcMaterialDefinitionRepresentation(ifcProductRepresentation):
    def __init__(self, schema, name="", description="", representations=None, material=None):
        ifcProductRepresentation.__init__(self, schema, name, description, representations)
        self.represented_material = material

    def __str__(self):
        return "%s(%s,%s)" % (
            self.header_str,
            ifcProductRepresentation.__str__(self),
            self.get_line(self.represented_material)
        )


class ifcPresentationStyleAssignment(StackEntity):
    def __init__(self, schema, styles):
        StackEntity.__init__(self, schema)
        self.styles = styles

    def __str__(self):
        return "%s(%s)" % (
            self.header_str,
            self.get_line(self.styles)
        )


class ifcPresentationStyle(StackEntity):
    def __init__(self, schema, name=""):
        StackEntity.__init__(self, schema)
        self.name = name


class ifcSurfaceStyle(ifcPresentationStyle):
    def __init__(self, schema, name="", side='POSITIVE', styles=None):
        ifcPresentationStyle.__init__(self, schema, name)
        self.side = side
        self.styles = styles

    def __str__(self):
        return "%s('%s',.%s.,%s)" % (
            self.header_str,
            self.name,
            self.side,
            self.get_line(self.styles)
        )


class ifcSurfaceStyleShading(StackEntity):
    def __init__(self, schema, colour, transparency=0.0):
        StackEntity.__init__(self, schema)
        self.colour = colour
        self.transparency = transparency

    def __str__(self):
        if self.schema >= 42:
            return "%s(%s,%.2f)" % (
                self.header_str,
                self.get_line(self.colour),
                self.transparency
            )
        else:
            return "%s(%s)" % (
                self.header_str,
                self.get_line(self.colour)
            )


class ifcSurfaceStyleRendering(ifcSurfaceStyleShading):
    def __init__(self, schema, colour, transparency=0.0,
                 diffuse=None,
                 transmission=None,
                 diffuse_transmission=None,
                 reflection=None,
                 specular=None,
                 specular_highlight=None,
                 reflection_method='NOTDEFINED'):
        ifcSurfaceStyleShading.__init__(self, schema, colour, transparency)
        # ArchiCad 2x3
        # 700= IFCSURFACESTYLERENDERING(#699,0.,IFCNORMALISEDRATIOMEASURE(0.82),$,$,$,IFCNORMALISEDRATIOMEASURE(0.5),$,.NOTDEFINED.);
        # color as ambient color
        self.diffuse = diffuse
        self.transmission = transmission
        self.diffuse_transmission = diffuse_transmission
        self.reflection = reflection
        self.specular = specular
        self.specular_highlight = specular_highlight
        self.reflection_method = reflection_method

    def get_line(self, value):
        if isinstance(value, float):
            return "%.4f" % value
        return ifcSurfaceStyleShading.get_line(self, value)

    def __str__(self):
        return "%s(%s,%.4f,%s,%s,%s,%s,%s,%s,.%s.)" % (
            self.header_str,
            self.get_line(self.colour),
            self.transparency,
            self.get_line(self.diffuse),
            self.get_line(self.transmission),
            self.get_line(self.diffuse_transmission),
            self.get_line(self.reflection),
            self.get_line(self.specular),
            self.get_line(self.specular_highlight),
            self.reflection_method
        )


class ifcIndexedColourMap(StackEntity):
    def __init__(self, schema, mapped_to=None, opacity=None, colours=None, colour_index=None):
        StackEntity.__init__(self, schema)
        self.mapped_to = mapped_to  # IfcTessellatedFaceSe
        self.colours = colours  # ifcColourRgbList
        self.opacity = opacity
        self.colour_index = colour_index

    def __str__(self):
        if self.schema >= 42:
            # IFCINDEXEDCOLOURMAP(#5,0.5,#8,(1,1,2,2,3,3,1,1,1,1,1,1));
            return "%s(%s,%.2f,%s,%s)" % (
                self.header_str,
                self.get_line(self.mapped_to),
                self.opacity,
                self.get_line(self.colours),
                self.get_list(self.colour_index)
            )
        else:
            # IFCINDEXEDCOLOURMAP(#5, #7, #8,(1,1,2,2,3,3,1,1,1,1,1,1));
            return "%s(%s,%s,%s,%s)" % (
                self.header_str,
                self.get_line(self.mapped_to),
                self.get_line(self.opacity),
                self.get_line(self.colours),
                self.get_list(self.colour_index)
            )


class ifcRelAssociatesMaterial(ifcRoot):
    def __init__(self, schema, owner, name="", description="", related_objects=None, material=None):
        ifcRoot.__init__(self, schema, owner, name, description)
        self.related_objects = related_objects
        self.material = material

    def __str__(self):
        return "%s(%s,%s)" % (
            self.header_str,
            self.get_line(self.related_objects),
            self.get_line(self.material)
        )


class ifcMaterialConstituentSet(ifcMaterialDefinition):
    _compat_version = 40

    def __init__(self, schema, name, description="", category=None, materials=None):
        ifcMaterialDefinition.__init__(self, schema, name, description, category)
        self.materials = materials

    def __str__(self):
        return "%s(%s,%s,%s)" % (
            self.header_str,
            self.opt_string(self.name),
            self.opt_string(self.description),
            self.get_line(self.materials)
        )


class ifcMaterialConstituent(ifcMaterialDefinition):
    _compat_version = 40

    def __init__(self, schema, name, description="", category=None, material=None, fraction=0.5):
        ifcMaterialDefinition.__init__(self, schema, name, description, category)
        self.material = material
        self.fraction = fraction

    def __str__(self):
        return "%s(%s,%s,%s,%.4f,%s)" % (
            self.header_str,
            self.opt_string(self.name),
            self.opt_string(self.description),
            self.get_line(self.material),
            self.fraction,
            self.opt_string(self.category)
        )


"""
mat = ifcMaterial(schema, "My mat")
sty = ifcSurfaceStyleShading(schema, colour)
sti = ifcStyledItem(schema, item=rep, styles=[sty])
msr = ifcStyledRepresentation(schema, context, rep_id="Body", rep_type="Brep", items=[sti])
mdr = ifcMaterialDefinitionRepresentation(schema, material=mat, representations=[msr])

# SCHEMA IFC4X2
#4= IFCCARTESIANPOINTLIST3D(((0,0,0),(1,0,0),(1,1,0),(0,1,0),(0,0,2),(1,0,2),(1,1,2),(0,1,2)));
#5= IFCTRIANGULATEDFACESET(#4,$,$,((1,6,5),(1,2,6), (6,2,7), (7,2,3), (7,8,6), (6,8,5), (5,8,1), (1,8,4), (4,2,1), (2,4,3), (4,8,7), (7,3,4)),$);
#6= IFCCOLOURRGB('grey',0.5,0.5,0.5);

#7= IFCSURFACESTYLESHADING(#6,0.5);
#8= IFCCOLOURRGBLIST(((1,0,0), (0,1,0), (1,1,0)));
#9= IFCINDEXEDCOLOURMAP(#5,0.5,#8,(1,1,2,2,3,3,1,1,1,1,1,1));


"""


class Brep:
    # virtual edges:
    # loops are cw, holes are ccw
    @staticmethod
    def filter_by_index(face, f, done):
        return face.index != f.index and face.index not in done

    @staticmethod
    def filter_by_material(face, f, done):
        return Brep.filter_by_index(face, f, done) and face.material_index == f.material_index

    @staticmethod
    def get_coplanar(f, res, done, filter_method):
        if f.index not in done:
            res.add(f.index)
            done.add(f.index)
            for loop in f.loops:
                edge = loop.edge
                if abs(edge.calc_face_angle(1)) < 0.0001:
                    for face in edge.link_faces:
                        if filter_method(face, f, done):
                            Brep.get_coplanar(face, res, done, filter_method)

    @staticmethod
    def get_connected(f, res, edges, faces):
        if f.index in faces:
            return
        faces.add(f.index)
        res[f.index] = f
        for ed in f.edges:
            if ed.index not in edges:
                edges.add(ed.index)
                for lf in ed.link_faces:
                    if lf.index != f.index:
                        Brep.get_connected(lf, res, edges, faces)

    @staticmethod
    def is_cw(_verts, face, loop):
        n = face.normal
        # center of the face
        x, y, z = face.calc_center_bounds()
        if n.length < 0.5:
            # fallback for faces with null normal
            itM = Matrix()
        else:
            if abs(n.z) > 0.95:
                vx = X_AXIS
            else:
                vx = n.cross(Z_AXIS)

            vy = vx.cross(n)
            itM = Matrix([
                [vx.x, vy.x, n.x, x],
                [vx.y, vy.y, n.y, y],
                [vx.z, vy.z, n.z, z],
                [0, 0, 0, 1]
            ]).inverted()
        _pts = [itM @ _verts[i].co for i in loop]
        p0 = _pts[-1]
        d = 0
        for p1 in _pts:
            d += (p1.x * p0.y - p1.y * p0.x)
            p0 = p1
        return d > 0

    @staticmethod
    def is_closed(faces):
        edges = set()
        edges2 = set()
        for i, f in faces.items():
            for ed in f.edges:
                idx = ed.index
                if idx in edges:
                    if idx in edges2:
                        # found more than twice
                        return False
                    edges2.add(idx)
                else:
                    # found once
                    edges.add(idx)
        return len(edges) == len(edges2)

    @staticmethod
    def _createBrep(schema, s_type, _faces, _verts, _points, faces, filter_method):
        polys = []

        done = set()
        for i, f in faces.items():
            if i in done:
                continue

            has_outer = False
            loops = {}

            # coplanar faces indexes
            """
            coplanar = set()
            Brep.get_coplanar(f, coplanar, done, filter_method)
            face = [_faces[fi] for fi in coplanar]
            f1 = face[0]
            for f2 in face:
                for loop in f2.loops:
                    # only use edges with angle > 0
                    # if True or abs(loop.edge.calc_face_angle(0)) > 0.0001:
                    v0 = loop.vert.index
                    _v0, _v1 = loop.edge.verts
                    if _v0.index != v0:
                        v1 = _v0.index
                    else:
                        v1 = _v1.index

                    loops[v0] = v1
            """
            # dumb without holes -> exact bmesh copy
            # this is not ideal and probably is wrong according the rules
            # but does work
            f1 = f
            for loop in f.loops:
                v0 = loop.vert.index
                _v0, _v1 = loop.edge.verts
                if _v0.index != v0:
                    v1 = _v0.index
                else:
                    v1 = _v1.index
                loops[v0] = v1

            # loops / holes
            poly = []
            inner = []
            found = set()
            for v0, v in loops.items():
                if v0 in found:
                    continue
                found.add(v0)
                loop = [v0]
                v1 = v
                while v0 != v1 and v1 not in found:
                    loop.append(v1)
                    found.add(v1)
                    if v1 in loops:
                        v1 = loops[v1]
                    else:
                        break
                pts = [_points[i] for i in loop]
                pl = ifcPolyLoop(schema, points=pts)

                if Brep.is_cw(_verts, f1, loop):
                    if has_outer:
                        raise ValueError("Face already has outer")
                    fb = ifcFaceOuterBound(schema, bound=pl, orientation=True)
                    poly.append(fb)
                    has_outer = True
                else:
                    pl.points.reverse()
                    fb = ifcFaceBound(schema, bound=pl, orientation=True)
                    inner.append(fb)

            poly.extend(inner)
            fc = ifcFace(schema, bounds=poly)

            polys.append(fc)

        shape = None
        if len(polys) > 0:

            if s_type == "Brep":
                shell = ifcClosedShell(schema, faces=polys)
                shape = ifcFacetedBrep(schema, outer=shell)

            else:
                shell = ifcOpenShell(schema, faces=polys)
                shape = ifcShellBasedSurfaceModel(schema, outer=[shell])

        # 913= IFCOPENSHELL((#911));
        # 915= IFCSHELLBASEDSURFACEMODEL((#913));
        # 917= IFCCOLOURRGB($,0.980392156863,0.980392156863,1.);
        # 918= IFCSURFACESTYLERENDERING(#917,0.7,IFCNORMALISEDRATIOMEASURE(0.19),$,$,$,IFCNORMALISEDRATIOMEASURE(0.5),$,.NOTDEFINED.);
        # 919= IFCSURFACESTYLE('Transp - Verre clair',.BOTH.,(#918));
        # 921= IFCPRESENTATIONSTYLEASSIGNMENT((#919));
        # 923= IFCSTYLEDITEM(#915,(#921),$);
        # 946= IFCSHAPEREPRESENTATION(#154,'Body','SurfaceModel',(#698,#756,#826,#896,#915,#941));
        # 949= IFCPRODUCTDEFINITIONSHAPE($,$,(#946));
        # 952= IFCSHAPEREPRESENTATION(#154,'Body','SurfaceModel',(#698,#756,#826,#896,#915,#941));
        # 954= IFCSHAPEASPECT((#952),'Component 1','',.U.,#949);

        # 699= IFCCOLOURRGB($,0.356862745098,0.356862745098,0.356862745098);
        # 700= IFCSURFACESTYLERENDERING(#699,0.,IFCNORMALISEDRATIOMEASURE(0.82),$,$,$,IFCNORMALISEDRATIOMEASURE(0.5),$,.NOTDEFINED.);
        # 701= IFCSURFACESTYLE('Peinture - Gris fonc\X2\00E9\X0\',.BOTH.,(#700));
        # 703= IFCPRESENTATIONSTYLEASSIGNMENT((#701));
        # 1099= IFCCLOSEDSHELL((#1046,#1055,#1066,#1073,#1082,#1087,#1092,#1097));
        # 1101= IFCFACETEDBREP(#1099);
        # 1102= IFCSTYLEDITEM(#1101,(#703),$);
        # 1327= IFCCLOSEDSHELL((#1292,#1301,#1308,#1315,#1320,#1325));
        # 1329= IFCFACETEDBREP(#1327);
        # 1330= IFCSTYLEDITEM(#1329,(#703),$);
        # 1333= IFCSHAPEREPRESENTATION(#154,'Body','Brep',(#1101,#1171,#1277,#1329));
        # 1336= IFCPRODUCTDEFINITIONSHAPE($,$,(#1333));
        # 1339= IFCSHAPEREPRESENTATION(#154,'Body','Brep',(#1101,#1171,#1277,#1329));
        # 1341= IFCSHAPEASPECT((#1339),'Component 1','',.U.,#1336);

        return shape

    @staticmethod
    def create(schema, s_type, bm, itM, mats=None, shape_reps=None):
        bm.verts.ensure_lookup_table()
        bm.edges.ensure_lookup_table()
        bm.faces.ensure_lookup_table()
        # bm.normal_update()
        _verts = bm.verts
        _faces = bm.faces
        _points = [ifcCartesianPoint(schema, itM @ v.co) for v in _verts]

        # closed shape: all faces edges are found twice

        if mats is not None and len(mats) > 0:

            filter_method = Brep.filter_by_material
            for i, mat in enumerate(mats):
                faces = {f.index: f for f in _faces if f.material_index == i}
                shape = Brep._createBrep(schema, s_type, _faces, _verts, _points, faces, filter_method)
                if shape is not None:
                    sty = create_style(schema, mat.name, mat.diffuse_color[:3])
                    ifcStyledItem(schema, item=shape, styles=[sty])

                    # Store in overall ifcProductDefinitionShape items
                    shape_reps.append(shape)

        else:
            filter_method = Brep.filter_by_index
            faces = {f.index: f for f in _faces}
            shape = Brep._createBrep(schema, s_type, _faces, _verts, _points, faces, filter_method)
            if shape is not None:
                shape_reps.append(shape)


class Trep:

    @staticmethod
    def create(schema, s_type, bm, ltM, mats=None, shape_reps=None):

        bm.verts.ensure_lookup_table()
        bm.edges.ensure_lookup_table()
        bm.faces.ensure_lookup_table()
        bmesh.ops.triangulate(bm, faces=bm.faces, quad_method='BEAUTY', ngon_method='BEAUTY')

        # split by material index to handle multi material assignment through ifcShapeAspect
        # http://www.buildingsmart-tech.org/ifc/IFC4x1/final/html/schema/ifcrepresentationresource/lexical/ifcshapeaspect.htm
        # http://www.buildingsmart-tech.org/ifc/IFC2x3/TC1/html/ifcrepresentationresource/lexical/ifcshapeaspect.htm
        if mats is not None and len(mats) > 0:
            for i, mat in enumerate(mats):
                _faces = [f for f in bm.faces if f.material_index == i]
                _verts = set(v for f in _faces for v in f.verts)
                verts = ifcCartesianPointList3D(schema, [ltM @ v.co for v in _verts])
                trep = ifcTriangulatedFaceSet(schema,
                                              coordinates=verts,
                                              coord_index=[tuple(v.index for v in f.verts)
                                                           for f in _faces])

                # Store in overall ifcProductDefinitionShape items
                shape_reps.append(trep)

        else:
            verts = ifcCartesianPointList3D(schema, [ltM @ v.co for v in bm.verts])
            trep = ifcTriangulatedFaceSet(schema,
                                          coordinates=verts,
                                          coord_index=[tuple(v.index for v in f.verts)
                                                       for f in bm.faces])
            shape_reps.append(trep)


def create_Meshrep(schema, mesh_type, context, o, geom_line,
                   components=None,
                   use_instances=False,
                   use_materials=True,
                   bm_from_object=False):
    """
    :param schema:
    :param mesh_type: output mesh type in [SufaceModel, Brep] for open / closed meshes
    :param context:
    :param o: blender object
    :param geom_line: entity number of representation
    :param components: sub objects
    :param use_instances:
    :param use_materials:
    :param bm_from_object: Eval object using modifier stack
    :return:
    """
    global mesh_assoc
    global shape_mat_reps
    global export_materials

    if use_instances and id(o.data) in mesh_assoc:
        return mesh_assoc[id(o.data)]

    parts = [o]
    if components is not None and len(components) > 0:
        parts.extend(components)

    itM = o.matrix_world.inverted()

    # Store to add pointers from outside factory
    shape_reps = []

    if mesh_type in {"SurfaceModel", 'Brep'}:
        rep_factory = Brep
    else:
        # Trep is simple to generate and has smallest footstep IFC 4x2
        rep_factory = Trep

    mats = None
    for c in parts:

        if export_materials and use_materials:
            mats = c.data.materials

        ltM = itM @ c.matrix_world
        bm = bmesh.new(use_operators=True)
        if bm_from_object:
            bm.from_object(c, context.depsgraph, deform=True, cage=False, face_normals=True)
        else:
            bm.from_mesh(c.data)

        rep_factory.create(schema, mesh_type, bm, ltM, mats, shape_reps)
        bm.free()

    ds = None

    if len(shape_reps) > 0:
        rep = ifcShapeRepresentation(schema, geom_line, rep_id="Body", rep_type=mesh_type, items=shape_reps)
        ds = ifcProductDefinitionShape(schema, [rep])

        ifcShapeAspect(schema, representations=[rep], name=o.name, part_of_product=ds)

        mesh_assoc[id(o.data)] = ds
    return ds


styles = {}


def create_style(schema, name, colour, transparency=0, reflection_method='NOTDEFINED', side='POSITIVE'):
    # 699= IFCCOLOURRGB($,0.356862745098,0.356862745098,0.356862745098);
    # 700= IFCSURFACESTYLERENDERING(#699,0.,IFCNORMALISEDRATIOMEASURE(0.82),$,$,$,IFCNORMALISEDRATIOMEASURE(0.5),$,.NOTDEFINED.);
    # 701= IFCSURFACESTYLE('Peinture - Gris fonc\X2\00E9\X0\',.BOTH.,(#700));
    # 703= IFCPRESENTATIONSTYLEASSIGNMENT((#701));

    if name in styles:
        return styles[name]

    col = ifcColourRgb(schema, name=name, colour=colour)
    ssr = ifcSurfaceStyleRendering(schema, colour=col, transparency=transparency, reflection_method=reflection_method)
    iss = ifcSurfaceStyle(schema, name=name, side=side, styles=[ssr])
    if schema < 40:
        # ifcPresentationStyleAssignment deprecated in schema 40
        iss = ifcPresentationStyleAssignment(schema, styles=[iss])

    styles[name] = iss
    return iss


material_layers = {}


def create_material_layer(schema, context, name, thickness, colour,
                          transparency=0,
                          reflection_method='NOTDEFINED',
                          side='BOTH'):
    # 246= IFCMATERIAL('Isolation - Polyur\X2\00E9\X0\thane (int\X2\00E9\X0\rieur)');
    # 247= IFCCOLOURRGB($,0.250980392157,0.545098039216,1.);
    # 248= IFCSURFACESTYLERENDERING(#247,0.,IFCNORMALISEDRATIOMEASURE(0.59),$,$,$,IFCNORMALISEDRATIOMEASURE(0.69),$,.NOTDEFINED.);
    # 249= IFCSURFACESTYLE('Isolation - Panneau rigide bleu',.BOTH.,(#248));
    # 251= IFCPRESENTATIONSTYLEASSIGNMENT((#249));
    # 253= IFCSTYLEDITEM($,(#251),$);
    # 255= IFCSTYLEDREPRESENTATION(#154,$,$,(#253));
    # 257= IFCMATERIALDEFINITIONREPRESENTATION($,$,(#255),#246);
    # 261= IFCMATERIALLAYER(#246,0.04,.U.);

    if name in material_layers:
        return material_layers[name]

    mat = ifcMaterial(schema, name=name)
    psa = create_style(schema, name, colour, transparency, reflection_method, side)
    sti = ifcStyledItem(schema, name=None, item=None, styles=[psa])
    rep = ifcStyledRepresentation(schema, context, rep_id=None, rep_type=None, items=[sti])
    ifcMaterialDefinitionRepresentation(schema, representations=[rep], material=mat)
    ml = None
    # ml = ifcMaterialLayer(schema, mat, thickness, "U")

    material_layers[name] = ml
    return ml


def create_ClosedPolyline(schema, coords):
    _coords = [ifcCartesianPoint(schema, co) for co in coords]
    _coords.append(_coords[0])
    return ifcPolyline(schema, _coords)


def create_SweptSolid(schema, context, o, coords, z_axis, amount, voids=None, use_instances=False):
    global mesh_assoc
    if use_instances and id(o.data) in mesh_assoc:
        return mesh_assoc[id(o.data)]

    po0 = create_ClosedPolyline(schema, coords)
    if voids is None or len(voids) < 1:
        ac0 = ifcArbitraryClosedProfileDef(schema, po0)
    else:
        _voids = [create_ClosedPolyline(schema, co) for co in voids]
        ac0 = ifcArbitraryProfileDefWithVoids(schema, po0, inner_curves=_voids)

    sh0 = ifcExtrudedAreaSolid(schema, closed_profile_def=ac0, placement=10, direction=z_axis, amount=amount)
    sr0 = ifcShapeRepresentation(schema, context, rep_id="Body", rep_type="SweptSolid", items=[sh0])
    ds0 = ifcProductDefinitionShape(schema, [sr0])
    # TODO: Use materials layers
    # 278= IFCMATERIAL('Finition int - Enduit pl\X2\00E2\X0\tre');
    # 279= IFCCOLOURRGB($,0.952941176471,0.960784313725,0.937254901961);
    # 280= IFCSURFACESTYLERENDERING(#279,0.,IFCNORMALISEDRATIOMEASURE(0.65),$,$,$,IFCNORMALISEDRATIOMEASURE(0.67),$,.NOTDEFINED.);
    # 281= IFCSURFACESTYLE('Enduit - Pl\X2\00E2\X0\tre',.BOTH.,(#280));
    # 283= IFCPRESENTATIONSTYLEASSIGNMENT((#281));
    # 285= IFCSTYLEDITEM($,(#283),$);
    # 287= IFCSTYLEDREPRESENTATION(#154,$,$,(#285));
    # 289= IFCMATERIALDEFINITIONREPRESENTATION($,$,(#287),#278);
    # 293= IFCMATERIALLAYER(#278,0.01,.U.);
    # 294= IFCMATERIALLAYERSET((#228,#245,#261,#277,#293),'3.3 - Dalle BA + chape + parquet');

    # NOTE: sol .AXIS3.,.NEGATIVE.
    # 301= IFCMATERIALLAYERSETUSAGE(#294,.AXIS3.,.NEGATIVE.,0.34);
    # NOTE mur .AXIS2. .POSITIVE.
    # 485= IFCMATERIALLAYERSET((#451,#467,#483,#484),'1.1 - Mur ext. - Brique TC + Isolation p\X2\00E9\X0\riph\X2\00E9\X0\rique');
    # 491= IFCMATERIALLAYERSETUSAGE(#485,.AXIS2.,.POSITIVE.,-0.3);

    # 302= IFCRELASSOCIATESMATERIAL('1v8V4pioSs8Qnyt1LfJ2nA',#30,$,$,(#192),#301);
    mesh_assoc[id(o.data)] = ds0

    return ds0


def get_archipack_objects(o, res, objects):
    for c in o.children:
        if c not in objects:
            continue

        dat = c.data
        if dat:
            for key in dat.keys():
                if "archipack_" in key:
                    d = getattr(dat, key)[0]
                    if key not in res:
                        res[key] = {}
                    res[key][c] = d
        get_archipack_objects(c, res, objects)


def object_uid(o, entity):
    if "archipack_ifcuid" in o:
        return
        # entity.uid = o["archipack_ifcuid"]
    else:
        o["archipack_ifcuid"] = entity.uid


def ifc_export(context, prefs, filepath, selected_only):
    """Implements IFC4 Reference view MVD
    TODO: support for Design transfer view using proper Axis and parametric representations
    :param context: blender context
    :param prefs: importer dialog instance
    :param filepath:
    :return:
    """

    global mesh_assoc
    global stack
    global styles
    global material_layers
    global export_materials
    # shape representation for sub materials
    shape_mat_reps = {}
    stack.clear()
    mesh_assoc.clear()
    styles.clear()
    material_layers.clear()

    # TODO: use area as support for "ifcSpace" definition

    # brep based product definitions
    cls_map = {
        "archipack_fence": ifcRailing,
        "archipack_beam": ifcBeam,
        "archipack_roof": ifcRoof,
        "archipack_stair": ifcStair,
        "archipack_kitchen": ifcFurnishingElement,
        "archipack_truss": ifcBuildingElementProxy,
        "archipack_custom": ifcBuildingElementProxy,
        "archipack_blind": ifcBuildingElementProxy,
        "archipack_molding": ifcCovering
    }

    # owner line number in template (default = 5)
    owner = 5
    x_axis = 6
    y_axis = 7
    z_axis = 8
    p0 = 9
    geom_line = 11
    project_line = 20

    schema = int(prefs.schema)
    schema_str = "IFC%sX%s" % (prefs.schema[0], prefs.schema[1])

    # mesh type in [Tessellated, Brep]
    # Tessellated >= IFC2x4
    mesh_type = "Tessellated"
    if schema < 24:
        mesh_type = "SurfaceModel"

    # mesh_type = "Brep"

    export_materials = prefs.export_materials

    # Use shared representations
    use_instances = True

    # Store project uid in scene so we have constant uid
    if "archipack_project_ifcuid" in context.scene:
        project_uid = context.scene["archipack_project_ifcuid"]
    else:
        project_uid = ifcGuid.new()
        context.scene["archipack_project_uid"] = project_uid

    from . import __version__

    default = {
        "ifcschema": schema_str,
        "user": prefs.user,
        "company": prefs.company,
        "email": prefs.email,
        "now": int(time.time()),
        "id": project_uid,
        "project": prefs.project,
        "filename": os.path.basename(filepath),
        "timestamp": datetime.datetime.now().isoformat(),
        "version": __version__
    }

    # Init template
    [ifcStringEntity(schema, s, default) for s in [
        "1=IFCPERSON($,$,'%(user)s',$,$,$,$,$)",
        "2=IFCORGANIZATION($,'%(company)s',$,$,$)",
        "3=IFCPERSONANDORGANIZATION(#1,#2,$)",
        "4=IFCAPPLICATION(#2,'%(version)s','blender-archipack','4ab6ec4a_68ab_11e9_a75')",
        "5=IFCOWNERHISTORY(#3,#4,$,.ADDED.,$,#3,#4,%(now)s)",
        "6=IFCDIRECTION((1.,0.,0.))",
        "7=IFCDIRECTION((0.,1.,0.))",
        "8=IFCDIRECTION((0.,0.,1.))",
        "9=IFCCARTESIANPOINT((0.,0.,0.))",
        "10=IFCAXIS2PLACEMENT3D(#9,#8,#6)",
        "11=IFCGEOMETRICREPRESENTATIONCONTEXT('Plan','Model',3,1.E-05,#10,#10)",
        "12=IFCDIMENSIONALEXPONENTS(0,0,0,0,0,0,0)",
        "13=IFCSIUNIT(*,.LENGTHUNIT.,$,.METRE.)",
        "14=IFCSIUNIT(*,.AREAUNIT.,$,.SQUARE_METRE.)",
        "15=IFCSIUNIT(*,.VOLUMEUNIT.,$,.CUBIC_METRE.)",
        "16=IFCSIUNIT(*,.PLANEANGLEUNIT.,$,.RADIAN.)",
        "17=IFCMEASUREWITHUNIT(IFCPLANEANGLEMEASURE(0.017453292519943295),#16)",
        "18=IFCCONVERSIONBASEDUNIT(#12,.PLANEANGLEUNIT.,'DEGREE',#17)",
        "19=IFCUNITASSIGNMENT((#13,#14,#15,#18))",
        "20=IFCPROJECT('%(id)s',#5,'%(project)s',$,$,$,$,(#11),#19)"]]

    refs = [ref for ref in context.scene.objects if "archipack_reference_point" in ref]

    if selected_only:
        objects = context.selected_objects[:]
    else:
        objects = context.scene.objects[:]

    # Only one site / file, might use sun position as reference for lat lon and north
    # P0 is defined in text template as 0,0,0
    a0 = ifcAxis2Placement3d(schema, p0, z_axis, x_axis)
    l0 = ifcLocalPlacement(schema, axis_placement=a0)
    s0 = ifcSite(schema, owner, object_placement=l0)
    object_uid(context.scene, s0)
    ifcRelAggregates(schema, owner, name="Project Container", container=project_line, components=[s0])

    # Any number of buildings
    # TODO: expose and use "archipack building" object to support many buildings
    p1 = ifcCartesianPoint(schema, (0, 0, 0))
    a1 = ifcAxis2Placement3d(schema, p1, z_axis, x_axis)
    l1 = ifcLocalPlacement(schema, context=l0, axis_placement=a1)
    b0 = ifcBuilding(schema, owner, object_placement=l1)
    # object_uid(ref, b0)
    ifcRelAggregates(schema, owner, name="Site Container", container=s0, components=[b0])

    storey = []
    for ref in refs:
        apk = {}
        get_archipack_objects(ref, apk, objects)

        # shaders
        if export_materials:
            for key, items in apk.items():
                for c, d in items.items():
                    for mat in c.data.materials:
                        if mat.name not in styles:
                            colour = mat.diffuse_color[:3]
                            create_style(schema, mat.name, colour, transparency=0, reflection_method='FLAT')

        # Storey currently using Reference point as base object for storey
        l2 = ifcLocalPlacement(schema, context=l1, axis_placement=ref.matrix_world)
        e0 = ifcBuildingStorey(schema, owner, object_placement=l2)
        object_uid(ref, e0)

        storey.append(e0)
        itM = ref.matrix_world.inverted()
        storey_comps = []

        if "archipack_wall2" in apk:
            for c, d in apk["archipack_wall2"].items():

                # walls with windows / slice.z > 0 or with autofit roof are breps
                force_brep = export_materials or d.fit_roof or any([s.z != 0 for p in d.parts for s in p.slices])
                # use brep window holes instead, so we keep simple walls
                wall_brep = not export_materials

                create_holes = wall_brep or not force_brep

                """
                if not force_brep:
                    for child in d.childs:
                        c2, d2 = child.get_child(context)
                        if d2 is None:
                            continue
                        if "archipack_window" in c2.data:
                            force_brep = True
                            break
                """
                if force_brep:
                    wM = itM @ c.matrix_world
                else:
                    wM = itM @ c.matrix_world @ Matrix.Translation(Vector((0, 0, -d.z_offset)))

                l3 = ifcLocalPlacement(schema, context=l2, axis_placement=wM)

                if force_brep:
                    # walls with roofs or slices
                    if wall_brep:
                        wall_type = "Brep"
                    else:
                        wall_type = "SurfaceModel"

                    ds0 = create_Meshrep(schema, wall_type, context, c, geom_line,
                                         use_instances=use_instances,
                                         use_materials=export_materials,
                                         bm_from_object=export_materials)

                    if ds0 is None:
                        continue

                    w0 = ifcWall(schema, owner, object_placement=l3, representation=ds0)

                else:
                    g = d.get_generator()

                    # Standard Wall
                    inside = []
                    outside = []
                    voids = None
                    g.inside.get_verts(inside)
                    g.outside.get_verts(outside)
                    if d.closed:
                        for si, so in zip(g.inside.segs, g.outside.segs):
                            if si.length > so.length:
                                inside, outside = outside, inside
                                break
                            elif si.length < so.length:
                                break
                        voids = [inside]

                    else:
                        outside.extend(reversed(inside))

                    ds0 = create_SweptSolid(schema, geom_line, c, outside, z_axis, d.z,
                                            voids=voids,
                                            use_instances=use_instances)
                    w0 = ifcWallStandardCase(schema, owner, object_placement=l3, representation=ds0)

                object_uid(c, w0)

                storey_comps.append(w0)

                # Standard Openings
                for child in d.childs:
                    c2, d2 = child.get_child(context)
                    if d2 is None:
                        continue

                    altitude = 0
                    hole_oz = 0

                    # hole  xy plane
                    hole = d2.find_hole(c2)
                    ds1 = None
                    if "archipack_window" in c2.data:
                        if create_holes:
                            ds1 = create_Meshrep(schema, "Brep", context, hole, geom_line,
                                                     use_instances=use_instances,
                                                     use_materials=False)
                        # altitude = d2.altitude
                        components = [c3 for c3 in c2.children if c3.data and "archipack_window_panel" in c3.data]

                    elif "archipack_door" in c2.data:
                        components = [c3 for c3 in c2.children if c3.data and "archipack_door_panel" in c3.data]
                        hole_oz = d2.z_offset
                        x, y, z = 0.5 * d2.x + d2.frame_y, 0.5 * d2.y, d2.z + hole_oz + d2.frame_y
                        coords = [(-x, -y), (x, -y), (x, y), (-x, y)]
                        if create_holes:
                            ds1 = create_SweptSolid(schema, geom_line, hole, coords, z_axis, z,
                                                    use_instances=use_instances)
                    else:
                        continue

                    oM = itM @ c2.matrix_world @ Matrix.Translation(Vector((0, 0, altitude - hole_oz)))

                    l4 = ifcLocalPlacement(schema, context=l2, axis_placement=oM)
                    op0 = ifcOpeningElement(schema, owner, object_placement=l4, representation=ds1)
                    object_uid(hole, op0)

                    if create_holes:
                        ifcRelVoidsElement(schema, owner, element=w0, cutter=op0)

                    # window / door shape
                    oM = itM @ c2.matrix_world @ Matrix.Translation(Vector((0, 0, altitude)))
                    l5 = ifcLocalPlacement(schema, context=l2, axis_placement=oM)

                    ds2 = create_Meshrep(schema, mesh_type, context, c2, geom_line,
                                         components=components,
                                         use_instances=use_instances)

                    if "archipack_window" in c2.data:
                        w1 = ifcWindow(schema, owner, object_placement=l5, representation=ds2, width=d2.x, height=d2.z)
                    else:
                        # ds2 = create_SweptSolid(geom_line, c2, coords, z_axis, z)
                        w1 = ifcDoor(schema, owner, object_placement=l5, representation=ds2, width=d2.x, height=d2.z)

                    object_uid(c2, w1)

                    storey_comps.append(w1)
                    ifcRelFillsElement(schema, owner, element=op0, fill=w1)

        if "archipack_slab" in apk:
            for c, d in apk["archipack_slab"].items():
                wM = itM @ c.matrix_world @ Matrix.Translation(Vector((0, 0, -d.z)))
                icM = c.matrix_world.inverted()

                # not certain voids here are correct as it only cover the case of inside voids
                voids = []
                """
                if "archipack_slab_cutter" in apk:
                    for c2 in c.children:
                        if c2 in apk["archipack_slab_cutter"]:
                            _d = apk["archipack_slab_cutter"][c2]
                            g = _d.ensure_direction(icM @ c2.matrix_world)
                            coords = []
                            g.get_verts(coords, [])
                            voids.append(coords)
                """
                g = d.get_generator()
                # Standard Slab
                coords = []
                g.line.get_verts(coords)
                ds0 = create_SweptSolid(schema, geom_line, c, coords, z_axis, d.z, voids, use_instances=use_instances)
                l3 = ifcLocalPlacement(schema, context=l2, axis_placement=wM)
                w0 = ifcSlab(schema, owner, object_placement=l3, representation=ds0)
                object_uid(c, w0)

                storey_comps.append(w0)
                if "archipack_slab_cutter" in apk:
                    for c2 in c.children:
                        if c2 in apk["archipack_slab_cutter"]:
                            _d = apk["archipack_slab_cutter"][c2]
                            wM = itM @ c2.matrix_world
                            g = _d.ensure_direction()
                            coords = []
                            g.get_verts(coords, [])
                            ds0 = create_SweptSolid(schema, geom_line, c2, coords, z_axis, d.z,
                                                    use_instances=use_instances)
                            l4 = ifcLocalPlacement(schema, context=l2, axis_placement=wM)
                            op0 = ifcOpeningElement(schema, owner, object_placement=l4, representation=ds0)
                            object_uid(c2, op0)
                            ifcRelVoidsElement(schema, owner, element=w0, cutter=op0)

        if "archipack_floor" in apk:
            for c, d in apk["archipack_floor"].items():
                wM = itM @ c.matrix_world  # @ Matrix.Translation(Vector((0, 0, -d.z)))
                iwM = wM.inverted()
                g = d.get_generator()
                # Standard Slab
                coords = []
                g.get_verts(coords)

                ds0 = create_SweptSolid(schema, geom_line, c, coords, z_axis, d.thickness, use_instances=use_instances)
                l3 = ifcLocalPlacement(schema, context=l2, axis_placement=wM)
                w0 = ifcCovering(schema, owner, name="Flooring", object_placement=l3, representation=ds0)
                object_uid(c, w0)

                storey_comps.append(w0)
                if "archipack_floor_cutter" in apk:

                    for c2 in c.children:
                        if c2 in apk["archipack_floor_cutter"]:
                            _d = apk["archipack_floor_cutter"][c2]
                            wM = itM @ c2.matrix_world
                            g = _d.ensure_direction()
                            coords = []
                            g.get_verts(coords, [])
                            ds0 = create_SweptSolid(schema, geom_line, c2, coords, z_axis, d.thickness,
                                                    use_instances=use_instances)
                            l4 = ifcLocalPlacement(schema, context=l2, axis_placement=wM)
                            op0 = ifcOpeningElement(schema, owner, object_placement=l4, representation=ds0)
                            object_uid(c2, op0)
                            ifcRelVoidsElement(schema, owner, element=w0, cutter=op0)

                if "archipack_slab" in apk and "archipack_slab_cutter" in apk:
                    for c3, d in apk["archipack_slab"].items():
                        for c2 in c3.children:
                            if c2 in apk["archipack_slab_cutter"]:
                                _d = apk["archipack_slab_cutter"][c2]
                                wM = itM @ c2.matrix_world
                                g = _d.ensure_direction()
                                coords = []
                                g.get_verts(coords, [])
                                ds0 = create_SweptSolid(schema, geom_line, c2, coords, z_axis, d.thickness,
                                                        use_instances=use_instances)
                                l4 = ifcLocalPlacement(schema, context=l2, axis_placement=wM)
                                op0 = ifcOpeningElement(schema, owner, object_placement=l4, representation=ds0)
                                object_uid(c2, op0)
                                ifcRelVoidsElement(schema, owner, element=w0, cutter=op0)

                else:
                    continue

        for key, items in apk.items():
            if key in cls_map:
                for c, d in items.items():

                    if key == "archipack_kitchen":
                        components = c.children[:]
                    else:
                        components = None
                    wM = itM @ c.matrix_world
                    l3 = ifcLocalPlacement(schema, context=l2, axis_placement=wM)
                    ds0 = create_Meshrep(schema, mesh_type, context, c, geom_line,
                                         components=components,
                                         use_instances=use_instances)
                    w0 = cls_map[key](schema, owner, name=key[10:].capitalize(), object_placement=l3,
                                      representation=ds0)
                    object_uid(c, w0)
                    storey_comps.append(w0)

        ifcRelContainedInSpatialStructure(schema, owner, name="Content of Storey", container=e0,
                                          components=storey_comps)

    ifcRelAggregates(schema, owner, name="Building Container", container=b0, components=storey)

    for i in stack:
        print(i.__class__.__name__, "#%s" % i)

    default['content'] = ";\n".join(["#%s" % i for i in stack])
    res = ifctemplate % default

    with open(filepath, "w") as f:
        f.write(res)

    stack.clear()
    mesh_assoc.clear()
    styles.clear()
    material_layers.clear()
    print("export done")


class ARCHIPACK_IO_Export_ifc(Operator, ExportHelper):
    bl_idname = "archipack.export_ifc"
    bl_label = "Industry Foundation Classes (.ifc)"
    bl_description = "Industry Foundation Classes ASCII"

    bl_options = {'PRESET'}
    filename_ext = ".ifc"
    # ExportHelper class properties
    filter_glob: StringProperty(
        default="*.ifc",
        options={'HIDDEN'},
    )

    schema: EnumProperty(
        name="IFC schema",
        items=(
            ('23', "ifc 2X3", "", 9),
            # ('42', "ifc 4X2", "", 1)
        ),
        default='23'
    )
    export_materials: BoolProperty(
        name="Export materials",
        default=True
    )
    selected_only: BoolProperty(
        name="Only selected",
        default=False
    )
    user: StringProperty(
        default=getuser().capitalize()
    )
    email: StringProperty(
        default="%s@" % getuser().lower()
    )
    company: StringProperty(
        default="Company"
    )
    project: StringProperty(
        default="Project"
    )

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "schema")
        layout.prop(self, "export_materials")
        layout.prop(self, "selected_only")
        layout.prop(self, "project")
        layout.prop(self, "company")
        layout.prop(self, "user")
        layout.prop(self, "email")

    def execute(self, context):
        bpy.ops.object.mode_set(mode='OBJECT')
        result = {'FINISHED'}
        ifc_export(context, self, self.filepath, self.selected_only)

        return result


class ARCHIPACK_IO_Export_ifc_zip(Operator, ExportHelper):
    bl_idname = "archipack.export_ifc_zip"
    bl_label = "Industry Foundation Classes (.ifcZIP)"
    bl_description = "Compressed industry foundation classes"

    bl_options = {'PRESET'}
    filename_ext = ".ifcZIP"

    # ExportHelper class properties
    filter_glob: StringProperty(
        default="*.ifczip",
        options={'HIDDEN'},
    )
    schema: EnumProperty(
        name="IFC schema",
        items=(
            ('23', "ifc 2X3", "", 9),
            # ('42', "ifc 4X2", "", 1)
        ),
        default='23'
    )
    export_materials: BoolProperty(
        name="Export materials",
        default=True
    )
    selected_only: BoolProperty(
        name="Only selected",
        default=False
    )
    user: StringProperty(
        default=getuser().capitalize()
    )
    email: StringProperty(
        default="%s@" % getuser().lower()
    )
    company: StringProperty(
        default="Company"
    )
    project: StringProperty(
        default="Project"
    )

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "schema")
        layout.prop(self, "export_materials")
        layout.prop(self, "selected_only")
        layout.prop(self, "project")
        layout.prop(self, "company")
        layout.prop(self, "user")
        layout.prop(self, "email")

    def execute(self, context):
        import zipfile
        bpy.ops.object.mode_set(mode='OBJECT')
        result = {'FINISHED'}
        filename, ext = os.path.splitext(os.path.basename(self.filepath))
        tmp_name = "%s.ifc" % filename
        tmp_file = os.path.join(bpy.app.tempdir, tmp_name)
        ifc_export(context, self, tmp_file, self.selected_only)

        with zipfile.ZipFile(self.filepath, mode='w', compression=zipfile.ZIP_DEFLATED, compresslevel=9) as zf:
            zf.write(tmp_file, tmp_name)

        os.remove(tmp_file)

        return result


def menu_func_export(self, context):
    # TODO: remove experimental check once production ready
    prefs = get_prefs(context)
    if prefs.experimental_features:
        self.layout.operator(ARCHIPACK_IO_Export_ifc.bl_idname)
        self.layout.operator(ARCHIPACK_IO_Export_ifc_zip.bl_idname)


def register():
    bpy.utils.register_class(ARCHIPACK_IO_Export_ifc)
    bpy.utils.register_class(ARCHIPACK_IO_Export_ifc_zip)
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export)


def unregister():
    import bpy
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export)
    bpy.utils.unregister_class(ARCHIPACK_IO_Export_ifc)
    bpy.utils.unregister_class(ARCHIPACK_IO_Export_ifc_zip)
