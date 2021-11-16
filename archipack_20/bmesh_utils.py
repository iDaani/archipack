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
import logging

logger = logging.getLogger("archipack")
import bpy
import bmesh
from .archipack_i18n import Archipacki18n
from .archipack_object import objman


class BmeshEdit():
    
    @staticmethod
    def ensure_bmesh(bm):
        bm.verts.ensure_lookup_table()
        bm.edges.ensure_lookup_table()
        bm.faces.ensure_lookup_table()
        
    @staticmethod
    def _start(o):
        """
            private, start bmesh editing of active object
        """
        # objman.select_object(context, o, True)
        bm = bmesh.new(use_operators=True)

        # depsgraph = bpy.context.evaluated_depsgraph_get()
        # mesh = o.evaluated_get(depsgraph).to_mesh()

        bm.from_mesh(o.data)
        return bm

    @staticmethod
    def select(bm, verts, faces, edges):
        for v in bm.verts:
            v.select = verts
        for ed in bm.edges:
            ed.select = edges
        for f in bm.faces:
            f.select = faces

    @staticmethod
    def bmesh_join(o, list_of_bmeshes, temporary=False, normal_update=False):
        """
            takes as input a list of bm references and outputs a single merged bmesh
            allows an additional 'normal_update=True' to force _normal_ calculations.
        """
        if temporary:
            bm = bmesh.new(use_operators=True)
        else:
            bm = BmeshEdit._start(o)
        
        add_vert = bm.verts.new
        add_face = bm.faces.new
        add_edge = bm.edges.new

        for bm_to_add in list_of_bmeshes:
            offset = len(bm.verts)

            bevel = bm_to_add.verts.layers.bevel_weight.items()
            bevel_layers = []
            for name, src in bevel:
                dst = bm.verts.layers.bevel_weight.get(name)
                if dst is None:
                    dst = bm.verts.layers.bevel_weight.new(name)
                bevel_layers.append((src, dst))

            for vert in bm_to_add.verts:

                v = add_vert(vert.co)
                v.select = vert.select
                for src, dst in bevel_layers:
                    v[dst] = vert[src]

            bm.verts.index_update()
            BmeshEdit.ensure_bmesh(bm)
            
            if bm_to_add.faces:

                # vertex colors
                cols = bm_to_add.loops.layers.color.items()
                cols_layers = []
                for name, src in cols:
                    dst = bm.loops.layers.color.get(name)
                    if dst is None:
                        dst = bm.loops.layers.color.new(name)
                    cols_layers.append((src, dst))

                # uvs
                uvs = bm_to_add.loops.layers.uv.items()
                uvs_layers = []
                for name, src in uvs:
                    dst = bm.loops.layers.uv.get(name)
                    if dst is None:
                        dst = bm.loops.layers.uv.new(name)
                    uvs_layers.append((src, dst))

                for face in bm_to_add.faces:
                    f = add_face(tuple(bm.verts[i.index + offset] for i in face.verts))
                    f.select = face.select
                    f.material_index = face.material_index
                    for j, loop in enumerate(face.loops):
                        for src, dst in uvs_layers:
                            f.loops[j][dst].uv = loop[src].uv
                        # vertex colors
                        for src, dst in cols_layers:
                            f.loops[j][dst] = loop[src]

                bm.faces.index_update()

            if bm_to_add.edges:

                # bevel
                bevel = bm_to_add.edges.layers.bevel_weight.items()
                bevel_layers = []
                for name, src in bevel:
                    dst = bm.edges.layers.bevel_weight.get(name)
                    if dst is None:
                        dst = bm.edges.layers.bevel_weight.new(name)
                    bevel_layers.append((src, dst))

                # crease
                crease = bm_to_add.edges.layers.crease.items()
                crease_layers = []
                for name, src in crease:
                    dst = bm.edges.layers.crease.get(name)
                    if dst is None:
                        dst = bm.edges.layers.crease.new(name)
                    crease_layers.append((src, dst))

                for edge in bm_to_add.edges:
                    edge_seq = tuple(bm.verts[i.index + offset] for i in edge.verts)
                    try:
                        ed = add_edge(edge_seq)
                        ed.select = edge.select
                        for src, dst in bevel_layers:
                            ed[dst] = edge[src]
                        for src, dst in crease_layers:
                            ed[dst] = edge[src]
                    except ValueError:
                        # edge exists!
                        pass
                bm.edges.index_update()

        # cleanup
        for old_bm in list_of_bmeshes:
            old_bm.free()

        BmeshEdit.ensure_bmesh(bm)

        if temporary:
            return bm
        else:
            BmeshEdit._end(bm, o)

    @staticmethod
    def _end(bm, o):
        """
            private, end bmesh editing of active object
        """
        BmeshEdit.ensure_bmesh(bm)
        bm.normal_update()
        bm.to_mesh(o.data)
        bm.free()

    @staticmethod
    def _matids(bm, matids):
        for f, matid in zip(bm.faces, matids):
            f.material_index = matid

    @staticmethod
    def normal_update(bm):
        for f in bm.faces:
            f.normal_update()

    @staticmethod
    def _uvs(bm, uvs, layer_name):
        layer = bm.loops.layers.uv.verify()
        # layer = bm.loops.layers.uv.get(layer_name)
        # if layer is None:
        #    layer = bm.loops.layers.uv.new(layer_name)

        if len(bm.faces) > len(uvs):
            raise RuntimeError("Faces uvs count mismatch")

        for uv, face in zip(uvs, bm.faces):
            if len(face.loops) > len(uv):
                raise RuntimeError("Uv for face count mismatch")

            for co, loop in zip(uv, face.loops):
                loop[layer].uv = co

    @staticmethod
    def _vertex_color(bm, colors, layer_name):
        vlay = bm.loops.layers.color.get(layer_name)
        if vlay is None:
            vlay = bm.loops.layers.color.new(layer_name)

        for color, face in zip(colors, bm.faces):
            for loop in face.loops:
                loop[vlay] = color

    @staticmethod
    def _verts(bm, verts):
        for i, v in enumerate(verts):
            bm.verts[i].co = v
    
    @staticmethod
    def emptymesh(o):
        bm = BmeshEdit._start(o)
        bm.clear()
        BmeshEdit.ensure_bmesh(bm)
        BmeshEdit._end(bm, o)
        
    @staticmethod
    def buildmesh(o, verts, faces,
            matids=None, uvs=None, vcolors=None, edges=None, uvs_layer="Archipack", color_layer="Archipack", weld=False,
            clean=False, temporary=False):
        
        tim = time.time()
        # logger.debug("BmeshEdit.buildmesh() %s start", o.name)

        if temporary:
            bm = bmesh.new(use_operators=True)
        else:
            bm = BmeshEdit._start(o)
            bm.clear()

        # logger.debug("BmeshEdit.buildmesh() %s :%.2f seconds", o.name, time.time() - tim)

        # BmeshEdit.ensure_bmesh(bm)
        _verts = bm.verts
        _faces = bm.faces
        _edges = bm.edges

        _new_vert = _verts.new
        _new_face = _faces.new
        _new_edge = _edges.new

        for v in verts:
            _new_vert(v)
        
        _verts.ensure_lookup_table()
        _verts.index_update()

        # logger.debug("BmeshEdit.buildmesh() verts :%.2f seconds", time.time() - tim)
        if edges is not None:
            for ed in edges:
                v0, v1 = ed
                _new_edge(_verts[v0], _verts[v1])

        for f in faces:
            _new_face([_verts[i] for i in f])
        
        # bm.edges.ensure_lookup_table()
        _faces.ensure_lookup_table()
        _faces.index_update()
        # logger.debug("BmeshEdit.buildmesh() faces :%.2f seconds", time.time() - tim)
        
        if matids is not None:
            BmeshEdit._matids(bm, matids)
            # logger.debug("BmeshEdit.buildmesh() _matids :%.2f seconds", time.time() - tim)
        
        if uvs is not None:
            BmeshEdit._uvs(bm, uvs, uvs_layer)

        if vcolors is not None:
            BmeshEdit._vertex_color(bm, vcolors, color_layer)

        if weld:
            # logger.debug("BmeshEdit.buildmesh() weld :%.2f seconds", time.time() - tim)
            bmesh.ops.remove_doubles(bm, verts=bm.verts, dist=0.001)

        if clean:
            # logger.debug("BmeshEdit.buildmesh() clean :%.2f seconds", time.time() - tim)
            bmesh.ops.dissolve_degenerate(bm, edges=bm.edges, dist=0.001)

        if temporary:
            BmeshEdit.ensure_bmesh(bm)
            # return a temporary bmesh
            return bm

        BmeshEdit._end(bm, o)
        # logger.debug("BmeshEdit.buildmesh() _end :%.2f seconds", time.time() - tim)
        # logger.debug("BmeshEdit.buildmesh() mesh ops :%.2f seconds", time.time() - tim)

    @staticmethod
    def bevel(context, o,
            offset,
            offset_type='OFFSET',
            segments=1,
            profile=0.5,
            vertex_only=False,
            clamp_overlap=True,
            material=-1,
            use_selection=True):
        """
        /* Bevel offset_type slot values */
        enum {
          BEVEL_AMT_OFFSET,
          BEVEL_AMT_WIDTH,
          BEVEL_AMT_DEPTH,
          BEVEL_AMT_PERCENT
        };
        """
        bm = bmesh.new()
        bm.from_mesh(o.data)
        BmeshEdit.ensure_bmesh(bm)
        if use_selection:
            geom = [v for v in bm.verts if v.select]
            geom.extend([ed for ed in bm.edges if ed.select])
        else:
            geom = bm.verts[:]
            geom.extend(bm.edges[:])

        bmesh.ops.bevel(bm,
            geom=geom,
            offset=offset,
            offset_type=offset_type,
            segments=segments,
            profile=profile,
            vertex_only=vertex_only,
            clamp_overlap=clamp_overlap,
            material=material)

        bm.to_mesh(o.data)
        bm.free()

    @staticmethod
    def solidify(context, o, amt, floor_bottom=False, altitude=0):
        bm = bmesh.new()
        bm.from_mesh(o.data)
        BmeshEdit.ensure_bmesh(bm)
        
        geom = bm.faces[:]
        bmesh.ops.solidify(bm, geom=geom, thickness=amt)
        if floor_bottom:
            for v in bm.verts:
                if not v.select:
                    v.co.z = altitude
        bm.to_mesh(o.data)
        bm.free()

    @staticmethod
    def bisect(bm,
            plane_co,
            plane_no,
            dist=1e-6,
            use_snap_center=False,
            clear_outer=False,
            clear_inner=False,
            geom=None
            ):
        _geom = geom
        if _geom is None:
            _geom = bm.verts[:]
            _geom.extend(bm.edges[:])
            _geom.extend(bm.faces[:])

        bmesh.ops.bisect_plane(bm,
            geom=_geom,
            dist=dist,
            plane_co=plane_co,
            plane_no=plane_no,
            use_snap_center=use_snap_center,
            clear_outer=clear_outer,
            clear_inner=clear_inner
            )