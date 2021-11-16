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
import json
import sys
import os
import re


def log(str):
    print("[log]{}\n".format("\n[log]".join(str.split("\n"))))


def cleanup_node_groups(material, group_names):
    """ Use library file node groups when found by name
    :param material:
    :param group_names:
    :return:
    """
    if material.use_nodes:
        tree = material.node_tree
        # use node groups with same names
        for node in tree.nodes:
            if node.type == 'GROUP' and node.node_tree is not None:
                name = node.node_tree.name
                # find matching group by name
                for group_name in group_names:
                    res = re.match(r"{}([0-9\.]+)".format(group_name), name)
                    if res is not None:
                        old = node.node_tree
                        node.node_tree = bpy.data.node_groups[group_name]
                        if old.users < 1:
                            bpy.data.node_groups.remove(old)
                        break


def cleanup_textures(material, image_names):
    """ Use library file textures when found by name
    :param material:
    :param group_names:
    :return:
    """
    if material.use_nodes:
        tree = material.node_tree
        for node in tree.nodes:
            if node.type == 'TEX_IMAGE' and node.image is not None:
                name = node.image.name
                # find matching group by name
                for image_name in image_names:
                    res = re.match(r"{}([0-9\.]+)".format(image_name), name)
                    if res is not None:
                        old = node.image
                        node.image = bpy.data.images[image_name]
                        if old.users < 1:
                            bpy.data.images.remove(old)
                        break


def save_material(src_file, materials, clean):
    """Save / override a list of materials to library
    :param src_file: source file full name
    :param materials: json array of materials names
    :return:
    """
    log("\n******************************\nArchipack save materials:\n   %s\n" % "\n   ".join(materials))

    group_names = [d.name for d in bpy.data.node_groups]
    image_names = [d.name for d in bpy.data.images]

    # clear old if any
    for name in materials:
        old = bpy.data.materials.get(name)
        if old:
            bpy.data.materials.remove(old)
            # try to clear unused textures too
            to_remove = [tex for tex in bpy.data.images if tex.users < 1]
            for tex in to_remove:
                bpy.data.images.remove(tex)

    try:
        with bpy.data.libraries.load(src_file, False, False) as (data_from, data_to):
            data_to.materials = materials

        for mat in materials:
            mat.use_fake_user = True
            cleanup_node_groups(mat, group_names)
            cleanup_textures(mat, image_names)

        # cleanup nested node_groups without any users
        to_remove = [g for g in bpy.data.node_groups if g.users < 1]
        for g in to_remove:
            bpy.data.node_groups.remove(g)

        bpy.ops.file.pack_all()
        bpy.ops.wm.save_mainfile(check_existing=False)

    except Exception as ex:
        log("Archipack: Error while saving materials %s" % ex)
        pass
    log("******************************")
    # return
    if clean:
        try:
            os.remove(src_file)
        except:
            pass


def wait(src_file, timestamp, max):
    import time
    attempts = 0
    while os.path.getmtime(src_file) < timestamp and attempts < max:
        time.sleep(1)
        attempts += 1
        log("Wait temp file attempts", attempts)
    return attempts < max


if __name__ == "__main__":

    for arg in sys.argv:
        if arg.startswith("file:"):
            src_file = arg[5:]
        if arg.startswith("name:"):
            # reformat json
            print("arg:", arg[5::])
            fstr = arg[5::].strip().replace("[", "").replace("]", "").replace("'", "").replace("\"", "")
            print("fstr:", fstr)
            jstr = '["{}"]'.format('","'.join(fstr.split(",")))
            print("jstr:", jstr)
            materials = json.loads(jstr)
            print(materials)
        if arg.startswith("clean:"):
            clean = int(arg[6:]) == 1
        if arg.startswith("stime:"):
            timestamp = float(arg[6:])

    # saving file might be delayed, so wait for it max 30 sec
    res = wait(src_file, timestamp, 30)
    if res:
        save_material(src_file, materials, clean)
    else:
        log("Archipack: Error while saving materials temp file not found")