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
import subprocess
from bpy.types import Operator
from bpy.props import StringProperty


PYPATH = bpy.app.binary_path_python


class Pip:

    def __init__(self):
        self._ensurepip()

    def _cmd(self, action, options, module):

        cmd = [PYPATH, "-m", "pip", action]

        if options is not None:
            cmd.extend(options.split(" "))

        cmd.append(module)
        return self._run(cmd)

    def _popen(self, cmd):
        popen = subprocess.Popen(cmd, stdout=subprocess.PIPE, universal_newlines=True)
        for stdout_line in iter(popen.stdout.readline, ""):
            yield stdout_line
        popen.stdout.close()
        popen.wait()

    def _run(self, cmd):
        res = False
        status = ""
        for line in self._popen(cmd):
            if "ERROR:" in line:
                status = line.strip()
            print(line)
            if "Successfully" in line:
                status = line.strip()
                res = True
        return res, status

    def _ensurepip(self):
        pip_not_found = False
        try:
            import pip
        except ImportError:
            pip_not_found = True
            pass
        if pip_not_found:
            self._run([PYPATH, "-m", "ensurepip", "--default-pip"])

    @staticmethod
    def upgrade_pip():
        return Pip()._cmd("install", "--upgrade", "pip")

    @staticmethod
    def uninstall(module, options=None):
        """
        :param module: string module name with requirements see:[1]
        :param options: string command line options  see:[2]
        :return: True on uninstall, False if allready removed, raise on Error
        [1] https://pip.pypa.io/en/stable/reference/pip_install/#id29
        [2] https://pip.pypa.io/en/stable/reference/pip_install/#id47
        """
        if options is None or options == "":
            # force confirm
            options = "-y"
        return Pip()._cmd("uninstall", options, module)

    @staticmethod
    def install(module, options=None):
        """
        :param module: string module name with requirements see:[1]
        :param options: string command line options  see:[2]
        :return: True on install, False if already there, raise on Error
        [1] https://pip.pypa.io/en/stable/reference/pip_install/#id29
        [2] https://pip.pypa.io/en/stable/reference/pip_install/#id47
        """
        if options is None or options == "":
            # store in user writable directory, use wheel, without deps
            options = "--user --only-binary all --no-deps"
        return Pip()._cmd("install", options, module)


"""
Pip.install("scipy==1.2.1")
Pip.install("pyproj==2.1.3")
Pip.install("Pillow==6.0.0")
# setup and build GDAL under linux disto, must call apt-get install gdal before 
# cd /usr/src
# wget https://www.python.org/ftp/python/3.5.6/Python-3.5.6.tgz
Pip.install("", "--global-option=build_ext --global-option='-I/usr/include/gdal' GDAL==`gdal-config --version`")
"""


class ARCHIPACK_OT_install_python_module(Operator):

    bl_idname="archipack.install_python_module"
    bl_label="Install python module"
    bl_description = "Install python module in user profile folder"

    module: StringProperty()
    options: StringProperty()

    def execute(self, context):
        if self.module != "":
            res, status = Pip.install(self.module, options=self.options)
            if res:
                self.report({"INFO"}, status)
            else:
                self.report({"WARNING"}, "Module %s not installed %s" % (self.module, status))
        return {'FINISHED'}


class ARCHIPACK_OT_uninstall_python_module(Operator):

    bl_idname="archipack.uninstall_python_module"
    bl_label="Uninstall python module"
    bl_description = "Uninstall python module in user profile folder"

    module: StringProperty()

    def execute(self, context):
        if self.module != "":
            res, status = Pip.uninstall(self.module)
            if res:
                self.report({"INFO"}, status)
            else:
                self.report({"WARNING"}, "Module %s not installed %s" % (self.module, status))
        return {'FINISHED'}



def register():
    bpy.utils.register_class(ARCHIPACK_OT_install_python_module)
    bpy.utils.register_class(ARCHIPACK_OT_uninstall_python_module)


def unregister():
    bpy.utils.unregister_class(ARCHIPACK_OT_install_python_module)
    bpy.utils.unregister_class(ARCHIPACK_OT_uninstall_python_module)


"""
bpy.ops.archipack.install_python_module(module="scipy==1.2.1")

# Pip.install("numpy==1.15.0")
Pip.install("scipy==1.2.1")
# XXX warning this may break your numpy setup !!
# Pip.install("numpy==1.15")
Pip.install("scipy==1.2.1")
Pip.install("pyproj")
Pip.install("Pillow")
"""

