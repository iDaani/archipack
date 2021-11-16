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
# ----------------------------------------------------------
from bpy.utils import previews
import bgl
import bpy
import sys
import os

# use direct gl from preview.icons instead of bpy.data.images
# cause heap memory leak
USE_GL_PREVIEW = False # sys.platform != 'win32'


class GlImageFromPreview:
    """
     Drop in replacement for bpy.data.images
     to display preview icons right in gl
    """
    __slots__=('_bo', '_image', 'preview')

    def __init__(self, preview):
        self._bo = None
        self._image = None
        self.preview = preview

    def gl_load(self):

        if USE_GL_PREVIEW:
            _p = self.preview
            _bo = bgl.Buffer(bgl.GL_INT, [1])
            _pix = _p.image_pixels_float[:]
            w, h = _p.image_size
            buf = bgl.Buffer(bgl.GL_FLOAT, len(_pix), _pix)
            bgl.glGenTextures(2, _bo)
            bgl.glBindTexture(bgl.GL_TEXTURE_2D, _bo[-1])
            bgl.glTexImage2D(bgl.GL_TEXTURE_2D, 0, bgl.GL_RGBA, w, h, 0, bgl.GL_RGBA, bgl.GL_FLOAT, buf)
            bgl.glTexParameteri(bgl.GL_TEXTURE_2D, bgl.GL_TEXTURE_MIN_FILTER, bgl.GL_NEAREST)
            bgl.glTexParameteri(bgl.GL_TEXTURE_2D, bgl.GL_TEXTURE_MAG_FILTER, bgl.GL_NEAREST)
            bgl.glBindTexture(bgl.GL_TEXTURE_2D, 0)
            self._bo = _bo
        else:
            self._image = self.as_blender_image(self.preview)
            self._image.gl_load()

    def as_blender_image(self, preview):
        w, h = preview.image_size
        image = bpy.data.images.new("Icon_{}".format(preview.icon_id), width=w, height=h)
        if hasattr(image, "colorspace_settings"):
            image.colorspace_settings.name = 'Raw'
        image.pixels = preview.image_pixels_float[:]
        return image

    def gl_free(self):
        if self._bo is not None:
            # memory leak under w10
            bgl.glDeleteTextures(2, self._bo)
            self._bo = None

        elif self._image is not None:
            self._image.gl_free()
            bpy.data.images.remove(self._image)
            self._image = None

    @property
    def bindcode(self):

        if self._image is not None:
            return self._image.bindcode

        elif self._bo is None:
            return 0

        return self._bo[-1]


class IconsCollectionManager(dict):
    """
     Global icons management for archipack ui and presets
     Handle icons for ui
     Provide 
      - loading icon from folder 
      - reload single icon after change
      - remove icon / category
      - new icons loading after render
      - watch for icon changes on folder
     Icons for presets:
      - Icons for gl menuitems
      - Icons for dynamic enum for use in template_icon_view
    """
    def __init__(self, force_reload=True):
        dict.__init__(self)
        # category files remove
        self._refresh = {}
        # category files add/change watcher
        self._watcher = {}
        # hold reference to icon enums
        self._enums = {}
        self._folders = {}
        self.force_reload = force_reload
        self.allowed_ext = {
            '.png',
            '.jpg'
            }
        self._menuItems = []

    def add(self, category):
        """
         Add a category
        """
        if category not in self.keys():
            self._enums[category] = []
            self._folders[category] = {}
            self[category] = previews.new()

    def remove(self, category):
        """
         Remove a category
        """
        if category in self:
            self._enums[category].clear()
            self._folders[category].clear()
            self[category].close()
            previews.remove(self[category])
            del self[category]
            # print("icon.remove()", category)
        if category in self._watcher:
            del self._watcher[category]
        if category in self._refresh:
            del self._refresh[category]

    def add_files(self, files, category, format):
        """
         Add files in list
         @folder: folder with images
         @files: array of (full file path, filename.ext)
         @category: category name
        """
        folders = self._folders[category]
        icons = self[category]
        for folder, file in files:
            name, ext = os.path.splitext(file)
            if ext in self.allowed_ext and name not in icons:
                preset = os.path.join(folder, "{}.{}".format(name, format))
                if os.path.exists(preset) and name not in icons:
                    thumb = os.path.join(folder, file)
                    if name not in folders:
                        # translate thumbs labels
                        label = bpy.path.display_name(name)
                        folders[name] = {
                            'label': label,
                            'file_name': name,
                            'full_path': preset
                            }
                    icons.load(
                        name,
                        thumb,
                        'IMAGE',
                        force_reload=self.force_reload)

    def load(self, folder, category, format="png"):
        """
         Load files from folder
         @folder: folder with images
         @category: category name
        """
        self.add(category)
        if os.path.exists(folder):
            files = [(folder, file) for file in os.listdir(folder)]
            # print("IconsCollectionManager.load_presets", category, len(files), folder)
            self.add_files(files, category, format)
        # else:
        #    print("IconsCollectionManager.load_presets folder not found:", category, folder)

    def load_presets(self, category, format):
        """
         Load thumbs from archipack's preset dir,
         and all presets/category sub folders
         User presets override factory one
        """
        folders = bpy.utils.script_paths("presets")
        # append after user script so user override factory
        if category not in self:
            # only load factory on startup
            # so user is able to remove and override
            folders.append(os.path.join(
                os.path.dirname(__file__),
                "presets"))

        for folder in folders:
            self.load(os.path.join(folder, category), category, format)

    def add_preset(self, category, name, format):
        """
          Add / Replace a preset thumb (from user path only)
          call this when a preset is saved
          rescan folder until the count of images grow
        """
        self._watcher.setdefault(category, {})

        # refresh enum until file is loaded
        self._watcher[category][name] = True

        if category not in self:
            self.load_presets(category, format)

        if name in self[category]:
            self._folders[category].pop(name)
            self[category].pop(name)

    def remove_preset(self, category, name):
        """
          Remove a preset thumb (from user path only)
          call this when a preset is saved so enum will refresh from files
        """
        if name in self[category]:
            del self._folders[category][name]
            self[category].pop(name)

        if category in self._watcher and name in self._watcher[category]:
            del self._watcher[category][name]

        # ensure enum rescan folder once on next run
        self._refresh[category] = True

    def cleanup(self):
        """
         Clear all icons in all category
        """
        self._enums.clear()
        self._folders.clear()
        self._watcher.clear()
        self._refresh.clear()
        for full_path, label, image in self._menuItems:
            image.gl_free()
        self._menuItems.clear()
        for icons in self.values():
            previews.remove(icons)
        self.clear()

    def enum(self, context, category, format="json"):
        """
         Build and return icon enumerator
        """
        # load missing category
        if category not in self:
            self.load_presets(category, format)

        # refresh enum on file delete
        if category in self._refresh:
            del self._refresh[category]

        # reload until watched images are found
        if category in self._watcher:
            kill_watcher = True
            for name in self._watcher[category]:
                if name not in self[category]:
                    kill_watcher = False
            if kill_watcher:
                for name in self._watcher[category]:
                    # print("IconsCollectionManager.reload", category, name)
                    self[category][name].reload()
                del self._watcher[category]
            else:
                self.load_presets(category, format)

        self._enums[category].clear()
        folders = self._folders[category]
        items = list(self[category].items())
        items.sort(key=lambda i: i[0].lower())
        for i, item in enumerate(items):
            name, preview = item
            self._enums[category].append((
                folders[name]['file_name'],
                folders[name]['label'],
                folders[name]['label'],
                preview.icon_id,
                i
                ))

        return self._enums[category]

    def menuitems(self, category, format):
        """
         Build preset menu thumb compatible enum
         Using Image
        """
        # refresh thumbs
        self.load_presets(category, format)

        folders = self._folders[category]
        items = list(self[category].items())
        items.sort(key=lambda i: i[0].lower())
        self._menuItems.clear()
        self._menuItems.extend([(
                folders[name]['full_path'],
                folders[name]['label'],
                GlImageFromPreview(preview)
                )
                for name, preview in items])
        return self._menuItems

icons = IconsCollectionManager()


def unregister():
    global icons
    icons.cleanup()
