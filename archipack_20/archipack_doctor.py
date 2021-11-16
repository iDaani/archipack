import sys
import os
import bpy

class ArchipackDoctor:

    def __init__(self, tests, break_on_error=True, output=sys.stdout):
        self.archipack_folder = None
        self.errors = []
        self.infos = []
        self.output = output
        self.delimiter()
        self.print_title("Archipack diagnostic")
        self.print_nl()
        self.env()
        self.check(tests, break_on_error)
        self.delimiter()

    @property
    def failed(self):
        return len(self.errors) > 0

    @property
    def success(self):
        return not self.failed

    def info(self, msg):
        self.infos.append(msg)
    
    def error(self, msg):
        self.errors.append(msg)

    def platform(self):
        import os
        self.info(" %s, %s" % (os.sys.platform, os.sys.version.replace("\n", "")))

    def blender(self):
        import bpy
        infos = [x.decode() for x in [
            bpy.app.build_branch,
            bpy.app.build_hash,
            bpy.app.build_date,
            bpy.app.build_time
        ]]
        infos.append(
            "\n ".join([
                bpy.app.binary_path,
                bpy.app.binary_path_python
            ])
        )
        self.info(" build %s %s  %s-%s\n %s" % tuple(infos))

    def exception(self, msg):
        import traceback
        self.error(msg)
        self.error(traceback.format_exc())

    def numpy(self):
        self.info(" import numpy")
        try:
            import numpy
        except:
            self.exception("Unable to import numpy")
            pass

    def json(self):
        self.info(" import json")
        try:
            import json
        except:
            self.exception("Unable to import json")
            pass

    def archipack(self):
        import os
        import bpy
        addon_folder = bpy.utils.user_resource('SCRIPTS', "addons", create=False)
        self.info(" Addons folder: %s" % addon_folder)

        try:
            archipack_folders = [f for f in os.listdir(addon_folder) if "archipack" in f]
        except:
            self.exception("Unable to read addons folder")
            return
            pass

        if len(archipack_folders) == 0:
            self.error("Archipack not found")
            return

        self.info(" Archipack folder(s)\n  - %s" % "\n  - ".join(archipack_folders))

        archipack_module = archipack_folders[0]
        self.archipack_folder = os.path.join(addon_folder, archipack_module)

        self.numpy()
        if self.failed:
            return

        self.info(" import Archipack")
        apk = None
        try:
            import importlib as imp
            apk = imp.import_module(archipack_module)
        except:
            self.exception("Unable to import Archipack")
            return
            pass

        self.info(" Archipack version %s" % apk.__version__)

        return

    def json_load(self, filename):

        self.json()
        if self.failed:
            return

        self.info(" loading %s" % filename)
        import json

        try:
            with open(filename, 'r') as f:
                res = json.load(f)
                if len(res) < 1:
                    raise Exception("Empty json content")
        except:
            self.exception("Unable to load json")
            pass

    def materials_presets(self):

        if self.archipack_folder is None:
            self.check("archipack")
            if self.failed:
                return
            self.infos = []

        import os
        factory_folder = os.path.join(
                self.archipack_folder,
                "presets",
                "archipack_materials"
            )
        self.info(" Factory folder: %s" % factory_folder)

        try:
            factory_presets = os.listdir(factory_folder)
        except:
            self.exception("Factory presets folder not found")
            return
            pass

        self.info(" Found %s presets" % len(factory_presets))

        if len(factory_presets) < 10:
            self.error("Unable to find presets")
            return

        filename = os.path.join(factory_folder, factory_presets[0])
        self.json_load(filename)

    def print_title(self, title):
        print(" %s" % title.capitalize(), file=self.output)

    def print_arr(self, arr):
        print(" %s" % "\n ".join(arr), file=self.output)
        arr.clear()

    def print_nl(self):
        print("", file=self.output)

    def print_state(self, title, failed):
        if not failed:
            state = "Success"
        else:
            state = "Failure"
        self.print_title("%s %s" % (title, state))

    def check(self, tests, break_on_error=True):
        for test in tests:
            getattr(self, test)()
            self.print_title(test)
            self.print_arr(self.infos)
            failed = self.failed
            if failed:
                self.print_arr(self.errors)
            self.print_state(test, failed)
            self.print_nl()
            if failed and break_on_error:
                break

    def env(self):
        for test in ("platform", "blender"):
            getattr(self, test)()
            self.print_title(test)
            self.print_arr(self.infos)
            self.print_nl()

    def delimiter(self):
        print('#######################################', file=self.output)


st = bpy.context.space_data.text
st.clear()

ArchipackDoctor([
    "archipack",
    "materials_presets"
], output=st)

st.filepath = os.path.join(bpy.app.tempdir, "archipack_diagnostic.txt")
bpy.ops.text.jump(line=1)
