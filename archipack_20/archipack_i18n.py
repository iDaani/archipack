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
import os
import bpy
import json
from bpy.types import (
    Operator,
    PropertyGroup,
    Panel
)
from bpy.props import (
    StringProperty,
    CollectionProperty,
    BoolProperty,
    EnumProperty
)
from .archipack_prefs import get_prefs
from .archipack_object import ArchipackPanel
from bl_i18n_utils.settings import LANGUAGES

"""
import json
with open("/home/stephen/.config/blender/2.80/scripts/addons/archipack28/lang/lang.json", "r") as f:
    j = json.load(f)


# compact form for lang file (75%)
res = {"archipack":{}, "*":{}, "locales":{locale[0:2]: locale for locale in j.keys()}}
locale_map = {locale: short for short, locale in res['locales'].items()}

for locale, ctx in j.items():
    loc = locale_map[locale]
    for k_ctx, trs in ctx.items():
        for src, dst in trs.items():
            res[k_ctx].setdefault(src, {})
            res[k_ctx][src][loc] = dst

with open("/home/stephen/.config/blender/2.80/scripts/addons/archipack28/lang/lang_2.json", "w") as f:
    json.dump(res, f)


# bulk flat form
for key, _dict in j["fr_FR"].items():
    with open("/tmp/archipack_{}.txt".format(key.replace("*", "tips")), "w") as f:    
        f.write("\n".join(_dict.keys()))
        
# translate files and save as archipack_locale_[archipack|tips].txt
        
locales = ["ja_JP", "th_TH", "it_IT", "hi_IN", "pt_PT", "es_ES", "de_DE", "pl_PL", "ru_RU", "hu_HU", "cs_CZ", "sv_SE", "nl_NL"]

for locale in locales:
    j[locale] = {}
    for key, _dict in j["fr_FR"].items():
        j[locale][key] = {}
        with open("/tmp/archipack_{}_{}.txt".format(locale, key.replace("*", "tips")), "r") as f:    
            str = f.read()
            trs = str.split("\n")
            for k, s in zip(j['fr_FR'][key].keys(), trs):
                j[locale][key][k] = s
 

with open("/home/stephen/.config/blender/2.80/scripts/addons/archipack28/lang/lang.json", "w") as f:
    json.dump(j, f)
               
            
"""


trans_dict = {}

# Note:
# description "*"
# name "archipack"

def LoadTranslationDict():

    path = os.path.join(os.path.dirname(__file__), "lang", "lang.json")
    try:
        with open(path, 'r') as f:
            translations = json.load(f)

        for locale, contexts in translations.items():
            trans_dict[locale] = {}
            for ctx, translation in contexts.items():
                for src, dst in translation.items():
                    trans_dict[locale][(ctx, src)] = dst
    except:
        # print("Archipack: Error reading i18n translations")
        # import traceback
        # traceback.print_exc()
        pass


visited_dict = {}


def SaveTranslationDict():

    path = os.path.join(os.path.dirname(__file__), "lang", "lang.json")
    translations = {}
    for locale, contexts in trans_dict.items():
        translations[locale] = {}

        tmp = []
        for ctx_src, trs in contexts.items():
            ctx, src = ctx_src
            tmp.append((ctx, src, trs))
            translations[locale].setdefault(ctx, {})
            # translations[locale][ctx][src] = trs

        tmp.sort(key=lambda x: x[1])
        for ctx, src, trs in tmp:
            translations[locale][ctx][src] = trs

    with open(path, 'w') as f:
        json.dump(translations, f)


def update(self, context):
    self.update(context)


class archipack_translation_field(PropertyGroup):
    t_ctx: StringProperty()
    source: StringProperty()
    label: StringProperty()
    translation: StringProperty(update=update)

    def update(self, context):
        locale = context.preferences.view.language
        trans_dict.setdefault(locale, {})
        trans_dict[locale][(self.t_ctx, self.source)] = self.translation
        visited_dict[(self.t_ctx, self.source)] = self.translation

    def draw_field(self, layout, typ):
        layout.prop(self, "translation", text=typ)

    def draw(self, layout, i):
        row=layout.row(align=True)
        row.label(text="{}  {}".format(str(i).ljust(4, " "), self.label), translate=False)
        row.prop(self, "translation", text="", translate=False)


locales = []

def enum_locales(self, context):
    locales.clear()
    locales.append(('DEFAULT', 'Automatic (Automatic)', 'Automatic (Automatic)', 0))
    locales.extend([(locale, bpy.app.translations.pgettext(label), bpy.app.translations.pgettext(label), i + 1)
                    for i, label, locale in LANGUAGES
                    if locale in trans_dict
                    ])
    return locales


def update(self, context):
    self.load(context)


class archipack_translation(PropertyGroup):

    translations: CollectionProperty(type=archipack_translation_field)
    locale: EnumProperty(
        items=enum_locales,
        update=update
    )
    display: BoolProperty(default=False)
    sort: EnumProperty(
        name="Sort fields",
        items=(
            ("SOURCE", "Sort by source", "Sort by source", 0),
            ("TRANS", "Sort by translation", "Sort by translation", 1),
            ("MISSING", "Missing (source = translated)", "Fields with source = translated", 2)
        )
    )

    def refresh(self, context):
        for ctx_src, translation in visited_dict.items(): #trans_dict[locale].items():
            t_ctx, source = ctx_src
            if self.locale == 'DEFAULT' or self.locale not in trans_dict:
                label = source
            else:
                if (t_ctx, source) in trans_dict[self.locale]:
                    label = trans_dict[self.locale][(t_ctx, source)]
                else:
                    label = source

            self.add(t_ctx, source, label, translation)

    def load(self, context):
        locale = context.preferences.view.language

        self.translations.clear()

        if locale not in trans_dict:
            trans_dict[locale] = {}

        # ensure all fields are available
        for ctx_src, trans in trans_dict["fr_FR"].items():
            t_ctx, k = ctx_src
            if (t_ctx, k) not in trans_dict[locale]:
                trans_dict[locale][(t_ctx, k)] = k

        for ctx_src, translation in trans_dict[locale].items():
            t_ctx, source = ctx_src
            if self.locale == 'DEFAULT' or self.locale not in trans_dict:
                label = source
            else:
                if (t_ctx, source) in trans_dict[self.locale]:
                    label = trans_dict[self.locale][(t_ctx, source)]
                else:
                    label = source

            self.add(t_ctx, source, label, translation)

    def add(self, t_ctx, source, label, translation):
        for t in self.translations:
            if t.t_ctx == t_ctx and t.source == source:
                t.translation = translation
                return
        t = self.translations.add()
        t.t_ctx = t_ctx
        t.source = source
        t.label = label
        t.translation = translation

    def translate(self, layout, txt, locale, t_ctx, typ):
        if txt != "":
            if (t_ctx, txt) not in trans_dict[locale]:
                trans_dict[locale][(t_ctx, txt)] = txt

            if (t_ctx, txt) not in visited_dict:
                visited_dict[(t_ctx, txt)] = trans_dict[locale][(t_ctx, txt)]

            for t in self.translations:
                if t.t_ctx == t_ctx and t.source == txt:
                    t.draw_field(layout, typ)
                    return

    def draw_field(self, context, layout, name, prop, typ):

        locale = context.preferences.view.language
        trans_dict.setdefault(locale, {})
        if typ == "header":
            row = layout
        else:
            row = layout.row(align=True)
        if typ == "enum":
            t_ctx = "*"
        else:
            t_ctx = "archipack"
        self.translate(row, name, locale, t_ctx, typ)
        if prop is not None and hasattr(prop, "description"):
            self.translate(row, prop.description, locale, "*", "")

    def draw(self, context, layout):
        locale = context.preferences.view.language

        row = layout.row(align=True)
        row.operator("archipack.translation_load", text="load / create {}".format(locale))
        row.prop(self, "sort", text="")

        row = layout.row(align=True)
        row.prop(self, "locale", text="source")
        row.prop(context.preferences.view, "language", text="translation")

        if self.sort == 'SOURCE':
            sort = {t.label: i for i, t in enumerate(self.translations)}
        elif self.sort == 'TRANS':
            sort = {t.translation: i for i, t in enumerate(self.translations)}
        else:
            sort = {t.label: i for i, t in enumerate(self.translations) if t.source == t.translation}

        order = list(sort.keys())
        order.sort()
        for i, key in enumerate(order):
            if (i % 25) == 0:
                row = layout.row(align=True)
                row.operator("archipack.translation_save", icon="LINENUMBERS_ON")
            self.translations[sort[key]].draw(layout, i)


class Archipacki18n:

    # Ensure panel labels are translated in archipack context
    translation_context = "archipack"

    def draw_translation(self, context, layout, typ, text, description=None):
        if hasattr(context.window_manager, "archipack"):
            t = context.window_manager.archipack.translation
            t.draw_field(context, layout, text, description, typ)

    def draw_header(self, context):
        layout = self.layout
        if self.show_translate(context):
            self.draw_translation(context, layout, "header", self.bl_label)

    def show_translate(self, context):
        prefs = get_prefs(context)
        if hasattr(context.window_manager, "archipack"):
            t = context.window_manager.archipack.translation
            return prefs.translate and t.display
        return False

    def draw_translate(self, context, layout):
        prefs = get_prefs(context)
        if prefs.translate:
            if hasattr(context.window_manager, "archipack"):
                t = context.window_manager.archipack.translation
                row = layout.row(align=True)
                row.operator("archipack.translation_refresh", icon="LINENUMBERS_ON")
                row.operator("archipack.translation_save", icon="LINENUMBERS_ON")
                icon = 'HIDE_ON'
                if t.display:
                    icon = 'HIDE_OFF'
                row.prop(t, "display", text="", icon=icon)

    def translate(self, label):
        return bpy.app.translations.pgettext(label, self.translation_context)

    def draw_prop(self, context, layout, where, d, attr,
                  icon='NONE', expand=False, emboss=True, text=None, toggle=False, icon_value=0,
                  postfix=None, index=-1):
        translate = True
        rna_prop = d.bl_rna.properties[attr]

        label = rna_prop.name

        if text is not None:
            label = text

        name = label

        if postfix is not None:
            label = "{} {}".format(
                self.translate(label),
                postfix
            )
            translate = False

        show_translate = self.show_translate(context)

        if rna_prop.type == 'ENUM':
            where.prop(d,
                        attr,
                        text=label,
                        text_ctxt=self.translation_context,
                        translate=translate,
                        expand=expand)

            if show_translate:
                box=layout.box()
                self.draw_translation(context, box, "prop", name, rna_prop)
                for item in rna_prop.enum_items:
                    self.draw_translation(context, box, "enum", item.name, item)

        elif rna_prop.type == 'BOOLEAN':
            where.prop(d,
                        attr,
                        text=label,
                        text_ctxt=self.translation_context,
                        translate=translate,
                        icon=icon,
                        icon_value=icon_value,
                        emboss=emboss,
                        toggle=toggle,
                        index=index
                        )
            if show_translate:
                self.draw_translation(context, layout, "prop", name, rna_prop)

        else:
            where.prop(d,
                        attr,
                        text=label,
                        text_ctxt=self.translation_context,
                        translate=translate,
                        icon=icon,
                        icon_value=icon_value,
                        index=index)

            if show_translate:
                self.draw_translation(context, layout, "prop", name, rna_prop)

    def draw_label(self, context, layout, where, text, postfix=None):
        label = text
        translate = True
        if postfix is not None:
            label = "{} {}".format(
                self.translate(label),
                postfix
            )
            translate = False

        where.label(text=label,
                    text_ctxt=self.translation_context,
                    translate=translate)

        if self.show_translate(context):
            self.draw_translation(context, layout, "label", text)

    def draw_op(self, context, layout, where, name, icon='NONE', text=None, icon_value=0, postfix=None):
        prefix, op_name = name.split(".")
        op = getattr(getattr(bpy.ops, prefix), op_name).get_rna_type()
        label = op.name

        if text is not None:
            label = text

        op_name = label
        translate = True
        if postfix is not None:
            label = "{} {}".format(
                self.translate(label),
                postfix
            )
            translate = False

        res = where.operator(name,
                            text=label,
                            translate=translate,
                            text_ctxt=self.translation_context,
                            icon=icon,
                            icon_value=icon_value)

        if self.show_translate(context):
            self.draw_translation(context, layout, "op", op_name, op)

        return res


class ARCHIPACK_OT_translation_refresh(Operator):
    bl_idname = "archipack.translation_refresh"
    bl_label = "Refresh translation"
    bl_description = "Load translation fields on screen"

    def execute(self, context):

        if len(trans_dict) == 0:
            LoadTranslationDict()
        if hasattr(context.window_manager, "archipack"):
            t = context.window_manager.archipack.translation
            t.refresh(context)
            t.display = True
        return {'FINISHED'}


class ARCHIPACK_OT_translation_save(Operator):
    bl_idname = "archipack.translation_save"
    bl_label = "Save translations"
    bl_description = "Save translation to file"

    def execute(self, context):
        SaveTranslationDict()
        bpy.app.translations.unregister(__package__)
        bpy.app.translations.register(__package__, trans_dict)
        return {'FINISHED'}


class ARCHIPACK_OT_translation_load(Operator):
    bl_idname = "archipack.translation_load"
    bl_label = "Load all translations"
    bl_description = "Load translation fields"

    def execute(self, context):

        if len(trans_dict) == 0:
            LoadTranslationDict()
        if hasattr(context.window_manager, "archipack"):
            t = context.window_manager.archipack.translation
            t.load(context)
            t.loaded = True
        return {'FINISHED'}


class ARCHIPACK_PT_translation(ArchipackPanel, Panel):
    bl_idname = "ARCHIPACK_PT_translation"
    bl_label = "Translations"
    bl_category = "Translate Archipack"

    @classmethod
    def poll(cls, context):
        prefs = get_prefs(context)
        return prefs.translate

    def draw(self, context):
        if hasattr(context.window_manager, "archipack"):
            t = context.window_manager.archipack.translation
            layout = self.layout

            t.draw(context, layout)


def register():
    LoadTranslationDict()
    bpy.app.translations.register(__package__, trans_dict)
    bpy.utils.register_class(archipack_translation_field)
    bpy.utils.register_class(archipack_translation)
    bpy.utils.register_class(ARCHIPACK_OT_translation_refresh)
    bpy.utils.register_class(ARCHIPACK_OT_translation_save)
    bpy.utils.register_class(ARCHIPACK_OT_translation_load)
    bpy.utils.register_class(ARCHIPACK_PT_translation)


def unregister():
    try:
        bpy.utils.unregister_class(ARCHIPACK_PT_translation)
    except:
        pass
    bpy.utils.unregister_class(ARCHIPACK_OT_translation_refresh)
    bpy.utils.unregister_class(ARCHIPACK_OT_translation_save)
    bpy.utils.unregister_class(ARCHIPACK_OT_translation_load)
    bpy.utils.unregister_class(archipack_translation)
    bpy.utils.unregister_class(archipack_translation_field)
    bpy.app.translations.unregister(__package__)
    trans_dict.clear()
