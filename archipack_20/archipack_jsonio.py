import json
import inspect
from bpy.types import PropertyGroup


def to_dict(_d, blacklist):
    """
    :param _d: rna_property
    :param blacklist: set of attributes name to skip
    :return: dict
    """
    def _walk(value):
        if isinstance(value, PropertyGroup):
            return dict(
                (rna_name, _walk(getattr(value, rna_name)))
                for rna_name, rna_prop in value.bl_rna.properties.items()
                if not (rna_name in blacklist or rna_prop.is_hidden or rna_prop.is_skip_save)
            )
        elif type(value).__name__ == "bpy_prop_collection_idprop":
            return [_walk(prop)
                    for prop in value
                    if prop.bl_rna.name not in blacklist
                    ]
        else:
            try:
                value = value[:]
            except:
                pass
            return value

    return _walk(_d)


def from_dict(_d, _dict):

    def _walk(prop, value):
        if isinstance(prop, PropertyGroup):
            for attr, sub_value in value.items():
                if type(sub_value).__name__ in ('dict', 'list'):
                    try:
                        # recursion for nested PropertyGroups / CollectionProperties
                        sub_prop = getattr(prop, attr)
                        _walk(sub_prop, sub_value)
                    except:
                        pass
                # Property (also VectorProperties)
                try:
                    setattr(prop, attr, sub_value)
                except:
                    pass
        elif type(prop).__name__ == "bpy_prop_collection_idprop":
            prop.clear()
            for sub_value in value:
                sub_prop = prop.add()
                _walk(sub_prop, sub_value)

    _walk(_d, _dict)


class ArchipackJsonIO:

    @staticmethod
    def to_json(obj, key, blacklist):

        bl = set(blacklist)
        bl.update(["name", "version",
                   "dimension_points", "dimension_uid",
                   "manipulators",
                   "closed",
                   "user_defined_path", "user_defined_spline", "user_defined_resolution", "user_defined_reverse"])

        _d = getattr(obj.data, key)[0]
        _mat = obj.archipack_material[0]
        _dict = {
            key: to_dict(_d, bl),
            'archipack_material': to_dict(_mat, set(['name']))
        }
        return json.dumps(_dict, indent=None, sort_keys=True)

    @staticmethod
    def from_json(obj, _json):
        _dict = json.loads(_json)
        for key in _dict.keys():
            if "archipack_material" not in key:
                _d = getattr(obj.data, key)[0]
                _mat = obj.archipack_material[0]
                # _d.auto_update = False
                _dict[key]['auto_update'] = False
                from_dict(_d, _dict[key])
                from_dict(_mat, _dict['archipack_material'])
                # _d.auto_update = True
