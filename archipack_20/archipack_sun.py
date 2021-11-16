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

# --------------------------------------------------------------------------
# Inspired by Blender 2.5 Geographical Sun Add-On
# --------------------------------------------------------------------------
#
# Authors:
# Doug Hammond


import bpy
import datetime
from math import (
        asin, sin, cos,
        tan, atan, radians,
        degrees, floor, pi
)

from bpy.app.handlers import persistent
import time
today = datetime.datetime.now()
 
from bpy.types import (
        Operator,
        Menu,
        Panel,
        PropertyGroup,
        Mesh,
        UIList
)
from bpy.props import (
        BoolProperty,
        FloatProperty,
        StringProperty,
        IntProperty,
        CollectionProperty,
        PointerProperty,
        EnumProperty
)
from mathutils import Vector, Matrix
from .archipack_i18n import Archipacki18n
from .archipack_object import ArchipackObject, ArchipackCreateTool, ArchipackPanel
from .bmesh_utils import BmeshEdit as bmed
from .archipack_generator import Generator


class archipack_timezone(PropertyGroup):
    label: StringProperty()


class archipack_country(PropertyGroup):
    iso: StringProperty()
    label: StringProperty()


class archipack_city(PropertyGroup):
    label: StringProperty()
    lon: FloatProperty()
    lat: FloatProperty()
    zone: StringProperty()


class archipack_sun_presets(PropertyGroup):
    timezones: CollectionProperty(type=archipack_timezone, options={'HIDDEN'})
    city: CollectionProperty(type=archipack_city,options={'HIDDEN'})
    country: CollectionProperty(type=archipack_country,options={'HIDDEN'})


class ARCHIPACK_UL_Timezone(UIList):
    """
    NOTE: not defined using _UL_ as this prevent use_filter_show default value
    """

    def draw_item(self, context, layout, data, item, icon, active_data, active_propname):
        self.use_filter_show = True

        if self.layout_type in {'DEFAULT', 'COMPACT'}:
            # You should always start your row layout by a label (icon + text), or a non-embossed text field,
            # this will also make the row easily selectable in the list! The later also enables ctrl-click rename.
            # We use icon_value of label, as our given icon is an integer value, not an enum ID.
            # Note "data" names should never be translated!
            #split = layout.split(0.3)
            layout.label(text=item.label)
            #split.label(item.description)

        elif self.layout_type in {'GRID'}:
            layout.label(text=item.label)
            pass
    # Called once to filter/reorder items.
    def filter_items(self, context, data, propname):

        col = getattr(data, propname)
        filter_name = self.filter_name.lower()

        flt_flags = [self.bitflag_filter_item if any(
                filter_name in filter_set for filter_set in (
                    str(i), item.label.lower()
                    )
                )
            else 0 for i, item in enumerate(col, 1)
        ]

        if self.use_filter_sort_alpha:
            flt_neworder = [x[1] for x in sorted(
                    zip(
                        [x[0] for x in sorted(enumerate(col), key=lambda x: x[1].label)],
                        range(len(col))
            )
        )
            ]
        else:
            flt_neworder = []

        return flt_flags, flt_neworder


class ARCHIPACK_UL_City(UIList):

    # use_filter_show: BoolProperty(default=True)

    def draw_item(self, context, layout, data, item, icon, active_data, active_propname):
        self.use_filter_show = True

        if self.layout_type in {'DEFAULT', 'COMPACT'}:
            # You should always start your row layout by a label (icon + text), or a non-embossed text field,
            # this will also make the row easily selectable in the list! The later also enables ctrl-click rename.
            # We use icon_value of label, as our given icon is an integer value, not an enum ID.
            # Note "data" names should never be translated!
            #split = layout.split(0.3)
            layout.label(text=item.label)
            #split.label(item.description)

        elif self.layout_type in {'GRID'}:
            layout.label(text=item.label)
            pass

    # Called once to filter/reorder items.
    def filter_items(self, context, data, propname):

        col = getattr(data, propname)
        filter_name = self.filter_name.lower()

        flt_flags = [self.bitflag_filter_item if any(
                filter_name in filter_set for filter_set in (
                    str(i), item.label.lower()
        )
    )
            else 0 for i, item in enumerate(col, 1)
        ]

        if self.use_filter_sort_alpha:
            flt_neworder = [x[1] for x in sorted(
                    zip(
                        [x[0] for x in sorted(enumerate(col), key=lambda x: x[1].label)],
                        range(len(col))
            )
        )
            ]
        else:
            flt_neworder = []

        return flt_flags, flt_neworder


class ARCHIPACK_UL_Country(UIList):

    def draw_item(self, context, layout, data, item, icon, active_data, active_propname):

        self.use_filter_show = True

        if self.layout_type in {'DEFAULT', 'COMPACT'}:
            # You should always start your row layout by a label (icon + text), or a non-embossed text field,
            # this will also make the row easily selectable in the list! The later also enables ctrl-click rename.
            # We use icon_value of label, as our given icon is an integer value, not an enum ID.
            # Note "data" names should never be translated!
            #split = layout.split(0.3)
            layout.label(text=item.label)
            #split.label(item.description)

        elif self.layout_type in {'GRID'}:
            layout.label(text=item.label)
            pass

    # Called once to filter/reorder items.
    def filter_items(self, context, data, propname):

        col = getattr(data, propname)
        filter_name = self.filter_name.lower()

        flt_flags = [self.bitflag_filter_item if any(
                filter_name in filter_set for filter_set in (
                    str(i), item.label.lower()
        )
    )
            else 0 for i, item in enumerate(col, 1)
        ]

        if self.use_filter_sort_alpha:
            flt_neworder = [x[1] for x in sorted(
                    zip(
                        [x[0] for x in sorted(enumerate(col), key=lambda x: x[1].label)],
                        range(len(col))
            )
        )
            ]
        else:
            flt_neworder = []

        return flt_flags, flt_neworder


TAI_DT = 37    # TAI - UTC 12/4/2019 -> 2013->2019 +2s

# Sun rotation calculator implementation
class sun_calculator(object):
    """
   Based on SunLight v1.0 by Miguel Kabantsov (miguelkab@gmail.com)
  (Source for algorythm: http://de.wikipedia.org/wiki/Sonnenstand),
   Co-Ordinates: http://www.bcca.org/misc/qiblih/latlong.html
   Author: Nils-Peter Fischer (Nils-Peter.Fischer@web.de)
   """
    countrys = {
        'AF': 'Afghanistan',
        'AX': 'Åland Islands',
        'AL': 'Albania',
        'DZ': 'Algeria',
        'AS': 'American Samoa',
        'AD': 'Andorra',
        'AO': 'Angola',
        'AI': 'Anguilla',
        'AQ': 'Antarctica',
        'AG': 'Antigua and Barbuda',
        'AR': 'Argentina',
        'AM': 'Armenia',
        'AW': 'Aruba',
        'AU': 'Australia',
        'AT': 'Austria',
        'AZ': 'Azerbaijan',
        'BS': 'Bahamas',
        'BH': 'Bahrain',
        'BD': 'Bangladesh',
        'BB': 'Barbados',
        'BY': 'Belarus',
        'BE': 'Belgium',
        'BZ': 'Belize',
        'BJ': 'Benin',
        'BM': 'Bermuda',
        'BT': 'Bhutan',
        'BO': 'Bolivia  Plurinational State of',
        'BQ': 'Bonaire  Sint Eustatius and Saba',
        'BA': 'Bosnia and Herzegovina',
        'BW': 'Botswana',
        'BV': 'Bouvet Island',
        'BR': 'Brazil',
        'IO': 'British Indian Ocean Territory',
        'BN': 'Brunei Darussalam',
        'BG': 'Bulgaria',
        'BF': 'Burkina Faso',
        'BI': 'Burundi',
        'KH': 'Cambodia',
        'CM': 'Cameroon',
        'CA': 'Canada',
        'CV': 'Cape Verde',
        'KY': 'Cayman Islands',
        'CF': 'Central African Republic',
        'TD': 'Chad',
        'CL': 'Chile',
        'CN': 'China',
        'CX': 'Christmas Island',
        'CC': 'Cocos (Keeling) Islands',
        'CO': 'Colombia',
        'KM': 'Comoros',
        'CG': 'Congo',
        'CD': 'Congo  the Democratic Republic of the',
        'CK': 'Cook Islands',
        'CR': 'Costa Rica',
        'CI': "Côte d'Ivoire",
        'HR': 'Croatia',
        'CU': 'Cuba',
        'CW': 'Curaçao',
        'CY': 'Cyprus',
        'CZ': 'Czech Republic',
        'DK': 'Denmark',
        'DJ': 'Djibouti',
        'DM': 'Dominica',
        'DO': 'Dominican Republic',
        'EC': 'Ecuador',
        'EG': 'Egypt',
        'SV': 'El Salvador',
        'GQ': 'Equatorial Guinea',
        'ER': 'Eritrea',
        'EE': 'Estonia',
        'ET': 'Ethiopia',
        'FK': 'Falkland Islands (Malvinas)',
        'FO': 'Faroe Islands',
        'FJ': 'Fiji',
        'FI': 'Finland',
        'FR': 'France',
        'GF': 'French Guiana',
        'PF': 'French Polynesia',
        'TF': 'French Southern Territories',
        'GA': 'Gabon',
        'GM': 'Gambia',
        'GE': 'Georgia',
        'DE': 'Germany',
        'GH': 'Ghana',
        'GI': 'Gibraltar',
        'GR': 'Greece',
        'GL': 'Greenland',
        'GD': 'Grenada',
        'GP': 'Guadeloupe',
        'GU': 'Guam',
        'GT': 'Guatemala',
        'GG': 'Guernsey',
        'GN': 'Guinea',
        'GW': 'Guinea-Bissau',
        'GY': 'Guyana',
        'HT': 'Haiti',
        'HM': 'Heard Island and McDonald Islands',
        'VA': 'Holy See (Vatican City State)',
        'HN': 'Honduras',
        'HK': 'Hong Kong',
        'HU': 'Hungary',
        'IS': 'Iceland',
        'IN': 'India',
        'ID': 'Indonesia',
        'IR': 'Iran  Islamic Republic of',
        'IQ': 'Iraq',
        'IE': 'Ireland',
        'IM': 'Isle of Man',
        'IL': 'Israel',
        'IT': 'Italy',
        'JM': 'Jamaica',
        'JP': 'Japan',
        'JE': 'Jersey',
        'JO': 'Jordan',
        'KZ': 'Kazakhstan',
        'KE': 'Kenya',
        'KI': 'Kiribati',
        'KP': "Korea  Democratic People's Republic of",
        'KR': 'Korea  Republic of',
        'KW': 'Kuwait',
        'KG': 'Kyrgyzstan',
        'LA': "Lao People's Democratic Republic",
        'LV': 'Latvia',
        'LB': 'Lebanon',
        'LS': 'Lesotho',
        'LR': 'Liberia',
        'LY': 'Libya',
        'LI': 'Liechtenstein',
        'LT': 'Lithuania',
        'LU': 'Luxembourg',
        'MO': 'Macao',
        'MK': 'Macedonia  the Former Yugoslav Republic of',
        'MG': 'Madagascar',
        'MW': 'Malawi',
        'MY': 'Malaysia',
        'MV': 'Maldives',
        'ML': 'Mali',
        'MT': 'Malta',
        'MH': 'Marshall Islands',
        'MQ': 'Martinique',
        'MR': 'Mauritania',
        'MU': 'Mauritius',
        'YT': 'Mayotte',
        'MX': 'Mexico',
        'FM': 'Micronesia  Federated States of',
        'MD': 'Moldova  Republic of',
        'MC': 'Monaco',
        'MN': 'Mongolia',
        'ME': 'Montenegro',
        'MS': 'Montserrat',
        'MA': 'Morocco',
        'MZ': 'Mozambique',
        'MM': 'Myanmar',
        'NA': 'Namibia',
        'NR': 'Nauru',
        'NP': 'Nepal',
        'NL': 'Netherlands',
        'NC': 'New Caledonia',
        'NZ': 'New Zealand',
        'NI': 'Nicaragua',
        'NE': 'Niger',
        'NG': 'Nigeria',
        'NU': 'Niue',
        'NF': 'Norfolk Island',
        'MP': 'Northern Mariana Islands',
        'NO': 'Norway',
        'OM': 'Oman',
        'PK': 'Pakistan',
        'PW': 'Palau',
        'PS': 'Palestine  State of',
        'PA': 'Panama',
        'PG': 'Papua New Guinea',
        'PY': 'Paraguay',
        'PE': 'Peru',
        'PH': 'Philippines',
        'PN': 'Pitcairn',
        'PL': 'Poland',
        'PT': 'Portugal',
        'PR': 'Puerto Rico',
        'QA': 'Qatar',
        'RE': 'Réunion',
        'RO': 'Romania',
        'RU': 'Russian Federation',
        'RW': 'Rwanda',
        'BL': 'Saint Barthélemy',
        'SH': 'Saint Helena  Ascension and Tristan da Cunha',
        'KN': 'Saint Kitts and Nevis',
        'LC': 'Saint Lucia',
        'MF': 'Saint Martin (French part)',
        'PM': 'Saint Pierre and Miquelon',
        'VC': 'Saint Vincent and the Grenadines',
        'WS': 'Samoa',
        'SM': 'San Marino',
        'ST': 'Sao Tome and Principe',
        'SA': 'Saudi Arabia',
        'SN': 'Senegal',
        'RS': 'Serbia',
        'SC': 'Seychelles',
        'SL': 'Sierra Leone',
        'SG': 'Singapore',
        'SX': 'Sint Maarten (Dutch part)',
        'SK': 'Slovakia',
        'SI': 'Slovenia',
        'SB': 'Solomon Islands',
        'SO': 'Somalia',
        'ZA': 'South Africa',
        'GS': 'South Georgia and the South Sandwich Islands',
        'SS': 'South Sudan',
        'ES': 'Spain',
        'LK': 'Sri Lanka',
        'SD': 'Sudan',
        'SR': 'Suriname',
        'SJ': 'Svalbard and Jan Mayen',
        'SZ': 'Swaziland',
        'SE': 'Sweden',
        'CH': 'Switzerland',
        'SY': 'Syrian Arab Republic',
        'TW': 'Taiwan  Province of China',
        'TJ': 'Tajikistan',
        'TZ': 'Tanzania  United Republic of',
        'TH': 'Thailand',
        'TL': 'Timor-Leste',
        'TG': 'Togo',
        'TK': 'Tokelau',
        'TO': 'Tonga',
        'TT': 'Trinidad and Tobago',
        'TN': 'Tunisia',
        'TR': 'Turkey',
        'TM': 'Turkmenistan',
        'TC': 'Turks and Caicos Islands',
        'TV': 'Tuvalu',
        'UG': 'Uganda',
        'UA': 'Ukraine',
        'AE': 'United Arab Emirates',
        'GB': 'United Kingdom',
        'US': 'United States',
        'UM': 'United States Minor Outlying Islands',
        'UY': 'Uruguay',
        'UZ': 'Uzbekistan',
        'VU': 'Vanuatu',
        'VE': 'Venezuela  Bolivarian Republic of',
        'VN': 'Viet Nam',
        'VG': 'Virgin Islands  British',
        'VI': 'Virgin Islands  U.S.',
        'WF': 'Wallis and Futuna',
        'XK': 'Kosovo',
        'EH': 'Western Sahara',
        'YE': 'Yemen',
        'ZM': 'Zambia',
        'ZW': 'Zimbabwe'
        }

    timezones = {
        "Africa/Abidjan": 0.0,
        "Africa/Accra": 0.0,
        "Africa/Addis_Ababa": 3.0,
        "Africa/Algiers": 1.0,
        "Africa/Asmara": 3.0,
        "Africa/Bamako": 0.0,
        "Africa/Bangui": 1.0,
        "Africa/Banjul": 0.0,
        "Africa/Bissau": 0.0,
        "Africa/Blantyre": 2.0,
        "Africa/Brazzaville": 1.0,
        "Africa/Bujumbura": 2.0,
        "Africa/Cairo": 2.0,
        "Africa/Casablanca": 0.0,
        "Africa/Ceuta": 1.0,
        "Africa/Conakry": 0.0,
        "Africa/Dakar": 0.0,
        "Africa/Dar_es_Salaam": 3.0,
        "Africa/Djibouti": 3.0,
        "Africa/Douala": 1.0,
        "Africa/El_Aaiun": 0.0,
        "Africa/Freetown": 0.0,
        "Africa/Gaborone": 2.0,
        "Africa/Harare": 2.0,
        "Africa/Johannesburg": 2.0,
        "Africa/Juba": 3.0,
        "Africa/Kampala": 3.0,
        "Africa/Khartoum": 3.0,
        "Africa/Kigali": 2.0,
        "Africa/Kinshasa": 1.0,
        "Africa/Lagos": 1.0,
        "Africa/Libreville": 1.0,
        "Africa/Lome": 0.0,
        "Africa/Luanda": 1.0,
        "Africa/Lubumbashi": 2.0,
        "Africa/Lusaka": 2.0,
        "Africa/Malabo": 1.0,
        "Africa/Maputo": 2.0,
        "Africa/Maseru": 2.0,
        "Africa/Mbabane": 2.0,
        "Africa/Mogadishu": 3.0,
        "Africa/Monrovia": 0.0,
        "Africa/Nairobi": 3.0,
        "Africa/Ndjamena": 1.0,
        "Africa/Niamey": 1.0,
        "Africa/Nouakchott": 0.0,
        "Africa/Ouagadougou": 0.0,
        "Africa/Porto-Novo": 1.0,
        "Africa/Sao_Tome": 0.0,
        "Africa/Tripoli": 2.0,
        "Africa/Tunis": 1.0,
        "Africa/Windhoek": 2.0,
        "America/Adak": -10.0,
        "America/Anchorage": -9.0,
        "America/Anguilla": -4.0,
        "America/Antigua": -4.0,
        "America/Araguaina": -3.0,
        "America/Argentina/Buenos_Aires": -3.0,
        "America/Argentina/Catamarca": -3.0,
        "America/Argentina/Cordoba": -3.0,
        "America/Argentina/Jujuy": -3.0,
        "America/Argentina/La_Rioja": -3.0,
        "America/Argentina/Mendoza": -3.0,
        "America/Argentina/Rio_Gallegos": -3.0,
        "America/Argentina/Salta": -3.0,
        "America/Argentina/San_Juan": -3.0,
        "America/Argentina/San_Luis": -3.0,
        "America/Argentina/Tucuman": -3.0,
        "America/Argentina/Ushuaia": -3.0,
        "America/Aruba": -4.0,
        "America/Asuncion": -3.0,
        "America/Atikokan": -5.0,
        "America/Bahia": -3.0,
        "America/Bahia_Banderas": -6.0,
        "America/Barbados": -4.0,
        "America/Belem": -3.0,
        "America/Belize": -6.0,
        "America/Blanc-Sablon": -4.0,
        "America/Boa_Vista": -4.0,
        "America/Bogota": -5.0,
        "America/Boise": -7.0,
        "America/Cambridge_Bay": -7.0,
        "America/Campo_Grande": -3.0,
        "America/Cancun": -5.0,
        "America/Caracas": -4.5,
        "America/Cayenne": -3.0,
        "America/Cayman": -5.0,
        "America/Chicago": -6.0,
        "America/Chihuahua": -7.0,
        "America/Costa_Rica": -6.0,
        "America/Creston": -7.0,
        "America/Cuiaba": -3.0,
        "America/Curacao": -4.0,
        "America/Danmarkshavn": 0.0,
        "America/Dawson": -8.0,
        "America/Dawson_Creek": -7.0,
        "America/Denver": -7.0,
        "America/Detroit": -5.0,
        "America/Dominica": -4.0,
        "America/Edmonton": -7.0,
        "America/Eirunepe": -5.0,
        "America/El_Salvador": -6.0,
        "America/Fort_Nelson": -7.0,
        "America/Fortaleza": -3.0,
        "America/Glace_Bay": -4.0,
        "America/Godthab": -3.0,
        "America/Goose_Bay": -4.0,
        "America/Grand_Turk": -4.0,
        "America/Grenada": -4.0,
        "America/Guadeloupe": -4.0,
        "America/Guatemala": -6.0,
        "America/Guayaquil": -5.0,
        "America/Guyana": -4.0,
        "America/Halifax": -4.0,
        "America/Havana": -5.0,
        "America/Hermosillo": -7.0,
        "America/Indiana/Indianapolis": -5.0,
        "America/Indiana/Knox": -6.0,
        "America/Indiana/Marengo": -5.0,
        "America/Indiana/Petersburg": -5.0,
        "America/Indiana/Tell_City": -6.0,
        "America/Indiana/Vevay": -5.0,
        "America/Indiana/Vincennes": -5.0,
        "America/Indiana/Winamac": -5.0,
        "America/Inuvik": -7.0,
        "America/Iqaluit": -5.0,
        "America/Jamaica": -5.0,
        "America/Juneau": -9.0,
        "America/Kentucky/Louisville": -5.0,
        "America/Kentucky/Monticello": -5.0,
        "America/Kralendijk": -4.0,
        "America/La_Paz": -4.0,
        "America/Lima": -5.0,
        "America/Los_Angeles": -8.0,
        "America/Lower_Princes": -4.0,
        "America/Maceio": -3.0,
        "America/Managua": -6.0,
        "America/Manaus": -4.0,
        "America/Marigot": -4.0,
        "America/Martinique": -4.0,
        "America/Matamoros": -6.0,
        "America/Mazatlan": -7.0,
        "America/Menominee": -6.0,
        "America/Merida": -6.0,
        "America/Metlakatla": -9.0,
        "America/Mexico_City": -6.0,
        "America/Miquelon": -3.0,
        "America/Moncton": -4.0,
        "America/Monterrey": -6.0,
        "America/Montevideo": -3.0,
        "America/Montserrat": -4.0,
        "America/Nassau": -5.0,
        "America/New_York": -5.0,
        "America/Nipigon": -5.0,
        "America/Nome": -9.0,
        "America/Noronha": -2.0,
        "America/North_Dakota/Beulah": -6.0,
        "America/North_Dakota/Center": -6.0,
        "America/North_Dakota/New_Salem": -6.0,
        "America/Ojinaga": -7.0,
        "America/Panama": -5.0,
        "America/Pangnirtung": -5.0,
        "America/Paramaribo": -3.0,
        "America/Phoenix": -7.0,
        "America/Port-au-Prince": -5.0,
        "America/Port_of_Spain": -4.0,
        "America/Porto_Velho": -4.0,
        "America/Puerto_Rico": -4.0,
        "America/Rainy_River": -6.0,
        "America/Rankin_Inlet": -6.0,
        "America/Recife": -3.0,
        "America/Regina": -6.0,
        "America/Resolute": -6.0,
        "America/Rio_Branco": -5.0,
        "America/Santarem": -3.0,
        "America/Santiago": -3.0,
        "America/Santo_Domingo": -4.0,
        "America/Sao_Paulo": -2.0,
        "America/Scoresbysund": -1.0,
        "America/Sitka": -9.0,
        "America/St_Barthelemy": -4.0,
        "America/St_Johns": -3.5,
        "America/St_Kitts": -4.0,
        "America/St_Lucia": -4.0,
        "America/St_Thomas": -4.0,
        "America/St_Vincent": -4.0,
        "America/Swift_Current": -6.0,
        "America/Tegucigalpa": -6.0,
        "America/Thule": -4.0,
        "America/Thunder_Bay": -5.0,
        "America/Tijuana": -8.0,
        "America/Toronto": -5.0,
        "America/Tortola": -4.0,
        "America/Vancouver": -8.0,
        "America/Whitehorse": -8.0,
        "America/Winnipeg": -6.0,
        "America/Yakutat": -9.0,
        "America/Yellowknife": -7.0,
        "Antarctica/Casey": 8.0,
        "Antarctica/Davis": 7.0,
        "Antarctica/DumontDUrville": 10.0,
        "Antarctica/Macquarie": 11.0,
        "Antarctica/Mawson": 5.0,
        "Antarctica/McMurdo": 13.0,
        "Antarctica/Palmer": -3.0,
        "Antarctica/Rothera": -3.0,
        "Antarctica/Syowa": 3.0,
        "Antarctica/Troll": 0.0,
        "Antarctica/Vostok": 6.0,
        "Arctic/Longyearbyen": 1.0,
        "Asia/Aden": 3.0,
        "Asia/Almaty": 6.0,
        "Asia/Amman": 2.0,
        "Asia/Anadyr": 12.0,
        "Asia/Aqtau": 5.0,
        "Asia/Aqtobe": 5.0,
        "Asia/Ashgabat": 5.0,
        "Asia/Baghdad": 3.0,
        "Asia/Bahrain": 3.0,
        "Asia/Baku": 4.0,
        "Asia/Bangkok": 7.0,
        "Asia/Barnaul": 6.0,
        "Asia/Beirut": 2.0,
        "Asia/Bishkek": 6.0,
        "Asia/Brunei": 8.0,
        "Asia/Chita": 8.0,
        "Asia/Choibalsan": 8.0,
        "Asia/Colombo": 5.5,
        "Asia/Damascus": 2.0,
        "Asia/Dhaka": 6.0,
        "Asia/Dili": 9.0,
        "Asia/Dubai": 4.0,
        "Asia/Dushanbe": 5.0,
        "Asia/Gaza": 2.0,
        "Asia/Hebron": 2.0,
        "Asia/Ho_Chi_Minh": 7.0,
        "Asia/Hong_Kong": 8.0,
        "Asia/Hovd": 7.0,
        "Asia/Irkutsk": 8.0,
        "Asia/Jakarta": 7.0,
        "Asia/Jayapura": 9.0,
        "Asia/Jerusalem": 2.0,
        "Asia/Kabul": 4.5,
        "Asia/Kamchatka": 12.0,
        "Asia/Karachi": 5.0,
        "Asia/Kathmandu": 5.75,
        "Asia/Khandyga": 9.0,
        "Asia/Kolkata": 5.5,
        "Asia/Krasnoyarsk": 7.0,
        "Asia/Kuala_Lumpur": 8.0,
        "Asia/Kuching": 8.0,
        "Asia/Kuwait": 3.0,
        "Asia/Macau": 8.0,
        "Asia/Magadan": 10.0,
        "Asia/Makassar": 8.0,
        "Asia/Manila": 8.0,
        "Asia/Muscat": 4.0,
        "Asia/Nicosia": 2.0,
        "Asia/Novokuznetsk": 7.0,
        "Asia/Novosibirsk": 6.0,
        "Asia/Omsk": 6.0,
        "Asia/Oral": 5.0,
        "Asia/Phnom_Penh": 7.0,
        "Asia/Pontianak": 7.0,
        "Asia/Pyongyang": 8.5,
        "Asia/Qatar": 3.0,
        "Asia/Qyzylorda": 6.0,
        "Asia/Rangoon": 6.5,
        "Asia/Riyadh": 3.0,
        "Asia/Sakhalin": 10.0,
        "Asia/Samarkand": 5.0,
        "Asia/Seoul": 9.0,
        "Asia/Shanghai": 8.0,
        "Asia/Singapore": 8.0,
        "Asia/Srednekolymsk": 11.0,
        "Asia/Taipei": 8.0,
        "Asia/Tashkent": 5.0,
        "Asia/Tbilisi": 4.0,
        "Asia/Tehran": 3.5,
        "Asia/Thimphu": 6.0,
        "Asia/Tokyo": 9.0,
        "Asia/Tomsk": 6.0,
        "Asia/Ulaanbaatar": 8.0,
        "Asia/Urumqi": 6.0,
        "Asia/Ust-Nera": 10.0,
        "Asia/Vientiane": 7.0,
        "Asia/Vladivostok": 10.0,
        "Asia/Yakutsk": 9.0,
        "Asia/Yekaterinburg": 5.0,
        "Asia/Yerevan": 4.0,
        "Atlantic/Azores": -1.0,
        "Atlantic/Bermuda": -4.0,
        "Atlantic/Canary": 0.0,
        "Atlantic/Cape_Verde": -1.0,
        "Atlantic/Faroe": 0.0,
        "Atlantic/Madeira": 0.0,
        "Atlantic/Reykjavik": 0.0,
        "Atlantic/South_Georgia": -2.0,
        "Atlantic/St_Helena": 0.0,
        "Atlantic/Stanley": -3.0,
        "Australia/Adelaide": 10.5,
        "Australia/Brisbane": 10.0,
        "Australia/Broken_Hill": 10.5,
        "Australia/Currie": 11.0,
        "Australia/Darwin": 9.5,
        "Australia/Eucla": 8.75,
        "Australia/Hobart": 11.0,
        "Australia/Lindeman": 10.0,
        "Australia/Lord_Howe": 11.0,
        "Australia/Melbourne": 11.0,
        "Australia/Perth": 8.0,
        "Australia/Sydney": 11.0,
        "Europe/Amsterdam": 1.0,
        "Europe/Andorra": 1.0,
        "Europe/Astrakhan": 3.0,
        "Europe/Athens": 2.0,
        "Europe/Belgrade": 1.0,
        "Europe/Berlin": 1.0,
        "Europe/Bratislava": 1.0,
        "Europe/Brussels": 1.0,
        "Europe/Bucharest": 2.0,
        "Europe/Budapest": 1.0,
        "Europe/Busingen": 1.0,
        "Europe/Chisinau": 2.0,
        "Europe/Copenhagen": 1.0,
        "Europe/Dublin": 0.0,
        "Europe/Gibraltar": 1.0,
        "Europe/Guernsey": 0.0,
        "Europe/Helsinki": 2.0,
        "Europe/Isle_of_Man": 0.0,
        "Europe/Istanbul": 2.0,
        "Europe/Jersey": 0.0,
        "Europe/Kaliningrad": 2.0,
        "Europe/Kiev": 2.0,
        "Europe/Kirov": 3.0,
        "Europe/Lisbon": 0.0,
        "Europe/Ljubljana": 1.0,
        "Europe/London": 0.0,
        "Europe/Luxembourg": 1.0,
        "Europe/Madrid": 1.0,
        "Europe/Malta": 1.0,
        "Europe/Mariehamn": 2.0,
        "Europe/Minsk": 3.0,
        "Europe/Monaco": 1.0,
        "Europe/Moscow": 3.0,
        "Europe/Oslo": 1.0,
        "Europe/Paris": 1.0,
        "Europe/Podgorica": 1.0,
        "Europe/Prague": 1.0,
        "Europe/Riga": 2.0,
        "Europe/Rome": 1.0,
        "Europe/Samara": 4.0,
        "Europe/San_Marino": 1.0,
        "Europe/Sarajevo": 1.0,
        "Europe/Simferopol": 3.0,
        "Europe/Skopje": 1.0,
        "Europe/Sofia": 2.0,
        "Europe/Stockholm": 1.0,
        "Europe/Tallinn": 2.0,
        "Europe/Tirane": 1.0,
        "Europe/Ulyanovsk": 3.0,
        "Europe/Uzhgorod": 2.0,
        "Europe/Vaduz": 1.0,
        "Europe/Vatican": 1.0,
        "Europe/Vienna": 1.0,
        "Europe/Vilnius": 2.0,
        "Europe/Volgograd": 3.0,
        "Europe/Warsaw": 1.0,
        "Europe/Zagreb": 1.0,
        "Europe/Zaporozhye": 2.0,
        "Europe/Zurich": 1.0,
        "Indian/Antananarivo": 3.0,
        "Indian/Chagos": 6.0,
        "Indian/Christmas": 7.0,
        "Indian/Cocos": 6.5,
        "Indian/Comoro": 3.0,
        "Indian/Kerguelen": 5.0,
        "Indian/Mahe": 4.0,
        "Indian/Maldives": 5.0,
        "Indian/Mauritius": 4.0,
        "Indian/Mayotte": 3.0,
        "Indian/Reunion": 4.0,
        "Pacific/Apia": 14.0,
        "Pacific/Auckland": 13.0,
        "Pacific/Bougainville": 11.0,
        "Pacific/Chatham": 13.75,
        "Pacific/Chuuk": 10.0,
        "Pacific/Easter": -5.0,
        "Pacific/Efate": 11.0,
        "Pacific/Enderbury": 13.0,
        "Pacific/Fakaofo": 13.0,
        "Pacific/Fiji": 13.0,
        "Pacific/Funafuti": 12.0,
        "Pacific/Galapagos": -6.0,
        "Pacific/Gambier": -9.0,
        "Pacific/Guadalcanal": 11.0,
        "Pacific/Guam": 10.0,
        "Pacific/Honolulu": -10.0,
        "Pacific/Johnston": -10.0,
        "Pacific/Kiritimati": 14.0,
        "Pacific/Kosrae": 11.0,
        "Pacific/Kwajalein": 12.0,
        "Pacific/Majuro": 12.0,
        "Pacific/Marquesas": -9.5,
        "Pacific/Midway": -11.0,
        "Pacific/Nauru": 12.0,
        "Pacific/Niue": -11.0,
        "Pacific/Norfolk": 11.0,
        "Pacific/Noumea": 11.0,
        "Pacific/Pago_Pago": -11.0,
        "Pacific/Palau": 9.0,
        "Pacific/Pitcairn": -8.0,
        "Pacific/Pohnpei": 11.0,
        "Pacific/Port_Moresby": 10.0,
        "Pacific/Rarotonga": -10.0,
        "Pacific/Saipan": 10.0,
        "Pacific/Tahiti": -10.0,
        "Pacific/Tarawa": 12.0,
        "Pacific/Tongatapu": 13.0,
        "Pacific/Wake": 12.0,
        "Pacific/Wallis": 12.0
        }

    citys = []

    # mathematical helpers
    @staticmethod
    def sind(deg):
        return sin(radians(deg))
 
    @staticmethod
    def cosd(deg):
        return cos(radians(deg))
 
    @staticmethod
    def tand(deg):
        return tan(radians(deg))
 
    @staticmethod
    def asind(deg):
        return degrees(asin(deg))
 
    @staticmethod
    def atand(deg):
        return degrees(atan(deg))

    @staticmethod
    def geo_sun_astronomicJulianDate(Year, Month, Day, LocalTime, Timezone):
        if Month > 2.0:
            Y = Year
            M = Month
        else:
            Y = Year - 1.0
            M = Month + 12.0
 
        UT = LocalTime - Timezone
        hour = UT / 24.0
        A = int(Y / 100.0)
 
        JD = floor(365.25 * (Y + 4716.0)) + floor(30.6001 * (M + 1.0)) + Day + hour - 1524.4
 
        # The following section is adopted from netCDF4 netcdftime implementation.
        # Copyright: 2008 by Jeffrey Whitaker
        # License: http://www.opensource.org/licenses/mit-license.php
        if JD >= 2299170.5:
            # 1582 October 15 (Gregorian Calendar)
            B = 2.0 - A + int(A / 4.0)
        elif JD < 2299160.5:
            # 1582 October 5 (Julian Calendar)
            B = 0
        else:
            raise Exception('ERROR: Date falls in the gap between Julian and Gregorian calendars.')
            B = 0
 
        return JD + B
 
    @staticmethod
    def geoSunData(Latitude, Longitude, Year, Month, Day, LocalTime, Timezone):
        JD = sun_calculator.geo_sun_astronomicJulianDate(Year, Month, Day, LocalTime, Timezone)
 
        phi = degrees(Latitude)
        llambda = degrees(Longitude)
 
        n = JD - 2451545.0
        LDeg = (280.460 + 0.9856474 * n) - (floor((280.460 + 0.9856474 * n) / 360.0) * 360.0)
        gDeg = (357.528 + 0.9856003 * n) - (floor((357.528 + 0.9856003 * n) / 360.0) * 360.0)
        LambdaDeg = LDeg + 1.915 * sun_calculator.sind(gDeg) + 0.02 * sun_calculator.sind(2.0 * gDeg)
 
        epsilonDeg = 23.439 - 0.0000004 * n
 
        alphaDeg = sun_calculator.atand(
                (sun_calculator.cosd(epsilonDeg) * sun_calculator.sind(LambdaDeg)) /
                sun_calculator.cosd(LambdaDeg)
        )
 
        if sun_calculator.cosd(LambdaDeg) < 0.0:
            alphaDeg += 180.0
 
        deltaDeg = sun_calculator.asind(sun_calculator.sind(epsilonDeg) * sun_calculator.sind(LambdaDeg))
 
        JDNull = sun_calculator.geo_sun_astronomicJulianDate(Year, Month, Day, 0.0, 0.0)
 
        TNull = (JDNull - 2451545.0) / 36525.0
        T = LocalTime - Timezone
 
        thetaGh = 6.697376 + 2400.05134 * TNull + 1.002738 * T
        thetaGh -= floor(thetaGh / 24.0) * 24.0
 
        thetaG = thetaGh * 15.0
        theta = thetaG + llambda
 
        tau = theta - alphaDeg
 
        a = sun_calculator.atand(
                sun_calculator.sind(tau) / (sun_calculator.cosd(tau) * sun_calculator.sind(phi) -
                sun_calculator.tand(deltaDeg) * sun_calculator.cosd(phi))
        )
 
        if (sun_calculator.cosd(tau) * sun_calculator.sind(phi) -
                sun_calculator.tand(deltaDeg) * sun_calculator.cosd(phi) < 0.0):
            a += 180.0
 
        h = sun_calculator.asind(
                sun_calculator.cosd(deltaDeg) * sun_calculator.cosd(tau) * sun_calculator.cosd(phi) +
                sun_calculator.sind(deltaDeg) * sun_calculator.sind(phi)
        )
 
        R = 1.02 / (sun_calculator.tand(h + (10.3 / (h + 5.11))))
        hR = h + R / 60.0
 
        azimuth = a
        elevation = hR
 
        return -azimuth, elevation

    @staticmethod
    def julian_to_gregorian(jd, tz, dt):
        """
            convert JDE to Gregorian calendar, year, month, day and time
            includes time as fraction
        """
        t = jd + 0.5
        t += (tz / 24)  # rotate JDE to the local time zone
        t -= (dt / (24 * 3600))  # apply the current TAI-UTC correction
        z = int(t)  # integer part
        f = t - z  # fractional part
        if z < 2299161:
            a = z
        else:
            temp = int((z - 1867216.25) / 36524.25)
            a = z + 1 + temp - int(temp / 4)
        b = a + 1524
        c = int((b - 122.1) / 365.25)
        d = int(365.25 * c)
        e = int((b - d) / 30.6001)
        gd = b - d - int(30.6001 * e) + f
        if e < 14:
            gm = e - 1
        else:
            gm = e - 13
        if gm > 2:
            gy = c - 4716
        else:
            gy = c - 4715
        return gy, gm, gd

    @staticmethod
    def ephemeris(x, greg_year):
        """
        #   x = 0     march    equinox
        #   x = 1     june    solstice
        #   x = 2   september  equinox
        #   x = 3   december  solstice
        :param x:
        :param greg_year:
        :return: julian day including time
        """
        e = 0  # default to early era, 1000 BC to 1000 AD
        y = greg_year / 1000  # Greg_Year must be an integer
        if greg_year > 1000:
            e = 1  # the period is from 1000 AD to 3000 AD
            y -= 2  # this adjusts y to;  y = ( Greg_year - 2000 ) / 1000
        y2 = y * y
        y3 = y2 * y
        y4 = y2 * y2  # powers of y
        # polynomial coefficients   d( era  , event ,  term )
        d = ((  # earlier era  1000 BC to 1000 AD  Table 26.A page 166
                 (1721139.29189, 365242.13740, 0.06134, 0.00111, -0.00071),  # march
                 (1721233.25401, 365241.72562, -0.05323, 0.00907, 0.00025),  # june
                 (1721325.70455, 365242.49558, -0.11677, -0.00297, 0.00074),  # sept
                 (1721414.39987, 365242.88257, -0.00769, -0.00933, -0.00006)  # dec
             ), (  # later era    1000 AD to 3000 AD  Table 26.B page 166
                 (2451623.80984, 365242.37404, 0.05169, -0.00411, -0.00057),  # march
                 (2451716.56767, 365241.62603, 0.00325, 0.00888, -0.00030),  # june
                 (2451810.21715, 365242.01767, -0.11575, 0.00337, 0.00078),  # sept
                 (2451900.05952, 365242.74049, -0.06223, -0.00823, 0.00032)  # dec
             ))
        # this gives the instant of the "mean" equinox or solstice
        JDEo = d[e][x][0] + y * d[e][x][1] + y2 * d[e][x][2] + y3 * d[e][x][3] + y4 * d[e][x][4]
        # which can now be adjusted with the periodic terms
        # Table 26.C page 167
        abc = (
            #   a    b          c             a    b          c
            (485, 324.96, 1934.136), (45, 247.54, 29929.562),
            (203, 337.23, 32964.467), (44, 325.15, 31555.956),
            (199, 342.08, 20.186), (29, 60.93, 4443.417),
            (182, 27.85, 445267.112), (18, 155.12, 67555.328),
            (156, 73.14, 45036.886), (17, 288.79, 4562.452),
            (136, 171.52, 22518.443), (16, 198.04, 62894.029),
            (77, 222.54, 65928.934), (14, 199.76, 31436.921),
            (74, 296.72, 3034.906), (12, 95.39, 14577.848),
            (70, 243.58, 9037.513), (12, 287.11, 31931.756),
            (58, 119.81, 33718.147), (12, 320.81, 34777.259),
            (52, 297.17, 150.678), (9, 227.73, 1222.114),
            (50, 21.02, 2281.226), (8, 15.45, 16859.074)
        )
        t = (JDEo - 2451545.0) / 36525
        w = radians(t * 35999.373 - 2.47)
        delta_lambda = 1 + 0.0334 * cos(w) + 0.0007 * cos(2 * w)
        sum = 0
        for a, b, c in abc:  # evaluate periodic terms, order not important
            sum += a * cos(radians(b + c * t))

        return JDEo + (0.00001 * sum / delta_lambda)  # JDE


def update(self, context):
    # print("update tz", self.tz)
    self.update(context, update_mesh=False)


@persistent
def animate_location(context=None):
    for o in bpy.context.scene.objects:
        d = archipack_sun.datablock(o)
        if d is not None:
            d.update_childs(bpy.context, o)


def make_animatable(self, context):
    self.make_animatable()


def update_mesh(self, context):
    self.update(context, update_mesh=True)


def set_zone(self, context):
    sc = sun_calculator
    if hasattr(context.window_manager, "archipack"):
        sp = context.window_manager.archipack.sun
        timezone = sp.timezones[self.zone_idx].label
        self.tz = sc.timezones[timezone]


def set_city(self, context):
    sc = sun_calculator
    if hasattr(context.window_manager, "archipack"):
        sp = context.window_manager.archipack.sun

        country, name, lat, lon, timezone = sc.citys[self.city_idx]

        for idx, zone in enumerate(sp.timezones):
            if zone.label == timezone:
                self.zone_idx = idx
                break
        self.lat = radians(lat)
        self.lon = radians(lon)


def get_minit(self):
    return int((self.hours * 60) % 60)


def get_hour(self):
    return floor(self.hours)


def set_minit(self, value):
    self.hours = float(get_hour(self)) + (float(value) / 60)
    return None


def set_hour(self, value):
    self.hours = float(value) + float(get_minit(self) / 60)
    return None


class archipack_sun(ArchipackObject, PropertyGroup):

    tabs: EnumProperty(
        options={'SKIP_SAVE'},
        description="Display settings",
        name='Ui tabs',
        items=(
            ('LIGHT', 'Sunlight', 'Display sun light settings', 'LIGHT_SUN', 0),
            ('TIME', 'Time', 'Display time settings', 'PREVIEW_RANGE', 1),
            ('LOC', 'Location', 'Display location settings', 'URL', 2)
        ),
        default='LIGHT'
    )

    city_idx: IntProperty(
        name="City",
        description="Select a city to load coords from.",
        options={'HIDDEN'},
        default=0,
        update=set_city
    )

    zone_idx: IntProperty(
        name="UTC zone",
        description="Time zone: Difference from Greenwich England in hours.",
        options={'HIDDEN'},
        default=0,
        update=set_zone
    )
    hours: FloatProperty(
        name="Hours",
        description="Hours with animation support, add keys to this value to animate",
        min=0,
        max=25,
        default=today.hour + (today.minute / 60),
        update=update
    )
    minute: IntProperty(
        options={'SKIP_SAVE'},
        name="Minute",
        min=0,
        max=59,
        get=get_minit,
        set=set_minit,
        update=update
    )
    hour: IntProperty(
        options={'SKIP_SAVE'},
        name="Hour",
        min=0,
        max=24,
        get=get_hour,
        set=set_hour,
        update=update
    )
    day: IntProperty(
        name="Day",
        min=1,
        max=31,
        default=today.day,
        update=update
    )
    month: IntProperty(
        name="Month",
        min=1,
        max=12,
        default=today.month,
        update=update
    )
    year: IntProperty(
        name="Year",
        min=datetime.MINYEAR,
        max=datetime.MAXYEAR,
        default=today.year,
        update=update
    )
    tz: IntProperty(
        name="Time Zone",
        min=-13,
        max=13,
        default=-int(time.timezone/3600),
        update=update
    )
    dst: BoolProperty(
        name="Daylight saving time",
        default=False,
        update=update
    )
    lat: FloatProperty(
        name="Latitude",
        precision=5,
        min=-pi/2,
        max=pi/2,
        subtype='ANGLE', unit='ROTATION',
        default=0.0,
        update=update
    )
    lon: FloatProperty(
        name="Longitude",
        precision=5,
        min=-pi,
        max=pi,
        subtype='ANGLE', unit='ROTATION',
        default=0.0,
        update=update
    )
    dist: FloatProperty(
        unit='LENGTH', subtype='DISTANCE',
        name="Distance",
        min=0,
        default=10.0,
        update=update
    )
    scale: FloatProperty(
        name="Symbol scale",
        min=0.01,
        default=1.0,
        update=update_mesh
    )
    auto_update:  BoolProperty(
        options={'SKIP_SAVE'},
        default=True,
        update=update
    )
    animate: BoolProperty(
        name="Animation",
        description="Enable support for animation",
        default=False,
        update=make_animatable
    )
    eph0: StringProperty()
    eph1: StringProperty()
    eph2: StringProperty()
    eph3: StringProperty()

    def find_sun(self, context, o):
        lamp = None
        if any([c.type == 'LIGHT' for c in o.children]):
            for c in o.children:
                if c.type == 'LIGHT':
                    lamp = c
                    break
        else:
            ld = bpy.data.lights.new(name="Sun light", type='AREA')
            # basic presets for eevee light
            ld.energy = 150
            ld.use_contact_shadow = True
            ld.contact_shadow_distance = 2
            ld.contact_shadow_thickness = 2
            ld.shadow_buffer_soft = 3.0

            lamp = bpy.data.objects.new("Sun light", ld)
            self.link_object_to_scene(context, lamp, layer_name="Lights")
            lamp.parent = o
        return lamp

    def update_childs(self, context, o):
        dst = 0
        if self.dst:
            dst = 1
        az, el = sun_calculator.geoSunData(
            self.lat,
            self.lon,
            self.year,
            self.month,
            self.day,
            self.hours,
            self.tz + dst
        )
        lamp = self.find_sun(context, o)
        if lamp is not None:
            lamp.matrix_world =  o.matrix_world \
                      @ Matrix.Rotation(radians(az), 4, 'Z') \
                      @ Matrix.Rotation(radians(90 - el), 4, 'X') \
                      @ Matrix.Translation(Vector((0, 0, self.dist)))

    def buildmesh(self, o):
        _scale = self.scale
        verts = [_scale * Vector(v) for v in [
            (-0.112, -0.112, 0.0), (0.112, -0.112, 0.0), (-0.112, 0.112, 0.0),
            (0.112, 0.112, 0.0), (-1.0, 0.0, 0.0), (0.0, -1.0, 0.0),
            (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 0.05),
            (0.374, 0.374, 0.0), (-0.374, 0.374, 0.0), (0.374, -0.374, 0.0),
            (-0.374, -0.374, 0.0), (-0.206, -0.1, 0.0), (-0.1, -0.206, 0.0),
            (0.1, -0.206, 0.0), (0.206, -0.1, 0.0), (-0.1, 0.206, 0.0),
            (-0.206, 0.1, 0.0), (0.206, 0.1, 0.0), (0.1, 0.206, 0.0),
            (-0.038, 1.185, 0.0), (-0.038, 1.052, 0.0), (0.038, 1.185, 0.0),
            (0.038, 1.052, 0.0)
        ]]
        edges = [
            (0, 13), (1, 15), (2, 17),
            (2, 18), (0, 14), (1, 16),
            (8, 7), (5, 8), (8, 4),
            (6, 8), (15, 11), (19, 9),
            (17, 10), (13, 12), (12, 14),
            (11, 16), (10, 18), (9, 20),
            (3, 9), (2, 10), (4, 18),
            (5, 15), (3, 8), (7, 17),
            (7, 20), (5, 14), (6, 19),
            (2, 8), (4, 13), (6, 16),
            (1, 8), (1, 11), (0, 8),
            (0, 12), (3, 19), (3, 20),
            (22, 21), (24, 23), (21, 24)
        ]

        bm = bmed._start(o)
        bm.clear()
        for v in verts:
            bm.verts.new(v)
        bm.verts.ensure_lookup_table()
        for ed in edges:
            bm.edges.new((bm.verts[ed[0]], bm.verts[ed[1]]))
        bmed._end(bm, o)

    def update(self, context, update_mesh=True):
        o = self.find_in_selection(context, self.auto_update)
        if o is None:
            return

        if update_mesh:
            self.buildmesh(o)

        self.ephemeris_label()
        self.update_childs(context, o)
        self.restore_context(context)

    def make_animatable(self):
        idx = -1
        try:
            idx = bpy.app.handlers.frame_change_pre.index(animate_location)
        except:
            pass

        if self.animate:
            if idx == -1:
                bpy.app.handlers.frame_change_pre.append(animate_location)
        elif idx != -1:
            bpy.app.handlers.frame_change_pre.remove(animate_location)

    def ephemeris_label(self):
        for xx in range(4):
            dst = 0
            tz = self.tz
            if xx == 0 or xx == 3:
                tz += 1
                dst = 1
            jde = sun_calculator.ephemeris(xx, self.year)
            year, month, gd = sun_calculator.julian_to_gregorian(jde, tz, TAI_DT)
            day = int(gd)
            f = gd - day
            f = f * 24
            hh = int(f)
            f = f - hh
            f = f * 60
            mm = int(f)
            f = f - mm
            ss = int(f * 60)
            setattr(self, "eph%s" % xx, datetime.datetime(year, month, day, hh, mm, ss).isoformat())

    def ephemeris(self):
        """Create ephemeris lines
        :return:
        """
        every_min = 5
        min_step = int(60 / every_min)
        coords = []
        trs = []
        for xx in range(4):
            dst = 0
            tz = self.tz
            if xx == 0 or xx == 3:
                tz += 1  # adjust for summertime
                dst = 1
            jde = sun_calculator.ephemeris(xx, self.year)
            year, month, gd = sun_calculator.julian_to_gregorian(jde, tz, TAI_DT)
            day = int(gd)
            f = gd - day
            f = f * 24
            hh = int(f)
            f = f - hh
            f = f * 60
            mm = int(f)
            f = f - mm
            ss = int(f * 60)
            setattr(self, "eph%s" % xx, datetime.datetime(year, month, day, hh, mm, ss).isoformat())

            pts = []
            for hour in range(24):
                for min in range(min_step):
                    az, el = sun_calculator.geoSunData(
                        self.lat,
                        self.lon,
                        year,
                        month,
                        day,
                        hour + min * every_min / 60,
                        self.tz + dst
                    )
                    p = Matrix.Rotation(radians(az), 4, 'Z') \
                      @ Matrix.Rotation(radians(90 - el), 4, 'X') \
                      @ Vector((0, 0, self.dist))

                    if p.z > 0:
                        pts.append(p)

            if len(pts) > 1:
                coords.append(pts)

        for hour in range(8):
            pts = []
            for month in range(12):
                for day in range(0, 30, 5):
                    az, el = sun_calculator.geoSunData(
                        self.lat,
                        self.lon,
                        self.year,
                        month,
                        day,
                        3 * hour,
                        self.tz
                    )
                    p = Matrix.Rotation(radians(az), 4, 'Z') \
                        @ Matrix.Rotation(radians(90 - el), 4, 'X') \
                        @ Vector((0, 0, self.dist))
                    if p.z > 0:
                        pts.append(p)

            if len(pts) > 1:
                coords.append(pts)

        return Generator.pts_as_curve(coords, name="Ephemeris {}".format(self.year), closed=False, dimensions="3D")

    def shadow_study_anim(self, context, o):
        """
        :param context:
        :param o:
        :return:
        """
        every_min = 5
        min_step = int(60 / every_min)
        trs = []
        for xx in [1, 3]:

            dst = 0
            tz = self.tz
            if xx == 0 or xx == 3:
                tz += 1  # adjust for summertime
                dst = 1
            jde = sun_calculator.ephemeris(xx, self.year)
            year, month, gd = sun_calculator.julian_to_gregorian(jde, tz, TAI_DT)
            day = int(gd)
            f = gd - day
            f = f * 24
            hh = int(f)
            f = f - hh
            f = f * 60
            mm = int(f)
            f = f - mm
            ss = int(f * 60)

            for hour in range(24):
                for min in range(min_step):
                    az, el = sun_calculator.geoSunData(
                        self.lat,
                        self.lon,
                        year,
                        month,
                        day,
                        hour + min * every_min / 60,
                        self.tz + dst
                    )
                    p = Matrix.Rotation(radians(az), 4, 'Z') \
                        @ Matrix.Rotation(radians(90 - el), 4, 'X') \
                        @ Vector((0, 0, self.dist))

                    if p.z > 0:
                        trs.append((az, el))

        sun = self.find_sun(context, o)
        n_steps = len(trs)
        if n_steps < 1:
            return
        _start, _end = context.scene.frame_start, context.scene.frame_end
        frame_current = context.scene.frame_current
        frame_size = (_end - _start) / n_steps
        for frame, az_el in enumerate(trs):
            az, el = az_el
            sub_frame = _start + frame_size * frame
            context.scene.frame_set(sub_frame)
            sun.matrix_world = o.matrix_world \
                               @ Matrix.Rotation(radians(az), 4, 'Z') \
                               @ Matrix.Rotation(radians(90 - el), 4, 'X') \
                               @ Matrix.Translation(Vector((0, 0, self.dist)))
            sun.keyframe_insert("location", frame=sub_frame)
            sun.keyframe_insert("rotation_euler", frame=sub_frame)
        context.scene.frame_current = frame_current


class ARCHIPACK_OT_sun_bake(Operator):
    bl_idname = "archipack.sun_bake"
    bl_label = "Finalize animation"
    bl_description = "Turn animation into real motion path"

    @classmethod
    def poll(cls, context):
        o = context.active_object
        return archipack_sun.filter(o)

    def execute(self, context):
        o = context.active_object
        d = archipack_sun.datablock(o)
        if d:
            sun = None
            for c in o.children:
                if c.type == 'LIGHT':
                    sun = c
                    break
            if sun is not None and o.data.animation_data is not None:
                action = o.data.animation_data.action
                _start, _end = action.frame_range
                frame_current = context.scene.frame_current
                for frame in range(int(_start), int(_end)):
                    context.scene.frame_set(frame)
                    d.update_childs(context, o)
                    sun.keyframe_insert("location", frame=frame)
                    sun.keyframe_insert("rotation_euler", frame=frame)
                context.scene.frame_current = frame_current
                # remove base sun animation
                o.data.animation_data_clear()
                bpy.data.actions.remove(action)
                # disable pre_frame update handlers
                d.animate = False
        return {'FINISHED'}


class ARCHIPACK_OT_sun_time(Operator):
    bl_idname = "archipack.sun_time"
    bl_label = "Set time"
    bl_description = "Update the time and date settings to the current one"

    iso:StringProperty()

    @classmethod
    def poll(cls, context):
        o = context.active_object
        return archipack_sun.filter(o)

    def execute(self, context):
        o = context.active_object
        d = archipack_sun.datablock(o)
        if d:
            d.auto_update = False
            dt = datetime.datetime.fromisoformat(self.iso)
            for p in ("hour", "minute", "day", "month", "year"):
                setattr(
                    d,
                    p,
                    getattr(dt, p)
                )
            d.auto_update = True

        return {'FINISHED'}


class ARCHIPACK_OT_sun_location_add(Archipacki18n, Operator):
    bl_idname = "archipack.sun_location_add"
    bl_label = "Add location preset"
    bl_description = "Add / replace a location preset"

    zone_idx: IntProperty()
    country_idx: IntProperty()
    city: StringProperty(
        name="City name",
        description="Enter city name"
    )
    lat: FloatProperty(
        name="Latitude",
        precision=5,
        min=-pi / 2,
        max=pi / 2,
        subtype='ANGLE', unit='ROTATION',
        default=0.0
    )
    lon: FloatProperty(
        name="Longitude",
        precision=5,
        min=-pi,
        max=pi,
        subtype='ANGLE', unit='ROTATION',
        default=0.0
    )

    @classmethod
    def poll(cls, context):
        o = context.active_object
        return hasattr(context.window_manager, "archipack") and archipack_sun.filter(o)

    def execute(self, context):
        import os
        import json
        city = self.city.strip()
        if len(city) == 0:
            return {'CANCELLED'}

        sp = context.window_manager.archipack.sun
        city = city.capitalize()
        iso = sp.country[self.country_idx].iso
        tz = sp.timezones[self.zone_idx].label
        item = (iso,
                city,
                degrees(self.lat),
                degrees(self.lon),
                tz)

        sc = sun_calculator

        # replace the location ?
        replace = False
        for i, seek in enumerate(sc.citys):
            s_iso, s_city, lat, lon, s_tz = seek
            if iso == s_iso and city == s_city and tz == s_tz:
                sc.citys[i] = item
                replace = True
                break

        if not replace:
            sc.citys.append(item)
            sc.citys.sort(key=lambda x: x[1])

        user_path = bpy.utils.user_resource('SCRIPTS',
                                            os.path.join("presets", "archipack_sun"),
                                            create=True)
        preset_path = os.path.join(user_path, "citys.json")

        with open(preset_path, 'w') as f:
             json.dump(sc.citys, f)

        init_coll(None)

        return {'FINISHED'}

    def draw(self, context):
        sp = context.window_manager.archipack.sun
        layout = self.layout
        self.draw_translate(context, layout)
        box = layout.box()

        self.draw_label(context, layout, box, "Location")
        box.prop(self, 'lat', text_ctxt=self.translation_context)
        box.prop(self, 'lon', text_ctxt=self.translation_context)
        self.draw_op(context, layout, box, "wm.url_open",
                     icon='URL',
                     text="Find location online"
             ).url = "https://www.latlong.net/"
        box = layout.box()
        box.prop(self, "city", text_ctxt=self.translation_context)
        box = layout.box()
        self.draw_label(context, layout, box, "Country")
        box.template_list("ARCHIPACK_UL_Country", "save", sp, "country", self, "country_idx", rows=1, maxrows=5)
        box = layout.box()
        self.draw_label(context, layout, box, "Timezone")
        box.template_list("ARCHIPACK_UL_Timezone", "save", sp, "timezones", self, "zone_idx", rows=1, maxrows=5)
        tz = sun_calculator.timezones[sp.timezones[self.zone_idx].label]
        self.draw_label(context, layout, box, "GMT offset:", postfix=str(tz))

    def invoke(self, context, event):
        o = context.active_object
        d = archipack_sun.datablock(o)
        if d is None:
            return {'CANCELLED'}
        sc = sun_calculator
        sp = context.window_manager.archipack.sun
        iso, name, lat, lon, timezone = sc.citys[d.city_idx]
        for i, c in enumerate(sp.country):
            if c.iso == iso:
                self.country_idx = i
                break
        self.lat = d.lat
        self.lon = d.lon
        self.zone_idx = d.zone_idx
        return context.window_manager.invoke_props_dialog(self)


class ARCHIPACK_OT_ephemeris(ArchipackCreateTool, Operator):
    bl_idname = "archipack.ephemeris"
    bl_label = "Ephemeris"
    bl_description = "Create a ephemeris curves"

    @classmethod
    def poll(cls, context):
        o = context.active_object
        return hasattr(context.window_manager, "archipack") and archipack_sun.filter(o)

    def create(self, context):
        o = context.active_object
        d = archipack_sun.datablock(o)
        c = d.ephemeris()
        self.link_object_to_scene(context, c, layer_name="Lights")
        c.matrix_world = o.matrix_world.copy()
        return c

    def execute(self, context):
        self.create(context)
        return {'FINISHED'}


class ARCHIPACK_OT_shadow_study(ArchipackCreateTool, Operator):
    bl_idname = "archipack.shadow_study"
    bl_label = "Shadow study"
    bl_description = "Animate sun for shadow study"

    @classmethod
    def poll(cls, context):
        o = context.active_object
        return hasattr(context.window_manager, "archipack") and archipack_sun.filter(o)

    def create(self, context):
        o = context.active_object
        d = archipack_sun.datablock(o)
        d.shadow_study_anim(context, o)

    def execute(self, context):
        self.create(context)
        return {'FINISHED'}


class ARCHIPACK_OT_sun(ArchipackCreateTool, Operator):
    bl_idname = "archipack.sun"
    bl_label = "Sunlight"
    bl_description = "Create a sunlight"

    @classmethod
    def poll(cls, context):
        return hasattr(context.window_manager, "archipack")

    def create(self, context):
        m = bpy.data.meshes.new("Sun")
        o = bpy.data.objects.new("Sun", m)
        d = m.archipack_sun.add()
        self.link_object_to_scene(context, o, layer_name="Lights")
        # hide from render
        o.display.show_shadows = False
        o.hide_render = True
        o.cycles_visibility.camera = False
        o.cycles_visibility.diffuse = False
        o.cycles_visibility.glossy = False
        o.cycles_visibility.shadow = False
        o.cycles_visibility.scatter = False
        o.cycles_visibility.transmission = False

        # constraint update after rendering ..
        # c = lamp.constraints.new("TRACK_TO")
        # c.track_axis = "TRACK_NEGATIVE_Z"
        # c.up_axis = "UP_Y"
        # c.target = o
        self.select_object(context, o, True)
        self.add_material(context, o, category="dimension_auto", material="DEFAULT")
        self.load_preset(d)
        d.update(context, update_mesh=True)

        if len(context.window_manager.archipack.sun.city) == 0:
            # on addon setup should populate this list
            init_coll()

        return o

    def execute(self, context):
        o = self.create(context)
        return {'FINISHED'}

 
class ARCHIPACK_PT_sun(ArchipackPanel, Archipacki18n, Panel):
    bl_idname = "ARCHIPACK_PT_sun"
    bl_label = 'Sun'

    @classmethod
    def poll(cls, context):
        o = context.active_object
        return hasattr(context.window_manager, "archipack") and archipack_sun.filter(o)

    def draw(self, context):

        layout = self.layout
        o = context.active_object
        d = archipack_sun.datablock(o)
        if d is None:
            return
        sc = context.window_manager.archipack.sun

        self.draw_common(context, layout)
        self.draw_prop(context, layout, layout, d, 'tabs', expand=True)

        box = layout.box()
        if d.tabs == 'LIGHT':
            for c in o.children:
                if c.type == 'LIGHT':
                    self.draw_label(context, layout, box, "Light")
                    row = box.row(align=True)
                    row.prop(c.data, "type", expand=True)
                    box.prop(c.data, "energy")
                    self.draw_prop(context, layout, box, d, "dist")
                    box = layout.box()
                    break
            self.draw_prop(context, layout, box, d, "scale")

        elif d.tabs == 'TIME':
            row = box.row(align=True)
            self.draw_label(context, layout, row, "Time")

            row = box.row(align=True)
            self.draw_prop(context, layout, row, d, "hour")
            self.draw_prop(context, layout, row, d, "minute")

            # row = box.row(align=True)
            self.draw_label(context, layout, box, "Date")
            row = box.row(align=True)
            self.draw_prop(context, layout, row, d, "day")
            self.draw_prop(context, layout, row, d, "month")
            self.draw_prop(context, layout, box, d, "year")
            self.draw_prop(context, layout, box, d, "dst")
            self.draw_op(context, layout, box,
                         "archipack.sun_time",
                         icon="PREVIEW_RANGE").iso = datetime.datetime.now().isoformat()
            box = layout.box()
            self.draw_prop(context, layout, box, d, "animate", icon="TIME")
            if d.animate:
                self.draw_prop(context, layout, box, d, "hours")
                self.draw_op(context, layout, box, "archipack.sun_bake")

            self.draw_op(context, layout, box, "archipack.shadow_study", icon="TIME")

            box = layout.box()
            self.draw_op(context, layout, box, "archipack.ephemeris", icon="PREVIEW_RANGE")
            if len(d.eph0) > 0:
                self.draw_label(context, layout, box, d.eph0.split(" ")[0])
                self.draw_label(context, layout, box, d.eph1.split(" ")[0])
                self.draw_label(context, layout, box, d.eph2.split(" ")[0])
                self.draw_label(context, layout, box, d.eph3.split(" ")[0])

        elif d.tabs == 'LOC':
            row = box.row(align=True)
            self.draw_label(context, layout, row, "Location")

            box.template_list("ARCHIPACK_UL_City", "", sc, "city", d, "city_idx", rows=1, maxrows=5)
            row = box.row(align=True)
            self.draw_prop(context, layout, row, d, "lat")
            self.draw_prop(context, layout, row, d, "lon")
            self.draw_op(context, layout, box, "wm.url_open", text="Find online", icon="URL").url = "https://www.latlong.net/"
            box = layout.box()
            self.draw_label(context, layout, box, "Timezone")
            box.template_list("ARCHIPACK_UL_Timezone", "", sc, "timezones", d, "zone_idx", rows=1, maxrows=1)
            self.draw_label(context, layout, box, "GMT offset:", postfix=str(d.tz))
            self.draw_op(context, layout, box, "archipack.sun_location_add", icon="ADD")


classes = (
    archipack_sun,
    ARCHIPACK_OT_sun,
    ARCHIPACK_OT_sun_time,
    ARCHIPACK_OT_sun_bake,
    ARCHIPACK_OT_ephemeris,
    ARCHIPACK_OT_shadow_study,
    ARCHIPACK_OT_sun_location_add,
    archipack_city,
    archipack_timezone,
    archipack_country,
    archipack_sun_presets,
    ARCHIPACK_UL_Timezone,
    ARCHIPACK_UL_City,
    ARCHIPACK_UL_Country,
    ARCHIPACK_PT_sun
)


@persistent
def init_coll(dummy=None):
    # hack to init UILists from data
    if not hasattr(bpy.context.window_manager, "archipack"):
        return
    sp = bpy.context.window_manager.archipack.sun
    sc = sun_calculator

    if len(sc.citys) == 0:
        import json
        import os

        user_path = bpy.utils.user_resource('SCRIPTS',
                                            os.path.join("presets", "archipack_sun"),
                                            create=False)
        if os.path.exists(user_path):
            preset_path = os.path.join(user_path, "citys.json")
        else:
            preset_path = os.path.join(os.path.dirname(__file__), "presets", "archipack_sun", "citys.json")

        with open(preset_path, 'r') as f:
            sc.citys = json.load(f)

    try:
        sp.timezones.clear()
    except:
        pass
    for label, offset in sc.timezones.items():
        item = sp.timezones.add()
        item.label = label
    try:
        sp.city.clear()
    except:
        pass
    for country, city, lat, lon, zone in sc.citys:
        item = sp.city.add()

        item.label = "{} - {}, {}".format(country, city, sc.countrys[country])
        item.lat = lat
        item.lon = lon
        item.zone = zone
    try:
        sp.country.clear()
    except:
        pass
    for iso, country in sc.countrys.items():
        item = sp.country.add()
        item.iso = iso
        item.label = "{} - {}".format(iso, country)

    # setup / remove animation support
    for o in bpy.context.scene.objects:
        d = archipack_sun.datablock(o)
        if d is not None:
            d.make_animatable()

 
def register():
    for cls in classes:
        bpy.utils.register_class(cls)
    bpy.types.Mesh.archipack_sun = CollectionProperty(type=archipack_sun)
    bpy.app.handlers.load_post.append(init_coll)


def unregister():
    for cls in classes:
        bpy.utils.unregister_class(cls)
    del bpy.types.Mesh.archipack_sun
    bpy.app.handlers.load_post.remove(init_coll)