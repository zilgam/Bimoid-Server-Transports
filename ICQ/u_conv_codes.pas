// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_conv_codes;

interface

uses Windows, SysUtils, u_obimp_codes;

type
  TOscarLCID = record
    ObimpCode : Word;
    ValueISO  : string;
  end;

const
  OscarCountries: array[0..COUNTRY_CODE_MAX-1] of TOscarLCID = (
    (ObimpCode: COUNTRY_CODE_AFGHANISTAN; ValueISO: 'AF'),
    (ObimpCode: COUNTRY_CODE_ALBANIA; ValueISO: 'AL'),
    (ObimpCode: COUNTRY_CODE_ANTARCTICA; ValueISO: ''),
    (ObimpCode: COUNTRY_CODE_ALGERIA; ValueISO: 'DZ'),
    (ObimpCode: COUNTRY_CODE_AMERICAN_SAMOA; ValueISO: 'AS'),
    (ObimpCode: COUNTRY_CODE_ANDORRA; ValueISO: 'AD'),
    (ObimpCode: COUNTRY_CODE_ANGOLA; ValueISO: 'AO'),
    (ObimpCode: COUNTRY_CODE_ANTIGUA_AND_BARBUDA; ValueISO: 'AG'),
    (ObimpCode: COUNTRY_CODE_AZERBAIJAN; ValueISO: 'AZ'),
    (ObimpCode: COUNTRY_CODE_ARGENTINA; ValueISO: 'AR'),
    (ObimpCode: COUNTRY_CODE_AUSTRALIA; ValueISO: 'AU'),
    (ObimpCode: COUNTRY_CODE_AUSTRIA; ValueISO: 'AT'),
    (ObimpCode: COUNTRY_CODE_BAHAMAS; ValueISO: 'BS'),
    (ObimpCode: COUNTRY_CODE_BAHRAIN; ValueISO: 'BH'),
    (ObimpCode: COUNTRY_CODE_BANGLADESH; ValueISO: 'BD'),
    (ObimpCode: COUNTRY_CODE_ARMENIA; ValueISO: 'AM'),
    (ObimpCode: COUNTRY_CODE_BARBADOS; ValueISO: 'BB'),
    (ObimpCode: COUNTRY_CODE_BELGIUM; ValueISO: 'BE'),
    (ObimpCode: COUNTRY_CODE_BERMUDA; ValueISO: 'BM'),
    (ObimpCode: COUNTRY_CODE_BHUTAN; ValueISO: 'BT'),
    (ObimpCode: COUNTRY_CODE_BOLIVIA; ValueISO: 'BO'),
    (ObimpCode: COUNTRY_CODE_BOSNIA_AND_HERZEGOVINA; ValueISO: 'BA'),
    (ObimpCode: COUNTRY_CODE_BOTSWANA; ValueISO: 'BW'),
    (ObimpCode: COUNTRY_CODE_BOUVET_ISLAND; ValueISO: ''),
    (ObimpCode: COUNTRY_CODE_BRAZIL; ValueISO: 'BR'),
    (ObimpCode: COUNTRY_CODE_BELIZE; ValueISO: 'BZ'),
    (ObimpCode: COUNTRY_CODE_BRITISH_INDIAN_OCEAN_TERRITORY; ValueISO: ''),
    (ObimpCode: COUNTRY_CODE_SOLOMON_ISLANDS; ValueISO: 'SB'),
    (ObimpCode: COUNTRY_CODE_VIRGIN_ISLANDS_BRITISH; ValueISO: 'VG'),
    (ObimpCode: COUNTRY_CODE_BRUNEI_DARUSSALAM; ValueISO: 'BN'),
    (ObimpCode: COUNTRY_CODE_BULGARIA; ValueISO: 'BG'),
    (ObimpCode: COUNTRY_CODE_MYANMAR; ValueISO: 'MM'),
    (ObimpCode: COUNTRY_CODE_BURUNDI; ValueISO: 'BI'),
    (ObimpCode: COUNTRY_CODE_BELARUS; ValueISO: 'BY'),
    (ObimpCode: COUNTRY_CODE_CAMBODIA; ValueISO: 'KH'),
    (ObimpCode: COUNTRY_CODE_CAMEROON; ValueISO: 'CM'),
    (ObimpCode: COUNTRY_CODE_CANADA; ValueISO: 'CA'),
    (ObimpCode: COUNTRY_CODE_CAPE_VERDE; ValueISO: 'CV'),
    (ObimpCode: COUNTRY_CODE_CAYMAN_ISLANDS; ValueISO: 'KY'),
    (ObimpCode: COUNTRY_CODE_CENTRAL_AFRICAN_REPUBLIC; ValueISO: 'CF'),
    (ObimpCode: COUNTRY_CODE_SRI_LANKA; ValueISO: 'LK'),
    (ObimpCode: COUNTRY_CODE_CHAD; ValueISO: 'TD'),
    (ObimpCode: COUNTRY_CODE_CHILE; ValueISO: 'CL'),
    (ObimpCode: COUNTRY_CODE_CHINA; ValueISO: 'CN'),
    (ObimpCode: COUNTRY_CODE_TAIWAN_PROVINCE_OF_CHINA; ValueISO: 'TW'),
    (ObimpCode: COUNTRY_CODE_CHRISTMAS_ISLAND; ValueISO: 'CX'),
    (ObimpCode: COUNTRY_CODE_COCOS_KEELING_ISLANDS; ValueISO: 'CC'),
    (ObimpCode: COUNTRY_CODE_COLOMBIA; ValueISO: 'CO'),
    (ObimpCode: COUNTRY_CODE_COMOROS; ValueISO: 'KM'),
    (ObimpCode: COUNTRY_CODE_MAYOTTE; ValueISO: 'YT'),
    (ObimpCode: COUNTRY_CODE_CONGO; ValueISO: 'CG'),
    (ObimpCode: COUNTRY_CODE_CONGO_DEMOCRATIC_REPUBLIC_OF_THE; ValueISO: 'CD'),
    (ObimpCode: COUNTRY_CODE_COOK_ISLANDS; ValueISO: 'CK'),
    (ObimpCode: COUNTRY_CODE_COSTA_RICA; ValueISO: 'CR'),
    (ObimpCode: COUNTRY_CODE_CROATIA; ValueISO: 'HR'),
    (ObimpCode: COUNTRY_CODE_CUBA; ValueISO: 'CU'),
    (ObimpCode: COUNTRY_CODE_CYPRUS; ValueISO: 'CY'),
    (ObimpCode: COUNTRY_CODE_CZECH_REPUBLIC; ValueISO: 'CZ'),
    (ObimpCode: COUNTRY_CODE_BENIN; ValueISO: 'BJ'),
    (ObimpCode: COUNTRY_CODE_DENMARK; ValueISO: 'DK'),
    (ObimpCode: COUNTRY_CODE_DOMINICA; ValueISO: 'DM'),
    (ObimpCode: COUNTRY_CODE_DOMINICAN_REPUBLIC; ValueISO: 'DO'),
    (ObimpCode: COUNTRY_CODE_ECUADOR; ValueISO: 'EC'),
    (ObimpCode: COUNTRY_CODE_EL_SALVADOR; ValueISO: 'SV'),
    (ObimpCode: COUNTRY_CODE_EQUATORIAL_GUINEA; ValueISO: 'GQ'),
    (ObimpCode: COUNTRY_CODE_ETHIOPIA; ValueISO: 'ET'),
    (ObimpCode: COUNTRY_CODE_ERITREA; ValueISO: 'ER'),
    (ObimpCode: COUNTRY_CODE_ESTONIA; ValueISO: 'EE'),
    (ObimpCode: COUNTRY_CODE_FAROE_ISLANDS; ValueISO: 'FO'),
    (ObimpCode: COUNTRY_CODE_FALKLAND_ISLANDS_MALVINAS; ValueISO: 'FK'),
    (ObimpCode: COUNTRY_CODE_SOUTH_GEORGIA_AND_THE_SOUTH_SANDWICH_ISLANDS; ValueISO: ''), {GS}
    (ObimpCode: COUNTRY_CODE_FIJI; ValueISO: 'FJ'),
    (ObimpCode: COUNTRY_CODE_FINLAND; ValueISO: 'FI'),
    (ObimpCode: COUNTRY_CODE_ALAND_ISLANDS; ValueISO: ''), {AX}
    (ObimpCode: COUNTRY_CODE_FRANCE; ValueISO: 'FR'),
    (ObimpCode: COUNTRY_CODE_FRENCH_GUIANA; ValueISO: 'GF'),
    (ObimpCode: COUNTRY_CODE_FRENCH_POLYNESIA; ValueISO: 'PF'),
    (ObimpCode: COUNTRY_CODE_FRENCH_SOUTHERN_TERRITORIES; ValueISO: ''), {TF}
    (ObimpCode: COUNTRY_CODE_DJIBOUTI; ValueISO: 'DJ'),
    (ObimpCode: COUNTRY_CODE_GABON; ValueISO: 'GA'),
    (ObimpCode: COUNTRY_CODE_GEORGIA; ValueISO: 'GE'),
    (ObimpCode: COUNTRY_CODE_GAMBIA; ValueISO: 'GM'),
    (ObimpCode: COUNTRY_CODE_PALESTINIAN_TERRITORY_OCCUPIED; ValueISO: ''), {PS}
    (ObimpCode: COUNTRY_CODE_GERMANY; ValueISO: 'DE'),
    (ObimpCode: COUNTRY_CODE_GHANA; ValueISO: 'GH'),
    (ObimpCode: COUNTRY_CODE_GIBRALTAR; ValueISO: 'GI'),
    (ObimpCode: COUNTRY_CODE_KIRIBATI; ValueISO: 'KI'),
    (ObimpCode: COUNTRY_CODE_GREECE; ValueISO: 'GR'),
    (ObimpCode: COUNTRY_CODE_GREENLAND; ValueISO: 'GL'),
    (ObimpCode: COUNTRY_CODE_GRENADA; ValueISO: 'GD'),
    (ObimpCode: COUNTRY_CODE_GUADELOUPE; ValueISO: 'GP'),
    (ObimpCode: COUNTRY_CODE_GUAM; ValueISO: 'GU'),
    (ObimpCode: COUNTRY_CODE_GUATEMALA; ValueISO: 'GT'),
    (ObimpCode: COUNTRY_CODE_GUINEA; ValueISO: 'GN'),
    (ObimpCode: COUNTRY_CODE_GUYANA; ValueISO: 'GY'),
    (ObimpCode: COUNTRY_CODE_HAITI; ValueISO: 'HT'),
    (ObimpCode: COUNTRY_CODE_HEARD_ISLAND_AND_MCDONALD_ISLANDS; ValueISO: ''),
    (ObimpCode: COUNTRY_CODE_HOLY_SEE_VATICAN_CITY_STATE; ValueISO: 'VA'),
    (ObimpCode: COUNTRY_CODE_HONDURAS; ValueISO: 'HN'),
    (ObimpCode: COUNTRY_CODE_HONG_KONG; ValueISO: 'HK'),
    (ObimpCode: COUNTRY_CODE_HUNGARY; ValueISO: 'HU'),
    (ObimpCode: COUNTRY_CODE_ICELAND; ValueISO: 'IS'),
    (ObimpCode: COUNTRY_CODE_INDIA; ValueISO: 'IN'),
    (ObimpCode: COUNTRY_CODE_INDONESIA; ValueISO: 'ID'),
    (ObimpCode: COUNTRY_CODE_IRAN_ISLAMIC_REPUBLIC_OF; ValueISO: 'IR'),
    (ObimpCode: COUNTRY_CODE_IRAQ; ValueISO: 'IQ'),
    (ObimpCode: COUNTRY_CODE_IRELAND; ValueISO: 'IE'),
    (ObimpCode: COUNTRY_CODE_ISRAEL; ValueISO: 'IL'),
    (ObimpCode: COUNTRY_CODE_ITALY; ValueISO: 'IT'),
    (ObimpCode: COUNTRY_CODE_COTE_DIVOIRE; ValueISO: 'CI'),
    (ObimpCode: COUNTRY_CODE_JAMAICA; ValueISO: 'JM'),
    (ObimpCode: COUNTRY_CODE_JAPAN; ValueISO: 'JP'),
    (ObimpCode: COUNTRY_CODE_KAZAKHSTAN; ValueISO: 'KZ'),
    (ObimpCode: COUNTRY_CODE_JORDAN; ValueISO: 'JO'),
    (ObimpCode: COUNTRY_CODE_KENYA; ValueISO: 'KE'),
    (ObimpCode: COUNTRY_CODE_KOREA_DEMOCRATIC_PEOPLES_REPUBLIC_OF; ValueISO: 'KP'),
    (ObimpCode: COUNTRY_CODE_KOREA_REPUBLIC_OF; ValueISO: 'KR'),
    (ObimpCode: COUNTRY_CODE_KUWAIT; ValueISO: 'KW'),
    (ObimpCode: COUNTRY_CODE_KYRGYZSTAN; ValueISO: 'KG'),
    (ObimpCode: COUNTRY_CODE_LAO_PEOPLES_DEMOCRATIC_REPUBLIC; ValueISO: 'LA'),
    (ObimpCode: COUNTRY_CODE_LEBANON; ValueISO: 'LB'),
    (ObimpCode: COUNTRY_CODE_LESOTHO; ValueISO: 'LS'),
    (ObimpCode: COUNTRY_CODE_LATVIA; ValueISO: 'LV'),
    (ObimpCode: COUNTRY_CODE_LIBERIA; ValueISO: 'LR'),
    (ObimpCode: COUNTRY_CODE_LIBYAN_ARAB_JAMAHIRIYA; ValueISO: 'LY'),
    (ObimpCode: COUNTRY_CODE_LIECHTENSTEIN; ValueISO: 'LI'),
    (ObimpCode: COUNTRY_CODE_LITHUANIA; ValueISO: 'LT'),
    (ObimpCode: COUNTRY_CODE_LUXEMBOURG; ValueISO: 'LU'),
    (ObimpCode: COUNTRY_CODE_MACAO; ValueISO: 'MO'),
    (ObimpCode: COUNTRY_CODE_MADAGASCAR; ValueISO: 'MG'),
    (ObimpCode: COUNTRY_CODE_MALAWI; ValueISO: 'MW'),
    (ObimpCode: COUNTRY_CODE_MALAYSIA; ValueISO: 'MY'),
    (ObimpCode: COUNTRY_CODE_MALDIVES; ValueISO: 'MV'),
    (ObimpCode: COUNTRY_CODE_MALI; ValueISO: 'ML'),
    (ObimpCode: COUNTRY_CODE_MALTA; ValueISO: 'MT'),
    (ObimpCode: COUNTRY_CODE_MARTINIQUE; ValueISO: 'MQ'),
    (ObimpCode: COUNTRY_CODE_MAURITANIA; ValueISO: 'MR'),
    (ObimpCode: COUNTRY_CODE_MAURITIUS; ValueISO: 'MU'),
    (ObimpCode: COUNTRY_CODE_MEXICO; ValueISO: 'MX'),
    (ObimpCode: COUNTRY_CODE_MONACO; ValueISO: 'MC'),
    (ObimpCode: COUNTRY_CODE_MONGOLIA; ValueISO: 'MN'),
    (ObimpCode: COUNTRY_CODE_MOLDOVA_REPUBLIC_OF; ValueISO: 'MD'),
    (ObimpCode: COUNTRY_CODE_MONTENEGRO; ValueISO: 'ME'),
    (ObimpCode: COUNTRY_CODE_MONTSERRAT; ValueISO: 'MS'),
    (ObimpCode: COUNTRY_CODE_MOROCCO; ValueISO: 'MA'),
    (ObimpCode: COUNTRY_CODE_MOZAMBIQUE; ValueISO: 'MZ'),
    (ObimpCode: COUNTRY_CODE_OMAN; ValueISO: 'OM'),
    (ObimpCode: COUNTRY_CODE_NAMIBIA; ValueISO: 'NA'),
    (ObimpCode: COUNTRY_CODE_NAURU; ValueISO: 'NR'),
    (ObimpCode: COUNTRY_CODE_NEPAL; ValueISO: 'NP'),
    (ObimpCode: COUNTRY_CODE_NETHERLANDS; ValueISO: 'NL'),
    (ObimpCode: COUNTRY_CODE_NETHERLANDS_ANTILLES; ValueISO: 'AN'),
    (ObimpCode: COUNTRY_CODE_ARUBA; ValueISO: 'AW'),
    (ObimpCode: COUNTRY_CODE_NEW_CALEDONIA; ValueISO: 'NC'),
    (ObimpCode: COUNTRY_CODE_VANUATU; ValueISO: 'VU'),
    (ObimpCode: COUNTRY_CODE_NEW_ZEALAND; ValueISO: 'NZ'),
    (ObimpCode: COUNTRY_CODE_NICARAGUA; ValueISO: 'NI'),
    (ObimpCode: COUNTRY_CODE_NIGER; ValueISO: 'NE'),
    (ObimpCode: COUNTRY_CODE_NIGERIA; ValueISO: 'NG'),
    (ObimpCode: COUNTRY_CODE_NIUE; ValueISO: 'NU'),
    (ObimpCode: COUNTRY_CODE_NORFOLK_ISLAND; ValueISO: 'NF'),
    (ObimpCode: COUNTRY_CODE_NORWAY; ValueISO: 'NO'),
    (ObimpCode: COUNTRY_CODE_NORTHERN_MARIANA_ISLANDS; ValueISO: ''), {MP}
    (ObimpCode: COUNTRY_CODE_UNITED_STATES_MINOR_OUTLYING_ISLANDS; ValueISO: ''),
    (ObimpCode: COUNTRY_CODE_MICRONESIA_FEDERATED_STATES_OF; ValueISO: 'FM'),
    (ObimpCode: COUNTRY_CODE_MARSHALL_ISLANDS; ValueISO: 'MH'),
    (ObimpCode: COUNTRY_CODE_PALAU; ValueISO: 'PW'),
    (ObimpCode: COUNTRY_CODE_PAKISTAN; ValueISO: 'PK'),
    (ObimpCode: COUNTRY_CODE_PANAMA; ValueISO: 'PA'),
    (ObimpCode: COUNTRY_CODE_PAPUA_NEW_GUINEA; ValueISO: 'PG'),
    (ObimpCode: COUNTRY_CODE_PARAGUAY; ValueISO: 'PY'),
    (ObimpCode: COUNTRY_CODE_PERU; ValueISO: 'PE'),
    (ObimpCode: COUNTRY_CODE_PHILIPPINES; ValueISO: 'PH'),
    (ObimpCode: COUNTRY_CODE_PITCAIRN; ValueISO: ''), {PN}
    (ObimpCode: COUNTRY_CODE_POLAND; ValueISO: 'PL'),
    (ObimpCode: COUNTRY_CODE_PORTUGAL; ValueISO: 'PT'),
    (ObimpCode: COUNTRY_CODE_GUINEA_BISSAU; ValueISO: 'GW'),
    (ObimpCode: COUNTRY_CODE_TIMOR_LESTE; ValueISO: ''), {TI}
    (ObimpCode: COUNTRY_CODE_PUERTO_RICO; ValueISO: 'PR'),
    (ObimpCode: COUNTRY_CODE_QATAR; ValueISO: 'QA'),
    (ObimpCode: COUNTRY_CODE_REUNION; ValueISO: 'XRI'),
    (ObimpCode: COUNTRY_CODE_ROMANIA; ValueISO: 'RO'),
    (ObimpCode: COUNTRY_CODE_RUSSIAN_FEDERATION; ValueISO: 'RU'),
    (ObimpCode: COUNTRY_CODE_RWANDA; ValueISO: 'RW'),
    (ObimpCode: COUNTRY_CODE_SAINT_BARTHELEMY; ValueISO: ''), {BL}
    (ObimpCode: COUNTRY_CODE_SAINT_HELENA; ValueISO: 'SH'),
    (ObimpCode: COUNTRY_CODE_SAINT_KITTS_AND_NEVIS; ValueISO: 'KN'),
    (ObimpCode: COUNTRY_CODE_ANGUILLA; ValueISO: 'AI'),
    (ObimpCode: COUNTRY_CODE_SAINT_LUCIA; ValueISO: 'LC'),
    (ObimpCode: COUNTRY_CODE_SAINT_MARTIN_FRENCH_PART; ValueISO: ''), {MF}
    (ObimpCode: COUNTRY_CODE_SAINT_PIERRE_AND_MIQUELON; ValueISO: 'PM'),
    (ObimpCode: COUNTRY_CODE_SAINT_VINCENT_AND_THE_GRENADINES; ValueISO: 'VC'),
    (ObimpCode: COUNTRY_CODE_SAN_MARINO; ValueISO: 'SM'),
    (ObimpCode: COUNTRY_CODE_SAO_TOME_AND_PRINCIPE; ValueISO: 'ST'),
    (ObimpCode: COUNTRY_CODE_SAUDI_ARABIA; ValueISO: 'SA'),
    (ObimpCode: COUNTRY_CODE_SENEGAL; ValueISO: 'SN'),
    (ObimpCode: COUNTRY_CODE_SERBIA; ValueISO: 'RS'),
    (ObimpCode: COUNTRY_CODE_SEYCHELLES; ValueISO: 'SC'),
    (ObimpCode: COUNTRY_CODE_SIERRA_LEONE; ValueISO: 'SL'),
    (ObimpCode: COUNTRY_CODE_SINGAPORE; ValueISO: 'SG'),
    (ObimpCode: COUNTRY_CODE_SLOVAKIA; ValueISO: 'SK'),
    (ObimpCode: COUNTRY_CODE_VIET_NAM; ValueISO: 'VN'),
    (ObimpCode: COUNTRY_CODE_SLOVENIA; ValueISO: 'SI'),
    (ObimpCode: COUNTRY_CODE_SOMALIA; ValueISO: 'SO'),
    (ObimpCode: COUNTRY_CODE_SOUTH_AFRICA; ValueISO: 'ZA'),
    (ObimpCode: COUNTRY_CODE_ZIMBABWE; ValueISO: 'ZW'),
    (ObimpCode: COUNTRY_CODE_SPAIN; ValueISO: 'ES'),
    (ObimpCode: COUNTRY_CODE_WESTERN_SAHARA; ValueISO: ''), {EH}
    (ObimpCode: COUNTRY_CODE_SUDAN; ValueISO: 'SD'),
    (ObimpCode: COUNTRY_CODE_SURINAME; ValueISO: 'SR'),
    (ObimpCode: COUNTRY_CODE_SVALBARD_AND_JAN_MAYEN; ValueISO: ''), {SJ}
    (ObimpCode: COUNTRY_CODE_SWAZILAND; ValueISO: 'SZ'),
    (ObimpCode: COUNTRY_CODE_SWEDEN; ValueISO: 'SE'),
    (ObimpCode: COUNTRY_CODE_SWITZERLAND; ValueISO: 'CH'),
    (ObimpCode: COUNTRY_CODE_SYRIAN_ARAB_REPUBLIC; ValueISO: 'SY'),
    (ObimpCode: COUNTRY_CODE_TAJIKISTAN; ValueISO: 'TJ'),
    (ObimpCode: COUNTRY_CODE_THAILAND; ValueISO: 'TH'),
    (ObimpCode: COUNTRY_CODE_TOGO; ValueISO: 'TG'),
    (ObimpCode: COUNTRY_CODE_TOKELAU; ValueISO: 'TK'),
    (ObimpCode: COUNTRY_CODE_TONGA; ValueISO: 'TO'),
    (ObimpCode: COUNTRY_CODE_TRINIDAD_AND_TOBAGO; ValueISO: 'TT'),
    (ObimpCode: COUNTRY_CODE_UNITED_ARAB_EMIRATES; ValueISO: 'AE'),
    (ObimpCode: COUNTRY_CODE_TUNISIA; ValueISO: 'TN'),
    (ObimpCode: COUNTRY_CODE_TURKEY; ValueISO: 'TR'),
    (ObimpCode: COUNTRY_CODE_TURKMENISTAN; ValueISO: 'TM'),
    (ObimpCode: COUNTRY_CODE_TURKS_AND_CAICOS_ISLANDS; ValueISO: 'TC'),
    (ObimpCode: COUNTRY_CODE_TUVALU; ValueISO: 'TV'),
    (ObimpCode: COUNTRY_CODE_UGANDA; ValueISO: 'UG'),
    (ObimpCode: COUNTRY_CODE_UKRAINE; ValueISO: 'UA'),
    (ObimpCode: COUNTRY_CODE_MACEDONIA_THE_FORMER_YUGOSLAV_REPUBLIC_OF; ValueISO: 'MK'),
    (ObimpCode: COUNTRY_CODE_EGYPT; ValueISO: 'EG'),
    (ObimpCode: COUNTRY_CODE_UNITED_KINGDOM; ValueISO: 'GB'),
    (ObimpCode: COUNTRY_CODE_GUERNSEY; ValueISO: ''), {GG}
    (ObimpCode: COUNTRY_CODE_JERSEY; ValueISO: ''), {JE}
    (ObimpCode: COUNTRY_CODE_ISLE_OF_MAN; ValueISO: ''), {IM}
    (ObimpCode: COUNTRY_CODE_TANZANIA_UNITED_REPUBLIC_OF; ValueISO: 'TZ'),
    (ObimpCode: COUNTRY_CODE_UNITED_STATES; ValueISO: 'US'),
    (ObimpCode: COUNTRY_CODE_VIRGIN_ISLANDS_US; ValueISO: 'VI'),
    (ObimpCode: COUNTRY_CODE_BURKINA_FASO; ValueISO: 'BF'),
    (ObimpCode: COUNTRY_CODE_URUGUAY; ValueISO: 'UY'),
    (ObimpCode: COUNTRY_CODE_UZBEKISTAN; ValueISO: 'UZ'),
    (ObimpCode: COUNTRY_CODE_VENEZUELA_BOLIVARIAN_REPUBLIC_OF; ValueISO: 'VE'),
    (ObimpCode: COUNTRY_CODE_WALLIS_AND_FUTUNA; ValueISO: 'WF'),
    (ObimpCode: COUNTRY_CODE_SAMOA; ValueISO: 'WS'),
    (ObimpCode: COUNTRY_CODE_YEMEN; ValueISO: 'YE'),
    (ObimpCode: COUNTRY_CODE_ZAMBIA; ValueISO: 'ZM')
    );

  OscarLangs: array[0..LANGUAGE_CODE_MAX-1] of TOscarLCID = (
    (ObimpCode: LANGUAGE_CODE_AFRIKAANS; ValueISO : 'AFR'),
    (ObimpCode: LANGUAGE_CODE_ALBANIAN; ValueISO : 'ALB'),
    (ObimpCode: LANGUAGE_CODE_ARABIC; ValueISO : 'ARA'),
    (ObimpCode: LANGUAGE_CODE_ARMENIAN; ValueISO : 'ARM'),
    (ObimpCode: LANGUAGE_CODE_AZERBAIJANI; ValueISO : 'AZE'),
    (ObimpCode: LANGUAGE_CODE_BELORUSSIAN; ValueISO : 'XX'), {???}
    (ObimpCode: LANGUAGE_CODE_BHOJPURI; ValueISO : 'BHO'),
    (ObimpCode: LANGUAGE_CODE_BOSNIAN; ValueISO : 'BOS'),
    (ObimpCode: LANGUAGE_CODE_BULGARIAN; ValueISO : 'BUL'),
    (ObimpCode: LANGUAGE_CODE_BURMESE; ValueISO : 'MYA'),
    (ObimpCode: LANGUAGE_CODE_CANTONESE; ValueISO : 'ZH-CANTONESE'),
    (ObimpCode: LANGUAGE_CODE_CATALAN; ValueISO : 'CAT'),
    (ObimpCode: LANGUAGE_CODE_CHAMORRO; ValueISO : 'CHA'),
    (ObimpCode: LANGUAGE_CODE_CHINESE; ValueISO : 'ZHO'),
    (ObimpCode: LANGUAGE_CODE_CROATIAN; ValueISO : 'SCR'),
    (ObimpCode: LANGUAGE_CODE_CZECH; ValueISO : 'CZE'),
    (ObimpCode: LANGUAGE_CODE_DANISH; ValueISO : 'DAN'),
    (ObimpCode: LANGUAGE_CODE_DUTCH; ValueISO : 'NLD'),
    (ObimpCode: LANGUAGE_CODE_ENGLISH; ValueISO : 'ENG'),
    (ObimpCode: LANGUAGE_CODE_ESPERANTO; ValueISO : 'EPO'),
    (ObimpCode: LANGUAGE_CODE_ESTONIAN; ValueISO : 'EST'),
    (ObimpCode: LANGUAGE_CODE_FARSI; ValueISO : 'FAS'),
    (ObimpCode: LANGUAGE_CODE_FINNISH; ValueISO : 'FIN'),
    (ObimpCode: LANGUAGE_CODE_FRENCH; ValueISO : 'FRE'),
    (ObimpCode: LANGUAGE_CODE_GAELIC; ValueISO : 'GLA'),
    (ObimpCode: LANGUAGE_CODE_GERMAN; ValueISO : 'GER'),
    (ObimpCode: LANGUAGE_CODE_GREEK; ValueISO : 'GRE'),
    (ObimpCode: LANGUAGE_CODE_GUJARATI; ValueISO : 'GUJ'),
    (ObimpCode: LANGUAGE_CODE_HEBREW; ValueISO : 'HEB'),
    (ObimpCode: LANGUAGE_CODE_HINDI; ValueISO : 'HIN'),
    (ObimpCode: LANGUAGE_CODE_HUNGARIAN; ValueISO : 'HUN'),
    (ObimpCode: LANGUAGE_CODE_ICELANDIC; ValueISO : 'ICE'),
    (ObimpCode: LANGUAGE_CODE_INDONESIAN; ValueISO : 'IND'),
    (ObimpCode: LANGUAGE_CODE_ITALIAN; ValueISO : 'ITA'),
    (ObimpCode: LANGUAGE_CODE_JAPANESE; ValueISO : 'JPN'),
    (ObimpCode: LANGUAGE_CODE_KHMER; ValueISO : 'KHM'),
    (ObimpCode: LANGUAGE_CODE_KOREAN; ValueISO : 'KOR'),
    (ObimpCode: LANGUAGE_CODE_KURDISH; ValueISO : 'KUR'),
    (ObimpCode: LANGUAGE_CODE_LAO; ValueISO : 'LAO'),
    (ObimpCode: LANGUAGE_CODE_LATVIAN; ValueISO : 'LAV'),
    (ObimpCode: LANGUAGE_CODE_LITHUANIAN; ValueISO : 'LIT'),
    (ObimpCode: LANGUAGE_CODE_MACEDONIAN; ValueISO : 'MAC'),
    (ObimpCode: LANGUAGE_CODE_MALAY; ValueISO : 'MAY'),
    (ObimpCode: LANGUAGE_CODE_MANDARIN; ValueISO : 'ZH-MANDARIN'),
    (ObimpCode: LANGUAGE_CODE_MONGOLIAN; ValueISO : 'MON'),
    (ObimpCode: LANGUAGE_CODE_NORWEGIAN; ValueISO : 'NNO'),
    (ObimpCode: LANGUAGE_CODE_PERSIAN; ValueISO : 'PER'),
    (ObimpCode: LANGUAGE_CODE_POLISH; ValueISO : 'POL'),
    (ObimpCode: LANGUAGE_CODE_PORTUGUESE; ValueISO : 'POR'),
    (ObimpCode: LANGUAGE_CODE_PUNJABI; ValueISO : 'PAN'),
    (ObimpCode: LANGUAGE_CODE_ROMANIAN; ValueISO : 'RUM'),
    (ObimpCode: LANGUAGE_CODE_RUSSIAN; ValueISO : 'RUS'),
    (ObimpCode: LANGUAGE_CODE_SERBIAN; ValueISO : 'SRP'),
    (ObimpCode: LANGUAGE_CODE_SINDHI; ValueISO : 'SND'),
    (ObimpCode: LANGUAGE_CODE_SLOVAK; ValueISO : 'SLO'),
    (ObimpCode: LANGUAGE_CODE_SLOVENIAN; ValueISO : 'SLV'),
    (ObimpCode: LANGUAGE_CODE_SOMALI; ValueISO : 'SOM'),
    (ObimpCode: LANGUAGE_CODE_SPANISH; ValueISO : 'SPA'),
    (ObimpCode: LANGUAGE_CODE_SWAHILI; ValueISO : 'SWA'),
    (ObimpCode: LANGUAGE_CODE_SWEDISH; ValueISO : 'SWE'),
    (ObimpCode: LANGUAGE_CODE_TAGALOG; ValueISO : 'TGL'),
    (ObimpCode: LANGUAGE_CODE_TAIWANESE; ValueISO : 'ZH-TW'),
    (ObimpCode: LANGUAGE_CODE_TAMIL; ValueISO : 'TAM'),
    (ObimpCode: LANGUAGE_CODE_TATAR; ValueISO : 'TAT'),
    (ObimpCode: LANGUAGE_CODE_THAI; ValueISO : 'THA'),
    (ObimpCode: LANGUAGE_CODE_TURKISH; ValueISO : 'TUR'),
    (ObimpCode: LANGUAGE_CODE_UKRAINIAN; ValueISO : 'UKR'),
    (ObimpCode: LANGUAGE_CODE_URDU; ValueISO : 'URD'),
    (ObimpCode: LANGUAGE_CODE_VIETNAMESE; ValueISO : 'VIE'),
    (ObimpCode: LANGUAGE_CODE_WELSH; ValueISO : 'WEL'),
    (ObimpCode: LANGUAGE_CODE_YIDDISH; ValueISO : 'YID'),
    (ObimpCode: LANGUAGE_CODE_YORUBA; ValueISO : 'YOR'),
    (ObimpCode: LANGUAGE_CODE_KAZAKH; ValueISO : 'KAZ'),
    (ObimpCode: LANGUAGE_CODE_KYRGYZ; ValueISO : 'KIR'),
    (ObimpCode: LANGUAGE_CODE_TAJIK; ValueISO : 'TGK'),
    (ObimpCode: LANGUAGE_CODE_TURKMEN; ValueISO : 'TUK'),
    (ObimpCode: LANGUAGE_CODE_UZBEK; ValueISO : 'UZB'),
    (ObimpCode: LANGUAGE_CODE_GEORGIAN; ValueISO : 'GEO')
    );

    function Country_ImdToObimp(ImdCountry: string): Word;
    function Country_ObimpToImd(ObimpCountry: Word): string;
    function Language_ImdToObimp(ImdLang: string): Word;
    function Language_ObimpToImd(ObimpLang: Word): string;


implementation

{***************************************************************}
function Country_ImdToObimp(ImdCountry: string): Word;
var i: integer;
begin
  Result := 0;
  if (ImdCountry = '') then Exit;

  for i := Low(OscarCountries) to High(OscarCountries) do
  begin
    if WideSameText(OscarCountries[i].ValueISO, ImdCountry) then
    begin
      Result := OscarCountries[i].ObimpCode;
      Break;
    end;
  end;//for
end;

{***************************************************************}
function Country_ObimpToImd(ObimpCountry: Word): string;
var i: integer;
begin
  Result := '';
  if (ObimpCountry = 0) then Exit;

  for i := Low(OscarCountries) to High(OscarCountries) do
  begin
    if (OscarCountries[i].ObimpCode = ObimpCountry) then
    begin
      Result := OscarCountries[i].ValueISO;
      Break;
    end;
  end;//for
end;

{***************************************************************}
function Language_ImdToObimp(ImdLang: string): Word;
var i: integer;
begin
  Result := 0;
  if (ImdLang = '') then Exit;

  for i := Low(OscarLangs) to High(OscarLangs) do
  begin
    if WideSameText(OscarLangs[i].ValueISO, ImdLang) then
    begin
      Result := OscarLangs[i].ObimpCode;
      Break;
    end;
  end;//for
end;

{***************************************************************}
function Language_ObimpToImd(ObimpLang: Word): string;
var i: integer;
begin
  Result := '';
  if (ObimpLang = 0) then Exit;

  for i := Low(OscarLangs) to High(OscarLangs) do
  begin
    if (OscarLangs[i].ObimpCode = ObimpLang) then
    begin
      Result := OscarLangs[i].ValueISO;
      Break;
    end;
  end;//for
end;

end.
