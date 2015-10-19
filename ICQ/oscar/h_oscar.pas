// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit h_oscar;

interface

uses Windows, SysUtils, Classes, DateUtils, OverbyteIcsMD5;

const
  //==============================
  //=== Common OSCAR constants
  //==============================
  OSCAR_MAX_SNAC_SIZE        = 8192;
  OSCAR_MAX_EVIL             = 999;
  SNAC_FLAG_MORE_REPLIES     = $0001;
  SNAC_FLAG_OPT_TLV_PRESENT  = $8000;
  OSCAR_KEEP_ALIVE_INTERVAL  = 58; //secs
  CONNECT_ATTEMPT_TIMEOUT    = 60; //secs


  //==============================
  //=== SNAC families
  //==============================
  FAM_OSVC    = $0001; VER_FAM_OSVC    = $0004; TOOL_ID = $0110; TOOL_VER = $194F;
  FAM_LOCATE  = $0002; VER_FAM_LOCATE  = $0001;
  FAM_BUDDY   = $0003; VER_FAM_BUDDY   = $0001;
  FAM_ICBM    = $0004; VER_FAM_ICBM    = $0001;
  FAM_PD      = $0009; VER_FAM_PD      = $0001;
  FAM_LOOKUP  = $000A; VER_FAM_LOOKUP  = $0001;
  FAM_STATS   = $000B; VER_FAM_STATS   = $0001;
  FAM_BART    = $0010; VER_FAM_BART    = $0001;
  FAM_FEEDBAG = $0013; VER_FAM_FEEDBAG = $0004;
  FAM_ICQ     = $0015; VER_FAM_ICQ     = $0001;
  FAM_BUCP    = $0017;
  FAM_PLUGINS = $0022; VER_FAM_PLUGINS = $0001;
  FAM_UNK_x24 = $0024; VER_FAM_x24     = $0001;
  FAM_MDIR    = $0025; VER_FAM_MDIR    = $0001;

  //==============================
  //=== SNAC commands
  //==============================
  //OSVC commands:
  CMD_OSVC_ERROR                   = $0001;
  CMD_OSVC_CLIENT_ONLINE           = $0002;
  CMD_OSVC_HOST_ONLINE             = $0003;
  CMD_OSVC_SVC_REQUEST             = $0004;
  CMD_OSVC_SVC_RESPONSE            = $0005;
  CMD_OSVC_RATES_QUERY             = $0006;
  CMD_OSVC_RATES_REPLY             = $0007;
  CMD_OSVC_RATES_ADD_SUB           = $0008;
  CMD_OSVC_PAUSE                   = $000B;
  CMD_OSVC_PAUSE_ACK               = $000C;
  CMD_OSVC_RESUME                  = $000D;
  CMD_OSVC_NICK_INFO_QUERY         = $000E;
  CMD_OSVC_NICK_INFO_REPLY         = $000F;
  CMD_OSVC_MIGRATE_GROUPS          = $0012;
  CMD_OSVC_CLIENT_VERSIONS         = $0017;
  CMD_OSVC_HOST_VERSIONS           = $0018;
  CMD_OSVC_SET_NICKINFO            = $001E;
  CMD_OSVC_BART_REPLY              = $0021;

  //==============================
  //LOCATE commands:
  CMD_LOCATE_RIGHTS_QUERY          = $0002;
  CMD_LOCATE_RIGHTS_REPLY          = $0003;
  CMD_LOCATE_SET_INFO              = $0004;

  //==============================
  //BUDDY commands:
  CMD_BUDDY_RIGHTS_QUERY           = $0002;
  CMD_BUDDY_RIGHTS_REPLY           = $0003;
  CMD_BUDDY_REJECTED               = $000A;
  CMD_BUDDY_ARRIVED                = $000B;
  CMD_BUDDY_DEPARTED               = $000C;

  //==============================
  //ICBM commands:
  CMD_ICBM_ADD_PARAMS              = $0002;
  CMD_ICBM_DEL_PARAMS              = $0003;
  CMD_ICBM_PARAM_QUERY             = $0004;
  CMD_ICBM_PARAM_REPLY             = $0005;
  CMD_ICBM_MSG_TO_HOST             = $0006;
  CMD_ICBM_MSG_TO_CLIENT           = $0007;
  CMD_ICBM_HOST_ACK                = $000C;
  CMD_ICBM_OFFLINE_RETRIEVE        = $0010;
  CMD_ICBM_OFFLINE_DONE            = $0017;
  CMD_ICBM_CLIENT_EVENT            = $0014;

  //==============================
  //PD commands:
  CMD_PD_RIGHTS_QUERY              = $0002;
  CMD_PD_RIGHTS_REPLY              = $0003;

  //==============================
  //BART commands:
  CMD_BART_UPLOAD                  = $0002;
  CMD_BART_UPLOAD_REPLY            = $0003;
  CMD_BART_DOWNLOAD                = $0006;
  CMD_BART_DOWNLOAD_REPLY          = $0007;

  //==============================
  //FEEDBAG commands:
  CMD_FEEDBAG_ERROR                = $0001;
  CMD_FEEDBAG_RIGHTS_QUERY         = $0002;
  CMD_FEEDBAG_RIGHTS_REPLY         = $0003;
  CMD_FEEDBAG_CL_QUERY             = $0004;
  CMD_FEEDBAG_CL_REPLY             = $0006;
  CMD_FEEDBAG_CL_USE               = $0007;
  CMD_FEEDBAG_INSERT_ITEMS         = $0008;
  CMD_FEEDBAG_UPDATE_ITEMS         = $0009;
  CMD_FEEDBAG_DELETE_ITEMS         = $000A;
  CMD_FEEDBAG_STATUS               = $000E;
  CMD_FEEDBAG_START_CLUSTER        = $0011;
  CMD_FEEDBAG_END_CLUSTER          = $0012;
  CMD_FEEDBAG_REMOVE_ME            = $0016;
  CMD_FEEDBAG_REQ_AUTH_TO_HOST     = $0018;
  CMD_FEEDBAG_REQ_AUTH_TO_CLIENT   = $0019;
  CMD_FEEDBAG_RESP_AUTH_TO_HOST    = $001A;
  CMD_FEEDBAG_RESP_AUTH_TO_CLIENT  = $001B;
  CMD_FEEDBAG_YOU_WERE_ADDED       = $001C;

  //==============================
  //BUCP commands:
  CMD_BUCP_ERROR                   = $0001;
  CMD_BUCP_LOGIN                   = $0002;
  CMD_BUCP_LOGIN_REPLY             = $0003;
  CMD_BUCP_REQUEST_CHALLENGE       = $0006;
  CMD_BUCP_REPLY_CHALLENGE         = $0007;

  //==============================
  //MDIR commands:
  CMD_MDIR_ERROR                   = $0001;
  CMD_MDIR_INFO_QUERY              = $0002;
  CMD_MDIR_INFO_REPLY              = $0003;
  CMD_MDIR_INFO_UPD_QUERY          = $0004;
  CMD_MDIR_INFO_UPD_REPLY          = $0005;


  //==============================
  //=== Tags
  //==============================
  //OSVC TLV tags:
  OSVC_TAG_NICK_INFO_NICK_FLAGS        = $0001;
  OSVC_TAG_NICK_INFO_CREATE_TOD        = $0002;
  OSVC_TAG_NICK_INFO_SIGNON_TOD        = $0003;
  OSVC_TAG_NICK_INFO_IDLE_MINS         = $0004;
  OSVC_TAG_NICK_INFO_MEMBER_SINCE      = $0005;
  OSVC_TAG_NICK_INFO_STATUS            = $0006;
  //$0007,
  OSVC_TAG_NICK_INFO_CLIENTTYPE        = $0008;
  //$0009,
  OSVC_TAG_NICK_INFO_REALIPADDRESS     = $000A;
  //$000B,
  OSVC_TAG_NICK_INFO_DCINFO            = $000C;
  OSVC_TAG_NICK_INFO_OSCAR_CAPS        = $000D;
  OSVC_TAG_NICK_INFO_AOL_CAPS          = $000E;
  OSVC_TAG_NICK_INFO_OSCAR_ONL_SECS    = $000F;
  OSVC_TAG_NICK_INFO_AOL_ONL_SECS      = $0010;
  OSVC_TAG_NICK_INFO_ICQ_BROAD_BLOB    = $0011;
  OSVC_TAG_NICK_INFO_ICQ_BROAD_TYPE    = $0012;
  OSVC_TAG_NICK_INFO_INSTANCES         = $0013;
  OSVC_TAG_NICK_INFO_MY_INSTANCE_NUM   = $0014;
  OSVC_TAG_NICK_INFO_MY_PARENT_CTRLS   = $0015;
  OSVC_TAG_NICK_INFO_MY_ZIP            = $0016;
  OSVC_TAG_NICK_INFO_MY_BOT_INFO       = $0017;
  OSVC_TAG_NICK_INFO_ALIAS             = $0018;
  OSVC_TAG_NICK_INFO_SHORT_OSCAR_CAPS  = $0019;
  OSVC_TAG_NICK_INFO_SHORT_AOL_CAPS    = $001A;
  OSVC_TAG_NICK_INFO_ENCRYPT_CERT_MD5  = $001B;
  //$001C,
  OSVC_TAG_NICK_INFO_BART_INFO         = $001D;
  OSVC_TAG_NICK_INFO_MY_SUBSCRIPTIONS  = $001E;
  OSVC_TAG_NICK_INFO_NICK_FLAGS2       = $001F;
  //$0020,
  //$0021,
  //$0022, PEER_PORT
  OSVC_TAG_NICK_INFO_BUDDYFEED_TIME    = $0023;
  //$0024,
  //$0025,
  OSVC_TAG_NICK_INFO_SIG_TIME          = $0026;
  OSVC_TAG_NICK_INFO_AWAY_TIME         = $0027;
  OSVC_TAG_NICK_INFO_THIS_INSTANCE_NUM = $0028;
  //$0029, INSTANCE_SIGNON_TIME?
  OSVC_TAG_NICK_INFO_GEO_COUNTRY       = $002A;
  //$002B, LIFESTREAM IMAGE URL?
  //$002C, EVENTS_NUM?
  //$002D, EVENTS_NUM?
  //$002E,
  //$002F,
  //$0030, SOME_TIME?
  //$0031,
  //$0032,
  //$0033,
  //$0034,
  //$0035, SOCIALS?
  //$0036, PROMOS?
  //$0037, SOCIAL_EVENTS_NUM?
  //...
  //$0041, ? (max seen)



  //==============================
  //LOCATE TLV tags:
  LOCATE_TAG_RIGHTS_MAX_FULL_CAPS  = $0002;
  LOCATE_TAG_RIGHTS_MAX_SHORT_CAPS = $0005;
  LOCATE_TAG_INFO_CAPS             = $0005;

  //==============================
  //BUDDY TLV tags:
  BUDDY_TAG_RIGHTS_FLAGS           = $0005;
  BUDDY_TAG_RIGHTS_MAX_BUDDIES     = $0001;
  BUDDY_TAG_RIGHTS_MAX_WATCHERS    = $0002;
  BUDDY_TAG_RIGHTS_MAX_ICQ_BROAD   = $0003;
  BUDDY_TAG_RIGHTS_MAX_TMP_BUDDIES = $0004;

  //==============================
  //ICBM tags:
  ICBM_TAG_IM_DATA                 = $0002;
    IM_DATA_TAG_IM_TEXT            = $0101;
    IM_DATA_TAG_CAPS               = $0501;
    IM_DATA_TAG_MIME_ARRAY         = $0D01;
  ICBM_TAG_REQUEST_HOST_ACK        = $0003;
  ICBM_TAG_STORE_OFFLINE           = $0006;
  ICBM_TAG_OFFMSG_SENT_TIME        = $0016;

  //==============================
  //PD TLV tags:
  PD_TAG_RIGHTS_MAX_PERMIT_ENTRIES = $0001;
  PD_TAG_RIGHTS_MAX_DENY_ENTRIES   = $0002;
  PD_TAG_RIGHTS_MAX_TEMP_PERMIT    = $0003;

  //==============================
  //FEEDBAG TLV tags:
  FB_TAG_RIGHTS_FLAGS                  = $000B;
  FB_TAG_RIGHTS_MAX_CLASS_ATTRS        = $0002;
  FB_TAG_RIGHTS_MAX_ITEM_ATTRS_SIZE    = $0003;
  FB_TAG_RIGHTS_MAX_ITEMS_BY_CLASS     = $0004;
  FB_TAG_RIGHTS_MAX_CLIENT_ITEMS       = $0005;
  FB_TAG_RIGHTS_MAX_ITEM_NAME_LEN      = $0006;
  FB_TAG_RIGHTS_MAX_RECENT_BUDDIES     = $0007;
  FB_TAG_RIGHTS_INTERACTION_BUDDIES    = $0008;
  FB_TAG_RIGHTS_INTERACTION_HALF_LIFE  = $0009;
  FB_TAG_RIGHTS_INTERACTION_MAX_SCORE  = $000A;
  FB_TAG_RIGHTS_MAX_BUDDIES_PER_GROUP  = $000C;
  FB_TAG_RIGHTS_MAX_MEGA_BOTS          = $000D;
  FB_TAG_RIGHTS_MAX_SMART_GROUPS       = $000E;

  //==============================
  //BUCP TLV tags:
  BUCP_TAG_ACCOUNT           = $0001;
  BUCP_TAG_CLIENT_NAME       = $0003;
  BUCP_TAG_SERVER            = $0005;
  BUCP_TAG_COOKIE            = $0006;
  BUCP_TAG_LOGIN_ERROR       = $0008;
  BUCP_TAG_DISCONNECT        = $0009;
  BUCP_TAG_FAMILY            = $000D;
  BUCP_TAG_COUNTRY           = $000E;
  BUCP_TAG_LANGUAGE          = $000F;
  BUCP_TAG_CLIENT_ID         = $0016;
  BUCP_TAG_VER_MAJOR         = $0017;
  BUCP_TAG_VER_MINOR         = $0018;
  BUCP_TAG_VER_POINT         = $0019;
  BUCP_TAG_VER_BUILD         = $001A;
  BUCP_TAG_RESPONSE          = $0025;
  BUCP_TAG_MULTICON          = $004A;
  BUCP_TAG_MACHINE_INFO      = $004C;
  BUCP_TAG_ICQ_EMAIL         = $0056;
  BUCP_TAG_RECONNECT         = $0094;

  //==============================
  //=== Values
  //==============================
  //Client info values:
  CLIENT_NAME          : AnsiString  = 'ICQ Client';
  CLIENT_ID            : Word        = $010A;
  CLIENT_RECONNECT     : Byte        = $00;
  CLIENT_MC_DONT_USE   : Byte        = $00;
  CLIENT_MC_MULTIPLE   : Byte        = $01;
  CLIENT_MC_SINGLE     : Byte        = $03;
  CLIENT_VER_MAJOR     : Word        = $0007;
  CLIENT_VER_MINOR     : Word        = $0000;
  CLIENT_VER_POINT     : Word        = $0000;
  CLIENT_VER_BUILD     : Word        = $1B58;  //important thing for status flags, should be above 5k
  CLIENT_LANGUAGE      : AnsiString  = 'en';
  CLIENT_COUNTRY       : AnsiString  = 'us';

  //==============================
  //BUCP auth error codes:
  BUCP_ERR_INVALID_NICK_OR_PASSW      = $0001;
  BUCP_ERR_SERVICE_TEMP_UNAVAIL       = $0002;
  BUCP_ERR_ALL_OTHER_ERRORS           = $0003;
  BUCP_ERR_INCORR_NICK_OR_PASSWORD    = $0004;
  BUCP_ERR_MISMATCH_NICK_OR_PASSWD    = $0005;
  BUCP_ERR_BAD_INPUT                  = $0006;
  BUCP_ERR_INVALID_ACCT               = $0007;
  BUCP_ERR_DELETED_ACCT               = $0008;
  BUCP_ERR_EXPIRED_ACCT               = $0009;
  BUCP_ERR_NO_DB_ACCESS               = $000A;
  BUCP_ERR_NO_RESV_ACCESS             = $000B;
  BUCP_ERR_INVALID_DB_FIELDS          = $000C;
  BUCP_ERR_BAD_DB_STATUS              = $000D;
  BUCP_ERR_BAD_RESV_STATUS            = $000E;
  BUCP_ERR_INTERNAL_ERROR             = $000F;
  BUCP_ERR_SERVICE_TEMP_OFFLINE       = $0010;
  BUCP_ERR_SUSPENDED_ACCT             = $0011;
  BUCP_ERR_DB_SEND_ERR                = $0012;
  BUCP_ERR_DB_LINK_ERR                = $0013;
  BUCP_ERR_RESERVATION_MAP_ERR        = $0014;
  BUCP_ERR_RESERVATION_LINK_ERR       = $0015;
  BUCP_ERR_SAME_IP_CONNECTION         = $0016;
  BUCP_ERR_SAME_IP_RESERVATION        = $0017;
  BUCP_ERR_RESERVATION_RATE           = $0018;
  BUCP_ERR_USER_TOO_EVIL              = $0019;
  BUCP_ERR_RESERVATION_TIMEOUT        = $001A;
  BUCP_ERR_MANDATORY_UPGRADE          = $001B;
  BUCP_ERR_ADVISORY_UPGRADE           = $001C;
  BUCP_ERR_RATE_LIMITED               = $001D;
  BUCP_ERR_ALREADY_ONLINE             = $001E;
  BUCP_ERR_SECURID_TIMEOUT            = $001F;
  BUCP_ERR_SECURID_FAILED             = $0020;
  BUCP_ERR_MC_ERROR                   = $0021;
  BUCP_ERR_CREDIT_CARD_VALIDATION     = $0022;
  BUCP_ERR_REQUIRE_REVALIDATION       = $0023;
  BUCP_ERR_LINK_RULE_REJECT           = $0024;
  BUCP_ERR_MISS_INFO_OR_INVALID_SNAC  = $0025;
  BUCP_ERR_LINK_BROKEN                = $0026;
  BUCP_ERR_INVALID_CLIENT_IP          = $0027;
  BUCP_ERR_PARTNER_REJEC              = $0028;
  BUCP_ERR_SECUREID_MISSING           = $0029;
  BUCP_ERR_BUMP_USER                  = $002A;
  BUCP_ERR_LINK_BROKEN_DB             = $002B;
  BUCP_ERR_LINK_BROKEN_PARCON         = $002C;
  BUCP_ERR_INVALID_KEY                = $002D;
  BUCP_ERR_OVERLIMIT_KEY              = $002E;
  BUCP_ERR_EXPIRED_KEY                = $002F;
  BUCP_ERR_SUSPENDED_KEY              = $0030;
  BUCP_ERR_INVALID_FINGER_PRINT       = $0031;
  BUCP_ERR_OPEN_AUTH_ERROR            = $0032;
  //our fake code
  BUCP_ERR_ANOTHER_PLACE_LOGIN        = $F0000001;

  //OSVC nick info tag statuses
  NI_STATUS_ONLINE         = $00000000;
  NI_STATUS_AWAY           = $00000001;
  NI_STATUS_DND            = $00000002;
  NI_STATUS_NA             = $00000004;
  NI_STATUS_BUSY           = $00000010;
  NI_STATUS_INVISIBLE      = $00000100;
  NI_STATUS_FL_WEBAWARE    = $00010000;

  //OSVC nick info tag flags
  NI_FLAG_UNCONFIRMED      = $0001;
  NI_FLAG_ADMINISTRATOR    = $0002;
  NI_FLAG_AOL              = $0004;
  NI_FLAG_OSCAR_PAY        = $0008;
  NI_FLAG_OSCAR_FREE       = $0010;
  NI_FLAG_UNAVAILABLE      = $0020;
  NI_FLAG_ICQ              = $0040;
  NI_FLAG_WIRELESS         = $0080;
  NI_FLAG_INTERNAL         = $0100;
  NI_FLAG_IMF              = $0200;
  NI_FLAG_BOT              = $0400;
  NI_FLAG_BEAST            = $0800;
  NI_FLAG_ONE_WAY_WIRELESS = $1000;
  NI_FLAG_OFFICIAL         = $2000;

  //OSVC nick info tag client type
  NI_CLIENT_TYPE_ICQ6      = $0A06;
  NI_CLIENT_TYPE_ICQ7      = $0A07;
  NI_CLIENT_TYPE_COMPAD    = $2201;

  //BUDDY rights flags
  BUDDY_RIGHTS_FLAG_BART_SUPPORTED         = $0001;
  BUDDY_RIGHTS_FLAG_INITIAL_DEPARTS        = $0002;
  BUDDY_RIGHTS_FLAG_OFFLINE_BART_SUPPORTED = $0004;
  BUDDY_RIGHTS_FLAG_REJECT_PENDING_BUDDIES = $0008;

  //ICBM parameter flags
  ICBM_PARAM_FLAG_CH_MSGS_ALLOWED        = $00000001;
  ICBM_PARAM_FLAG_MISSED_CALLS_ENABLED   = $00000002;
  ICBM_PARAM_FLAG_BLOCK_AUTH_MSGS        = $00000004;
  ICBM_PARAM_FLAG_EVENTS_ALLOWED         = $00000008;
  ICBM_PARAM_FLAG_SMS_SUPPORTED          = $00000010;
  ICBM_PARAM_FLAG_SMS_LEGAL_TEXT_SUP     = $00000020;
  ICBM_PARAM_FLAG_SMS_SEGMENTS_SUP       = $00000040;
  ICBM_PARAM_FLAG_CHAT_SUPPORTED         = $00000080;
  ICBM_PARAM_FLAG_OFFLINE_MSGS_ALLOWED   = $00000100;
  ICBM_PARAM_FLAG_SIP_CH_SUPPORTED       = $00000200;
  ICBM_PARAM_FLAG_UNK_400                = $00000400;
  ICBM_PARAM_FLAG_UNK_800                = $00000800;
  ICBM_PARAM_FLAG_UNK_1000               = $00001000;
  ICBM_PARAM_FLAG_UNK_2000               = $00002000;

  ICBM_PARAM_MAX_INC_MSG_LEN             = 8000;
  ICBM_PARAM_MIN_INTERVAL                = 0;//msec
  ICBM_MAX_1CH_ENC_MSG_LEN               = 2546;

  ICBM_IM_ENCODING_ASCII     = $0000;  //ANSI ASCII -- ISO 646
  ICBM_IM_ENCODING_UNICODE   = $0002;  //ISO 10646.USC-2 Unicode
  ICBM_IM_ENCODING_LATIN_1   = $0003;  //ISO 8859-1

  ICBM_EVENT_NONE    = $0000;
  ICBM_EVENT_TYPED   = $0001;
  ICBM_EVENT_TYPING  = $0002;
  ICBM_EVENT_CLOSED  = $000F;

  //FEEDBAG rights flags
  FB_RIGHTS_FLAG_INTERACTION_SUPPORTED   = $0001;
  FB_RIGHTS_FLAG_AUTHORIZATION_SUPPORTED = $0002;
  FB_RIGHTS_FLAG_DOMAIN_SN_SUPPORTED     = $0004;
  FB_RIGHTS_FLAG_ICQ_NUM_SUPPORTED       = $0008;
  FB_RIGHTS_FLAG_SMS_NUM_SUPPORTED       = $0010;
  FB_RIGHTS_FLAG_ALIAS_ATTR_SUPPORTED    = $0020;
  FB_RIGHTS_FLAG_SMARTGRP_SUPPORTED      = $0040;

  //FEEDBAG item classes
  FB_CLSID_BUDDY                       = $0000;
  FB_CLSID_GROUP                       = $0001;
  FB_CLSID_PERMIT                      = $0002;
  FB_CLSID_DENY                        = $0003;
  FB_CLSID_PD_INFO                     = $0004;
  FB_CLSID_IGNORE                      = $000E;
  FB_CLSID_BART                        = $0014;

  //FEEDBAG item attributes
  FB_ATT_PENDING_AUTH                  = $0066;
  FB_ATT_RECENT                        = $006A;
  FB_ATT_ORDER                         = $00C8;
  FB_ATT_PD_MODE                       = $00CA;
  FB_ATT_PD_MASK                       = $00CB;
  FB_ATT_PD_FLAGS                      = $00CC;
  FB_ATT_BART_INFO                     = $00D5;
  FB_ATT_ALIAS                         = $0131;
  FB_ATT_PHONE_BUDDY                   = $014A;
  FB_ATT_NIL_BUDDY                     = $014E;
  FB_ATT_MDIR_KEY                      = $015C;
  FB_ATT_MDIR_DATE                     = $015D;

  //FEEDBAG permit/deny modes
  FB_PD_MODE_PERMIT_ALL  = $01;
  FB_PD_MODE_DENY_ALL    = $02;
  FB_PD_MODE_PERMIT_VIS  = $03;
  FB_PD_MODE_DENY_INVIS  = $04;
  FB_PD_MODE_PERMIT_CL   = $05;

  //FEEDBAG operation status codes
  FB_STATUS_CODE_SUCCESS            = $0000;
  FB_STATUS_CODE_DB_ERROR           = $0001;
  FB_STATUS_CODE_NOT_FOUND          = $0002;
  FB_STATUS_CODE_ALREADY_EXISTS     = $0003;
  FB_STATUS_CODE_UNAVAILABLE        = $0005;
  FB_STATUS_CODE_BAD_REQUEST        = $000A;
  FB_STATUS_CODE_DB_TIME_OUT        = $000B;
  FB_STATUS_CODE_OVER_ROW_LIMIT     = $000C;
  FB_STATUS_CODE_NOT_EXECUTED       = $000D;
  FB_STATUS_CODE_AUTH_REQUIRED      = $000E;
  FB_STATUS_CODE_BAD_ACC_NAME       = $0010;
  FB_STATUS_CODE_OVER_BUDDY_LIMIT   = $0011;
  FB_STATUS_CODE_INSERT_SMART_GROUP = $0014;
  FB_STATUS_CODE_TIMEOUT            = $001A;

  //FEEDBAG auth respond codes
  FB_AUTH_RESPOND_DENY  = $00;
  FB_AUTH_RESPOND_GRANT = $01;

  //FEEDBAG constant params
  FB_MAX_ATT_ALIAS_ENC_LEN  = 64;

  //BART types
  BART_BUDDY_ICON        = $0001;
  BART_STATUS_STR        = $0002;
  BART_BUDDY_ICON_BIG    = $000C;
  BART_ICQ_MOOD_NUM      = $000E;
  BART_ICQ_MOOD_STR      = $0010;

  //BART flags
  BART_FLAG_CUSTOM       = $01;
  BART_FLAG_DATA         = $04;
  BART_FLAG_UNKNOWN      = $40; //used only in CMD_OSVC_BART_REPLY
  BART_FLAG_REDIRECT     = $80; //used only in CMD_OSVC_BART_REPLY

  //BART reply codes
  BART_REPLY_CODE_SUCCESS      = 0;
  BART_REPLY_CODE_INVALID      = 1;
  BART_REPLY_CODE_NO_CUSTOM    = 2;
  BART_REPLY_CODE_TOO_SMALL    = 3;
  BART_REPLY_CODE_TOO_BIG      = 4;
  BART_REPLY_CODE_INVALID_TYPE = 5;
  BART_REPLY_CODE_BANNED       = 6;
  BART_REPLY_CODE_NOT_FOUND    = 7;

  //MDIR values
  IMD_MAX_RESULTS_COUNT        = 20;

  IMD_CODE_SUCCESS             = 0;
  IMD_CODE_UNSUPPORTED_FIELDS  = $0066;
  IMD_CODE_UNSUPPORTED_VALUES  = $0067;
  IMD_CODE_FAILED              = $0068;
  IMD_CODE_NOT_EXIST           = $0069;

  IMD_INFO_LEVEL_MIN   = 1;
  IMD_INFO_LEVEL_MID   = 2;
  IMD_INFO_LEVEL_FULL  = 3;

  IMD_SEARCH_KEYWORD   = $00D0;
  IMD_SEARCH_ACCOUNT   = $00D1;
  IMD_SEARCH_EMAIL     = $00D2;
  IMD_SEARCH_PROFILE   = $0065;
  IMD_SEARCH_PERSONAL  = $0066;
  IMD_SEARCH_AGE_RANGE = $00CD;
  IMD_SEARCH_COUNTRY   = $00CE;

  IMD_CNT_STATUS_OFFLINE       = $00000000;
  IMD_CNT_STATUS_ONLINE        = $00000001;
  IMD_CNT_STATUS_DISABLED      = $00000002;

  IMD_PRIV_LEVEL_LOW           = $00000000;
  IMD_PRIV_LEVEL_MID           = $00000001;
  IMD_PRIV_LEVEL_HIGH          = $00000002;

  IMD_GENDER_NONE              = $00000000;
  IMD_GENDER_MALE              = $00000001;
  IMD_GENDER_FEMALE            = $00000002;

  IMD_PHONE_HOME               = $00000001;
  IMD_PHONE_WORK               = $00000002;
  IMD_PHONE_MOBILE             = $00000003;
  IMD_PHONE_HOME_FAX           = $00000004;
  IMD_PHONE_WORK_FAX           = $00000005;
  IMD_PHONE_OTHER              = $00000006;

  IMD_PHONE_TLV_NUMBER         = $0064;
  IMD_PHONE_TLV_TYPE           = $006E;

  IMD_ADDRESS_TLV_STREET       = $0064;
  IMD_ADDRESS_TLV_CITY         = $006E;
  IMD_ADDRESS_TLV_STATE        = $0078;
  IMD_ADDRESS_TLV_ZIP          = $0082;
  IMD_ADDRESS_TLV_COUNTRY      = $008C;

  IMD_WORK_TLV_POSITION        = $0064;
  IMD_WORK_TLV_COMPANY         = $006E;
  IMD_WORK_TLV_WEBSITE         = $0078;
  IMD_WORK_TLV_DEPARTMENT      = $007D;
  IMD_WORK_TLV_INDUSTRY        = $0082;
  IMD_WORK_TLV_SUB_INDUSTRY    = $008C;
  IMD_WORK_TLV_START_DATE      = $0096;
  IMD_WORK_TLV_END_DATE        = $00A0;
  IMD_WORK_TLV_STREET          = $00AA;
  IMD_WORK_TLV_CITY            = $00B4;
  IMD_WORK_TLV_STATE           = $00BE;
  IMD_WORK_TLV_ZIP             = $00C8;
  IMD_WORK_TLV_COUNTRY         = $00D2;

  IMD_INTEREST_TLV_TEXT        = $0064;
  IMD_INTEREST_TLV_CODE        = $006E;

  IMD_TLV_FIRSTNAME            = $0066;
  IMD_TLV_LASTNAME             = $0067;
  IMD_TLV_GENDER               = $0068;
  IMD_TLV_ADDRESS              = $0069;
  IMD_TLV_LOGIN_ALIAS          = $006A;
  IMD_TLV_HOMEPAGE             = $006B;
  IMD_TLV_MARITAL              = $006C;
  IMD_TLV_LANGUAGE1            = $006D;
  IMD_TLV_WORK_INFO            = $006E;
  IMD_TLV_ABOUT                = $006F;
  IMD_TLV_BIRTHDAY             = $0070;
  IMD_TLV_PRIV_KEY             = $07D0;
  IMD_TLV_ORIGIN               = $07D1;
  IMD_TLV_LANGUAGE2            = $07D3;
  IMD_TLV_LANGUAGE3            = $07D4;
  IMD_TLV_VALIDATED_EMAIL      = $07D6;
  IMD_TLV_PENDING_EMAIL        = $07D7;
  IMD_TLV_EMAILS               = $07D8;
  IMD_TLV_PHONES               = $07D9;
  IMD_TLV_INTERESTS            = $07E7;
  IMD_TLV_PAST_ORGS            = $07E8;
  IMD_TLV_ANNIVERSARY          = $07EA;
  IMD_TLV_CHILDREN             = $07EB;
  IMD_TLV_RELIGION             = $07EC;
  IMD_TLV_SEX_ORIENTATION      = $07ED;
  IMD_TLV_SMOKING              = $07EE;
  IMD_TLV_HEIGHT               = $07EF;
  IMD_TLV_HAIR_COLOR           = $07F0;
  IMD_TLV_TIME_ZONE            = $07F1;
  IMD_TLV_STATUS               = $07F3;
  IMD_TLV_PRIVACY_LEVEL        = $07FF;
  IMD_TLV_EXPOSE_STATUS        = $0802; //Bool
  IMD_TLV_AUTH_REQUIRED        = $0809; //Bool
  IMD_TLV_NICKNAME             = $080B;


  //MOOD indexes in status picture selection
  STPIC_SHOPPING    = 1;  // 1 - 24 mood num same as index num
  STPIC_DUCK        = 2;
  STPIC_TIRED       = 3;
  STPIC_PARTY       = 4;
  STPIC_BEER        = 5;
  STPIC_THINKING    = 6;
  STPIC_EATING      = 7;
  STPIC_TV          = 8;
  STPIC_FRIENDS     = 9;
  STPIC_COFFEE      = 10;
  STPIC_MUSIC       = 11;
  STPIC_BUSINESS    = 12;
  STPIC_CAMERA      = 13;
  STPIC_FUNNY       = 14;
  STPIC_PHONE       = 15;
  STPIC_GAMES       = 16;
  STPIC_COLLEGE     = 17;
  STPIC_SICK        = 18;
  STPIC_SLEEPING    = 19;
  STPIC_SURFING     = 20;
  STPIC_BROWSING    = 21;
  STPIC_ENGINEERING = 22;
  STPIC_TYPING      = 23;
  STPIC_ANGRY       = 24;
  STPIC_CHINA1      = 25; //mood num 66
  STPIC_CHINA2      = 26; //mood num 81
  STPIC_CHINA3      = 27; //mood num 71
  STPIC_CHINA4      = 28; //mood num 70
  STPIC_CHINA5      = 29; //mood num 68
  STPIC_DE1         = 30; //mood num 77
  STPIC_DE2         = 31; //mood num 83
  STPIC_DE3         = 32; //mood num 61
  STPIC_RUSEARCH    = 33; //mood num 85
  STPIC_RULOVE      = 34; //mood num 76
  STPIC_RUJOURNAL   = 35; //mood num 84

  STPIC_MAX_COUNT   = 35;


  //OSCAR MOOD numbers
  MOOD_SHOPPING     = 0;
  MOOD_DUCK         = 1;
  MOOD_TIRED        = 2;
  MOOD_PARTY        = 3;
  MOOD_BEER         = 4;
  MOOD_THINKING     = 5;
  MOOD_EATING       = 6;
  MOOD_TV           = 7;
  MOOD_FRIENDS      = 8;
  MOOD_COFFEE       = 9;
  MOOD_MUSIC        = 10;
  MOOD_BUSINESS     = 11;
  MOOD_CAMERA       = 12;
  MOOD_FUNNY        = 13;
  MOOD_PHONE        = 14;
  MOOD_GAMES        = 15;
  MOOD_COLLEGE      = 16;
  MOOD_SICK         = 17;
  MOOD_SLEEPING     = 18;
  MOOD_SURFING      = 19;
  MOOD_BROWSING     = 20;
  MOOD_ENGINEERING  = 21;
  MOOD_TYPING       = 22;
  MOOD_ANGRY        = 23;
  MOOD_CHINA1       = 66;
  MOOD_CHINA2       = 81;
  MOOD_CHINA3       = 71;
  MOOD_CHINA4       = 70;
  MOOD_CHINA5       = 68;
  MOOD_DE1          = 77;
  MOOD_DE2          = 83;
  MOOD_DE3          = 61;
  MOOD_RUSEARCH     = 85;
  MOOD_RULOVE       = 76;
  MOOD_RUJOURNAL    = 84;

  MOOD_FFC          = 62;
  MOOD_ATHOME       = 63;
  MOOD_ATWORK       = 64;
  MOOD_LUNCH        = 80;

  MOOD_STR_MAX_ENC_LEN   = 60;
  STATUS_STR_MAX_ENC_LEN = 250;


  //Other
  CONNECT_STATE_OFFLINE    = 0;
  CONNECT_STATE_CONNECTING = 1;
  CONNECT_STATE_ONLINE     = 2;

  THE_SPC = ' ';
  THE_DOT = '.';
  THE_COM = ',';
  THE_CLN = ':';
  CRLF    = #13#10;

  FAM_INFO_TYPE_FAMILIES = 1;
  FAM_INFO_TYPE_VERSIONS = 2;
  FAM_INFO_TYPE_TOOLVERS = 3;

  REQ_TYPE_OWN_KEEP_INFO   = 1;
  REQ_TYPE_CNT_DETAILS     = 2;
  REQ_TYPE_UPDATE_OWN_INFO = 3;
  REQ_TYPE_SEARCH          = 4;
  REQ_TYPE_UPDATE_OPTS     = 5;

  CUST_ST_PIC_EVIL           = 1;
  CUST_ST_PIC_DEPRES         = 2;
  CUST_ST_PIC_AIM_ONLINE     = 3;
  CUST_ST_PIC_AIM_AWAY       = 4;
  CUST_ST_PIC_MAILRU_ONLINE  = 5;
  CUST_ST_PIC_MAILRU_AWAY    = 6;
  CUST_ST_PIC_MAILRU_OFFLINE = 7;
  CUST_ST_PIC_MAILRU_NIL     = 8;
  CUST_ST_PIC_AWARE_ONLINE   = 9;
  CUST_ST_PIC_AWARE_OFFLINE  = 10;
  CUST_ST_PIC_AWARE_DISABLED = 11;

type
  TOscarFlap = record
    Signature : Byte;
    FrameType : Byte;
    Seq       : Word;
    Len       : Word;
    Data      : RawByteString;
  end;

  TOscarSnac = record
    Fam   : Word;
    Cmd   : Word;
    Flags : Word;
    ReqID : DWord;
    Data  : RawByteString;
  end;

  TLocateParams = record
    MaxFullCapsAllowed   : Word;
    MaxShortCapsAllowed  : Word;
  end;

  TBuddyParams = record
    MaxBuddies           : Word;
    MaxWatchers          : Word;
    MaxIcqBroadcast      : Word;
    MaxTempBuddies       : Word;
  end;

  TIcbmParams = record
    MaxSlots             : Word;
    IcbmFlags            : DWord;
    MaxIncomingIcbmLen   : Word;
    MaxSourceEvil        : Word;
    MaxDestinationEvil   : Word;
    MinInterIcbmInterval : DWord;
  end;

  TPdParams = record
    MaxPermitEntries     : Word;
    MaxDenyEntries       : Word;
    MaxTmpPermitEntries  : Word;
  end;

  TFeedbagParams = record
    MaxClassAttrs        : Word;
    MaxItemAllAttrsSize  : Word;
    MaxItemsbyClass      : RawByteString;
    MaxDevClientItems    : Word;
    MaxItemNameLen       : Word;
    MaxRecentBuddies     : Word;
    InteractBuddiesTop   : Word;
    InteractHalfLifeSecs : DWord;
    InteractMaxScore     : DWord;
    MaxBuddiesPerGroup   : Word;
    MaxBotsBuddies       : Word;
    MaxSmartGroups       : Word;
  end;

  TIcqDcInfo = record
    IntIP     : DWord;
    IntPort   : DWord;
    ConFlag   : Byte;
    ConVer    : Word;
    Cookie    : DWord;
    WebPort   : DWord;
    Features  : DWord;
    Time1     : DWord;
    Time2     : DWord;
    Time3     : DWord;
    NotUsed   : Word;
  end;

  TNickBarts = record
    AvatarHashAdded : Boolean;
    AvatarFlags     : Byte;
    AvatarHash      : RawByteString;  //$001D  $0001
    StatusStrAdded  : Boolean;
    StatusStr       : string;         //$001D  $0002
    MoodNumAdded    : Boolean;
    MoodNum         : Integer;        //$001D  $000E
    MoodStrAdded    : Boolean;
    MoodStr         : string;         //$001D  $0010
  end;

  TNickInfo = record
    Online      : Boolean;
    WarnLevel   : Word;
    NickFlags   : Word;           //$0001
    SignonTime  : DWord;          //$0003
    IdleMins    : DWord;          //$0004
    MemberSince : DWord;          //$0005
    Status      : DWord;          //$0006
    ClientType  : Word;           //$0008
    RealIP      : DWord;          //$000A
    DcInfo      : TIcqDcInfo;     //$000C
    CapsFull    : RawByteString;  //$000D
    OnlineSecs  : DWord;          //$000F
    CapsShort   : RawByteString;  //$0019
    Barts       : TNickBarts;     //$001D
    MyInstNum   : Byte;           //$0014
    ThisInstNum : Byte;           //$0028
  end;

  TIcbmOutMsg = record
    MsgCookie    : UInt64;
    MsgChannel   : Word;
    Account      : string;
    MsgText      : string;
    ReqHostAck   : Boolean;
    Store        : Boolean;
  end;

  TIcbmIncMsg = record
    MsgCookie    : UInt64;
    MsgChannel   : Word;
    Account      : string;
    MsgText      : string;
    NickInfo     : TNickInfo;
    IsOffline    : Boolean;
    OffMsgTime   : DWord;
  end;

  TIcbmAck = record
    MsgCookie  : UInt64;
    MsgChannel : Word;
    Account    : string;
  end;

  TIcbmEvent = record
    MsgCookie  : UInt64;
    MsgChannel : Word;
    Account    : string;
    EventCode  : Word;
  end;

  TOscarAuthMsg = record
    Account    : string;
    ReasonText : string;
    Code       : Byte;
  end;

  TImdSrchReq = record
    Account       : string;
    Email         : string;
    Nickname      : string;
    Firstname     : string;
    Lastname      : string;
    Gender        : DWord;
    HomeCity      : string;
    HomeCountry   : string;
    Language      : string;
    Keywords      : string;
    AgeMin        : DWord;
    AgeMax        : DWord;
    OnlineOnly    : DWord;
  end;
  pImdSrchReq = ^TImdSrchReq;

  TImdInfo = record
    Account       : string;
    Nickname      : string;
    LoginAlias    : string;
    Firstname     : string;
    Lastname      : string;
    ValidEmail    : string;
    PendingEmail  : string;
    Birthday      : DWord;
    Gender        : DWord;
    HomeStreet    : string;
    HomeCity      : string;
    HomeState     : string;
    HomeZip       : string;
    HomeCountry   : string;
    Language1     : string;
    Language2     : string;
    PhoneHome     : string;
    PhoneWork     : string;
    PhoneMobile   : string;
    PhoneHomeFax  : string;
    PhoneWorkFax  : string;
    PhoneOther    : string;
    Homepage      : string;
    WorkPosition  : string;
    WorkCompany   : string;
    WorkWebsite   : string;
    WorkDepart    : string;
    WorkIndustry  : DWord;
    WorkSubIndust : DWord;
    WorkStartDate : DWord;
    WorkEndDate   : DWord;
    WorkStreet    : string;
    WorkCity      : string;
    WorkState     : string;
    WorkZip       : string;
    WorkCountry   : string;
    InterestText1 : string;
    InterestCode1 : DWord;
    InterestText2 : string;
    InterestCode2 : DWord;
    InterestText3 : string;
    InterestCode3 : DWord;
    InterestText4 : string;
    InterestCode4 : DWord;
    About         : string;
    OnlineStatus  : DWord;
    ExposeStatus  : Boolean;
    AuthRequired  : Boolean;
    PrivacyLevel  : DWord;
  end;
  pImdInfo = ^TImdInfo;

  TImdArray = array of TImdInfo;

  TImdReply = record
    ReqType   : Byte;
    ReqID     : DWord;
    ImdCode   : DWord;
    Matches   : DWord;
    Skipped   : DWord;
    Results   : DWord;
    ImdData   : TImdArray;
  end;

  TOscarInfo = record
    LocateParams  : TLocateParams;
    BuddyParams   : TBuddyParams;
    IcbmParams    : TIcbmParams;
    PdParams      : TPdParams;
    FbParams      : TFeedbagParams;
    NickInfo      : TNickInfo;
    IcqDcInfo     : TIcqDcInfo;
    ImdInfo       : TImdInfo;
    InstanceCount : Word;
    OtherInstIPs  : string;
    AvatarFlags   : Byte;
    AvatarHash    : RawByteString;
  end;
  pOscarInfo = ^TOscarInfo;

  function  tb(const Value: Word): RawByteString; overload;
  function  tb(const Value: DWord): RawByteString; overload;
  function  tb(const Value: Integer): RawByteString; overload;
  function  tb(const Value: Int64): RawByteString; overload;
  function  tb(const Vals: array of Byte): RawByteString; overload;

  function  TLV(Tag: Word; Value: Word): RawByteString; overload;
  function  TLV(Tag: Word; Value: RawByteString = ''): RawByteString; overload;
  function  LnbValUTF8(S: string): RawByteString;
  function  LnwValUTF8(S: string): RawByteString;

  function  ReadByte(var Data: RawByteString; const DelCount: DWord = 0): Byte;
  function  ReadBool(var Data: RawByteString; const DelCount: DWord = 0): Boolean;
  function  ReadWord(var Data: RawByteString; const DelCount: DWord = 0): Word;
  function  ReadDWord(var Data: RawByteString; const DelCount: DWord = 0): LongWord;
  function  ReadQWord(var Data: RawByteString; const DelCount: DWord = 0): Int64;
  function  ReadStr(var Data: RawByteString; const Count: DWord; const DelCount: DWord = 0): RawByteString;
  function  ReadStrUTF8(var Data: RawByteString; const Count: DWord; const DelCount: DWord = 0): string;
  function  ReadLnbUTF8(var Data: RawByteString; const DelLnbAndVal: Boolean = False; const ReplaceSpaces: Boolean = False): string;
  function  ReadLnwUTF8(var Data: RawByteString; const DelLnwAndVal: Boolean = False): string;

  function  GetOscarRndWord: Word;
  function  GetOscarRndDWord: DWord;
  function  GetOscarRndQWord: Int64;
  function  GetOscarSrvPort(S: string; var AServer, APort: string): Boolean;

  function  OscAcc(Acc: string): string;
  function  SameOscAcc(Acc1, Acc2: string): Boolean;


  function  HexToRaw(const HexStr: string): RawByteString;
  function  RawToHex(const Data: RawByteString): string;
  function  MD5Raw(Value: RawByteString): RawByteString;
  function  MD5Pwd(Challenge: RawByteString; Password: AnsiString): RawByteString;
  function  StrToRawUniBE(S: string; const MaxEncLen: Integer = 0): RawByteString;
  function  RawToUniEnc(MsgEnc: Word; S: RawByteString): string;
  function  IpToInt(IP: string): DWord;
  function  IntToIP(Value: DWord): string;
  function  GetNickFlagsDesc(NickFlags: Word): string;
  //function  MoodNumIsStatusMood(MoodNum: Integer): Boolean;
  function  MoodNumToStrID(MoodNum: Integer): RawByteString;
  function  MoodStrIDToMoodNum(MoodStrID: string): Integer;
  function  StPicToMoodNum(StPicNum: DWord): Integer;
  function  MoodNumToStPic(MoodNum: Integer): DWord;
  function  GenBartData(BartType: Word; BartFlags: Byte; BartValue: RawByteString): RawByteString;
  function  GetBartData(BartFlags: Byte; BartValue: RawByteString; const GetAvatarHash: Boolean = False): RawByteString;
  function  UTF8EncodeMaxEncLen(const s: string; MaxEncLen: integer): RawByteString;
  procedure SetNickInfoDefs(var NickInfo: TNickInfo);
  function  IsNickInfoChanged(const OldNI, NewNI: TNickInfo; var OnlyAvatarChanged: Boolean): Boolean;
  function  IsPresInfoChanged(const OscarInfo: TOscarInfo; const NewNI: TNickInfo): Boolean;
  function  OleTimeToUnix(OleTime: Int64): Int64;
  function  CutHtmlTags(const S: string): string;
  function  SysNow: TDateTime;
  function  IsUin(const s: string): Boolean;
  function  GetAge(BirthDate, CheckDate: TDateTime): Byte;
  procedure InitImdInfo(ImdInfo: pImdInfo);

  procedure OutDebugStr(s: string);
  //procedure AddLog(s: string);

implementation

{*****************************************************************}
procedure OutDebugStr(s: string);
begin
  OutputDebugString(PChar(s));
end;

{*****************************************************************}
{procedure LogToFile(FileName: string; Data: RawByteString; const AddCRLF: Boolean = True);
const CRLF_A: RawByteString = #13#10;
var aFS: TFileStream;
begin
  aFS := nil;
  try
    if FileExists(FileName) then
      aFS := TFileStream.Create(FileName, fmOpenWrite or fmShareDenyWrite)
    else
      aFS := TFileStream.Create(FileName, fmCreate);
  except
    if Assigned(aFS) then aFS.Free;
    Exit;
  end;

  aFS.Position := aFS.Size;

  try
    if AddCRLF then
      Data := Data + CRLF_A;

    SetLength(Data, Length(Data));
    aFS.Write(Data[1], Length(Data));
  finally
    aFS.Free;
  end;
end;

{*****************************************************************}
{procedure AddLog(s: string);
begin
  LogToFile('c:\_transp.txt', UTF8Encode( FormatDateTime('hh":"nn":"ss', Now) + ' ' + s ));
end;

{*****************************************************************}
function tb(const Value: Word): RawByteString;
begin
  Result := AnsiChar((Value shr 8) and $FF) +
            AnsiChar(Value and $FF);
end;

{*****************************************************************}
function tb(const Value: DWord): RawByteString;
begin
  Result := AnsiChar((Value shr 24) and $FF) +
            AnsiChar((Value shr 16) and $FF) +
            AnsiChar((Value shr 8)  and $FF) +
            AnsiChar(Value and $FF);
end;

{*****************************************************************}
function tb(const Value: Int64): RawByteString;
begin
  Result := AnsiChar((Value shr 56) and $FF) +
            AnsiChar((Value shr 48) and $FF) +
            AnsiChar((Value shr 40) and $FF) +
            AnsiChar((Value shr 32) and $FF) +
            AnsiChar((Value shr 24) and $FF) +
            AnsiChar((Value shr 16) and $FF) +
            AnsiChar((Value shr 8)  and $FF) +
            AnsiChar(Value and $FF);
end;

{*****************************************************************}
function tb(const Value: Integer): RawByteString;
begin
  Result := tb(DWord(Value));
end;

{*****************************************************************}
function tb(const Vals: array of Byte): RawByteString;
begin
  Result := '';

  if (Length(Vals) > 0) then
  begin
    SetLength(Result, Length(Vals));
    Move(Vals[0], Result[1], Length(Result));
  end;
end;

{*******************************************************************}
function TLV(Tag: Word; Value: Word): RawByteString;
begin
  Result := tb(Tag) + tb(Word(2)) + tb(Value);
end;

{*******************************************************************}
function TLV(Tag: Word; Value: RawByteString = ''): RawByteString;
begin
  Result := tb(Tag) + tb(Word(Length(Value))) + Value;
end;

{*******************************************************************}
function LnbValUTF8(S: string): RawByteString;
begin
  Result := tb([Byte(Length(UTF8Encode(S)))]) + UTF8Encode(S);
end;

{*******************************************************************}
function LnwValUTF8(S: string): RawByteString;
begin
  Result := tb(Word(Length(UTF8Encode(S)))) + UTF8Encode(S);
end;

{*******************************************************************}
function Swap2(Value: Word): Word; assembler;
asm
  rol ax,8
end;

{*******************************************************************}
function Swap4(Value: DWord): DWord; assembler;
asm
  bswap eax
end;

{*******************************************************************}
function Swap8(Value: Int64): Int64; assembler;
asm
  mov edx, dword ptr [Value]
  mov eax, dword ptr [Value+4]
  bswap edx
  bswap eax
end;

{*****************************************************************}
function ReadByte(var Data: RawByteString; const DelCount: DWord = 0): Byte;
begin
  if (Data = '') then begin Result := 0; Exit; end;
  Result := PByte(Integer(Data))^; Delete(Data, 1, DelCount);
end;

{*****************************************************************}
function ReadBool(var Data: RawByteString; const DelCount: DWord = 0): Boolean;
begin
  Result := False;
  if (Data = '') then Exit;
  Result := Boolean(PByte(Integer(Data))^); Delete(Data, 1, DelCount);
end;

{*****************************************************************}
function ReadWord(var Data: RawByteString; const DelCount: DWord = 0): Word;
begin
  if (Data = '') then begin Result := 0; Exit; end;
  Result := Swap2(PWord(Integer(Data))^); Delete(Data, 1, DelCount);
end;

{*****************************************************************}
function ReadDWord(var Data: RawByteString; const DelCount: DWord = 0): DWord;
begin
  if (Data = '') then begin Result := 0; Exit; end;
  Result := Swap4(PDWord(Integer(Data))^); Delete(Data, 1, DelCount);
end;

{*****************************************************************}
function ReadQWord(var Data: RawByteString; const DelCount: DWord = 0): Int64;
begin
  if (Data = '') then begin Result := 0; Exit; end;
  Result := Swap8(PInt64(Integer(Data))^); Delete(Data, 1, DelCount);
end;

{*****************************************************************}
function ReadStr(var Data: RawByteString; const Count: DWord; const DelCount: DWord = 0): RawByteString;
begin
  if (Data = '') then begin Result := ''; Exit; end;
  Result := Copy(Data, 1, Count); Delete(Data, 1, DelCount);
end;

{*****************************************************************}
function ReadStrUTF8(var Data: RawByteString; const Count: DWord; const DelCount: DWord = 0): string;
begin
  if (Data = '') then begin Result := ''; Exit; end;
  Result := UTF8ToUnicodeString( ReadStr(Data, Count, DelCount) );
end;

{*****************************************************************}
function ReadLnbUTF8(var Data: RawByteString; const DelLnbAndVal: Boolean = False; const ReplaceSpaces: Boolean = False): string;
var iLnb: Byte;
begin
  if DelLnbAndVal then
    iLnb := ReadByte(Data, 1)
  else
    iLnb := ReadByte(Data);

  if DelLnbAndVal then
    Result := ReadStrUTF8(Data, iLnb, iLnb)
  else
    Result := ReadStrUTF8(Data, iLnb);

  if ReplaceSpaces then
    Result := OscAcc(Result);
end;

{*****************************************************************}
function ReadLnwUTF8(var Data: RawByteString; const DelLnwAndVal: Boolean = False): string;
var iLnw: Word;
begin
  if DelLnwAndVal then
    iLnw := ReadWord(Data, 2)
  else
    iLnw := ReadWord(Data);

  if DelLnwAndVal then
    Result := ReadStrUTF8(Data, iLnw, iLnw)
  else
    Result := ReadStrUTF8(Data, iLnw);
end;

{*****************************************************************}
function GetOscarRndWord: Word;
var w3, w2, w1: Word;
    b: Boolean;
begin
  //some creazy and smart random thing...
  w2 := Random($7FFF);
  w1 := w2;
  b  := w2 = 0;
  w3 := 0;

  while not b do
  begin
    w3 := w3 + w1;
    w1 := w1 shr 3;
    b  := w1 = 0;
  end;//while

  Result := ((w2 - w3) and 7 or (w2 and $FFF8)) + 3;
end;

{*******************************************************************}
function GetOscarRndDWord: DWord;
begin
  //fantasy enabled random
  Result := DateTimeToUnix(Now) + Random(MaxInt) + DWord(-1);
end;

{*******************************************************************}
function GetOscarRndQWord: Int64;
begin
  Result := ( Int64(GetOscarRndDWord) shl 32 ) or GetOscarRndDWord;
end;

{*******************************************************************}
function GetOscarSrvPort(S: string; var AServer, APort: string): Boolean;
var i: integer;
begin
  Result := False;
  S := Trim(S);
  if (S = '') then Exit;

  AServer := S;
  APort   := '5190';

  i := Pos(':', S);
  if (i > 0) then
  begin
    AServer := Copy(S, 1, i-1);
    Delete(S, 1, i);

    if (StrToIntDef(S, 0) > 0) then
      APort := S;
  end;

  Result := True;
end;

{*******************************************************************}
function OscAcc(Acc: string): string;
begin
  //account can be formatted with spaces, remove spaces when needed
  Result := StringReplace(Acc, THE_SPC, '', [rfReplaceAll]);
end;

{*******************************************************************}
function SameOscAcc(Acc1, Acc2: string): Boolean;
begin
  Result := WideSameText(OscAcc(Acc1), OscAcc(Acc2));
end;

{*******************************************************************}
function HexToRaw(const HexStr: string): RawByteString;
begin
  Result := '';
  if (HexStr = '') then Exit;

  if not Odd(Length(HexStr)) then
  begin
    SetLength(Result, Length(HexStr) div 2);
    HexToBin(PWideChar(HexStr), PAnsiChar(Result), Length(Result));
  end;
end;

{*******************************************************************}
function RawToHex(const Data: RawByteString): string;
begin
  Result := '';

  if (Length(Data) > 0) then
  begin
    SetLength(Result, Length(Data) * 2);
    BinToHex(PAnsiChar(Data), PWideChar(Result), Length(Data));
  end;
end;

{*******************************************************************}
function MD5Raw(Value: RawByteString): RawByteString;
var MD5Digest: TMD5Digest;
    MD5Context: TMD5Context;
    i: Integer;
begin
  SetLength(Value, Length(Value));

  Result := '';

  MD5DigestInit(MD5Digest);
  MD5Init(MD5Context);
  MD5UpdateBuffer(MD5Context, @Value[1], Length(Value));
  MD5Final(MD5Digest, MD5Context);

  for i := 0 to 15 do
    Result := Result + AnsiChar(MD5Digest[i]);
end;

{*******************************************************************}
function MD5Pwd(Challenge: RawByteString; Password: AnsiString): RawByteString;
begin
  Result := MD5Raw(Challenge + MD5Raw(Password) +
            tb([$41,$4F,$4C,$20,$49,$6E,$73,$74,$61,$6E,$74,$20,$4D,$65,$73,$73,$65,$6E,$67,$65,$72,$20,$28,$53,$4D,$29]) ); //realm
end;

{*******************************************************************}
function StrToRawUniBE(S: string; const MaxEncLen: Integer = 0): RawByteString;
var bar: TBytes;
    iLen: Integer;
begin
  Result := '';
  if (S = '') then Exit;
  try
    bar := TEncoding.Convert(TEncoding.Unicode, TEncoding.BigEndianUnicode, TEncoding.Unicode.GetBytes(S));

    if (MaxEncLen > 0) and (Length(bar) > MaxEncLen) then
      iLen := MaxEncLen
    else
      iLen := Length(bar);

    SetLength(Result, iLen);
    Move(bar[0], Result[1], iLen);
  except end;
end;

{*******************************************************************}
function RawToUniEnc(MsgEnc: Word; S: RawByteString): string;
var bar: TBytes;
begin
  Result := '';
  if (S = '') then Exit;
  try

  bar := BytesOf(S);

  case MsgEnc of
   {=========================================================}
    ICBM_IM_ENCODING_UNICODE:
      begin
        bar    := TEncoding.Convert(TEncoding.BigEndianUnicode, TEncoding.Unicode, bar);
        Result := TEncoding.Unicode.GetString(bar);
      end;
   {=========================================================}
    ICBM_IM_ENCODING_LATIN_1:
      begin
        bar    := TEncoding.Convert(TEncoding.GetEncoding(1252{or 28591}), TEncoding.Unicode, bar);
        Result := TEncoding.Unicode.GetString(bar);
      end;
   {=========================================================}
    else //ICBM_IM_ENCODING_ASCII and all other
      begin
        //not true ASCII converting, but using default ANSI codepage
        bar    := TEncoding.Convert(TEncoding.Default, TEncoding.Unicode, bar);
        Result := TEncoding.Unicode.GetString(bar);
      end;
   {=========================================================}
  end;//case

  except end;
end;

{***************************************************************}
function IpToInt(IP: string): DWord;
var k,p1,p2,p3,p4: integer;
    s: string;
begin
  Result := 0;
  if (IP = '') then Exit;

  k  := Pos(THE_DOT, IP);
  if (k = 0) then Exit;
  s  := Copy(IP, 1, k-1); Delete(IP, 1, k);
  p1 := StrToIntDef(s, 0);

  k  := Pos(THE_DOT, IP);
  if (k = 0) then Exit;
  s  := Copy(IP, 1, k-1); Delete(IP, 1, k);
  p2 := StrToIntDef(s, 0);

  k  := Pos(THE_DOT, IP);
  if (k = 0) then Exit;
  s  := Copy(IP, 1, k-1); Delete(IP, 1, k);
  p3 := StrToIntDef(s, 0);

  p4 := StrToIntDef(IP, 0);

  Result := (p1 shl 24) + (p2 shl 16) + (p3 shl 8) + p4;
end;

{***************************************************************}
function IntToIP(Value: DWord): string;
var p1,p2,p3,p4: Byte;
begin
  p1 := (Value shr 24) and $FF;
  p2 := (Value shr 16) and $FF;
  p3 := (Value shr 8)  and $FF;
  p4 := Value and $FF;

  Result := IntToStr(p1) + THE_DOT + IntToStr(p2) + THE_DOT + IntToStr(p3) + THE_DOT + IntToStr(p4);
end;

{***************************************************************}
function GetNickFlagsDesc(NickFlags: Word): string;
begin
  Result := '';

  if ( (NickFlags and NI_FLAG_OFFICIAL) = NI_FLAG_OFFICIAL ) then
    Result := Result + 'Official' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_ONE_WAY_WIRELESS) = NI_FLAG_ONE_WAY_WIRELESS ) then
    Result := Result + 'OneWW' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_BEAST) = NI_FLAG_BEAST ) then
    Result := Result + 'Beast' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_BOT) = NI_FLAG_BOT ) then
    Result := Result + 'Bot' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_IMF) = NI_FLAG_IMF ) then
    Result := Result + 'IMF' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_INTERNAL) = NI_FLAG_INTERNAL ) then
    Result := Result + 'Int' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_WIRELESS) = NI_FLAG_WIRELESS ) then
    Result := Result + 'Wireless' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_ICQ) = NI_FLAG_ICQ ) then
    Result := Result + 'ICQ' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_UNAVAILABLE) = NI_FLAG_UNAVAILABLE ) then
    Result := Result + 'Away' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_OSCAR_FREE) = NI_FLAG_OSCAR_FREE ) then
    Result := Result + 'Free' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_OSCAR_PAY) = NI_FLAG_OSCAR_PAY ) then
    Result := Result + 'Pay' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_AOL) = NI_FLAG_AOL ) then
    Result := Result + 'AOL' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_ADMINISTRATOR) = NI_FLAG_ADMINISTRATOR ) then
    Result := Result + 'Admin' + THE_COM + THE_SPC;

  if ( (NickFlags and NI_FLAG_UNCONFIRMED) = NI_FLAG_UNCONFIRMED ) then
    Result := Result + 'UC' + THE_COM + THE_SPC;

  if (Result <> '') then
    Delete(Result, Length(Result)-1, 2);
end;

{***************************************************************}
{function MoodNumIsStatusMood(MoodNum: Integer): Boolean;
begin
  Result := (MoodNum = MOOD_FFC) or (MoodNum = MOOD_ATHOME) or (MoodNum = MOOD_ATWORK) or (MoodNum = MOOD_LUNCH);
end;}

{***************************************************************}
function MoodNumToStrID(MoodNum: Integer): RawByteString;
begin
  Result := UTF8Encode('0icqmood' + IntToStr(MoodNum));
end;

{***************************************************************}
function MoodStrIDToMoodNum(MoodStrID: string): Integer;
var i: integer;
begin
  Result := -1;

  i := Pos('icqmood', LowerCase(MoodStrID));
  if (i < 1) then Exit;
  Delete(MoodStrID, 1, i+6);

  Result := StrToIntDef(Trim(MoodStrID), -1);
end;

{***************************************************************}
function StPicToMoodNum(StPicNum: DWord): Integer;
begin
  //convert to stpic index
  //from 1-24 stpic indexes almost same as mood numbers
  Result := StPicNum - 1;

  //get index independent moods
  case StPicNum of
    STPIC_CHINA1    : Result := MOOD_CHINA1;
    STPIC_CHINA2    : Result := MOOD_CHINA2;
    STPIC_CHINA3    : Result := MOOD_CHINA3;
    STPIC_CHINA4    : Result := MOOD_CHINA4;
    STPIC_CHINA5    : Result := MOOD_CHINA5;
    STPIC_DE1       : Result := MOOD_DE1;
    STPIC_DE2       : Result := MOOD_DE2;
    STPIC_DE3       : Result := MOOD_DE3;
    STPIC_RUSEARCH  : Result := MOOD_RUSEARCH;
    STPIC_RULOVE    : Result := MOOD_RULOVE;
    STPIC_RUJOURNAL : Result := MOOD_RUJOURNAL;
  end;//case
end;

{***************************************************************}
function MoodNumToStPic(MoodNum: Integer): DWord;
begin
  Result := 0;
  if (MoodNum < 0) then Exit;

  case MoodNum of
    //from 0-23 moods almost same as stpic indexes
    MOOD_SHOPPING..MOOD_ANGRY : Result := MoodNum + 1;
    //index independend moods
    MOOD_CHINA1     : Result := STPIC_CHINA1;
    MOOD_CHINA2     : Result := STPIC_CHINA2;
    MOOD_CHINA3     : Result := STPIC_CHINA3;
    MOOD_CHINA4     : Result := STPIC_CHINA4;
    MOOD_CHINA5     : Result := STPIC_CHINA5;
    MOOD_DE1        : Result := STPIC_DE1;
    MOOD_DE2        : Result := STPIC_DE2;
    MOOD_DE3        : Result := STPIC_DE3;
    MOOD_RUSEARCH   : Result := STPIC_RUSEARCH;
    MOOD_RULOVE     : Result := STPIC_RULOVE;
    MOOD_RUJOURNAL  : Result := STPIC_RUJOURNAL;
  end;//case
end;

{***************************************************************}
function GenBartData(BartType: Word; BartFlags: Byte; BartValue: RawByteString): RawByteString;
var sRaw: RawByteString;
begin
  sRaw := '';

  case BartFlags of
   {================================================}
    0 {no flags}      : sRaw := BartValue;
   {================================================}
    BART_FLAG_CUSTOM  : sRaw := BartValue; //should be 16 bytes
   {================================================}
    BART_FLAG_DATA    :
      begin
        if (Length(BartValue) > 0) then
          sRaw := tb(Word(Length(BartValue))) + BartValue + tb($0000)
        else
          BartFlags := 0;
      end;
   {================================================}
  end;//case

  Result := tb(BartType) +
            tb([BartFlags]) +
            tb([Length(sRaw)]) +
            sRaw;
end;

{***************************************************************}
function GetBartData(BartFlags: Byte; BartValue: RawByteString; const GetAvatarHash: Boolean = False): RawByteString;
var iLen: Word;
begin
  Result := '';

  case BartFlags of
   {================================================}
    0 {no flags}      : Result := BartValue;
   {================================================}
    BART_FLAG_CUSTOM  : Result := BartValue; //should be 16 bytes
   {================================================}
    BART_FLAG_DATA    :
      begin
        if (Length(BartValue) > 0) then
        begin
          iLen := ReadWord(BartValue, 2);
          if (iLen > 0) then
            Result := ReadStr(BartValue, iLen, iLen);
        end;
      end;
   {================================================}
  end;//case

  //if only avatar hash required
  if GetAvatarHash and (BartFlags > BART_FLAG_CUSTOM) then
    Result := '';
end;

{***************************************************************}
function UTF8EncodeMaxEncLen(const s: string; MaxEncLen: integer): RawByteString;
begin
  Result := UTF8Encode(s);

  if (MaxEncLen > 0) and (Length(Result) > MaxEncLen) then
    Result := Copy(Result, 1, MaxEncLen);
end;

{***************************************************************}
procedure SetNickInfoDefs(var NickInfo: TNickInfo);
begin
  //set some defaults params
  NickInfo.Barts.MoodNum := -1;
end;

{***************************************************************}
function IsNickInfoChanged(const OldNI, NewNI: TNickInfo; var OnlyAvatarChanged: Boolean): Boolean;
begin
  //check for changes only we use in sending to OBIMP server
  Result := (OldNI.Online <> NewNI.Online) or
            (OldNI.WarnLevel <> NewNI.WarnLevel) or
            (OldNI.NickFlags <> NewNI.NickFlags) or
            (OldNI.SignonTime <> NewNI.SignonTime) or
            (OldNI.MemberSince <> NewNI.MemberSince) or
            (OldNI.Status <> NewNI.Status) or
            (OldNI.RealIP <> NewNI.RealIP) or
            (OldNI.DcInfo.ConVer <> NewNI.DcInfo.ConVer) or
            (OldNI.CapsFull <> NewNI.CapsFull) or
            (OldNI.CapsShort <> NewNI.CapsShort) or
            ( (OldNI.Barts.StatusStr <> NewNI.Barts.StatusStr) and NewNI.Barts.StatusStrAdded ) or
            ( (OldNI.Barts.MoodNum <> NewNI.Barts.MoodNum) and NewNI.Barts.MoodNumAdded ) or
            ( (OldNI.Barts.MoodStr <> NewNI.Barts.MoodStr) and NewNI.Barts.MoodStrAdded );

  if not Result and ( (OldNI.Barts.AvatarHash <> NewNI.Barts.AvatarHash) and NewNI.Barts.AvatarHashAdded ) then
  begin
    OnlyAvatarChanged := True;
    Result := True;
  end;
end;

{***************************************************************}
function IsPresInfoChanged(const OscarInfo: TOscarInfo; const NewNI: TNickInfo): Boolean;
begin
  //check for changes only we use in sending to OBIMP server
  Result := (OscarInfo.NickInfo.WarnLevel <> NewNI.WarnLevel) or
            (OscarInfo.NickInfo.NickFlags <> NewNI.NickFlags) or
            (OscarInfo.NickInfo.SignonTime <> NewNI.SignonTime) or
            (OscarInfo.NickInfo.MemberSince <> NewNI.MemberSince) or
            (OscarInfo.NickInfo.RealIP <> NewNI.RealIP);
end;

{***************************************************************}
function OleTimeToUnix(OleTime: Int64): Int64;

  function SortableInt64ToDouble(const i64: Int64): Double;
  type tBA = array [0..SizeOf(double)-1] of byte;
  var i: integer;
  begin
    if (i64 >= 0) then
      tBA(Result) := tBA(i64)
    else
    begin
      tBA(Result)[high(tBA)] := tBA(i64)[high(tBA)] xor $7F;
      for i := high(tBA)-1 downto 0 do
        tBA(Result)[i] := not tBA(i64)[i];
    end;
  end;

var DT: TDateTime;
begin
  Result := 0;

  DT := SortableInt64ToDouble(OleTime);
  try
    Result := DateTimeToUnix(DT);
  except
  end;
end;

{***************************************************************}
function CutHtmlTags(const S: string): string;
var i: integer;
    bInTag: Boolean;
begin
  Result := '';
  bInTag := False;

  for i := 1 to Length(S) do
  begin
    if (S[i] = '<') then
      bInTag := True
    else
      if (S[i] = '>') then
        bInTag := False
      else
        if not bInTag then
          Result := Result + S[i];
  end;//for
end;

{***************************************************************}
function SysNow: TDateTime;
var SystemTime: TSystemTime;
begin
  GetSystemTime(SystemTime);
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
end;

{***************************************************************}
function IsUin(const s: string): Boolean;
begin
  Result := StrToInt64Def(s, -1) > -1;
end;

{***************************************************************}
function GetAge(BirthDate, CheckDate: TDateTime): Byte;
var iMonth, iDay, iYear,
    iCurYear, iCurMonth, iCurDay: Word;
    iRes: integer;
begin
  Result := 0;

  DecodeDate(BirthDate, iYear, iMonth, iDay);
  DecodeDate(CheckDate, iCurYear, iCurMonth, iCurDay);

  if (iYear > iCurYear) or ( (iYear = iCurYear) and (iMonth > iCurMonth) ) or
     ( (iYear = iCurYear) and (iMonth = iCurMonth) and (iDay > iCurDay) ) then
  begin
    Exit;
  end;

  iRes := iCurYear - iYear;
  if (iMonth > iCurMonth) then
    Dec(iRes)
  else
    if (iMonth = iCurMonth) and (iDay > iCurDay) then
       Dec(iRes);

  if (iRes <= 255) then Result := iRes;
end;

{***************************************************************}
procedure InitImdInfo(ImdInfo: pImdInfo);
begin
  ImdInfo^.Birthday      := $80000000;
  ImdInfo^.WorkStartDate := $80000000;
  ImdInfo^.WorkEndDate   := $80000000;
end;

end.
