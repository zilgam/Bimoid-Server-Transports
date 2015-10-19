// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_obimp_const;

interface

uses Windows;

const
  //=== OBIMP packet ID
  OBIMP_ID_INT = $23;

  //=== OBIMP bexs maximal data length
  OBIMP_BEX_MAX_DEF_DATA_LEN = $00020000;
  OBIMP_BEX_MAX_SET_DATA_LEN = $00020000;

  //=== OBIMP other server params
  OBIMP_DEF_MAX_CLIENTS          = 10000;
  OBIMP_SALT                     = 'OBIMPSALT';
  OBIMP_DEF_SRV_PORT_MAIN        = 7023;
  OBIMP_DEF_SRV_PORT_ADMIN       = 7024;
  OBIMP_DEF_SRV_PORT_MAIN_SSL    = 7033;
  OBIMP_DEF_SRV_PORT_ADMIN_SSL   = 7034;
  OBIMP_AUTH_TIMEOUT             = 60;  //seconds
  OBIMP_KEEP_ALIVE_SECS          = 300; //seconds
  OBIMP_DEF_ADM_KEY_CHANGE_SECS  = 120; //seconds
  OBIMP_MAX_OFF_MSGS_COUNT       = 50;
  OBIMP_MAX_OFF_AUTH_COUNT       = 50;
  OBIMP_SEARCH_RES_PER_REQ       = 30;
  OBIMP_MAX_BAN_LIST_LEN         = 68000;
  OBIMP_MAX_SYS_MSG_TEXT_LEN     = 2048;
  OBIMP_MAX_PASS_SEC_EMAIL_LEN   = 1024;
  OBIMP_MAX_REGS_PER_IP          = 2;
  OBIMP_REG_RATE_CLEAR           = 3600; //seconds
  OBIMP_READ_ONLY_READ_SECS      = 30;   //seconds
  OBIMP_DEF_MAX_HOST_IP_LEN      = 1024;
  OBIMP_DEF_MAX_FILE_NAME_LEN    = 255;
  OBIMP_DEF_MAX_FILE_PATH_LEN    = 32767;
  OBIMP_MAX_FILE_DATA_WTLD_LEN   = $00000800;//2048 bytes
  OBIMP_MAX_CLIENT_OS_LEN        = 128;
  OBIMP_MAX_CLIENT_HOST_LEN      = 255;
  OBIMP_MAX_CLIENT_DESCR_LEN     = 512;
  OBIMP_MAX_AVATAR_WH            = 48;
  OBIMP_MAX_CL_ITEMS             = MAXWORD;
  OBIMP_SYS_ACCNAME              = '#';
  OBIMP_MAX_USERS_LIST_PER_REQ   = 100;
  OBIMP_MAX_HIST_MSGS_PER_REQ    = 50;

  //============================================
  //BEXs developer ID
  OBIMP_BEXS_DEV_BIMOID = $0001;

  //============================================
  //OBIMP general BEX types:
  OBIMP_BEX_COM   = $0001; OBIMP_BEX_COM_VER_MAJOR  =  1; OBIMP_BEX_COM_VER_MINOR  =  2;
  OBIMP_BEX_CL    = $0002; OBIMP_BEX_CL_VER_MAJOR   =  1; OBIMP_BEX_CL_VER_MINOR   =  3;
  OBIMP_BEX_PRES  = $0003; OBIMP_BEX_PRES_VER_MAJOR =  1; OBIMP_BEX_PRES_VER_MINOR =  5;
  OBIMP_BEX_IM    = $0004; OBIMP_BEX_IM_VER_MAJOR   =  1; OBIMP_BEX_IM_VER_MINOR   =  2;
  OBIMP_BEX_UD    = $0005; OBIMP_BEX_UD_VER_MAJOR   =  1; OBIMP_BEX_UD_VER_MINOR   =  1;
  OBIMP_BEX_UA    = $0006; OBIMP_BEX_UA_VER_MAJOR   =  1; OBIMP_BEX_UA_VER_MINOR   =  1;
  OBIMP_BEX_FT    = $0007; OBIMP_BEX_FT_VER_MAJOR   =  1; OBIMP_BEX_FT_VER_MINOR   =  1;
  OBIMP_BEX_TP    = $0008; OBIMP_BEX_TP_VER_MAJOR   =  1; OBIMP_BEX_TP_VER_MINOR   =  2;

  //============================================
  //OBIMP administrative BEX types:
  OBIMP_BEX_WADM    = $F001;

  //============================================
  //OBIMP_BEX_COM sub types:
  OBIMP_BEX_COM_CLI_HELLO               = $0001;
  OBIMP_BEX_COM_SRV_HELLO               = $0002;
  OBIMP_BEX_COM_CLI_LOGIN               = $0003;
  OBIMP_BEX_COM_SRV_LOGIN_REPLY         = $0004;
  OBIMP_BEX_COM_SRV_BYE                 = $0005;
  OBIMP_BEX_COM_CLI_SRV_KEEPALIVE_PING  = $0006;
  OBIMP_BEX_COM_CLI_SRV_KEEPALIVE_PONG  = $0007;
  OBIMP_BEX_COM_CLI_REGISTER            = $0008;
  OBIMP_BEX_COM_SRV_REGISTER_REPLY      = $0009;

  OBIMP_BEX_COM_MAX                     = OBIMP_BEX_COM_SRV_REGISTER_REPLY;

  //============================================
  //OBIMP_BEX_CL sub types:
  OBIMP_BEX_CL_CLI_PARAMS             = $0001;
  OBIMP_BEX_CL_SRV_PARAMS_REPLY       = $0002;
  OBIMP_BEX_CL_CLI_REQUEST            = $0003;
  OBIMP_BEX_CL_SRV_REPLY              = $0004;
  OBIMP_BEX_CL_CLI_VERIFY             = $0005;
  OBIMP_BEX_CL_SRV_VERIFY_REPLY       = $0006;
  OBIMP_BEX_CL_CLI_ADD_ITEM           = $0007;
  OBIMP_BEX_CL_SRV_ADD_ITEM_REPLY     = $0008;
  OBIMP_BEX_CL_CLI_DEL_ITEM           = $0009;
  OBIMP_BEX_CL_SRV_DEL_ITEM_REPLY     = $000A;
  OBIMP_BEX_CL_CLI_UPD_ITEM           = $000B;
  OBIMP_BEX_CL_SRV_UPD_ITEM_REPLY     = $000C;
  OBIMP_BEX_CL_CLI_SRV_AUTH_REQUEST   = $000D;
  OBIMP_BEX_CL_CLI_SRV_AUTH_REPLY     = $000E;
  OBIMP_BEX_CL_CLI_SRV_AUTH_REVOKE    = $000F;
  OBIMP_BEX_CL_CLI_REQ_OFFAUTH        = $0010;
  OBIMP_BEX_CL_SRV_DONE_OFFAUTH       = $0011;
  OBIMP_BEX_CL_CLI_DEL_OFFAUTH        = $0012;
  OBIMP_BEX_CL_SRV_ITEM_OPER          = $0013;
  OBIMP_BEX_CL_SRV_BEGIN_UPDATE       = $0014;
  OBIMP_BEX_CL_SRV_END_UPDATE         = $0015;

  OBIMP_BEX_CL_MAX                    = OBIMP_BEX_CL_SRV_END_UPDATE;

  //============================================
  //OBIMP_BEX_PRES sub types:
  OBIMP_BEX_PRES_CLI_PARAMS              = $0001;
  OBIMP_BEX_PRES_SRV_PARAMS_REPLY        = $0002;
  OBIMP_BEX_PRES_CLI_SET_PRES_INFO       = $0003;
  OBIMP_BEX_PRES_CLI_SET_STATUS          = $0004;
  OBIMP_BEX_PRES_CLI_ACTIVATE            = $0005;
  OBIMP_BEX_PRES_SRV_CONTACT_ONLINE      = $0006;
  OBIMP_BEX_PRES_SRV_CONTACT_OFFLINE     = $0007;
  OBIMP_BEX_PRES_CLI_REQ_PRES_INFO       = $0008;
  OBIMP_BEX_PRES_SRV_PRES_INFO           = $0009;
  OBIMP_BEX_PRES_SRV_MAIL_NOTIF          = $000A;
  OBIMP_BEX_PRES_CLI_REQ_OWN_MAIL_URL    = $000B;
  OBIMP_BEX_PRES_SRV_OWN_MAIL_URL        = $000C;

  OBIMP_BEX_PRES_MAX                     = OBIMP_BEX_PRES_SRV_OWN_MAIL_URL;

  //============================================
  //OBIMP_BEX_IM sub types:
  OBIMP_BEX_IM_CLI_PARAMS                = $0001;
  OBIMP_BEX_IM_SRV_PARAMS_REPLY          = $0002;
  OBIMP_BEX_IM_CLI_REQ_OFFLINE           = $0003;
  OBIMP_BEX_IM_SRV_DONE_OFFLINE          = $0004;
  OBIMP_BEX_IM_CLI_DEL_OFFLINE           = $0005;
  OBIMP_BEX_IM_CLI_MESSAGE               = $0006;
  OBIMP_BEX_IM_SRV_MESSAGE               = $0007;
  OBIMP_BEX_IM_CLI_SRV_MSG_REPORT        = $0008;
  OBIMP_BEX_IM_CLI_SRV_NOTIFY            = $0009;
  OBIMP_BEX_IM_CLI_SRV_ENCRYPT_KEY_REQ   = $000A;
  OBIMP_BEX_IM_CLI_SRV_ENCRYPT_KEY_REPLY = $000B;
  OBIMP_BEX_IM_CLI_MULTIPLE_MSG          = $000C;

  OBIMP_BEX_IM_MAX                       = OBIMP_BEX_IM_CLI_MULTIPLE_MSG;

  //============================================
  //OBIMP_BEX_UD sub types:
  OBIMP_BEX_UD_CLI_PARAMS             = $0001;
  OBIMP_BEX_UD_SRV_PARAMS_REPLY       = $0002;
  OBIMP_BEX_UD_CLI_DETAILS_REQ        = $0003;
  OBIMP_BEX_UD_SRV_DETAILS_REQ_REPLY  = $0004;
  OBIMP_BEX_UD_CLI_DETAILS_UPD        = $0005;
  OBIMP_BEX_UD_SRV_DETAILS_UPD_REPLY  = $0006;
  OBIMP_BEX_UD_CLI_SEARCH             = $0007;
  OBIMP_BEX_UD_SRV_SEARCH_REPLY       = $0008;
  OBIMP_BEX_UD_CLI_SECURE_UPD         = $0009;
  OBIMP_BEX_UD_SRV_SECURE_UPD_REPLY   = $000A;

  OBIMP_BEX_UD_MAX                    = OBIMP_BEX_UD_SRV_SECURE_UPD_REPLY;

  //============================================
  //OBIMP_BEX_UA sub types:
  OBIMP_BEX_UA_CLI_PARAMS             = $0001;
  OBIMP_BEX_UA_SRV_PARAMS_REPLY       = $0002;
  OBIMP_BEX_UA_CLI_AVATAR_REQ         = $0003;
  OBIMP_BEX_UA_SRV_AVATAR_REPLY       = $0004;
  OBIMP_BEX_UA_CLI_AVATAR_SET         = $0005;
  OBIMP_BEX_UA_SRV_AVATAR_SET_REPLY   = $0006;

  OBIMP_BEX_UA_MAX                    = OBIMP_BEX_UA_SRV_AVATAR_SET_REPLY;

  //============================================
  //OBIMP_BEX_FT sub types:
  OBIMP_BEX_FT_CLI_PARAMS                = $0001;
  OBIMP_BEX_FT_SRV_PARAMS_REPLY          = $0002;
  OBIMP_BEX_FT_CLI_SRV_SEND_FILE_REQUEST = $0003;
  OBIMP_BEX_FT_CLI_SRV_SEND_FILE_REPLY   = $0004;
  OBIMP_BEX_FT_CLI_SRV_CONTROL           = $0005;

  OBIMP_BEX_FT_MAX                       = OBIMP_BEX_FT_CLI_SRV_CONTROL;

  //OBIMP_BEX_FT direct/proxy connection sub types:
  OBIMP_BEX_FT_DIR_PROX_ERROR            = $0101;
  OBIMP_BEX_FT_DIR_PROX_HELLO            = $0102;
  OBIMP_BEX_FT_DIR_PROX_FILE             = $0103;
  OBIMP_BEX_FT_DIR_PROX_FILE_REPLY       = $0104;
  OBIMP_BEX_FT_DIR_PROX_FILE_DATA        = $0105;

  //============================================
  //OBIMP_BEX_TP sub types:
  OBIMP_BEX_TP_CLI_PARAMS             = $0001;
  OBIMP_BEX_TP_SRV_PARAMS_REPLY       = $0002;
  OBIMP_BEX_TP_SRV_ITEM_READY         = $0003;
  OBIMP_BEX_TP_CLI_SETTINGS           = $0004;
  OBIMP_BEX_TP_SRV_SETTINGS_REPLY     = $0005;
  OBIMP_BEX_TP_CLI_MANAGE             = $0006;
  OBIMP_BEX_TP_SRV_TRANSPORT_INFO     = $0007;
  OBIMP_BEX_TP_SRV_SHOW_NOTIF         = $0008;
  OBIMP_BEX_TP_SRV_OWN_AVATAR_HASH    = $0009;

  OBIMP_BEX_TP_MAX                    = OBIMP_BEX_TP_SRV_OWN_AVATAR_HASH;

  //============================================
  //OBIMP_BEX_ADM sub types:
  OBIMP_BEX_WADM_CLI_LOGIN            = $0001;
  OBIMP_BEX_WADM_SRV_LOGIN_REPLY      = $0002;
  OBIMP_BEX_WADM_CLI_PARAMS           = $0003;
  OBIMP_BEX_WADM_SRV_PARAMS_REPLY     = $0004;
  OBIMP_BEX_WADM_CLI_SET              = $0005;
  OBIMP_BEX_WADM_SRV_SET_REPLY        = $0006;
  OBIMP_BEX_WADM_CLI_BROADCAST        = $0007;
  OBIMP_BEX_WADM_SRV_BROADCAST_REPLY  = $0008;
  OBIMP_BEX_WADM_CLI_USER             = $0009;
  OBIMP_BEX_WADM_SRV_USER_REPLY       = $000A;
  OBIMP_BEX_WADM_SRV_STATE            = $000B;
  OBIMP_BEX_WADM_CLI_LIST             = $000C;
  OBIMP_BEX_WADM_SRV_LIST_REPLY       = $000D;
  OBIMP_BEX_WADM_CLI_EXT_LIST_REQ     = $000E;
  OBIMP_BEX_WADM_SRV_EXT_LIST_REPLY   = $000F;
  OBIMP_BEX_WADM_CLI_EXT_UPD          = $0010;
  OBIMP_BEX_WADM_SRV_EXT_UPD_REPLY    = $0011;
  OBIMP_BEX_WADM_CLI_HIST             = $0012;
  OBIMP_BEX_WADM_SRV_HIST_REPLY       = $0013;


  //============================================
  //Hello error codes:
  HELLO_ERROR_ACCOUNT_INVALID          = $0001;
  HELLO_ERROR_SERVICE_TEMP_UNAVAILABLE = $0002;
  HELLO_ERROR_ACCOUNT_BANNED           = $0003;
  HELLO_ERROR_WRONG_COOKIE             = $0004;
  HELLO_ERROR_TOO_MANY_CLIENTS         = $0005;
  HELLO_ERROR_INVALID_LOGIN            = $0006;

  //============================================
  //Login error codes:
  LOGIN_ERROR_ACCOUNT_INVALID          = $0001;
  LOGIN_ERROR_SERVICE_TEMP_UNAVAILABLE = $0002;
  LOGIN_ERROR_ACCOUNT_BANNED           = $0003;
  LOGIN_ERROR_WRONG_PASSWORD           = $0004;
  LOGIN_ERROR_INVALID_LOGIN            = $0005;  

  //============================================
  //Bye reason codes:
  BYE_REASON_SRV_SHUTDOWN       = $0001;
  BYE_REASON_CLI_NEW_LOGIN      = $0002;
  BYE_REASON_ACCOUNT_KICKED     = $0003;
  BYE_REASON_INCORRECT_SEQ      = $0004;
  BYE_REASON_INCORRECT_BEX_TYPE = $0005;
  BYE_REASON_INCORRECT_BEX_SUB  = $0006;
  BYE_REASON_INCORRECT_BEX_STEP = $0007;
  BYE_REASON_TIMEOUT            = $0008;
  BYE_REASON_INCORRECT_WTLD     = $0009;
  BYE_REASON_NOT_ALLOWED        = $000A;
  BYE_REASON_FLOODING           = $000B;

  //============================================
  //Registration result codes:
  REG_RES_SUCCESS                   = $0000;
  REG_RES_DISABLED                  = $0001;
  REG_RES_ACCOUNT_EXISTS            = $0002;
  REG_RES_BAD_ACCOUNT_NAME          = $0003;
  REG_RES_BAD_REQUEST               = $0004;
  REG_RES_BAD_SERVER_KEY            = $0005;
  REG_RES_SERVICE_TEMP_UNAVAILABLE  = $0006;
  REG_RES_EMAIL_REQUIRED            = $0007;

  //============================================
  //Administrative status codes:
  ADM_RES_CODE_SUCCESS = $0000;
  ADM_RES_CODE_BAD_KEY = $0001;

  //============================================
  //Setting change result codes:
  SET_CHANGE_SUCCESS      = $0000;
  SET_CHANGE_NOT_ALLOWED  = $0001;

  //============================================
  //Broadcast flags:
  BROADCAST_MSG_TO_ALL            = $0001;
  BROADCAST_MSG_TO_CONNECTED_ONLY = $0002;
  BROADCAST_DROP_CONNECTIONS      = $0003;

  //============================================
  //Broadcast result codes:
  BC_RES_SUCCESS       = $0000;
  BC_RES_BAD_REQUEST   = $0001;
  BC_RES_MSG_LEN_LIMIT = $0002;

  //============================================
  //Available user commands:
  CMD_USER_ADD     = $0001;
  CMD_USER_DEL     = $0002;
  CMD_USER_UPD     = $0003;
  CMD_USER_BAN     = $0004;
  CMD_USER_UNBAN   = $0005;
  CMD_USER_KICK    = $0006;
  CMD_USER_DETAILS = $0007;
  CMD_USER_CLEARCL = $0008;

  //============================================
  //User operation result codes:
  USER_RES_SUCCESS                  = $0000;
  USER_RES_BAD_REQUEST              = $0001;
  USER_RES_NOT_FOUND                = $0002;
  USER_RES_ALREADY_EXISTS           = $0003;
  USER_RES_LEN_LIMIT                = $0004;
  USER_RES_SERVICE_TEMP_UNAVAILABLE = $0005;

  //============================================
  //Admin list types:
  LIST_TYPE_CONNECTED     = $0001;
  LIST_TYPE_REGISTERED    = $0002;
  LIST_TYPE_LAST_REGED    = $0003;

  //============================================
  //List request result codes:
  LIST_REQ_FINISHED         = $0000;
  LIST_REQ_MORE_AVAILABLE   = $0001;
  LIST_REQ_BAD_REQUEST      = $0002;

  //============================================
  //Administrative extension update settings result codes:
  ADM_EXT_UPD_SET_SUCCESS = $0000;
  ADM_EXT_UPD_SET_FAILED  = $0001;

  //============================================
  //Req history types:
  HIST_TYPE_TIME_PERIOD   = $0001;
  HIST_TYPE_LAST_MSGS     = $0002;
  HIST_TYPE_ALL_MSGS      = $0003;
  HIST_TYPE_USER_MSGS     = $0004;
  HIST_TYPE_TWO_USERS     = $0005;

  //============================================
  //History request result codes:
  HIST_REQ_FINISHED         = $0000;
  HIST_REQ_MORE_AVAILABLE   = $0001;
  HIST_REQ_BAD_REQUEST      = $0002;


  //============================================
  //Base reply codes:
  BASE_REQ_DONE             = $0000;
  BASE_LOADED               = $0001;
  BASE_LOAD_FAILED          = $0002;
  BASE_UNAVAILABLE          = $0003;
  BASE_NOT_FOUND            = $0004;
  BASE_CANT_ADD_ACC_EXISTS  = $0005;
  BASE_BAD_REQUEST          = $0006;

  //============================================
  //Internal authorization messages types:
  AUTH_TYPE_REQUEST = $01;
  AUTH_TYPE_REPLY   = $02;
  AUTH_TYPE_REVOKE  = $03;

  AUTH_TYPE_MAX     = AUTH_TYPE_REVOKE;

  //============================================
  //Contact list default limits
  CL_MAX_ACC_NAME_LEN      = 255;
  CL_MAX_GROUPS_COUNT      = 1000;
  CL_MAX_GROUP_NAME_LEN    = 255;
  CL_MAX_CONTACTS_COUNT    = 8000;
  CL_MAX_CONTACT_NAME_LEN  = 255;
  CL_MAX_AUTH_REASON_LEN   = 2048;
  CL_MAX_USER_STLDS_COUNT  = 3;
  CL_MAX_USER_STLD_LEN     = 1024;
  CL_MAX_NOTES_COUNT       = 1000;
  CL_MAX_NOTE_NAME_LEN     = 255;
  CL_MAX_NOTE_TEXT_LEN     = 1024;

  //============================================
  //Presence default limits
  PRES_MAX_STATUS_NAME_LEN = 128;
  PRES_MAX_ST_PIC_DESC_LEN = 512;
  PRES_MAX_CLIENT_NAME_LEN = 128;
  PRES_MAX_CAPS_COUNT      = 64;

  //============================================
  //Instant messaging default limits
  IM_MAX_MSG_DATA_LEN          = 32768;
  IM_MAX_MULTIPLE_STLD_PER_BEX = 30;

  //============================================
  //User directory default limits
  UD_MAX_DETAILS_FIELD_LEN = 255;
  UD_MAX_ABOUT_FIELD_LEN   = 2048;

  //============================================
  //User avatars default limits
  UA_MAX_AVATAR_FILE_SIZE  = 32768;

  //============================================
  //Default max transports count per account
  TP_DEF_MAX_COUNT_PER_ACC = 5;

  //============================================
  //Contact list Item types:
  CL_ITEM_TYPE_GROUP      = $0001;
  CL_ITEM_TYPE_CONTACT    = $0002;
  CL_ITEM_TYPE_TRANSPORT  = $0003;
  CL_ITEM_TYPE_NOTE       = $0004;

  //============================================
  //Contact list Item sTLD types:
  TLD_CL_GROUP_NAME     = $0001;
  TLD_CL_ACCOUNT_NAME   = $0002;
  TLD_CL_CONTACT_NAME   = $0003;
  TLD_CL_PRIVACY_TYPE   = $0004;
  TLD_CL_AUTH_FLAG      = $0005;
  TLD_CL_GEN_FLAG       = $0006;
  TLD_CL_TP_ITEM_ID     = $1001;
  TLD_CL_TP_UUID        = $1002;
  TLD_CL_TP_ACC_NAME    = $1003;
  TLD_CL_TP_FRLY_NAME   = $1004;
  TLD_CL_NOTE_NAME      = $2001;
  TLD_CL_NOTE_TYPE      = $2002;
  TLD_CL_NOTE_TEXT      = $2003;
  TLD_CL_NOTE_DATE      = $2004;
  TLD_CL_NOTE_MD5       = $2005;

  TLD_CL_DEV_STLD_MIN   = $8000;
  TLD_CL_DEV_STLD_MAX   = $FFFF;

  //============================================
  //Contact privacy types:
  CL_PRIV_TYPE_NONE               = $00;
  CL_PRIV_TYPE_VISIBLE_LIST       = $01;
  CL_PRIV_TYPE_INVISIBLE_LIST     = $02;
  CL_PRIV_TYPE_IGNORE_LIST        = $03;
  CL_PRIV_TYPE_IGNORE_NOT_IN_LIST = $04;
  //dont forget to update if adding new privacy type
  CL_PRIV_TYPE_MAX                = CL_PRIV_TYPE_IGNORE_NOT_IN_LIST;

  //============================================
  //Notes types:
  CL_NOTE_TYPE_TEXT      = $00;
  CL_NOTE_TYPE_COMMAND   = $01;
  CL_NOTE_TYPE_LINK      = $02;
  CL_NOTE_TYPE_EMAIL     = $03;
  CL_NOTE_TYPE_PHONE     = $04;
  //dont forget to update if adding new note type
  CL_NOTE_TYPE_MAX       = CL_NOTE_TYPE_PHONE;

  //============================================
  //Adding contact result codes:
  ADD_RES_SUCCESS                     = $0000;
  ADD_RES_ERROR_WRONG_ITEM_TYPE       = $0001;
  ADD_RES_ERROR_WRONG_PARENT_GROUP    = $0002;
  ADD_RES_ERROR_NAME_LEN_LIMIT        = $0003;
  ADD_RES_ERROR_WRONG_NAME            = $0004;
  ADD_RES_ERROR_ITEM_ALREADY_EXISTS   = $0005;
  ADD_RES_ERROR_ITEM_LIMIT_REACHED    = $0006;
  ADD_RES_ERROR_BAD_REQUEST           = $0007;
  ADD_RES_ERROR_BAD_ITEM_STLD         = $0008;
  ADD_RES_ERROR_NOT_ALLOWED           = $0009;

  //============================================
  //Deletion contact result codes:
  DEL_RES_SUCCESS                     = $0000;
  DEL_RES_ERROR_NOT_FOUND             = $0001;
  DEL_RES_ERROR_NOT_ALLOWED           = $0002;
  DEL_RES_ERROR_GROUP_NOT_EMPTY       = $0003;

  //============================================
  //Update contact operation result codes:
  UPD_RES_SUCCESS                     = $0000;
  UPD_RES_ERROR_NOT_FOUND             = $0001;
  UPD_RES_ERROR_WRONG_PARENT_GROUP    = $0002;
  UPD_RES_ERROR_NAME_LEN_LIMIT        = $0003;
  UPD_RES_ERROR_WRONG_NAME            = $0004;
  UPD_RES_ERROR_ITEM_ALREADY_EXISTS   = $0005;
  UPD_RES_ERROR_BAD_REQUEST           = $0006;
  UPD_RES_ERROR_BAD_ITEM_STLD         = $0007;
  UPD_RES_ERROR_NOT_ALLOWED           = $0008;

  //============================================
  //Authorization reply code:
  AUTH_REPLY_GRANTED  = $0001;
  AUTH_REPLY_DENIED   = $0002;

  //============================================
  //Server side item operation code:
  OPER_ADD_ITEM = $0001;
  OPER_DEL_ITEM = $0002;
  OPER_UPD_ITEM = $0003;

  //============================================
  //Required optional client flags:
  PRES_REQ_FLAG_HOSTNAME = $00000001;

  //============================================
  //Available client capabilities:
  CAP_MSGS_UTF8       = $0001;
  CAP_MSGS_RTF        = $0002;
  CAP_MSGS_HTML       = $0003;
  CAP_MSGS_ENCRYPT    = $0004;
  CAP_NOTIFS_TYPING   = $0005;
  CAP_AVATARS         = $0006;
  CAP_FILE_TRANSFER   = $0007;
  CAP_TRANSPORTS      = $0008;
  CAP_NOTIFS_ALARM    = $0009;
  CAP_NOTIFS_MAIL     = $000A;

  //============================================
  //Available client types:
  CLIENT_TYPE_USER    = $0001;
  CLIENT_TYPE_BOT     = $0002;
  CLIENT_TYPE_SERVICE = $0003;

  CLIENT_TYPE_MAX     = CLIENT_TYPE_SERVICE;

  //============================================
  //Available default presence status values:
  PRES_STATUS_ONLINE              = $0000;
  PRES_STATUS_INVISIBLE           = $0001;
  PRES_STATUS_INVISIBLE_FOR_ALL   = $0002;
  PRES_STATUS_FREE_FOR_CHAT       = $0003;
  PRES_STATUS_AT_HOME             = $0004;
  PRES_STATUS_AT_WORK             = $0005;
  PRES_STATUS_LUNCH               = $0006;
  PRES_STATUS_AWAY                = $0007;
  PRES_STATUS_NOT_AVAILABLE       = $0008;
  PRES_STATUS_OCCUPIED            = $0009;
  PRES_STATUS_DO_NOT_DISTURB      = $000A;

  PRES_STATUS_MAX                 = PRES_STATUS_DO_NOT_DISTURB;
  PRES_STATUS_DEVELOPER           = $80000000;

  PRES_STATUS_OFFLINE             = $FFFF; //not used in protocol

  STATUS_ONLINE_STR               = 'Online';

  //============================================
  //Available client flags:
  PRES_CF_RCV_CLIENT_NAME_AND_VER = $00000001; //set by default
  PRES_CF_RCV_OS_INFORMATION      = $00000002; //set by default
  PRES_CF_RCV_CLIENT_DESCRIPTION  = $00000004;
  PRES_CF_RCV_CLIENT_ID_BLK       = $00000008;

  //============================================
  //Message types:
  MSG_TYPE_UTF8     = $0001;
  MSG_TYPE_RTF      = $0002;
  MSG_TYPE_HTML     = $0003;

  MSG_TYPE_MAX      = MSG_TYPE_HTML;

  //============================================
  //Available notifications types:
  NOTIF_TYPE_USER_TYPING = $0001;
  NOTIF_TYPE_WAKE_ALARM  = $0002;

  //============================================
  //Notification values:
  NOTIF_VALUE_USER_TYPING_START   = $0001;
  NOTIF_VALUE_USER_TYPING_FINISH  = $0002;
  NOTIF_VALUE_WAKE_ALARM_PLAY     = $0003;
  NOTIF_VALUE_WAKE_ALARM_WAIT     = $0004;

  //============================================
  //Available encryption types:
  ENC_TYPE_DISABLED = $0000;
  ENC_TYPE_OBIMP    = $0001;
  ENC_TYPE_PGP      = $0002;

  ENC_TYPE_MAX      = ENC_TYPE_PGP;

  //============================================
  //User details request result codes:
  DETAILS_RES_SUCCESS                  = $0000;
  DETAILS_RES_NOT_FOUND                = $0001;
  DETAILS_RES_TOO_MANY_REQUESTS        = $0002;
  DETAILS_RES_SERVICE_TEMP_UNAVAILABLE = $0003;

  //============================================
  //Update details result codes:
  UPD_DETAILS_RES_SUCCESS                  = $0000;
  UPD_DETAILS_RES_BAD_REQUEST              = $0001;
  UPD_DETAILS_RES_SERVICE_TEMP_UNAVAILABLE = $0002;
  UPD_DETAILS_RES_NOT_ALLOWED              = $0003;

  //============================================
  //Search result codes:
  SEARCH_RES_SUCCESS                  = $0000;
  SEARCH_RES_NOT_FOUND                = $0001;
  SEARCH_RES_BAD_REQUEST              = $0002;
  SEARCH_RES_TOO_MANY_REQUESTS        = $0003;
  SEARCH_RES_SERVICE_TEMP_UNAVAILABLE = $0004;

  SEARCH_ST_PIC_FLAG_CUSTOM           = $00010000;

  //============================================
  //Update secure email or password result codes:
  UPD_SECURE_RES_SUCCESS                  = $0000;
  UPD_SECURE_RES_BAD_REQUEST              = $0001;
  UPD_SECURE_RES_SERVICE_TEMP_UNAVAILABLE = $0002;
  UPD_SECURE_RES_NOT_ALLOWED              = $0003;

  //============================================
  //Gender:
  GENDER_FEMALE      = $01;
  GENDER_MALE        = $02;

  //============================================
  //Zodiac signs:
  ZODIAC_ARIES       = $01;
  ZODIAC_TAURUS      = $02;
  ZODIAC_GEMINI      = $03;
  ZODIAC_CANCER      = $04;
  ZODIAC_LEO         = $05;
  ZODIAC_VIRGO       = $06;
  ZODIAC_LIBRA       = $07;
  ZODIAC_SCORPIO     = $08;
  ZODIAC_SAGITTARIUS = $09;
  ZODIAC_CAPRICORN   = $0A;
  ZODIAC_AQUARIUS    = $0B;
  ZODIAC_PISCES      = $0C;

  //============================================
  //Avatar request result:
  AVATAR_REQ_SUCCESS      = $0000;
  AVATAR_REQ_NOT_FOUND    = $0001;
  AVATAR_REQ_NOT_ALLOWED  = $0002;

  //============================================
  //Avatar set result:
  AVATAR_SET_SUCCESS          = $0000;
  AVATAR_SET_BAD_MD5          = $0001;
  AVATAR_SET_NOT_ALLOWED      = $0002;
  AVATAR_SET_TEMP_UNAVAILABLE = $0003;
  AVATAR_SET_TOO_BIG          = $0004;
  AVATAR_SET_TOO_SMALL        = $0005;
  AVATAR_SET_BANNED           = $0006;
  AVATAR_SET_INVALID_TYPE     = $0007;
  AVATAR_SET_OTHER_ERROR      = $0008;

  //============================================
  //File transfer reply codes:
  FT_REPLY_CODE_ACCEPT        = $0001;
  FT_REPLY_CODE_DECLINE       = $0002;
  FT_REPLY_CODE_DISABLED      = $0003;
  FT_REPLY_CODE_NOT_ALLOWED	  = $0004;

  FT_REPLY_CODE_MAX       	  = FT_REPLY_CODE_NOT_ALLOWED;

  //============================================
  //File transfer control codes:
  FT_CONTROL_CODE_CANCEL                    = $0001;
  FT_CONTROL_CODE_DIRECT_FAILED             = $0002;
  FT_CONTROL_CODE_DIRECT_FAILED_TRY_REVERSE = $0003;
  FT_CONTROL_CODE_DIRECT_FAILED_TRY_PROXY   = $0004;
  FT_CONTROL_CODE_PROXY_FAILED              = $0005;
  FT_CONTROL_CODE_READY                     = $0006;

  FT_CONTROL_CODE_MAX         = FT_CONTROL_CODE_READY;

  //============================================
  //File transfer error codes:
  FT_ERROR_CODE_TIMEOUT             = $0001;
  FT_ERROR_CODE_WRONG_UNIQ_FT_ID    = $0002;
  FT_ERROR_CODE_WRONG_FILE_NAME     = $0003;
  FT_ERROR_CODE_WRONG_RELATIVE_PATH = $0004;
  FT_ERROR_CODE_WRONG_RESUME_POS    = $0005;
  FT_ERROR_CODE_PROXY_TRAFFIC_LIMIT = $0006;

  //============================================
  //Transport settings flags:
  TP_SF_HIDE_SRV_PARAMS = $0001;

  //============================================
  //Transport options types:
  TP_OT_BOOL     = 1;
  TP_OT_BYTE     = 2;
  TP_OT_WORD     = 3;
  TP_OT_LONGWORD = 4;
  TP_OT_QUADWORD = 5;
  TP_OT_UTF8     = 6;

  //============================================
  //Transport options flags:
  TP_OF_CHECK      = $00000001;
  TP_OF_EDIT       = $00000002;
  TP_OF_COMBO      = $00000004;
  TP_OF_LINK       = $00000008;
  TP_OF_CHANGE     = $00010000;
  TP_OF_READ_ONLY  = $00020000;
  TP_OF_DISABLED   = $00040000;

  //============================================
  //Transport options other:
  OPT_DIV        = '\';
  VAL_DIV        = '|';

  //============================================
  //Transport settings update result codes:
  TP_SET_RES_SUCCESS              = $0000;
  TP_SET_RES_ERROR_WRONG_ID       = $0001;
  TP_SET_RES_ERROR_NOT_FOUND      = $0002;
  TP_SET_RES_ERROR_NOT_ALLOWED    = $0003;

  //============================================
  //Transport connection managing codes:
  TP_CON_MAN_CONNECT    = $0001;
  TP_CON_MAN_STATUS     = $0002;
  TP_CON_MAN_DISCONNECT = $0003;

  TP_CON_MAN_MAX        = TP_CON_MAN_DISCONNECT;

  //============================================
  //Transport state codes:
  TP_STATE_LOGGEDIN                 = $0000;
  TP_STATE_LOGGEDOFF                = $0001;
  TP_STATE_STATUS_CHANGED           = $0002;
  TP_STATE_CON_FAILED               = $0003;
  TP_STATE_ACCOUNT_INVALID          = $0004;
  TP_STATE_SERVICE_TEMP_UNAVAILABLE = $0005;
  TP_STATE_WRONG_PASSWORD           = $0006;
  TP_STATE_INVALID_LOGIN            = $0007;
  TP_STATE_OTHER_PLACE_LOGIN        = $0008;
  TP_STATE_TOO_MANY_TRIES_TRY_LATER = $0009;
  TP_STATE_SRV_PAUSED               = $000A;
  TP_STATE_SRV_RESUMED              = $000B;
  TP_STATE_SRV_MIGRATED             = $000C;

  TP_STATE_MAX                      = TP_STATE_SRV_MIGRATED;

type
  TContactListParams = record
    MaxGroupsCount    : DWord;
    MaxGroupNameLen   : DWord;
    MaxContactsCount  : DWord;
    MaxAccountNameLen : DWord;
    MaxContactNameLen : DWord;
    MaxAuthReasonLen  : DWord;
    MaxUserSTLDsCount : DWord;
    MaxUserSTLDLen    : DWord;
    AutoRemoveAuth    : Boolean;
    MaxNotesCount     : DWord;
    MaxNoteNameLen    : DWord;
    MaxNoteTextLen    : DWord;
  end;


  TPresenceParams = record
    MaxStatusNameLen     : DWord;
    MaxStatusPicDescLen  : DWord;
    MaxClientNameLen     : DWord;
    MaxCapsCount         : DWord;
    ReqOptPresFlags      : DWord;
  end;


  TInstantMessagingParams = record
    MaxAccountNameLen    : DWord;
    MaxMsgDataLen        : DWord;
    MultipleEnabled      : Boolean;
  end;


  TUserDirectoryParams = record
    MaxAccountNameLen    : DWord;
    MaxDetailsFieldLen   : DWord;
    MaxAboutFieldLen     : DWord;
    AllowChangeSecEmail  : Boolean;
    AllowChangePassword  : Boolean;
    SecEmailPassURL      : WideString;
  end;


  TUserAvatarsParams = record
    MaxAvatarFileSize    : DWord;
  end;


  TFileTransferParams = record
    MaxAccountNameLen    : DWord;
    MaxHostIPLen         : DWord;
    MaxFileNameLen       : DWord;
    MaxFilePathLen       : DWord;
    FileTransferEnabled  : Boolean;
    FTProxiedEnabled     : Boolean;
    FTProxyHost          : WideString;
    FTProxyPort          : DWord;
    {--- srv part ---}
    FTProxyLimitMB       : DWord; //MBytes
    FTProxyLog           : Boolean;
    FTMaxClientsCount    : DWord;
    FTTodayTransf        : Int64; //Bytes
  end;


  TTransportsParams = record
    MaxTpCountPerCli     : DWord;
  end;


  TObimpParams = record
    BEX_CL_PARAMS     : TContactListParams;
    BEX_PRES_PARAMS   : TPresenceParams;
    BEX_IM_PARAMS     : TInstantMessagingParams;
    BEX_UD_PARAMS     : TUserDirectoryParams;
    BEX_UA_PARAMS     : TUserAvatarsParams;
    BEX_FT_PARAMS     : TFileTransferParams;
    BEX_TP_PARAMS     : TTransportsParams;
  end;
  pObimpParams = ^TObimpParams;


implementation

end.
