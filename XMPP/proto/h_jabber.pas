// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit h_jabber;

interface

uses Windows, Classes, SysUtils, OverbyteIcsMD5, OverbyteIcsMimeUtils, DateUtils;

const
  XMPP_KEEP_ALIVE_INTERVAL  = 60; //secs
  CONNECT_ATTEMPT_TIMEOUT   = 60; //secs
  XMPP_MAX_RESINFO_COUNT    = 10; //max resources count inside one roster item resarray

  THE_SPC = ' ';
  THE_DOT = '.';
  THE_COM = ',';
  THE_CLN = ':';
  CRLF    = #13#10;

  XNS_URI                   = 'urn:ietf:params:xml:ns:';
  XNS_STREAM_STREAM         = 'stream:stream';
  XNS_STREAM_ERROR          = 'stream:error';
  XNS_STREAM_FEATURES       = 'stream:features';
  XNS_START_TLS             = 'starttls';
  XNS_REQUIRED              = 'required';
  XNS_PROCEED               = 'proceed';
  XNS_AUTH                  = 'auth';
  XNS_JABBER_COMPRESS       = 'http://jabber.org/protocol/compress';
  XNS_COMPRESSION           = 'compression';
  XNS_COMPRESS              = 'compress';
  XNS_COMPRESSED            = 'compressed';
  XNS_METHOD                = 'method';
  XNS_MECHANISMS            = 'mechanisms';
  XNS_MECHANISM             = 'mechanism';
  XNS_REGISTER              = 'register';
  XNS_CHALLENGE             = 'challenge';
  XNS_RESPONSE              = 'response';
  XNS_SUCCESS               = 'success';
  XNS_FAILURE               = 'failure';
  XNS_BYE                   = 'xmppbye'; //helper
  XNS_BIND                  = 'bind';
  XNS_SESSION               = 'session';
  XNS_IQ                    = 'iq';
  XNS_RESOURCE              = 'resource';
  XNS_PRESENCE              = 'presence';
  XNS_PRIORITY              = 'priority';
  XNS_SHOW                  = 'show';
  XNS_STATUS                = 'status';
  XNS_JID                   = 'jid';
  XNS_JABBER_DISCO_INFO     = 'http://jabber.org/protocol/disco#info';
  XNS_JABBER_IQ_ROSTER      = 'jabber:iq:roster';
  XNS_JABBER_IQ_PRIVATE     = 'jabber:iq:private';
  XNS_JABBER_IQ_PRIVACY     = 'jabber:iq:privacy';
  XNS_JABBER_IQ_SEARCH      = 'jabber:iq:search';
  XNS_JABBER_IQ_VERSION     = 'jabber:iq:version';
  XNS_MSG_OFFLINE           = 'msgoffline';
  XNS_JABBER_CHAT_STATES    = 'http://jabber.org/protocol/chatstates';
  XNS_IDENTITY              = 'identity';
  XNS_FEATURE               = 'feature';
  XNS_ROSTER                = 'roster';
  XNS_ROSTER_DELIMITER      = 'roster:delimiter';
  XNS_QUERY                 = 'query';
  XNS_DEFAULT               = 'default';
  XNS_ACTIVE                = 'active';
  XNS_LIST                  = 'list';
  XNS_ITEM                  = 'item';
  XNS_MESSAGE               = 'message';
  XNS_PRESENCE_IN           = 'presence-in';
  XNS_PRESENCE_OUT          = 'presence-out';
  XNS_XMPP_PING             = 'urn:xmpp:ping';
  XNS_XMPP_RECEIPTS         = 'urn:xmpp:receipts';
  XNS_XMPP_DELAY            = 'urn:xmpp:delay';
  XNS_PING                  = 'ping';
  XNS_NAME                  = 'name';
  XNS_TYPE                  = 'type';
  XNS_TEXT                  = 'text';
  XNS_VERSION               = 'version';
  XNS_OS                    = 'os';
  XNS_GROUP                 = 'group';
  XNS_JABBER_X_AVATAR       = 'jabber:x:avatar';
  XNS_VCARD_TEMP            = 'vcard-temp';
  XNS_VCARD_TEMP_X_UPDATE   = 'vcard-temp:x:update';
  XNS_PHOTO                 = 'photo';
  XNS_HASH                  = 'hash';
  XNS_X                     = 'x';
  XNS_ITEM_NOT_FOUND        = 'item-not-found';
  XNS_ERROR                 = 'error';
  XNS_SVC_UNAVAIL           = 'service-unavailable';
  XNS_XMPP_STANZAS          = 'xmpp-stanzas';
  XNS_SUBJECT               = 'subject';
  XNS_BODY                  = 'body';
  XNS_THREAD                = 'thread';
  XNS_REQUEST               = 'request';
  XNS_RECEIVED              = 'received';
  XNS_COMPOSING             = 'composing';
  XNS_PAUSED                = 'paused';
  XNS_INACTIVE              = 'inactive';
  XNS_GONE                  = 'gone';
  XNS_VCARD                 = 'vCard';
  XNS_FN                    = 'FN';
  XNS_NICKNAME              = 'NICKNAME';
  XNS_N                     = 'N';
  XNS_GIVEN                 = 'GIVEN';
  XNS_FAMILY                = 'FAMILY';
  XNS_MIDDLE                = 'MIDDLE';
  XNS_ADR                   = 'ADR';
  XNS_REGION                = 'REGION';
  XNS_LOCALITY              = 'LOCALITY';
  XNS_PCODE                 = 'PCODE';
  XNS_EXTADD                = 'EXTADD';
  XNS_EXTADR                = 'EXTADR'; //bad
  XNS_STREET                = 'STREET';
  XNS_CTRY                  = 'CTRY';
  XNS_COUNTRY               = 'COUNTRY'; //bad
  XNS_HOME                  = 'HOME';
  XNS_BDAY                  = 'BDAY';
  XNS_URL                   = 'URL';
  XNS_DESC                  = 'DESC';
  XNS_EMAIL                 = 'EMAIL';
  XNS_USERID                = 'USERID';
  XNS_TEL                   = 'TEL';
  XNS_NUMBER                = 'NUMBER';
  XNS_CELL                  = 'CELL';
  XNS_FAX                   = 'FAX';
  XNS_WORK                  = 'WORK';
  XNS_ORG                   = 'ORG';
  XNS_ORGNAME               = 'ORGNAME';
  XNS_ORGUNIT               = 'ORGUNIT';
  XNS_TITLE                 = 'TITLE';
  XNS_ROLE                  = 'ROLE';
  XNS_BINVAL                = 'BINVAL';

  XURI_XMPP_TLS             = 'xmpp-tls';
  XURI_XMPP_SASL            = 'xmpp-sasl';
  XURI_XMPP_BIND            = 'xmpp-bind';
  XURI_XMPP_SESSION         = 'xmpp-session';

  XTAG_HEADER               = '<?xml version=''1.0'' encoding=''UTF-8''?>';
  XTAG_INIT_STREAM          = '<' + XNS_STREAM_STREAM + ' to=''%'' xmlns=''jabber:client'' xmlns:stream=''http://etherx.jabber.org/streams'' xml:lang=''%'' version=''1.0''>';
  XTAG_END_STREAM           = '</' + XNS_STREAM_STREAM + '>';
  XTAG_START_TLS_EMPTY      = '<' + XNS_START_TLS + ' xmlns=''' + XNS_URI + XURI_XMPP_TLS + '''/>';
  XTAG_COMPRESS             = '<' + XNS_COMPRESS + ' xmlns=''' + XNS_JABBER_COMPRESS + '''><method>%</method></' + XNS_COMPRESS + '>';
  XTAG_AUTH_MECHANISM1      = '<' + XNS_AUTH     + ' xmlns=''' + XNS_URI + XURI_XMPP_SASL + ''' ' + XNS_MECHANISM + '=''%''/>';
  XTAG_AUTH_MECHANISM2      = '<' + XNS_AUTH     + ' xmlns=''' + XNS_URI + XURI_XMPP_SASL + ''' ' + XNS_MECHANISM + '=''%''>%</' + XNS_AUTH + '>';
  XTAG_RESPONSE             = '<' + XNS_RESPONSE + ' xmlns=''' + XNS_URI + XURI_XMPP_SASL + '''>%</' + XNS_RESPONSE + '>';
  XTAG_RESPONSE_EMPTY       = '<' + XNS_RESPONSE + ' xmlns=''' + XNS_URI + XURI_XMPP_SASL + '''/>';
  XTAG_BYE                  = '<' + XNS_BYE      + '/>'; //helper
  XTAG_IQ                   = '<' + XNS_IQ       + ' type=''%'' id=''%''>%</' + XNS_IQ + '>';
  XTAG_IQ_EMPTY             = '<' + XNS_IQ       + ' type=''%'' id=''%''/>';
  XTAG_IQ_FROM_TO           = '<' + XNS_IQ       + ' type=''%'' from=''%'' to=''%'' id=''%''>%</' + XNS_IQ + '>';
  XTAG_IQ_FROM_TO_EMPTY     = '<' + XNS_IQ       + ' type=''%'' from=''%'' to=''%'' id=''%''/>';
  XTAG_IQ_TO                = '<' + XNS_IQ       + ' type=''%'' to=''%'' id=''%''>%</' + XNS_IQ + '>';
  XTAG_BIND                 = '<' + XNS_BIND     + ' xmlns=''' + XNS_URI + XURI_XMPP_BIND + '''>%</' + XNS_BIND + '>';
  XTAG_BIND_EMPTY           = '<' + XNS_BIND     + ' xmlns=''' + XNS_URI + XURI_XMPP_BIND + '''/>';
  XTAG_RESOURCE             = '<' + XNS_RESOURCE + '>%</' + XNS_RESOURCE + '>';
  XTAG_SESSION_EMPTY        = '<' + XNS_SESSION  + ' xmlns=''' + XNS_URI + XURI_XMPP_SESSION + '''/>';
  XTAG_PRESENCE             = '<' + XNS_PRESENCE + '>%</' + XNS_PRESENCE + '>';
  XTAG_PRESENCE_TO          = '<' + XNS_PRESENCE + ' to=''%''>%</' + XNS_PRESENCE + '>';
  XTAG_PRESENCE_TYTO_EMPTY  = '<' + XNS_PRESENCE + ' to=''%'' type=''%''/>';
  XTAG_PRESENCE_TYPE_EMPTY  = '<' + XNS_PRESENCE + ' type=''%''/>';
  XTAG_PRESENCE_TYPE        = '<' + XNS_PRESENCE + ' type=''%''>%</' + XNS_PRESENCE + '>';
  XTAG_PRESENCE_SHOW        = '<' + XNS_PRESENCE + ' show=''%''>%</' + XNS_PRESENCE + '>';
  XTAG_PRESENCE_SHOW_TO     = '<' + XNS_PRESENCE + ' to=''%'' show=''%''>%</' + XNS_PRESENCE + '>';
  XTAG_PRIORITY             = '<' + XNS_PRIORITY + '>%</' + XNS_PRIORITY + '>';
  XTAG_SHOW                 = '<' + XNS_SHOW     + '>%</' + XNS_SHOW + '>';
  XTAG_STATUS               = '<' + XNS_STATUS   + '>%</' + XNS_STATUS + '>';
  XTAG_NAME                 = '<' + XNS_NAME     + '>%</' + XNS_NAME + '>';
  XTAG_VERSION              = '<' + XNS_VERSION  + '>%</' + XNS_VERSION + '>';
  XTAG_OS                   = '<' + XNS_OS       + '>%</' + XNS_OS + '>';
  XTAG_ROSTER_EMPTY         = '<' + XNS_ROSTER   + ' xmlns=''%''/>';
  XTAG_QUERY                = '<' + XNS_QUERY    + ' xmlns=''%''>%</' + XNS_QUERY + '>';
  XTAG_QUERY_EMPTY          = '<' + XNS_QUERY    + ' xmlns=''%''/>';
  XTAG_QUERY_NODE           = '<' + XNS_QUERY    + ' xmlns=''%'' node=''%''>%</' + XNS_QUERY + '>';
  XTAG_LIST                 = '<' + XNS_LIST     + ' name=''%''>%</' + XNS_LIST + '>';
  XTAG_LIST_EMPTY           = '<' + XNS_LIST     + ' name=''%''/>';
  XTAG_ACTIVE_EMPTY         = '<' + XNS_ACTIVE   + ' name=''%''/>';
  XTAG_ITEM_EMPTY           = '<' + XNS_ITEM     + ' %/>';
  XTAG_ITEM                 = '<' + XNS_ITEM     + ' %>%</' + XNS_ITEM + '>';
  XTAG_MESSAGE_EMPTY        = '<' + XNS_MESSAGE  + '/>' ;
  XTAG_PRESENCE_IN          = '<' + XNS_PRESENCE_IN + '/>' ;
  XTAG_PRESENCE_OUT         = '<' + XNS_PRESENCE_OUT + '/>' ;
  XTAG_IQ_NO_ATTS           = '<' + XNS_IQ       + '/>' ;
  XTAG_PING                 = '<' + XNS_PING     + ' xmlns=''' + XNS_XMPP_PING + '''/>';
  XTAG_GROUP                = '<' + XNS_GROUP    + '>%</' + XNS_GROUP + '>';
  XTAG_IDENTITY_EMPTY       = '<' + XNS_IDENTITY + ' category=''%'' type=''%'' name=''%''/>';
  XTAG_FEATURE_EMPTY        = '<' + XNS_FEATURE  + ' var=''%''/>';
  XTAG_ERROR                = '<' + XNS_ERROR    + ' type=''%''>%</' + XNS_ERROR + '>';
  XTAG_SVC_UNAVAIL          = '<' + XNS_SVC_UNAVAIL + ' xmlns=''' + XNS_URI + XNS_XMPP_STANZAS + '''/>';
  XTAG_MESSAGE              = '<' + XNS_MESSAGE  + ' %>%</' + XNS_MESSAGE + '>';
  XTAG_BODY                 = '<' + XNS_BODY     + '>%</' + XNS_BODY + '>';
  XTAG_REQUEST              = '<' + XNS_REQUEST  + ' xmlns=''' + XNS_XMPP_RECEIPTS + '''/>';
  XTAG_RECEIVED             = '<' + XNS_RECEIVED + ' xmlns=''' + XNS_XMPP_RECEIPTS + ''' id=''%''/>';
  XTAG_CHAT_STATE           = '<% xmlns=''' + XNS_JABBER_CHAT_STATES + '''/>';
  XTAG_VCARD_EMPTY          = '<' + XNS_VCARD    + ' xmlns=''' + XNS_VCARD_TEMP + '''/>';
  XTAG_VCARD                = '<' + XNS_VCARD    + ' xmlns=''' + XNS_VCARD_TEMP + '''>%</' + XNS_VCARD + '>';
  XTAG_X                    = '<' + XNS_X        + ' xmlns=''%''>%</' + XNS_X + '>';

  //Attributes
  XATT_LANG                 = 'en';
  XATT_XMLNS                = 'xmlns';
  XATT_ID                   = 'id';
  XATT_VAR                  = 'var';
  XATT_TYPE                 = 'type';
  XATT_FROM                 = 'from';
  XATT_TO                   = 'to';
  XATT_NAME                 = 'name';
  XATT_VALUE                = 'value';
  XATT_ACTION               = 'action';
  XATT_ORDER                = 'order';
  XATT_JID                  = 'jid';
  XATT_SUBSCRIPTION         = 'subscription';
  XATT_ASK                  = 'ask';
  XATT_SHOW                 = 'show';
  XATT_NODE                 = 'node';
  XATT_CATEGORY             = 'category';
  XATT_STAMP                = 'stamp';

  //Values
  XVAL_PLAIN                = 'PLAIN';
  XVAL_DIGEST_MD5           = 'DIGEST-MD5';
  XVAL_CRAM_MD5             = 'CRAM-MD5';
  XVAL_SCRAM_SHA1           = 'SCRAM-SHA-1';
  XVAL_XGOOGLE_TOKEN        = 'X-GOOGLE-TOKEN';
  XVAL_ZLIB                 = 'zlib';
  XVAL_SET                  = 'set';
  XVAL_GET                  = 'get';
  XVAL_RESULT               = 'result';
  XVAL_ERROR                = 'error';
  XVAL_JID                  = 'jid';
  XVAL_GROUP                = 'group';
  XVAL_SUBSCRIPTION         = 'subscription';
  XVAL_NONE                 = 'none';
  XVAL_FROM                 = 'from';
  XVAL_TO                   = 'to';
  XVAL_BOTH                 = 'both';
  XVAL_REMOVE               = 'remove';
  XVAL_ALLOW                = 'allow';
  XVAL_DENY                 = 'deny';
  XVAL_CLIENT               = 'client';
  XVAL_PC                   = 'pc';
  XVAL_CANCEL               = 'cancel';

  //Helper strings
  XHLP_NO_REASON            = 'no reason';
  XHLP_STREAM_CLOSED        = 'server closed stream';
  XHLP_BAD_USERNAME         = 'bad username';
  XHLP_NO_AUTH_MECHANISMS   = 'no auth mechanisms';
  XHLP_SUP_AUTH_NOT_FOUND   = 'supported auth mechanisms not found';
  XHLP_SASL_NOT_AUTHORIZED  = 'not-authorized';
  XHLP_CONFLICT             = 'conflict';
  XHLP_INTERNAL_SRV_ERROR   = 'internal-server-error';
  XHLP_SASL_TEMP_AUTH_FAIL  = 'temporary-auth-failure';

  BIN_IMG_TYPE_PNG          = 'image/png';
  OFFLINE_STORAGE           = 'Offline Storage';

  //sequence counter types
  SEQ_MAX_NUM               = MAXSHORT;
  SEQ_ID_BIMOID             = 'bt';
  SEQ_ID_ACK                = 'ba';
  SEQ_DIV                   = '_';

  SEQT_BIND                 = 1;
  SEQT_SESSION              = 2;
  SEQT_SRV_DISCO_INFO       = 3;
  SEQT_CLI_DISCO_INFO       = 4;
  SEQT_CLIENT_PING          = 5;
  SEQT_ROSTER_DELIM         = 6;
  SEQT_PRIVACY              = 7;
  SEQT_ROSTER               = 8;
  SEQT_PRIVACY_LIST         = 9;
  SEQT_REQUEST_LIST         = 10;
  SEQT_REQ_PUSHED_LIST      = 11;
  SEQT_ACTIVE_LIST          = 12;
  SEQT_UPDCUR_LIST          = 13;
  SEQT_ROSTER_OPER          = 14;
  SEQT_CLIENT_VER           = 15;
  SEQT_REQ_VCARD            = 16;
  SEQT_UPD_VCARD            = 17;
  SEQT_UPD_VCARD_AVATAR     = 18;
  SEQT_REQ_VCARD_AVATAR     = 19;

  {$IFDEF GTALK}
  SEQT_GTALK_GET_SET_LOGIN  = 101;
  SEQT_GTALK_GET_SETTING    = 102;
  SEQT_GTALK_SET_SETTING    = 103;
  SEQT_GTALK_GET_SHARED_ST  = 104;
  SEQT_GTALK_SET_SHARED_ST  = 105;
  SEQT_GTALK_GET_UNREAD_MAIL= 106;
  SEQT_GTALK_GET_NEW_MAIL   = 107;
  {$ENDIF}

  //specific
  {.$message 'rename priv list to bimoid'}
  LIST_BIMOID               = 'bimoidtp';

  //presence show values
  PRES_SHOW_ONLINE          = '';
  PRES_SHOW_AWAY            = 'away';
  PRES_SHOW_CHAT            = 'chat';
  PRES_SHOW_DND             = 'dnd';
  PRES_SHOW_XA              = 'xa';
  PRES_SHOW_INVIS_SPC       = 'invisible'; //non RFC
  PRES_SHOW_IFA_SPC         = 'ifa'; //non RFC

  //presence types
  PRES_TYPE_NONE            = '';
  PRES_TYPE_UNAVAILABLE     = 'unavailable';
  PRES_TYPE_SUBSCRIBE       = 'subscribe';
  PRES_TYPE_SUBSCRIBED      = 'subscribed';
  PRES_TYPE_UNSUBSCRIBE     = 'unsubscribe';
  PRES_TYPE_UNSUBSCRIBED    = 'unsubscribed';
  PRES_TYPE_PROBE           = 'probe';
  PRES_TYPE_ERROR           = 'error';

  //message type
  MSG_TYPE_ERROR            = 'error';
  MSG_TYPE_NORMAL           = 'normal';
  MSG_TYPE_CHAT             = 'chat';
  MSG_TYPE_GROUPCHAT        = 'groupchat';
  MSG_TYPE_HEADLINE         = 'headline';

  //notif types
  XMPP_NOTIF_MSG_FAILED     = 1;

type
  TOnErrorMsg = procedure (Sender: TObject; const ErrMsg: string) of object;

  TXmppOutMsg = record
    Jid             : string;
    Resource        : string;
    Body            : string;
    AckID           : string;
    AckRequired     : Boolean;
    ChatStateActive : Boolean;
  end;
  pXmppOutMsg = ^TXmppOutMsg;

  TXmppIncMsg = record
    Jid         : string;
    Resource    : string;
    Subj        : string;
    Body        : string;
    AckID       : string;
    AckRequired : Boolean;
    IsOffline   : Boolean;
    OfflineDT   : Int64;
  end;
  pXmppIncMsg = ^TXmppIncMsg;

  TXmppMsgAck = record
    Jid         : string;
    Resource    : string;
    AckID       : string;
  end;
  pXmppMsgAck = ^TXmppMsgAck;

  TXmppChatState = record
    Jid         : string;
    Resource    : string;
    ChatState   : string;
  end;
  pXmppChatState = ^TXmppChatState;

  TAvatarRec = record
    ImgType       : string;
    AvatarSha1Hex : string;
    AvatarBase64  : AnsiString;
  end;
  pAvatarRec = ^TAvatarRec;


  procedure OutDebugStr(s: string);

  function  MD5Raw(Value: RawByteString): RawByteString;
  function  RawToHex(const Data: RawByteString; const UseLowerCase: Boolean = False): string;
  function  GetRndHexBytes(Count: Byte): string;

  procedure CanonicToNorm(Canonic: string; var Username, Account, Domain, Resource: string);
  function  CanonicName(Username, Resource: string): string;

  function  XmlTag(const Tag: string; const Value: string = ''): string; overload;
  function  XmlTag(const Tag: string; const Value1, Value2: string): string; overload;
  function  XmlTag(const Tag: string; Args: array of string): string; overload;
  function  XmlTagIq(const IqType, IqId, Value: string): string; overload;
  function  XmlTagIq(const IqType, IqFrom, IqTo, IqId: string; const Value: string = ''): string; overload;
  function  XmlTagIqTo(const IqType, IqTo, IqId, Value: string): string;
  function  XmlTagIqQuery(const IqType, IqId, QueryNS: string; const QueryValue: string = ''): string; overload;
  function  XmlTagIqQuery(const IqType, IqFrom, IqTo, IqId, QueryNS: string; const QueryValue: string = ''): string; overload;

  function  XmlAtt(const Attribute: string; const AttValue: string = ''): string;
  function  MakeTag(const XnsVal: string; const TagVal: string = ''; const XnsAtts: string = ''): string;

  function  Int64ToVer(Value: Int64): string;

  //copied from HTTPApp module but removed exception raise moments
  function  XmppHTMLEncode(const AStr: String): String; //NativeXML has sdEscapeString function but it is slower than this one
  //function  XmppHTMLDecode(const AStr: String): String; //not required, cauz NativeXML decodes itself

  function  VCardDateToUnix(s: string): Int64;
  function  UnixToVCardDate(const UDT: Int64): string;
  function  Iso8601ToUnix(s: string): Int64;
  function  PosRight(const substr, str: string): integer;


implementation

{*****************************************************************}
procedure OutDebugStr(s: string);
begin
  OutputDebugString(PChar(s));
end;

{*******************************************************************}
function RawToHex(const Data: RawByteString; const UseLowerCase: Boolean = False): string;
begin
  Result := '';

  if (Length(Data) > 0) then
  begin
    SetLength(Result, Length(Data) * 2);
    BinToHex(PAnsiChar(Data), PWideChar(Result), Length(Data));
  end;

  if UseLowerCase then
    Result := LowerCase(Result);
end;

{*******************************************************************}
function GetRndHexBytes(Count: Byte): string;
var i: integer;
begin
  Result := '';
  for i := 1 to Count do
    Result := Result + LowerCase( IntToHex(Random(255)+1, 2) );
end;

{*******************************************************************}
function MD5Raw(Value: RawByteString): RawByteString;
var MD5Digest  : TMD5Digest;
    MD5Context : TMD5Context;
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
procedure CanonicToNorm(Canonic: string; var Username, Account, Domain, Resource: string);
var i: integer;
    s: string;
begin
  Username := Trim(Canonic);

  Account  := '';
  Domain   := '';
  Resource := '';

  if (Username = '') then Exit;

  s := Username;

  //get account
  i := Pos('@', s);
  if (i > 0) then
  begin
    Account := Copy(s, 1, i-1);
    Delete(s, 1, i);
  end
  else
  begin
    i := PosRight('/', s);
    if (i > 0) then
    begin
      Account := Copy(s, 1, i-1);
      Delete(s, 1, i);

      Resource := s;

      Username := Account;
    end;

    Exit;
  end;

  //get domain
  if (s = '') then Exit;
  i := Pos('/', s); //if canonical name with resource
  if (i > 0) then
  begin
    Domain := Copy(s, 1, i-1);
    Delete(s, 1, i);

    Resource := s;

    Username := Account + '@' + Domain;
  end
  else
    Domain := s;
end;

{*******************************************************************}
function CanonicName(Username, Resource: string): string;
begin
  if (Resource <> '') then
    Result := Username + '/' + Resource
  else
    Result := Username;
end;

{*******************************************************************}
function FormatSpecial(const S: string; Args: array of string): string;
var i, j: Integer;
    sTemp: string;
begin
  Result := S;
  if (Length(Args) = 0) then Exit;

  i := Pos('%', S);
  if (i = 0) then Exit;

  j      := 0;
  sTemp  := S;
  Result := '';

  while (i > 0) do
  begin
    Result := Result + Copy(sTemp, 1, i-1) + Args[j];
    Delete(sTemp, 1, i);

    Inc(j);
    if ((Length(Args)-1) < j) then
    begin
      Result := Result + sTemp;
      Break;
    end;

    i := Pos('%', sTemp);
  end;//while
end;

{*******************************************************************}
function XmlTag(const Tag: string; const Value: string = ''): string;
begin
  Result := Tag;
  if (Value = '') then Exit;
  Result := FormatSpecial(Tag, [Value]);
end;

{*******************************************************************}
function XmlTag(const Tag: string; const Value1, Value2: string): string;
begin
  Result := FormatSpecial(Tag, [Value1, Value2]);
end;

{*******************************************************************}
function XmlTag(const Tag: string; Args: array of string): string;
begin
  Result := FormatSpecial(Tag, Args);
end;

{*******************************************************************}
function XmlTagIq(const IqType, IqId, Value: string): string;
begin
  Result := FormatSpecial(XTAG_IQ, [IqType, IqId, Value]);
end;

{*******************************************************************}
function XmlTagIq(const IqType, IqFrom, IqTo, IqId: string; const Value: string = ''): string;
begin
  if (Value = '') then
    Result := FormatSpecial(XTAG_IQ_FROM_TO_EMPTY, [IqType, IqFrom, IqTo, IqId])
  else
    Result := FormatSpecial(XTAG_IQ_FROM_TO, [IqType, IqFrom, IqTo, IqId, Value]);
end;

{*******************************************************************}
function XmlTagIqTo(const IqType, IqTo, IqId, Value: string): string;
begin
  Result := FormatSpecial(XTAG_IQ_TO, [IqType, IqTo, IqId, Value]);
end;

{*******************************************************************}
function XmlTagIqQuery(const IqType, IqId, QueryNS: string; const QueryValue: string = ''): string;
begin
  if (QueryValue = '') then
    Result := XmlTag(XTAG_QUERY_EMPTY, QueryNS)
  else
    Result := XmlTag(XTAG_QUERY, QueryNS, QueryValue);

  Result := XmlTagIq(IqType, IqId, Result);
end;

{*******************************************************************}
function XmlTagIqQuery(const IqType, IqFrom, IqTo, IqId, QueryNS: string; const QueryValue: string = ''): string;
begin
  if (QueryValue = '') then
    Result := XmlTag(XTAG_QUERY_EMPTY, QueryNS)
  else
    Result := XmlTag(XTAG_QUERY, QueryNS, QueryValue);

  Result := FormatSpecial(XTAG_IQ_FROM_TO, [IqType, IqFrom, IqTo, IqId, Result]);
end;

{*******************************************************************}
function XmlAtt(const Attribute: string; const AttValue: string = ''): string;
begin
  Result := Attribute + '=''' + AttValue + '''';
end;

{*******************************************************************}
function MakeTag(const XnsVal: string; const TagVal: string = ''; const XnsAtts: string = ''): string;
begin
  Result := '';
  if (XnsVal = '') then Exit;
  Result := '<' + XnsVal;

  if (XnsAtts <> '') then
    Result := Result + THE_SPC + Trim(XnsAtts);

  if (TagVal = '') then
    Result := Result + '/>'
  else
    Result := Result + '>' + TagVal + '</' + XnsVal + '>';
end;

{*******************************************************************}
function Int64ToVer(Value: Int64): string;
var p1,p2,p3,p4: Word;
begin
  p1 := (Value shr 48) and $FFFF;
  p2 := (Value shr 32) and $FFFF;
  p3 := (Value shr 16) and $FFFF;
  p4 := Value and $FFFF;

  Result := IntToStr(p1) + THE_DOT + IntToStr(p2) + THE_DOT + IntToStr(p3) + THE_DOT + IntToStr(p4);
end;

{*******************************************************************}
function XmppHTMLEncode(const AStr: String): String;
const
  Convert = ['&','<','>','"'];
var
  Sp, Rp: PChar;
begin
  SetLength(Result, Length(AStr) * 10);
  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do
  begin
    case Sp^ of
      '&': begin
             FormatBuf(Rp, 5, '&amp;', 5, []);
             Inc(Rp,4);
           end;
      '<',
      '>': begin
             if Sp^ = '<' then
               FormatBuf(Rp, 4, '&lt;', 4, [])
             else
               FormatBuf(Rp, 4, '&gt;', 4, []);
             Inc(Rp,3);
           end;
      '"': begin
             FormatBuf(Rp, 6, '&quot;', 6, []);
             Inc(Rp,5);
           end;
    else
      Rp^ := Sp^
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

{*******************************************************************}
function VCardDateToUnix(s: string): Int64;
var iYear, iMonth, iDay: Word;
    sTmp: string;
    i: Integer;
begin
  Result := 0;
  s := Trim(s);
  if (s = '') then Exit;

  i := Pos('-', s);
  if (i = 0) then Exit;
  sTmp  := Copy(s, 1, i-1); Delete(s, 1, i);
  iYear := StrToIntDef(sTmp, 0);
  if (iYear = 0) then Exit;

  i := Pos('-', s);
  if (i = 0) then Exit;
  sTmp   := Copy(s, 1, i-1); Delete(s, 1, i);
  iMonth := StrToIntDef(sTmp, 0);
  if (iMonth = 0) or (iMonth > 12) then Exit;

  i := Pos('T', s);
  if (i > 0) then
    sTmp := Copy(s, 1, i-1)
  else
    sTmp := Copy(s, 1, Length(s));

  iDay := StrToIntDef(sTmp, 0);
  if (iDay = 0) or (iDay > 31) then Exit;

  try
    Result := DateTimeToUnix( EncodeDate(iYear, iMonth, iDay) );

    //null is unix start date 1 jan 1970, adding 1 second to make it real
    if (Result = 0) then Result := 1;
  except
  end;
end;

{*******************************************************************}
function UnixToVCardDate(const UDT: Int64): string;
var DT: TDateTime;
    iYear, iMonth, iDay: Word;
begin
  Result := '';
  if (UDT = 0) then Exit;

  iYear  := 0;
  iMonth := 0;
  iDay   := 0;

  try
    DT := UnixToDateTime(UDT);

    DecodeDate(DT, iYear, iMonth, iDay);
  except
    Exit;
  end;

  if (iYear = 0) or (iMonth = 0) or (iMonth > 12) or (iDay = 0) or (iDay > 31) then Exit;

  Result := IntToStr(iYear) + '-' + IntToStr(iMonth) + '-' + IntToStr(iDay);
end;

{*******************************************************************}
function Iso8601ToUnix(s: string): Int64;
var iYear, iMonth, iDay, iHour, iMin, iSec, iMsec, iBiasHour, iBiasMin: Word;
    sTmp, sTime: string;
    i, iBias: Integer;
    bPlus: Boolean;
begin
  Result := 0;
  s := Trim(s);
  if (s = '') then Exit;

  i := Pos('-', s);
  if (i = 0) then Exit;
  sTmp  := Copy(s, 1, i-1); Delete(s, 1, i);
  iYear := StrToIntDef(sTmp, 0);
  if (iYear = 0) then Exit;

  i := Pos('-', s);
  if (i = 0) then Exit;
  sTmp   := Copy(s, 1, i-1); Delete(s, 1, i);
  iMonth := StrToIntDef(sTmp, 0);
  if (iMonth = 0) or (iMonth > 12) then Exit;

  i := Pos('T', s);
  if (i > 0) then
  begin
    sTmp := Copy(s, 1, i-1);
    Delete(s, 1, i);
  end
  else
  begin
    sTmp := Copy(s, 1, Length(s));
    Delete(s, 1, Length(s));
  end;

  iDay := StrToIntDef(sTmp, 0);
  if (iDay = 0) or (iDay > 31) then Exit;

  sTime := '';
  iHour := 0;
  iMin  := 0;
  iSec  := 0;
  iMsec := 0;
  iBias := 0;

  if (s <> '') then
  begin
    i := Pos('Z', s);
    if (i > 0) then
    begin
      sTime := Copy(s, 1, i-1);
      Delete(s, 1, i-1);
    end
    else
    begin
      i := Pos('+', s);
      if (i > 0) then
      begin
        sTime := Copy(s, 1, i-1);
        Delete(s, 1, i-1);
      end
      else
      begin
        i := Pos('-', s);
        if (i > 0) then
        begin
          sTime := Copy(s, 1, i-1);
          Delete(s, 1, i-1);
        end;
      end;
    end;

    if (sTime = '') then Exit;

    i := Pos(':', sTime);
    if (i = 0) then Exit;
    sTmp  := Copy(sTime, 1, i-1); Delete(sTime, 1, i);
    iHour := StrToIntDef(sTmp, 0);
    if (iHour > 23) then Exit;

    i := Pos(':', sTime);
    if (i > 0) then
    begin
      sTmp := Copy(sTime, 1, i-1); Delete(sTime, 1, i);
      iMin := StrToIntDef(sTmp, 0);
      if (iMin > 59) then Exit;

      i := Pos('.', sTime);
      if (i > 0) then
      begin
        sTmp := Copy(sTime, 1, i-1); Delete(sTime, 1, i);
        iSec := StrToIntDef(sTmp, 0);
        if (iSec > 59) then Exit;
        iMsec := StrToIntDef(sTime, 0);
        if (iMsec > 999) then Exit;
      end
      else
      begin
        iSec := StrToIntDef(sTime, 0);
        if (iSec > 59) then Exit;
      end;
    end
    else
    begin
      iMin := StrToIntDef(sTime, 0);
      if (iMin > 59) then Exit;
    end;

    //TZD
    if (Length(s) > 0) then
    begin
      if (s[1] = '+') or (s[1] = '-') then
      begin
        bPlus := s[1] = '+';
        Delete(s, 1, 1);

        i := Pos(':', s);
        if (i = 0) then Exit;
        sTmp := Copy(s, 1, i-1); Delete(s, 1, i);
        iBiasHour := StrToIntDef(sTmp, 0);
        if (iBiasHour > 23) then Exit;

        iBiasMin := StrToIntDef(s, 0);
        if (iBiasMin > 59) then Exit;

        iBias := iBiasHour * SecsPerHour + iBiasMin * SecsPerMin;
        if bPlus then iBias := -iBias;
      end;
    end;
  end;

  try
    Result := DateTimeToUnix( EncodeDate(iYear, iMonth, iDay) + EncodeTime(iHour, iMin, iSec, iMsec) ) + iBias;
  except
  end;
end;

{*******************************************************************}
function PosRight(const substr, str: string): integer;
var i: integer;
    s: string;
begin
  Result := 0;
  if (substr = '') or (str = '') then Exit;

  for i := Length(str) downto 1 do
  begin
    if (str[i] = substr[1]) then
    begin
      s := Copy(str, i, Length(str) - i + 1);
      if (Pos(substr, s) > 0) then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;
end;


end.
