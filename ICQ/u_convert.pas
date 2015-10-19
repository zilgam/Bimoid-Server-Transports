// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_convert;

interface

uses Windows, Classes, SysUtils, Graphics, DateUtils, jpeg, pngimage, u_obimp_const, u_obimp_codes, u_ext_info, h_oscar, u_oscar_caps,
     u_oscar_item, u_oscar_cli, u_oscar_avatars, u_oscar_hlp, u_conv_codes;

const
  LI_IMD_LANG_COUNTRY   = 0;
  LI_INFORMATON         = 1;
  LI_TRANSPORT          = 2;
  LI_CL_OPER_FAILED     = 3;
  LI_LIMIT_REACHED      = 4;
  LI_PRIVACY_LEVEL      = 5;
  LI_LOW                = 6;
  LI_MEDIUM             = 7;
  LI_HIGH               = 8;
  LI_DEPRESSION         = 9;
  LI_EVIL               = 10;
  LI_DISABLE_MULTISES   = 11;
  LI_SHOW_ST_SEARCH     = 12;
  LI_MUST_AUTH_ON_ADD   = 13;
  LI_PRIVACY            = 14;
  LI_OPTIONS            = 15;
  LI_ALLOW_UNS_UPDATE   = 16;
  LI_UNSEARCHABLE       = 17;

  //client detection additional parameters:
  PARAM7_WIRELESS          = $0001;
  PARAM7_AIM               = $0002;
  PARAM7_MAIL_RU           = $0003;

  function  GetLI(ClientLang: Word; LI: Word): string;
  function  GetOptBool(const OptValue: WideString): Boolean;
  function  GetOptByte(const OptValue: WideString): Byte;

  function  ConvProtoConErrorCode(ProtoCode: DWord): Word;
  procedure ConvObimpStatus(ObimpStatus, StPicID: DWord; var ProtoStatus: DWord; var ProtoPrivStatus: Byte; var ProtoMoodNum: Integer; var ProtoMoodName: string; var ProtoAddCaps: RawByteString);
  procedure ConvProtoStatus(const NickInfo: TNickInfo; var cs: TExtContactStatus; var StatusName: string; const ClientLang: Word);
  function  ConvProtoClientType(OscarNickFlags: Word): Word;
  procedure ConvProtoBuddyOnline(oItem: pOscarItem; var cs: TExtContactStatus; const ClientLang: Word);
  procedure ConvProtoBuddyOffline(oItem: pOscarItem; var cs: TExtContactStatus);
  procedure ConvProtoIncMsg(Client: TOscarClient; const IcbmMsg: TIcbmIncMsg; var ExtMsg: TExtMsg);
  function  ConvProtoMsgAck(Client: TOscarClient; const IcbmAck: TIcbmAck; var MsgAck: TExtMsgAck): Boolean;
  procedure ConvProtoEventMsg(const IcbmEvent: TIcbmEvent; var ExtNotif: TExtNotif);
  procedure ConvObimpOutMsg(Client: TOscarClient; InstantMsg: pExtMsg; var IcbmMsg: TIcbmOutMsg);
  function  ConvObimpMsgAck(Client: TOscarClient; MsgAck: pExtMsgAck; var IcbmEvent: TIcbmEvent): Boolean;
  procedure ConvObimpNotif(ExtNotif: pExtNotif; var IcbmEvent: TIcbmEvent);
  procedure ConvObimpAuthRequest(AuthReq: pExtAuthMsg; var oAuthMsg: TOscarAuthMsg);
  procedure ConvObimpAuthReply(AuthReq: pExtAuthMsg; var oAuthMsg: TOscarAuthMsg);
  procedure ConvProtoAuthRequest(const AuthReq: TOscarAuthMsg; var ExtAuthMsg: TExtAuthMsg; const OfflineMsg: Boolean);
  procedure ConvProtoAuthReply(const AuthResp: TOscarAuthMsg; var ExtAuthMsg: TExtAuthMsg; const OfflineMsg: Boolean);
  procedure ConvProtoPresInfo(Client: TOscarClient; ReqID: DWord; var opi: TExtOwnPresenceInfo; const ClientLang: Word);
  function  ConvProtoImdReply(const ImdReply: TImdReply; var ExtUserDets: TExtUserDets; var DetsResCode: Word): Boolean;
  procedure ConvProtoSearchReply(const ImdReply: TImdReply; ResIndex: Integer; var ExtSrchReply: TExtSrchReply; var SrchResCode: Word);
  procedure ConvProtoImdInfo(ImdInfo: pImdInfo; var ExtUserDets: TExtUserDets; const ObimpReqID: DWord);
  function  ConvProtoImdUpdReply(const ImdUpdReply: TImdReply; var OwnDetsUpdReply: TExtOwnDetsUpdReply): Boolean;
  function  ConvProtoGender(ProtoGender: DWord): Byte;
  function  ConvObimpGender(ObimpGender: Byte): DWord;
  function  ConvProtoBirthday(ProtoBirth: DWord): Int64;
  function  ConvObimpBirthday(ObimpBirth: Int64): DWord;
  function  ConvProtoInterests(ImdInfo: pImdInfo): string;
  procedure ConvObimpInterests(ImdInfo: pImdInfo; ObimpInts: string);
  function  ConvProtoPhone(ProtoPhone: string): string;
  function  ConvObimpPhone(ObimpPhone: string): string;
  function  ConvObimpDetsUpdate(Client: TOscarClient; ExtUserDets: pExtUserDets; var ImdInfo: TImdInfo): Boolean;
  function  ConvObimpPngToJpeg(ms: TMemoryStream): Word;
  procedure ConvProtoAvatarUploadRelpy(const BartReplyCode: Byte; const Bart: TOscarBart; const AvatarReqID: DWord; var oasr: TExtOwnAvatarSetReply);
  function  ConvObimpSearch(ExtSrchReq: pExtSrchReq; var ImdSrch: TImdSrchReq): Boolean;

implementation

const
  LANG_MAX = 17;

  LangEN: array[0..LANG_MAX] of string = (
  'en-US',
  'Information',
  'Transport',
  'Contact list operation failed.',
  'Sorry, you have reached the limit of contact list items for this type.',
  'Privacy level',
  'Low',
  'Medium',
  'High',
  'Depression',
  'Evil',
  'Disable multisession support (reconnect required)',
  'Show my status in the users search',
  'Users must request authorization from me before adding',
  'Privacy',
  'Options',
  'Allow details/privacy update (for unsearchable account)',
  'Possibly your account has no user details. To update details enable it''s option in the transport privacy settings.'
  );

  LangRU: array[0..LANG_MAX] of string = (
  'ru-RU',
  'Информация',
  'Транспорт',
  'Не удалось выполнить операцию со списком контактов.',
  'Извините, достигнут лимит элементов списка контактов для этого типа.',
  'Уровень приватности',
  'Низкий',
  'Средний',
  'Высокий',
  'Депрессия',
  'Злой',
  'Отключить поддержку мультисессий (необходимо переподключиться)',
  'Показывать состояние моего статуса в поиске',
  'Пользователи должны запрашивать у меня авторизацию перед добавлением',
  'Приватность',
  'Опции',
  'Разрешить обновление данных/приватности (для номера без анкетных данных)',
  'Возможно, ваш номер не имеет анкетных данных. Чтобы обновить данные, включите опцию в настройках приватности транспорта.'
  );

{***************************************************************}
function GetLI(ClientLang: Word; LI: Word): string;
begin
  Result := '';
  if (LI > LANG_MAX) then Exit;

  case ClientLang of
   {====================================================}
    LANGUAGE_CODE_RUSSIAN: Result := LangRU[LI];
   {====================================================}
    else
      Result := LangEN[LI];
   {====================================================}
  end;//case
end;

{***************************************************************}
function GetOptBool(const OptValue: WideString): Boolean;
begin
  Result := StrToIntDef(OptValue, 0) > 0;
end;

{***************************************************************}
function GetOptByte(const OptValue: WideString): Byte;
begin
  Result := StrToIntDef(OptValue, 0);
end;

{***************************************************************}
function ConvProtoConErrorCode(ProtoCode: DWord): Word;
begin
  case ProtoCode of
   {===================================================}
    0: //connection failed
      begin
        Result := TP_STATE_CON_FAILED;
      end;
   {===================================================}
    BUCP_ERR_INVALID_ACCT,
    BUCP_ERR_DELETED_ACCT,
    BUCP_ERR_EXPIRED_ACCT,
    BUCP_ERR_SUSPENDED_ACCT:
      begin
        Result := TP_STATE_ACCOUNT_INVALID;
      end;
   {===================================================}
    BUCP_ERR_INVALID_NICK_OR_PASSW,
    BUCP_ERR_INCORR_NICK_OR_PASSWORD,
    BUCP_ERR_MISMATCH_NICK_OR_PASSWD:
      begin
        Result := TP_STATE_WRONG_PASSWORD;
      end;
   {===================================================}
    BUCP_ERR_RESERVATION_RATE,
    BUCP_ERR_RATE_LIMITED:
      begin
        Result := TP_STATE_TOO_MANY_TRIES_TRY_LATER;
      end;
   {===================================================}
    BUCP_ERR_ANOTHER_PLACE_LOGIN:
      begin
        Result := TP_STATE_OTHER_PLACE_LOGIN;
      end;
   {===================================================}
    else
      Result := TP_STATE_SERVICE_TEMP_UNAVAILABLE;
   {===================================================}
  end;//case
end;

{***************************************************************}
function ConvObimpStPicToXstCap(StPicID: DWord): RawByteString;
begin
  //this function is used only to support old clients
  Result := '';

  if (StPicID > 0) then
  begin
    //convert to cap index
    Dec(StPicID);

    if ((Length(OscCapsMoods)-1) >= StPicID) then
      Result := tb(OscCapsMoods[StPicID].UUID);
  end;
end;

{***************************************************************}
procedure ConvObimpStatus(ObimpStatus, StPicID: DWord; var ProtoStatus: DWord; var ProtoPrivStatus: Byte;
                          var ProtoMoodNum: Integer; var ProtoMoodName: string; var ProtoAddCaps: RawByteString);
begin
  ProtoStatus      := NI_STATUS_ONLINE;
  ProtoPrivStatus  := FB_PD_MODE_DENY_INVIS;
  ProtoMoodNum     := -1;
  ProtoMoodName    := '';
  ProtoAddCaps     := ConvObimpStPicToXstCap(StPicID);

  case ObimpStatus of
   {===================================================}
    PRES_STATUS_ONLINE             : ProtoStatus := NI_STATUS_ONLINE;
   {===================================================}
    PRES_STATUS_INVISIBLE          :
      begin
        ProtoStatus     := NI_STATUS_INVISIBLE;
        ProtoPrivStatus := FB_PD_MODE_PERMIT_VIS;
      end;
   {===================================================}
    PRES_STATUS_INVISIBLE_FOR_ALL  :
      begin
        ProtoStatus     := NI_STATUS_INVISIBLE;
        ProtoPrivStatus := FB_PD_MODE_DENY_ALL;
      end;
   {===================================================}
    PRES_STATUS_FREE_FOR_CHAT      :
      begin
        ProtoAddCaps  := ProtoAddCaps + tb(OscCapsUnoff[CAP_UNOFF_ICQ_ST_FFC].UUID);
        ProtoMoodNum  := MOOD_FFC;
        ProtoMoodName := 'Free For Chat';
      end;
   {===================================================}
    PRES_STATUS_AT_HOME            :
      begin
        ProtoAddCaps  := ProtoAddCaps + tb(OscCapsUnoff[CAP_UNOFF_ICQ_ST_ATHOME].UUID);
        ProtoMoodNum  := MOOD_ATHOME;
        ProtoMoodName := '@Home';
      end;
   {===================================================}
    PRES_STATUS_AT_WORK            :
      begin
        ProtoAddCaps  := ProtoAddCaps + tb(OscCapsUnoff[CAP_UNOFF_ICQ_ST_ATWORK].UUID);
        ProtoMoodNum  := MOOD_ATWORK;
        ProtoMoodName := '@Work';
      end;
   {===================================================}
    PRES_STATUS_LUNCH              :
      begin
        ProtoAddCaps  := ProtoAddCaps + tb(OscCapsUnoff[CAP_UNOFF_ICQ_ST_LUNCH].UUID);
        ProtoMoodNum  := MOOD_LUNCH;
        ProtoMoodName := 'Lunch';
      end;
   {===================================================}
    PRES_STATUS_AWAY               : ProtoStatus := NI_STATUS_AWAY;
   {===================================================}
    PRES_STATUS_NOT_AVAILABLE      : ProtoStatus := NI_STATUS_NA;
   {===================================================}
    PRES_STATUS_OCCUPIED           : ProtoStatus := NI_STATUS_BUSY;
   {===================================================}
    PRES_STATUS_DO_NOT_DISTURB     : ProtoStatus := NI_STATUS_DND;
   {===================================================}
  end;//case
end;

{***************************************************************}
procedure ConvProtoStatus(const NickInfo: TNickInfo; var cs: TExtContactStatus; var StatusName: string; const ClientLang: Word);
begin
  cs.Status  := PRES_STATUS_ONLINE;
  StatusName := '';

  if ( (NickInfo.Status and NI_STATUS_INVISIBLE) = NI_STATUS_INVISIBLE ) then
    cs.Status := PRES_STATUS_INVISIBLE
  else
  if ( (NickInfo.Status and NI_STATUS_BUSY) = NI_STATUS_BUSY ) then
    cs.Status := PRES_STATUS_OCCUPIED
  else
  if ( (NickInfo.Status and NI_STATUS_NA) = NI_STATUS_NA ) then
    cs.Status := PRES_STATUS_NOT_AVAILABLE
  else
  if ( (NickInfo.Status and NI_STATUS_DND) = NI_STATUS_DND ) then
    cs.Status := PRES_STATUS_DO_NOT_DISTURB
  else
  if ( (NickInfo.Status and NI_STATUS_AWAY) = NI_STATUS_AWAY ) then
    cs.Status := PRES_STATUS_AWAY;

  //check for AIM away nickinfo flag
  if (cs.Status = PRES_STATUS_ONLINE) and ( (NickInfo.NickFlags and NI_FLAG_UNAVAILABLE) = NI_FLAG_UNAVAILABLE ) then
    cs.Status := PRES_STATUS_AWAY;

  //check for AIM/MailRu
  if not IsUin(cs.Account) then
  begin
    if (Pos('@', cs.Account) > 0) then
    begin
      case cs.Status of
        PRES_STATUS_ONLINE : cs.CustStatusPicID := CUST_ST_PIC_MAILRU_ONLINE;
        PRES_STATUS_AWAY   : cs.CustStatusPicID := CUST_ST_PIC_MAILRU_AWAY;
      end;//case
    end
    else
    begin
      case cs.Status of
        PRES_STATUS_ONLINE : cs.CustStatusPicID := CUST_ST_PIC_AIM_ONLINE;
        PRES_STATUS_AWAY   : cs.CustStatusPicID := CUST_ST_PIC_AIM_AWAY;
      end;//case
    end;
  end;

  //check unofficial status caps
  if HasOscarCap(OscCapsUnoff[CAP_UNOFF_ICQ_ST_FFC].UUID, NickInfo.CapsFull) then
    cs.Status := PRES_STATUS_FREE_FOR_CHAT
  else
  if HasOscarCap(OscCapsUnoff[CAP_UNOFF_ICQ_ST_ATHOME].UUID, NickInfo.CapsFull) then
    cs.Status := PRES_STATUS_AT_HOME
  else
  if HasOscarCap(OscCapsUnoff[CAP_UNOFF_ICQ_ST_ATWORK].UUID, NickInfo.CapsFull) then
    cs.Status := PRES_STATUS_AT_WORK
  else
  if HasOscarCap(OscCapsUnoff[CAP_UNOFF_ICQ_ST_LUNCH].UUID, NickInfo.CapsFull) then
    cs.Status := PRES_STATUS_LUNCH;

  //convert special mood number to status
  if (cs.Status = PRES_STATUS_ONLINE) then
  begin
    case NickInfo.Barts.MoodNum of
     {=======================================}
      MOOD_FFC    :
        begin
          cs.Status := PRES_STATUS_FREE_FOR_CHAT;
          if (NickInfo.Barts.MoodStr <> '') then StatusName := NickInfo.Barts.MoodStr;
        end;
     {=======================================}
      MOOD_ATHOME :
        begin
          cs.Status := PRES_STATUS_AT_HOME;
          if (NickInfo.Barts.MoodStr <> '') then StatusName := NickInfo.Barts.MoodStr;
        end;
     {=======================================}
      MOOD_ATWORK :
        begin
          cs.Status := PRES_STATUS_AT_WORK;
          if (NickInfo.Barts.MoodStr <> '') then StatusName := NickInfo.Barts.MoodStr;
        end;
     {=======================================}
      MOOD_LUNCH  :
        begin
          cs.Status := PRES_STATUS_LUNCH;
          if (NickInfo.Barts.MoodStr <> '') then StatusName := NickInfo.Barts.MoodStr;
        end;
     {=======================================}
    end;//case
  end;

  //convert custom status pics if available
  if (cs.Status = PRES_STATUS_ONLINE) then
  begin
    if HasOscarCap(OscCapsUnoff[CAP_UNOFF_ICQ_ST_DEPRES].UUID, NickInfo.CapsFull) then
    begin
      StatusName         := GetLI(ClientLang, LI_DEPRESSION);
      cs.CustStatusPicID := CUST_ST_PIC_DEPRES;
    end
    else
    if HasOscarCap(OscCapsUnoff[CAP_UNOFF_ICQ_ST_EVIL].UUID, NickInfo.CapsFull) then
    begin
      StatusName         := GetLI(ClientLang, LI_EVIL);
      cs.CustStatusPicID := CUST_ST_PIC_EVIL;
    end;
  end;

  //if not online or away, then clear custom status pic
  if (cs.Status <> PRES_STATUS_ONLINE) and (cs.Status <> PRES_STATUS_AWAY) and (cs.CustStatusPicID > 0) then
    cs.CustStatusPicID := 0;
end;

{***************************************************************}
function ConvProtoClientType(OscarNickFlags: Word): Word;
begin
  Result := CLIENT_TYPE_USER;

  if ( (OscarNickFlags and NI_FLAG_OFFICIAL) = NI_FLAG_OFFICIAL ) then
    Result := CLIENT_TYPE_SERVICE
  else
  if ( (OscarNickFlags and NI_FLAG_BOT) = NI_FLAG_BOT ) or
     ( (OscarNickFlags and NI_FLAG_BEAST) = NI_FLAG_BEAST ) then
    Result := CLIENT_TYPE_BOT;
end;

{***************************************************************}
function GetOscarClientDesc(oItem: pOscarItem): string;
begin
  //Bimoid messenger supports some special tag "L" that will convert language code of "L"
  //into current client's language string instead of tag content. If messenger will not
  //find that language code in tag, it will add tag content.
  //Only one "L" tag per line allowed, and tag has to be at the line's start
  //else it will not be parsed.
  //
  //Bimoid IM langauge codes can be found at the http://forum.bimoid.com

  Result := '<L=604>WL</L>: ' + IntToStr(oItem^.NickInfo.WarnLevel) + '%' + CRLF +
            '<L=605>Flags</L>: ' + GetNickFlagsDesc(oItem^.NickInfo.NickFlags);
end;

{***************************************************************}
procedure ConvProtoBuddyOnline(oItem: pOscarItem; var cs: TExtContactStatus; const ClientLang: Word);
var sStName: string;
    bUin: Boolean;
begin
  //##############################################################################
  //#### If changed anything here, then should recheck IsNickInfoChanged function
  //##############################################################################

  Finalize(cs);
  ZeroMemory(@cs, SizeOf(TExtContactStatus));

  cs.Account := oItem^.Name;

  //get obimp status
  ConvProtoStatus(oItem^.NickInfo, cs, sStName, ClientLang);

  //set status name if available
  if (oItem^.NickInfo.Barts.StatusStr <> '') then
    cs.StatusName := oItem^.NickInfo.Barts.StatusStr
  else
    cs.StatusName := sStName;

  //==== additional status pic
  //1. mood num has priority
  cs.AdditStPicNum := MoodNumToStPic(oItem^.NickInfo.Barts.MoodNum);
  if (cs.AdditStPicNum > 0) then
  begin
    cs.AdditStPicDesc := Trim(oItem^.NickInfo.Barts.MoodStr);
    //if used StatusStr to show mood string then change strings by places
    if (cs.AdditStPicDesc = '') and (oItem^.NickInfo.Barts.StatusStr <> '') then
    begin
      cs.AdditStPicDesc := oItem^.NickInfo.Barts.StatusStr;
      cs.StatusName     := '';
    end;
  end
  else
    cs.AdditStPicDesc := '';

  //2. get any xstatus cap if available
  if (cs.AdditStPicNum = 0) then
  begin
    cs.AdditStPicNum := FindXstCap(oItem^.NickInfo.CapsFull);
    if (cs.AdditStPicNum > 0) then
    begin
      if (cs.AdditStPicDesc = '') and (oItem^.NickInfo.Barts.StatusStr <> '') then
      begin
        cs.AdditStPicDesc := oItem^.NickInfo.Barts.StatusStr;
        cs.StatusName     := '';
      end;
    end;
  end;
  //===========================

  //can get client type through nick flags
  cs.ClientType     := ConvProtoClientType(oItem^.NickInfo.NickFlags);

  //client identification done on messenger side
  //check for AIM/MailRu
  bUin := IsUin(cs.Account);
  if not bUin then
  begin
    if (Pos('@', cs.Account) > 0) then
      cs.ClientName := 'Mail.ru'
    else
      cs.ClientName := 'AIM';
  end
  else
    cs.ClientName := 'ICQ';

  cs.CliVerMajor    := 0;
  cs.CliVerMinor    := 0;
  cs.CliVerRelease  := 0;
  cs.CliVerBuild    := 0;

  //there can be IP
  if (oItem^.NickInfo.RealIP > 0) then
    cs.ClientIP := IntToIP(oItem^.NickInfo.RealIP);

  //no OS in oscar
  cs.ClientOS := '';

  //client description
  cs.ClientDesc := GetOscarClientDesc(oItem);

  //date/times, in OSCAR UTC already
  cs.OnlineSince := oItem^.NickInfo.SignonTime;
  cs.RegDate     := oItem^.NickInfo.MemberSince;

  //avatar
  cs.AvatarHashHex := RawToHex(oItem^.NickInfo.Barts.AvatarHash);
  if (cs.AvatarHashHex = BART_NOAVATAR_HASH_HEX) then
    cs.AvatarHashHex := '';

  //client identification sTLDs to identify client on the messenger side
  if bUin or (oItem^.NickInfo.DcInfo.ConVer > 0) then
    cs.ClientIdBlk := cs.ClientIdBlk + WideString( TLV($0001, oItem^.NickInfo.DcInfo.ConVer) );

  cs.ClientIdBlk := cs.ClientIdBlk +
                    WideString( TLV($0002, oItem^.NickInfo.CapsFull) ) +
                    WideString( TLV($0003, oItem^.NickInfo.CapsShort) );

  if bUin or (oItem^.NickInfo.DcInfo.Time1 > 0) or (oItem^.NickInfo.DcInfo.Time2 > 0) or (oItem^.NickInfo.DcInfo.Time3 > 0) then
    cs.ClientIdBlk := cs.ClientIdBlk +
                      WideString( TLV($0004, tb(oItem^.NickInfo.DcInfo.Time1)) +
                                  TLV($0005, tb(oItem^.NickInfo.DcInfo.Time2)) +
                                  TLV($0006, tb(oItem^.NickInfo.DcInfo.Time3)) );

  if bUin then
  begin
    if ((oItem^.NickInfo.NickFlags and NI_FLAG_WIRELESS) = NI_FLAG_WIRELESS) then
      cs.ClientIdBlk := cs.ClientIdBlk +
                        WideString( TLV($0007, tb(Word(PARAM7_WIRELESS))) )
    else
      cs.ClientIdBlk := cs.ClientIdBlk +
                        WideString( TLV($0007, tb(Word($0000))) );
  end
  else
  begin
    if (Pos('@', cs.Account) > 0) then
      cs.ClientIdBlk := cs.ClientIdBlk +
                        WideString( TLV($0007, tb(Word(PARAM7_MAIL_RU))) )
    else
      cs.ClientIdBlk := cs.ClientIdBlk +
                        WideString( TLV($0007, tb(Word(PARAM7_AIM))) );
  end;

  //##############################################################################
  //#### If changed anything here, then should recheck IsNickInfoChanged function
  //##############################################################################
end;

{***************************************************************}
procedure ConvProtoBuddyOffline(oItem: pOscarItem; var cs: TExtContactStatus);
begin
  Finalize(cs);
  ZeroMemory(@cs, SizeOf(TExtContactStatus));

  cs.Account := oItem^.Name;
end;

{***************************************************************}
procedure ConvProtoIncMsg(Client: TOscarClient; const IcbmMsg: TIcbmIncMsg; var ExtMsg: TExtMsg);
begin
  Finalize(ExtMsg);
  ZeroMemory(@ExtMsg, SizeOf(TExtMsg));

  ExtMsg.Account       := IcbmMsg.Account;
  ExtMsg.MsgText       := IcbmMsg.MsgText;
  ExtMsg.AckRequired   := True;
  ExtMsg.IsOfflineMsg  := IcbmMsg.IsOffline;
  ExtMsg.OfflineMsgDT  := IcbmMsg.OffMsgTime;

  //save cookie only if we can send msg alternative acks
  if (IcbmMsg.MsgCookie > 0) and Client.CanSendAlterAck(IcbmMsg.Account) then
    ExtMsg.UniqID := Client.CookieMan.AddOscarCookie(IcbmMsg.Account, IcbmMsg.MsgCookie);

  //we have to check that cookie value is not null, else server will not accept it
  if (ExtMsg.UniqID = 0) then
    ExtMsg.UniqID := 1;
end;

{***************************************************************}
function ConvProtoMsgAck(Client: TOscarClient; const IcbmAck: TIcbmAck; var MsgAck: TExtMsgAck): Boolean;
var aCookie: pHelpCookie;
begin
  Result := False;

  aCookie := Client.CookieMan.GetCookieByOscar(IcbmAck.Account, IcbmAck.MsgCookie);
  if (aCookie = nil) then Exit;

  Finalize(MsgAck);
  ZeroMemory(@MsgAck, SizeOf(TExtMsgAck));

  MsgAck.Account := IcbmAck.Account;
  MsgAck.UniqID  := aCookie^.ObimpCookie;

  Result := True;

  //del cookie
  Client.CookieMan.DelCookieByOscar(IcbmAck.Account, IcbmAck.MsgCookie);
end;

{***************************************************************}
procedure ConvProtoEventMsg(const IcbmEvent: TIcbmEvent; var ExtNotif: TExtNotif);
begin
  Finalize(ExtNotif);
  ZeroMemory(@ExtNotif, SizeOf(TExtNotif));

  ExtNotif.Account    := IcbmEvent.Account;
  ExtNotif.NotifType  := NOTIF_TYPE_USER_TYPING;

  if (IcbmEvent.EventCode = ICBM_EVENT_TYPING) then
    ExtNotif.NotifValue := NOTIF_VALUE_USER_TYPING_START
  else
    ExtNotif.NotifValue := NOTIF_VALUE_USER_TYPING_FINISH;
end;

{***************************************************************}
procedure ConvObimpOutMsg(Client: TOscarClient; InstantMsg: pExtMsg; var IcbmMsg: TIcbmOutMsg);
begin
  Finalize(IcbmMsg);
  ZeroMemory(@IcbmMsg, SizeOf(TIcbmOutMsg));

  IcbmMsg.MsgCookie  := Client.CookieMan.AddObimpCookie(InstantMsg^.Account, InstantMsg^.UniqID);
  IcbmMsg.MsgChannel := 1;
  IcbmMsg.Account    := InstantMsg^.Account;
  IcbmMsg.MsgText    := InstantMsg^.MsgText;

  //get server ack and store in case if buddy offline
  IcbmMsg.ReqHostAck := True;
  IcbmMsg.Store      := True;
end;

{***************************************************************}
function ConvObimpMsgAck(Client: TOscarClient; MsgAck: pExtMsgAck; var IcbmEvent: TIcbmEvent): Boolean;
var aCookie: pHelpCookie;
begin
  Result := False;

  aCookie := Client.CookieMan.GetCookieByObimp(MsgAck^.Account, MsgAck^.UniqID);
  if (aCookie = nil) then Exit;

  Finalize(IcbmEvent);
  ZeroMemory(@IcbmEvent, SizeOf(TIcbmEvent));

  IcbmEvent.MsgCookie  := aCookie^.OscarCookie;
  IcbmEvent.MsgChannel := 1;
  IcbmEvent.Account    := MsgAck^.Account;
  IcbmEvent.EventCode  := ICBM_EVENT_NONE;

  Result := True;

  //del cookie
  Client.CookieMan.DelCookieByObimp(MsgAck^.Account, MsgAck^.UniqID);
end;

{***************************************************************}
procedure ConvObimpNotif(ExtNotif: pExtNotif; var IcbmEvent: TIcbmEvent);
begin
  Finalize(IcbmEvent);
  ZeroMemory(@IcbmEvent, SizeOf(TIcbmEvent));

  //cookies not used in OSCAR for typing notifs
  IcbmEvent.MsgCookie  := 0;
  IcbmEvent.MsgChannel := 1;
  IcbmEvent.Account    := ExtNotif^.Account;

  if (ExtNotif^.NotifValue = NOTIF_VALUE_USER_TYPING_START) then
    IcbmEvent.EventCode := ICBM_EVENT_TYPING
  else
    IcbmEvent.EventCode := ICBM_EVENT_NONE;
end;

{***************************************************************}
procedure ConvObimpAuthRequest(AuthReq: pExtAuthMsg; var oAuthMsg: TOscarAuthMsg);
begin
  Finalize(oAuthMsg);
  ZeroMemory(@oAuthMsg, SizeOf(TOscarAuthMsg));

  oAuthMsg.Account    := AuthReq^.Account;
  oAuthMsg.ReasonText := AuthReq^.MsgText;
end;

{***************************************************************}
procedure ConvObimpAuthReply(AuthReq: pExtAuthMsg; var oAuthMsg: TOscarAuthMsg);
begin
  Finalize(oAuthMsg);
  ZeroMemory(@oAuthMsg, SizeOf(TOscarAuthMsg));

  oAuthMsg.Account := AuthReq^.Account;

  case AuthReq^.AuthReplyCode of
    AUTH_REPLY_GRANTED  : oAuthMsg.Code := FB_AUTH_RESPOND_GRANT;
    AUTH_REPLY_DENIED   : oAuthMsg.Code := FB_AUTH_RESPOND_DENY;
  end;//case
end;

{***************************************************************}
procedure ConvProtoAuthRequest(const AuthReq: TOscarAuthMsg; var ExtAuthMsg: TExtAuthMsg; const OfflineMsg: Boolean);
begin
  Finalize(ExtAuthMsg);
  ZeroMemory(@ExtAuthMsg, SizeOf(TExtAuthMsg));

  ExtAuthMsg.Account      := AuthReq.Account;
  ExtAuthMsg.MsgText      := AuthReq.ReasonText;
  ExtAuthMsg.IsOfflineMsg := OfflineMsg;
  ExtAuthMsg.OfflineMsgDT := DateTimeToUnix(SysNow); //offline auth requests doesnt have times
end;

{***************************************************************}
procedure ConvProtoAuthReply(const AuthResp: TOscarAuthMsg; var ExtAuthMsg: TExtAuthMsg; const OfflineMsg: Boolean);
begin
  Finalize(ExtAuthMsg);
  ZeroMemory(@ExtAuthMsg, SizeOf(TExtAuthMsg));

  ExtAuthMsg.Account      := AuthResp.Account;
  ExtAuthMsg.IsOfflineMsg := OfflineMsg;
  ExtAuthMsg.OfflineMsgDT := DateTimeToUnix(SysNow);  //offline auth reply doesnt have times

  //just in case
  ExtAuthMsg.AuthReplyCode := AUTH_REPLY_DENIED;

  case AuthResp.Code of
    FB_AUTH_RESPOND_DENY   : ExtAuthMsg.AuthReplyCode := AUTH_REPLY_DENIED;
    FB_AUTH_RESPOND_GRANT  : ExtAuthMsg.AuthReplyCode := AUTH_REPLY_GRANTED;
  end;//case
end;

{***************************************************************}
procedure ConvProtoPresInfo(Client: TOscarClient; ReqID: DWord; var opi: TExtOwnPresenceInfo; const ClientLang: Word);
begin
  //##############################################################################
  //#### If changed anything here, then should recheck IsPresInfoChanged function
  //##############################################################################

  Finalize(opi);
  ZeroMemory(@opi, SizeOf(TExtOwnPresenceInfo));

  opi.ReqID          := ReqID;
  opi.RegDate        := Client.OscarInfo.NickInfo.MemberSince;
  opi.OnlineSince    := Client.OscarInfo.NickInfo.SignonTime;
  opi.PrevOnline     := 0;
  opi.CurSessionIP   := ''; //will added to srv added descr
  opi.PrevSessionIP  := '';
  opi.CurInstCount   := Client.OscarInfo.InstanceCount;

  //srv added description
  opi.SrvAddedDescr  := '';

  //IPs
  if (Client.OscarInfo.NickInfo.RealIP > 0) or (Client.OscarInfo.OtherInstIPs <> '') then
  begin
    opi.SrvAddedDescr := opi.SrvAddedDescr + '<L=613>IPs</L>:' + CRLF;

    if (Client.OscarInfo.NickInfo.RealIP > 0) then
      opi.SrvAddedDescr  := opi.SrvAddedDescr + IntToIP(Client.OscarInfo.NickInfo.RealIP) + CRLF;

    if (Client.OscarInfo.OtherInstIPs <> '') then
      opi.SrvAddedDescr  := opi.SrvAddedDescr + Client.OscarInfo.OtherInstIPs + CRLF;

    opi.SrvAddedDescr  := opi.SrvAddedDescr + CRLF;
  end;

  //if imd info rcvd
  if (Client.OscarInfo.ImdInfo.Account <> '') and
    (Client.OscarInfo.ImdInfo.ValidEmail <> '') and (Client.OscarInfo.ImdInfo.PendingEmail <> '') then
  begin
    if (Client.OscarInfo.ImdInfo.ValidEmail <> '') then
      opi.SrvAddedDescr := opi.SrvAddedDescr + '<L=0>E-mail 1</L>: ' + Client.OscarInfo.ImdInfo.ValidEmail + CRLF;

    if (Client.OscarInfo.ImdInfo.PendingEmail <> '') then
      opi.SrvAddedDescr := opi.SrvAddedDescr + '<L=0>E-mail 2</L>: ' + Client.OscarInfo.ImdInfo.PendingEmail + CRLF;

    opi.SrvAddedDescr  := opi.SrvAddedDescr + CRLF;
  end;

  opi.SrvAddedDescr  := opi.SrvAddedDescr +
                        '<L=604>Warn</L>: ' + IntToStr(Client.OscarInfo.NickInfo.WarnLevel) + '%' + CRLF +
                        '<L=605>Flags</L>: ' + GetNickFlagsDesc(Client.OscarInfo.NickInfo.NickFlags);

  //##############################################################################
  //#### If changed anything here, then should recheck IsPresInfoChanged function
  //##############################################################################
end;

{***************************************************************}
function ConvProtoGender(ProtoGender: DWord): Byte;
begin
  Result := 0;

  case ProtoGender of
    IMD_GENDER_MALE   : Result := GENDER_MALE;
    IMD_GENDER_FEMALE : Result := GENDER_FEMALE;
  end;//case
end;

{***************************************************************}
function ConvObimpGender(ObimpGender: Byte): DWord;
begin
  Result := IMD_GENDER_NONE;

  case ObimpGender of
    GENDER_MALE   : Result := IMD_GENDER_MALE;
    GENDER_FEMALE : Result := IMD_GENDER_FEMALE;
  end;//case
end;

{***************************************************************}
function ConvProtoBirthday(ProtoBirth: DWord): Int64;
begin
  if (ProtoBirth = $80000000) then
    Result := 0
  else
    if (ProtoBirth = 0) then
      Result := 1
    else
      Result := ProtoBirth;
end;

{***************************************************************}
function ConvObimpBirthday(ObimpBirth: Int64): DWord;
begin
  if (ObimpBirth = 0) then
    Result := $80000000
  else
    if (ObimpBirth = 1) then
      Result := 0
    else
      Result := ObimpBirth;
end;

{***************************************************************}
function ConvProtoInterests(ImdInfo: pImdInfo): string;
begin
  Result := '';

  if (ImdInfo^.InterestCode1 > 0) and (Trim(ImdInfo^.InterestText1) <> '') then
    Result := Result + ImdInfo^.InterestText1;
end;

{***************************************************************}
procedure ConvObimpInterests(ImdInfo: pImdInfo; ObimpInts: string);
begin
  ObimpInts := Trim(ObimpInts);
  if (ObimpInts = '') then
  begin
    ImdInfo.InterestText1 := '';
    ImdInfo.InterestCode1 := 0;
  end
  else
  begin
    ImdInfo.InterestCode1 := $0000006C; //hobbies
    ImdInfo.InterestText1 := ObimpInts;
  end;
end;

{***************************************************************}
function ConvProtoPhone(ProtoPhone: string): string;
begin
  Result := '';

  ProtoPhone := Trim(ProtoPhone);

  if (ProtoPhone <> '') then
  begin
    if (ProtoPhone[1] <> '+') then
      Result := '+' + ProtoPhone
    else
      Result := ProtoPhone;
  end;
end;

{***************************************************************}
function ConvObimpPhone(ObimpPhone: string): string;
begin
  Result := '';

  ObimpPhone := Trim(ObimpPhone);

  if (ObimpPhone <> '') then
    Result := StringReplace(ObimpPhone, '+', '', [rfReplaceAll, rfIgnoreCase]);
end;

{***************************************************************}
function ConvProtoImdReply(const ImdReply: TImdReply; var ExtUserDets: TExtUserDets; var DetsResCode: Word): Boolean;
begin
  Result := True;

  DetsResCode := DETAILS_RES_SERVICE_TEMP_UNAVAILABLE;

  case ImdReply.ImdCode of
    IMD_CODE_SUCCESS    : if (ImdReply.Results > 0) then DetsResCode := DETAILS_RES_SUCCESS;
    IMD_CODE_NOT_EXIST  : DetsResCode := DETAILS_RES_NOT_FOUND;
  end;//case

  if (DetsResCode <> DETAILS_RES_SUCCESS) then
  begin
    Finalize(ExtUserDets);
    ZeroMemory(@ExtUserDets, SizeOf(TExtUserDets));

    ExtUserDets.ReqID := ImdReply.ReqID;
    Exit;
  end;

  //convert first ImdInfo
  ConvProtoImdInfo(@ImdReply.ImdData[0], ExtUserDets, ImdReply.ReqID);
end;

{***************************************************************}
procedure ConvProtoImdInfo(ImdInfo: pImdInfo; var ExtUserDets: TExtUserDets; const ObimpReqID: DWord);
begin
  Finalize(ExtUserDets);
  ZeroMemory(@ExtUserDets, SizeOf(TExtUserDets));

  ExtUserDets.ReqID := ObimpReqID;

  ExtUserDets.Account     := ImdInfo^.Account;
  ExtUserDets.NickName    := ImdInfo^.Nickname;
  if (ExtUserDets.NickName = '') then ExtUserDets.NickName := ImdInfo^.LoginAlias;
  ExtUserDets.FirstName   := ImdInfo^.Firstname;
  ExtUserDets.LastName    := ImdInfo^.Lastname;
  ExtUserDets.CountryCode := Country_ImdToObimp(ImdInfo^.HomeCountry);
  ExtUserDets.RegionState := ImdInfo^.HomeState;
  ExtUserDets.City        := ImdInfo^.HomeCity;
  ExtUserDets.ZipCode     := ImdInfo^.HomeZip;
  ExtUserDets.Address     := ImdInfo^.HomeStreet;
  ExtUserDets.LangCode1   := Language_ImdToObimp(ImdInfo^.Language1);
  ExtUserDets.LangCode2   := Language_ImdToObimp(ImdInfo^.Language2);
  ExtUserDets.Gender      := ConvProtoGender(ImdInfo^.Gender);
  ExtUserDets.Birthday    := ConvProtoBirthday(ImdInfo^.Birthday);
  ExtUserDets.HomePage    := ImdInfo^.Homepage;
  ExtUserDets.About       := ImdInfo^.About;
  ExtUserDets.Interests   := ConvProtoInterests(ImdInfo);
  ExtUserDets.HomePhone   := ConvProtoPhone(ImdInfo^.PhoneHome);
  ExtUserDets.WorkPhone   := ConvProtoPhone(ImdInfo^.PhoneWork);
  ExtUserDets.CellPhone   := ConvProtoPhone(ImdInfo^.PhoneMobile);
  ExtUserDets.FaxNum      := ConvProtoPhone(ImdInfo^.PhoneWorkFax);
  ExtUserDets.Company     := ImdInfo^.WorkCompany;
  ExtUserDets.DivDept     := ImdInfo^.WorkDepart;
  ExtUserDets.Position    := ImdInfo^.WorkPosition;
end;

{***************************************************************}
procedure ConvProtoSearchReply(const ImdReply: TImdReply; ResIndex: Integer; var ExtSrchReply: TExtSrchReply; var SrchResCode: Word);
var ImdInfo: pImdInfo;
    iBirth: Int64;
begin
  Finalize(ExtSrchReply);
  ZeroMemory(@ExtSrchReply, SizeOf(TExtSrchReply));

  ExtSrchReply.ReqID := ImdReply.ReqID;

  SrchResCode := SEARCH_RES_SERVICE_TEMP_UNAVAILABLE;

  case ImdReply.ImdCode of
    IMD_CODE_NOT_EXIST  : SrchResCode := SEARCH_RES_NOT_FOUND;
  end;//case

  //if search failed then no results and exit
  if (ResIndex = -1) then
  begin
    //very important!
    ExtSrchReply.LastReply := True;
    Exit;
  end;

  if (ResIndex > Pred(Length(ImdReply.ImdData))) then Exit;

  //search success
  SrchResCode := SEARCH_RES_SUCCESS;

  //for work with shorter name
  ImdInfo := @ImdReply.ImdData[ResIndex];

  ExtSrchReply.Online   := ImdInfo^.OnlineStatus = IMD_CNT_STATUS_ONLINE;
  ExtSrchReply.Account  := ImdInfo^.Account;

  if (ImdInfo^.LoginAlias = '') then
    ExtSrchReply.NickName := ImdInfo^.LoginAlias
  else
    ExtSrchReply.NickName := ImdInfo^.Nickname;

  ExtSrchReply.FirstName  := ImdInfo^.Firstname;
  ExtSrchReply.LastName   := ImdInfo^.Lastname;
  ExtSrchReply.Gender     := ConvProtoGender(ImdInfo^.Gender);

  iBirth := ConvProtoBirthday(ImdInfo^.Birthday);
  try
    if (iBirth <> 0) then
      ExtSrchReply.Age := GetAge(UnixToDateTime(iBirth), SysNow);
  except
  end;

  //very important!
  ExtSrchReply.LastReply := ResIndex = (Length(ImdReply.ImdData)-1);

  if ExtSrchReply.LastReply then
    ExtSrchReply.TotalMatched := ImdReply.Matches;

  //result custom status pic
  ExtSrchReply.StPicIndex := SEARCH_ST_PIC_FLAG_CUSTOM;
  if (Pos('@', ExtSrchReply.Account) > 0) then
  begin
    case ImdInfo^.OnlineStatus of
      IMD_CNT_STATUS_ONLINE   : ExtSrchReply.StPicIndex := ExtSrchReply.StPicIndex or CUST_ST_PIC_MAILRU_ONLINE;
      IMD_CNT_STATUS_DISABLED : ExtSrchReply.StPicIndex := ExtSrchReply.StPicIndex or CUST_ST_PIC_MAILRU_NIL;
      else
        ExtSrchReply.StPicIndex := ExtSrchReply.StPicIndex or CUST_ST_PIC_MAILRU_OFFLINE;
    end;//case
  end
  else
  begin
    case ImdInfo^.OnlineStatus of
      IMD_CNT_STATUS_ONLINE   : ExtSrchReply.StPicIndex := ExtSrchReply.StPicIndex or CUST_ST_PIC_AWARE_ONLINE;
      IMD_CNT_STATUS_DISABLED : ExtSrchReply.StPicIndex := ExtSrchReply.StPicIndex or CUST_ST_PIC_AWARE_DISABLED;
      else
        ExtSrchReply.StPicIndex := ExtSrchReply.StPicIndex or CUST_ST_PIC_AWARE_OFFLINE;
    end;//case
  end;
end;

{***************************************************************}
function ConvProtoImdUpdReply(const ImdUpdReply: TImdReply; var OwnDetsUpdReply: TExtOwnDetsUpdReply): Boolean;
begin
  Result := True;

  ZeroMemory(@OwnDetsUpdReply, SizeOf(TExtOwnDetsUpdReply));

  OwnDetsUpdReply.UpdResultCode := UPD_DETAILS_RES_SERVICE_TEMP_UNAVAILABLE;
  OwnDetsUpdReply.ReqID         := ImdUpdReply.ReqID;

  case ImdUpdReply.ImdCode of
    IMD_CODE_SUCCESS : OwnDetsUpdReply.UpdResultCode := UPD_DETAILS_RES_SUCCESS;
  end;//case
end;

{***************************************************************}
function ConvObimpDetsUpdate(Client: TOscarClient; ExtUserDets: pExtUserDets; var ImdInfo: TImdInfo): Boolean;
begin
  Result := True;

  Finalize(ImdInfo);
  ZeroMemory(@ImdInfo, SizeOf(TImdInfo));

  //set current info and then set changes
  ImdInfo := Client.OscarInfo.ImdInfo;

  ImdInfo.Account       := ExtUserDets^.Account;
  ImdInfo.Nickname      := ExtUserDets^.NickName;
  ImdInfo.LoginAlias    := ExtUserDets^.NickName;
  ImdInfo.Firstname     := ExtUserDets^.FirstName;
  ImdInfo.Lastname      := ExtUserDets^.LastName;
  ImdInfo.Birthday      := ConvObimpBirthday(ExtUserDets^.Birthday);
  ImdInfo.Gender        := ConvObimpGender(ExtUserDets^.Gender);
  ImdInfo.HomeStreet    := ExtUserDets^.Address;
  ImdInfo.HomeCity      := ExtUserDets^.City;
  ImdInfo.HomeState     := ExtUserDets^.RegionState;
  ImdInfo.HomeZip       := ExtUserDets^.ZipCode;
  ImdInfo.HomeCountry   := Country_ObimpToImd(ExtUserDets^.CountryCode);
  ImdInfo.Language1     := Language_ObimpToImd(ExtUserDets^.LangCode1);
  ImdInfo.Language2     := Language_ObimpToImd(ExtUserDets^.LangCode2);
  ImdInfo.PhoneHome     := ConvObimpPhone(ExtUserDets^.HomePhone);
  ImdInfo.PhoneWork     := ConvObimpPhone(ExtUserDets^.WorkPhone);
  ImdInfo.PhoneMobile   := ConvObimpPhone(ExtUserDets^.CellPhone);
  ImdInfo.PhoneWorkFax  := ConvObimpPhone(ExtUserDets^.FaxNum);
  ImdInfo.Homepage      := ExtUserDets^.HomePage;
  ImdInfo.WorkPosition  := ExtUserDets^.Position;
  ImdInfo.WorkCompany   := ExtUserDets^.Company;
  ImdInfo.WorkDepart    := ExtUserDets^.DivDept;
  ConvObimpInterests(@ImdInfo, ExtUserDets^.Interests);
  ImdInfo.About         := ExtUserDets^.About;
end;

{***************************************************************}
function ConvObimpPngToJpeg(ms: TMemoryStream): Word;
var png: TPngImage;
    bmp: TBitmap;
    jpg: TJPEGImage;
begin
  Result := AVATAR_SET_OTHER_ERROR;
  if (ms = nil) or (ms.Size = 0) then Exit;

  ms.Position := 0;

  png := TPngImage.Create;
  bmp := TBitmap.Create;
  jpg := TJPEGImage.Create;
  try
    try
      Png.LoadFromStream(ms);
    except
      Exit;
    end;

    ms.Clear;

    if png.Empty then Exit;

    bmp.PixelFormat := pf24bit;

    bmp.Canvas.Brush.Color := clWhite;
    bmp.Width  := png.Width;
    bmp.Height := png.Height;

    png.Draw(bmp.Canvas, Bounds(0, 0, bmp.Width, bmp.Height));

    try
      //use JPG for sizes bigger than 24x24 px, why? to make file size fit the limit
      //another reason is JPG of size 1x1 px causes unwanted errors
      if (bmp.Width > 24) and (bmp.Height > 24) then
      begin
        jpg.Assign(bmp);
        if jpg.Empty then Exit;
        jpg.SaveToStream(ms);
      end
      else
        bmp.SaveToStream(ms);

      if (ms.Size > 7168{max BART_BUDDY_ICON size}) then
        Result := AVATAR_SET_TOO_BIG
      else
      if (ms.Size > 0) then
        Result := AVATAR_REQ_SUCCESS;
    except
      Exit;
    end;

  finally
    jpg.Free;
    bmp.Free;
    png.Free;
  end;
end;

{***************************************************************}
procedure ConvProtoAvatarUploadRelpy(const BartReplyCode: Byte; const Bart: TOscarBart; const AvatarReqID: DWord; var oasr: TExtOwnAvatarSetReply);
begin
  ZeroMemory(@oasr, SizeOf(TExtOwnAvatarSetReply));

  oasr.ReqID         := AvatarReqID;
  oasr.SetResultCode := AVATAR_SET_OTHER_ERROR;

  case BartReplyCode of
    BART_REPLY_CODE_SUCCESS      : oasr.SetResultCode := AVATAR_SET_SUCCESS;
    BART_REPLY_CODE_TOO_BIG      : oasr.SetResultCode := AVATAR_SET_TOO_BIG;
    BART_REPLY_CODE_TOO_SMALL    : oasr.SetResultCode := AVATAR_SET_TOO_SMALL;
    BART_REPLY_CODE_INVALID_TYPE : oasr.SetResultCode := AVATAR_SET_INVALID_TYPE;
    BART_REPLY_CODE_BANNED       : oasr.SetResultCode := AVATAR_SET_BANNED;
  end;//case
end;

{***************************************************************}
function ConvObimpSearch(ExtSrchReq: pExtSrchReq; var ImdSrch: TImdSrchReq): Boolean;
begin
  Result := True;

  Finalize(ImdSrch);
  ZeroMemory(@ImdSrch, SizeOf(TImdSrchReq));

  ImdSrch.Account       := ExtSrchReq^.Account;
  ImdSrch.Email         := ExtSrchReq^.Email;
  ImdSrch.Nickname      := ExtSrchReq^.NickName;
  ImdSrch.Firstname     := ExtSrchReq^.FirstName;
  ImdSrch.Lastname      := ExtSrchReq^.LastName;
  ImdSrch.Gender        := ConvObimpGender(ExtSrchReq^.Gender);
  ImdSrch.HomeCity      := ExtSrchReq^.City;
  ImdSrch.HomeCountry   := Country_ObimpToImd(ExtSrchReq^.CountryCode);
  ImdSrch.Language      := Language_ObimpToImd(ExtSrchReq^.LanguageCode);
  ImdSrch.Keywords      := ExtSrchReq^.Interests;
  ImdSrch.AgeMin        := ExtSrchReq^.AgeMin;
  ImdSrch.AgeMax        := ExtSrchReq^.AgeMax;

  if ExtSrchReq^.OnlineOnly then
    ImdSrch.OnlineOnly := 1;
end;

end.
