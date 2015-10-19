// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_convert;

interface

uses Windows, Classes, SysUtils, u_obimp_const, u_obimp_codes, u_ext_info, h_jabber, u_jabber, u_jabber_hlp, u_roster, u_roster_info;

const
  LI_ADDITIONAL            = 0;
  LI_RESOURCE              = 1;
  LI_PRIORITY              = 2;
  LI_INFORMATION           = 3;
  LI_RECONNECT_REQUIRED    = 4;
  LI_TRANSPORT             = 5;
  LI_STATUS_ONLINE         = 6;
  LI_STATUS_AWAY           = 7;
  LI_STATUS_FREE_FOR_CHAT  = 8;
  LI_STATUS_DO_NOT_DISTURB = 9;
  LI_STATUS_NOT_AVAILABLE  = 10;
  LI_STATUS_INVISIBLE      = 11;
  LI_RECEIVER              = 12;
  LI_NOTIF_MSG_SEND_FAILED = 13;
  LI_SESSION               = 14;

  function  GetLI(ClientLang: Word; LI: Word): string;
  function  GetOptBool(const OptValue: WideString): Boolean;
  function  GetOptByte(const OptValue: WideString): Byte;
  function  GetResourceDescr(aItem: pRosterItem; const ClientLang: Word): string;

  function  StatusShowToLangID(const StatusShow: string): Word;
  procedure ConvObimpStatus(ObimpStatus: DWord; var Status: string);
  procedure ConvProtoStatus(const StatusShow, StatusAdvText: string; var cs: TExtContactStatus);
  function  ConvProtoConErrorCode(ProtoErrMsg: string): Word;
  function  ConvProtoBuddyOnline(aItem: pRosterItem; const Resource: string; var cs: TExtContactStatus; const ClientLang: Word): Boolean;
  procedure ConvProtoBuddyOffline(aItem: pRosterItem; var cs: TExtContactStatus);
  procedure ConvProtoIncMsg(Client: TJabberClient; const IncMsg: TXmppIncMsg; var ExtMsg: TExtMsg);
  function  ConvProtoMsgAck(Client: TJabberClient; const XmppAck: TXmppMsgAck; var MsgAck: TExtMsgAck): Boolean;
  function  ConvProtoEventMsg(const ChatState: TXmppChatState; var ExtNotif: TExtNotif): Boolean;
  procedure ConvObimpOutMsg(Client: TJabberClient; InstantMsg: pExtMsg; var XmppMsg: TXmppOutMsg);
  function  ConvObimpMsgAck(Client: TJabberClient; MsgAck: pExtMsgAck; var XmppAck: TXmppMsgAck): Boolean;
  function  ConvObimpNotif(Client: TJabberClient; ExtNotif: pExtNotif; var ChatState: TXmppChatState): Boolean;


implementation

const
  LANG_MAX = 14;

  LangEN: array[0..LANG_MAX] of string = (
  'Additional',
  'Resource',
  'Priority',
  'Information',
  'Reconnect required to apply new settings.',
  'Transport',
  'Online',
  'Away',
  'Free for chat',
  'Do not disturb',
  'Not available',
  'Invisible',
  'Receiver',
  'Message send failed',
  'Session'
  );

  LangRU: array[0..LANG_MAX] of string = (
  'Дополнительно',
  'Ресурс',
  'Приоритет',
  'Информация',
  'Чтобы новые настройки вступили в силу, необходимо переподключиться.',
  'Транспорт',
  'В сети',
  'Отошёл',
  'Готов поболтать',
  'Не беспокоить',
  'Недоступен',
  'Невидимый',
  'Получатель',
  'Не удалось отправить сообщение',
  'Сессия'
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
function GetResourceDescr(aItem: pRosterItem; const ClientLang: Word): string;
var i: Integer;
begin
  Result := '';
  if (aItem^.GetResCount = 0) then Exit;

  if (aItem^.GetResCount = 1) then
  begin
    if (aItem^.ResArray[0].Resource <> '') then
      Result := '<L=606>R</L>: ' + aItem^.ResArray[0].Resource +
                ' [' + IntToStr(aItem^.ResArray[0].Priority) + ']';
  end
  else
  begin
    for i := 0 to Length(aItem^.ResArray)-1 do
    begin
      if (aItem^.ResArray[i].Resource <> '') then
        Result := Result +
                  '<L=606>R</L> ' + IntToStr(i+1) + ': ' + aItem^.ResArray[i].Resource +
                  ' [' + IntToStr(aItem^.ResArray[i].Priority) + '] (' + GetLI(ClientLang, StatusShowToLangID(aItem^.ResArray[i].StatusShow)) + ')';

      if (aItem^.ResArray[i].ClientVer.Name <> '') then
        Result := Result + THE_SPC + ' @ ' + aItem^.ResArray[i].ClientVer.Name;

      Result := Result + CRLF;
    end;//for
  end;

  Result := Trim(Result);
end;

{***************************************************************}
function StatusShowToLangID(const StatusShow: string): Word;
begin
  Result := LI_STATUS_ONLINE;

  if WideSameText(StatusShow, PRES_SHOW_AWAY) then
    Result := LI_STATUS_AWAY
  else
  if WideSameText(StatusShow, PRES_SHOW_CHAT) then
    Result := LI_STATUS_FREE_FOR_CHAT
  else
  if WideSameText(StatusShow, PRES_SHOW_DND) then
    Result := LI_STATUS_DO_NOT_DISTURB
  else
  if WideSameText(StatusShow, PRES_SHOW_XA) then
    Result := LI_STATUS_NOT_AVAILABLE
  else
  if WideSameText(StatusShow, PRES_SHOW_INVIS_SPC) then
    Result := LI_STATUS_INVISIBLE;
end;

{***************************************************************}
procedure ConvObimpStatus(ObimpStatus: DWord; var Status: string);
begin
  Status := '';

  case ObimpStatus of
   {===================================================}
    PRES_STATUS_ONLINE             : Status := PRES_SHOW_ONLINE;
   {===================================================}
    PRES_STATUS_INVISIBLE          : Status := PRES_SHOW_INVIS_SPC;
   {===================================================}
    PRES_STATUS_INVISIBLE_FOR_ALL  : Status := PRES_SHOW_IFA_SPC;
   {===================================================}
    PRES_STATUS_FREE_FOR_CHAT      : Status := PRES_SHOW_CHAT;
   {===================================================}
    PRES_STATUS_AWAY               : Status := PRES_SHOW_AWAY;
   {===================================================}
    PRES_STATUS_NOT_AVAILABLE      : Status := PRES_SHOW_XA;
   {===================================================}
    PRES_STATUS_DO_NOT_DISTURB     : Status := PRES_SHOW_DND;
   {===================================================}
  end;//case
end;

{***************************************************************}
procedure ConvProtoStatus(const StatusShow, StatusAdvText: string; var cs: TExtContactStatus);
begin
  cs.Status := PRES_STATUS_ONLINE;

  if WideSameText(StatusShow, PRES_SHOW_AWAY) then
    cs.Status := PRES_STATUS_AWAY
  else
  if WideSameText(StatusShow, PRES_SHOW_CHAT) then
    cs.Status := PRES_STATUS_FREE_FOR_CHAT
  else
  if WideSameText(StatusShow, PRES_SHOW_DND) then
    cs.Status := PRES_STATUS_DO_NOT_DISTURB
  else
  if WideSameText(StatusShow, PRES_SHOW_XA) then
    cs.Status := PRES_STATUS_NOT_AVAILABLE
  else
  if WideSameText(StatusShow, PRES_SHOW_INVIS_SPC) then
    cs.Status := PRES_STATUS_INVISIBLE;

  cs.StatusName := StatusAdvText;
end;

{***************************************************************}
function ConvProtoConErrorCode(ProtoErrMsg: string): Word;
begin
  Result := TP_STATE_CON_FAILED;

  if (ProtoErrMsg = XHLP_SASL_NOT_AUTHORIZED) then
    Result := TP_STATE_INVALID_LOGIN
  else
  if (ProtoErrMsg = XHLP_CONFLICT) then
    Result := TP_STATE_OTHER_PLACE_LOGIN
  else
  if (ProtoErrMsg = XHLP_BAD_USERNAME) then
    Result := TP_STATE_ACCOUNT_INVALID
  else
  if (ProtoErrMsg = XHLP_INTERNAL_SRV_ERROR) or (ProtoErrMsg = XHLP_SASL_TEMP_AUTH_FAIL) then
    Result := TP_STATE_SERVICE_TEMP_UNAVAILABLE;
end;

{*****************************************************************}
function tb(const Value: Word): RawByteString;
begin
  Result := AnsiChar((Value shr 8) and $FF) +
            AnsiChar(Value and $FF);
end;

{*******************************************************************}
function TLV(Tag: Word; Value: RawByteString = ''): RawByteString;
begin
  Result := tb(Tag) + tb(Word(Length(Value))) + Value;
end;

{***************************************************************}
function ConvProtoBuddyOnline(aItem: pRosterItem; const Resource: string; var cs: TExtContactStatus; const ClientLang: Word): Boolean;
var aResInfo: pResInfo;
begin
  Result := False;
  Finalize(cs);
  ZeroMemory(@cs, SizeOf(TExtContactStatus));

  aResInfo := aItem^.GetRes(Resource);
  if (aResInfo = nil) then Exit;

  cs.Account := aItem^.JID;

  //get obimp status
  ConvProtoStatus(aResInfo^.StatusShow, aResInfo^.StatusText, cs);

  cs.ClientType := CLIENT_TYPE_USER;

  if (aResInfo^.ClientVer.Name = '') then
    cs.ClientName := 'XMPP'
  else
    cs.ClientName := aResInfo^.ClientVer.Name;

  if (aResInfo^.ClientVer.Ver <> '') then
    cs.ClientName := cs.ClientName + THE_SPC + '('+ aResInfo^.ClientVer.Ver + ')';

  cs.ClientOS := aResInfo^.ClientVer.OS;

  cs.ClientDesc := GetResourceDescr(aItem, ClientLang);

  cs.AvatarHashHex := aItem^.AvatarMd5Hex;

  if (aResInfo^.ClientVer.Name <> '') then
    cs.ClientIdBlk := WideString( TLV($0001, UTF8Encode(aResInfo^.ClientVer.Name)) );

  Result := True;
end;

{***************************************************************}
procedure ConvProtoBuddyOffline(aItem: pRosterItem; var cs: TExtContactStatus);
begin
  Finalize(cs);
  ZeroMemory(@cs, SizeOf(TExtContactStatus));

  cs.Account := aItem^.JID;
end;

{***************************************************************}
procedure ConvProtoIncMsg(Client: TJabberClient; const IncMsg: TXmppIncMsg; var ExtMsg: TExtMsg);
begin
  Finalize(ExtMsg);
  ZeroMemory(@ExtMsg, SizeOf(TExtMsg));

  if (IncMsg.Subj <> '') then
    ExtMsg.MsgText := IncMsg.Subj + CRLF;

  ExtMsg.Account       := IncMsg.Jid;
  ExtMsg.MsgText       := Trim(ExtMsg.MsgText + IncMsg.Body);
  ExtMsg.AckRequired   := IncMsg.AckRequired;
  ExtMsg.IsOfflineMsg  := IncMsg.IsOffline;
  ExtMsg.OfflineMsgDT  := IncMsg.OfflineDT;

  if IncMsg.AckRequired then
    ExtMsg.UniqID := Client.CookieMan.AddXmppCookie(IncMsg.Jid, IncMsg.Resource, IncMsg.AckID);

  //we have to check that cookie value is not null, else server will not accept it
  if (ExtMsg.UniqID = 0) then
    ExtMsg.UniqID := 1;
end;

{***************************************************************}
function ConvProtoMsgAck(Client: TJabberClient; const XmppAck: TXmppMsgAck; var MsgAck: TExtMsgAck): Boolean;
var aCookie: pHelpCookie;
begin
  Result := False;

  aCookie := Client.CookieMan.GetCookieByXmpp(XmppAck.Jid, XmppAck.AckID);
  if (aCookie = nil) then Exit;

  Finalize(MsgAck);
  ZeroMemory(@MsgAck, SizeOf(TExtMsgAck));

  MsgAck.Account := XmppAck.Jid;
  MsgAck.UniqID  := aCookie^.ObimpCookie;

  Result := True;

  //del cookie
  Client.CookieMan.DelCookieByXmpp(XmppAck.Jid, XmppAck.AckID);
end;

{***************************************************************}
function ConvProtoEventMsg(const ChatState: TXmppChatState; var ExtNotif: TExtNotif): Boolean;
begin
  Result := False;

  Finalize(ExtNotif);
  ZeroMemory(@ExtNotif, SizeOf(TExtNotif));

  ExtNotif.Account    := ChatState.Jid;
  ExtNotif.NotifType  := NOTIF_TYPE_USER_TYPING;

  if WideSameText(ChatState.ChatState, XNS_COMPOSING) then
    ExtNotif.NotifValue := NOTIF_VALUE_USER_TYPING_START
  else
  if WideSameText(ChatState.ChatState, XNS_ACTIVE) or WideSameText(ChatState.ChatState, XNS_PAUSED) or
    WideSameText(ChatState.ChatState, XNS_INACTIVE) or WideSameText(ChatState.ChatState, XNS_GONE) then
    ExtNotif.NotifValue := NOTIF_VALUE_USER_TYPING_FINISH;

  if (ExtNotif.NotifValue = 0) then Exit;
  Result := True;
end;

{***************************************************************}
procedure ConvObimpOutMsg(Client: TJabberClient; InstantMsg: pExtMsg; var XmppMsg: TXmppOutMsg);
var aResInfo: pResInfo;
begin
  Finalize(XmppMsg);
  ZeroMemory(@XmppMsg, SizeOf(TXmppOutMsg));

  XmppMsg.Body := InstantMsg^.MsgText;
  XmppMsg.Jid  := InstantMsg^.Account;

  aResInfo := Client.Roster.GetJidCurResInfo(InstantMsg^.Account);
  if (aResInfo <> nil) then
  begin
    XmppMsg.Resource := aResInfo^.Resource;

    if InstantMsg^.AckRequired and (cdi_Receipts in aResInfo^.DiscoInfo) then
    begin
      XmppMsg.AckRequired := True;
      XmppMsg.AckID       := Client.CookieMan.AddObimpCookie(InstantMsg^.Account, InstantMsg^.UniqID);
    end;

    if (cdi_ChatStates in aResInfo^.DiscoInfo) then
      XmppMsg.ChatStateActive := True;
  end;
end;

{***************************************************************}
function ConvObimpMsgAck(Client: TJabberClient; MsgAck: pExtMsgAck; var XmppAck: TXmppMsgAck): Boolean;
var aCookie: pHelpCookie;
begin
  Result := False;

  aCookie := Client.CookieMan.GetCookieByObimp(MsgAck^.Account, MsgAck^.UniqID);
  if (aCookie = nil) then Exit;

  Finalize(XmppAck);
  ZeroMemory(@XmppAck, SizeOf(TXmppMsgAck));

  XmppAck.AckID    := aCookie^.XmppCookie;

  XmppAck.Jid      := aCookie^.Jid;
  XmppAck.Resource := aCookie^.Resource;

  Result := True;

  //del cookie
  Client.CookieMan.DelCookieByObimp(MsgAck^.Account, MsgAck^.UniqID);
end;

{***************************************************************}
function ConvObimpNotif(Client: TJabberClient; ExtNotif: pExtNotif; var ChatState: TXmppChatState): Boolean;
var aResInfo: pResInfo;
begin
  Result := False;

  Finalize(ChatState);
  ZeroMemory(@ChatState, SizeOf(TXmppChatState));

  ChatState.Jid := ExtNotif^.Account;

  aResInfo := Client.Roster.GetJidCurResInfo(ExtNotif^.Account);
  if (aResInfo <> nil) then
  begin
    ChatState.Resource := aResInfo^.Resource;

    if (cdi_ChatStates in aResInfo^.DiscoInfo) then
    begin
      Result := True;

      if (ExtNotif^.NotifValue = NOTIF_VALUE_USER_TYPING_START) then
        ChatState.ChatState := XNS_COMPOSING
      else
        ChatState.ChatState := XNS_PAUSED;
    end;
  end;
end;


end.
