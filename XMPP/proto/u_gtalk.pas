// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_gtalk;

interface

uses Windows, Classes, SysUtils, NativeXML, u_xmppsock, h_jabber, u_gtalk_auth, u_xepclass, u_roster_info, u_privacy_list,
     u_obimp_codes, u_ext_info;

// GOOGLE TALK

const
  SRV_HOST_GTALK = 'talk.google.com';
  SRV_PORT_GTALK = 5222;

  GT_ROSTER_VER_EXT = '2';
  GT_SHARED_ST_VER  = '2';

  XNS_GT_GOOGLE_SETTING     = 'google:setting';
  XNS_GT_GOOGLE_ROSTER      = 'google:roster';
  XNS_GT_SHARED_STATUS      = 'google:shared-status';
  XNS_GT_MAIL_NOTIFY        = 'google:mail:notify';
  XNS_GT_USER_SETTING       = 'usersetting';
  XNS_GT_AUTO_ACCEPT_REQ    = 'autoacceptrequests';
  XNS_GT_AUTO_ACCEPT_SUG    = 'autoacceptsuggestions';
  XNS_GT_ARCHIVING_ENABLED  = 'archivingenabled';
  XNS_GT_MAIL_NOTIFS        = 'mailnotifications';
  XNS_GT_STATUS_LIST        = 'status-list';
  XNS_GT_INVISIBLE          = 'invisible';
  XNS_GT_NEW_MAIL           = 'new-mail';
  XNS_GT_MAILBOX            = 'mailbox';
  XNS_GT_MAIL_THREAD_INFO   = 'mail-thread-info';
  XNS_GT_SENDERS            = 'senders';
  XNS_GT_SENDER             = 'sender';
  XNS_GT_SNIPPET            = 'snippet';

  XTAG_GT_USER_SETTING      = '<' + XNS_GT_USER_SETTING + ' xmlns=''' + XNS_GT_GOOGLE_SETTING + '''>%</' + XNS_GT_USER_SETTING + '>';
  XTAG_GT_USER_SETTING_EMPTY= '<' + XNS_GT_USER_SETTING + ' xmlns=''' + XNS_GT_GOOGLE_SETTING + '''/>';
  XTAG_GT_GR_QUERY          = '<' + XNS_QUERY + ' xmlns=''' + XNS_JABBER_IQ_ROSTER + ''' xmlns:gr=''' + XNS_GT_GOOGLE_ROSTER + ''' gr:ext=''' + GT_ROSTER_VER_EXT + '''>%</' + XNS_QUERY + '>';
  XTAG_GT_GR_QUERY_EMPTY    = '<' + XNS_QUERY + ' xmlns=''' + XNS_JABBER_IQ_ROSTER + ''' xmlns:gr=''' + XNS_GT_GOOGLE_ROSTER + ''' gr:ext=''' + GT_ROSTER_VER_EXT + '''/>';

  XATT_GT_ROS_T             = 'ros:t';
  XATT_GT_GR_T              = 'gr:t';
  XATT_GT_VERSION           = 'version';
  XATT_GT_TOTAL_MATCHED     = 'total-matched';
  XATT_GT_NEWER_THAN_TIME   = 'newer-than-time';
  XATT_GT_NEWER_THAN_TID    = 'newer-than-tid';
  XATT_GT_RESULT_TIME       = 'result-time';
  XATT_GT_TID               = 'tid';
  XATT_GT_URL               = 'url';
  XATT_GT_UNREAD            = 'unread';
  XATT_GT_ADDRESS           = 'address';


  XVAL_FALSE                = 'false';
  XVAL_TRUE                 = 'true';

  GT_BLOCKED                = 'B';
  GT_HIDDEN                 = 'H';
  GT_PINNED                 = 'P';

  LI_GT_GOOGLE_TALK         = 0;
  LI_GT_SAVE_CHAT_HIST      = 1;
  LI_GT_MAIL_NOTIF          = 2;

  LANG_GT_MAX = 2;

type
  TGoogleSettings = record
    AutoAcceptRequests     : Boolean;
    AutoAcceptSuggestions  : Boolean;
    ArchivingEnabled       : Boolean;
    MailNotifications      : Boolean; //local setting
  end;
  pGoogleSettings = ^TGoogleSettings;

  TStatusList = record
    ListName : string;
    Statuses : array of string;
  end;

  TStatusLists = array of TStatusList;

  TSharedStatus = record
    StatusAdv   : string;
    StatusShow  : string;
    Invisible   : Boolean;
    StatusLists : TStatusLists;
  end;

  TGoogleTalk = class(TXepClass)
  private
    FSocket      : TXmppSocket;
    FCurMailTid  : string;
    FCurMailTime : string;
    procedure GetMailMsg(XmlNode: TXmlNode; var MailNotif: TExtMailNotif);
  public
    GAuth        : TGoogleAuth;
    Settings     : TGoogleSettings;
    SharedStatus : TSharedStatus;
    constructor Create(XmppSocket: TXmppSocket);
    destructor  Destroy; override;
    procedure ResetData; override;
    function  CheckItemPrivacy(aItem: pRosterItem; PrivType: Byte): string;
    procedure Send_GetUserSettings(const Username: string; const DuringLogin: Boolean);
    procedure Send_SetUserSettings(const Username: string; const DuringLogin: Boolean);
    procedure Rcvd_Setting(XmlNode: TXmlNode; const Username: string; const DuringLogin: Boolean);
    procedure Send_GetSharedStatus(const Username: string);
    procedure Rcvd_SharedStatus(XmlNode: TXmlNode);
    function  Send_GoggleSharedStatus(const Username, StatusShow: string): Boolean;
    procedure Send_GetNewMail(const OnlyUnreadCount: Boolean; const ResetLastTime: Boolean = False);
    procedure Rcvd_NewMail(XmlNode: TXmlNode; var MailNotif: TExtMailNotif; const OnlyUnreadCount: Boolean);
  end;

  function  GetLI_GTalk(ClientLang: Word; LI_GT: Word): string;
  function  GoogleRosterIqQuery(const IqType, IqId: string; const QueryValue: string = ''): string;
  function  ConvertTidToHex(const Tid: string; Url: string): string;

implementation

const
  LangEN: array[0..LANG_GT_MAX] of string = (
  'Google Talk',
  'Save chat history in my Gmail account',
  'Notify upon receiving new mail'
  );

  LangRU: array[0..LANG_GT_MAX] of string = (
  'Google Talk',
  'Сохранять историю переписки в моей учётной записи Gmail',
  'Уведомлять при получении новых писем'
  );

{***************************************************************}
function GetLI_GTalk(ClientLang: Word; LI_GT: Word): string;
begin
  Result := '';
  if (LI_GT > LANG_GT_MAX) then Exit;

  case ClientLang of
   {====================================================}
    LANGUAGE_CODE_RUSSIAN: Result := LangRU[LI_GT];
   {====================================================}
    else
      Result := LangEN[LI_GT];
   {====================================================}
  end;//case
end;

{*****************************************************************}
function GoogleRosterIqQuery(const IqType, IqId: string; const QueryValue: string = ''): string;
begin
  if (QueryValue = '') then
    Result := XmlTag(XTAG_GT_GR_QUERY_EMPTY)
  else
    Result := XmlTag(XTAG_GT_GR_QUERY, QueryValue);

  Result := XmlTagIq(IqType, IqId, Result);
end;

{*****************************************************************}
function ConvertTidToHex(const Tid: string; Url: string): string;
var i: Integer;
    iTid: UInt64;
begin
  Result := Url;
  if (Tid = '') or (Url = '') then Exit;

  i := Pos(Tid, Url);
  if (i > 0) then
  begin
    iTid := StrToInt64Def(Tid, 0);
    if (iTid > 0) then
      Result := StringReplace(Url, Tid, LowerCase(IntToHex(iTid, 16)), [rfReplaceAll, rfIgnoreCase]);
  end;
end;

{ TGoogleTalk }
{*****************************************************************}
constructor TGoogleTalk.Create(XmppSocket: TXmppSocket);
begin
  FSocket := XmppSocket;

  GAuth := TGoogleAuth.Create;

  //set default settings
  Settings.MailNotifications := True;
end;

{*****************************************************************}
destructor TGoogleTalk.Destroy;
begin
  GAuth.Free;
  inherited;
end;

{*****************************************************************}
procedure TGoogleTalk.ResetData;
begin
  inherited;

  FCurMailTid  := '';
  FCurMailTime := '';

  GAuth.KillThreads;
end;

{*****************************************************************}
function TGoogleTalk.CheckItemPrivacy(aItem: pRosterItem; PrivType: Byte): string;
begin
  if (PrivType = PRIV_BIM_IGNORE) then
    Result := GT_BLOCKED
  else
  begin
    if WideSameText(aItem^.GTalk_T, GT_BLOCKED) then
      Result := ''
    else
      Result := aItem^.GTalk_T;
  end;
end;

{*****************************************************************}
procedure TGoogleTalk.Send_GetUserSettings(const Username: string; const DuringLogin: Boolean);
begin
  if DuringLogin then
    FSocket.Send(XmlTagIqTo(XVAL_GET, Username, FSocket.NewSeqNum(SEQT_GTALK_GET_SET_LOGIN),
                 XmlTag(XTAG_GT_USER_SETTING_EMPTY) ) )
  else
    FSocket.Send(XmlTagIqTo(XVAL_GET, Username, FSocket.NewSeqNum(SEQT_GTALK_GET_SETTING),
                 XmlTag(XTAG_GT_USER_SETTING_EMPTY) ) );
end;

{*****************************************************************}
procedure TGoogleTalk.Send_SetUserSettings(const Username: string; const DuringLogin: Boolean);
var sTags: string;
begin
  if DuringLogin then
  begin
    sTags := MakeTag(XNS_GT_AUTO_ACCEPT_REQ, '', XmlAtt(XATT_VALUE, LowerCase(BoolToStr(Settings.AutoAcceptRequests, True))) ) +
             MakeTag(XNS_GT_MAIL_NOTIFS, '', XmlAtt(XATT_VALUE, XVAL_TRUE)); //always true for mail notifs, recommended by google
  end
  else
  begin
    sTags := MakeTag(XNS_GT_AUTO_ACCEPT_SUG, '', XmlAtt(XATT_VALUE, LowerCase(BoolToStr(Settings.AutoAcceptSuggestions, True))) ) +
             MakeTag(XNS_GT_ARCHIVING_ENABLED, '', XmlAtt(XATT_VALUE, LowerCase(BoolToStr(Settings.ArchivingEnabled, True))) ) +
             MakeTag(XNS_GT_MAIL_NOTIFS, '', XmlAtt(XATT_VALUE, XVAL_TRUE)); //always true for mail notifs, recommended by google
  end;

  sTags := XmlTag(XTAG_GT_USER_SETTING, sTags);

  FSocket.Send(XmlTagIqTo(XVAL_SET, Username, FSocket.NewSeqNum(SEQT_GTALK_SET_SETTING), sTags));
end;

{*****************************************************************}
procedure TGoogleTalk.Rcvd_Setting(XmlNode: TXmlNode; const Username: string; const DuringLogin: Boolean);
var sName, sValue: string;
    i: Integer;
    aSets: TGoogleSettings;
    sets: pGoogleSettings;
begin
  if DuringLogin then
  begin
    ZeroMemory(@aSets, SizeOf(TGoogleSettings));
    sets := @aSets;
  end
  else
    sets := @Settings;

  for i := 0 to XmlNode.ChildContainerCount-1 do
  begin
    sName   := UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name);
    sValue  := XmlNode.ChildContainers[i].AttributeValueByNameWide[UTF8Encode(XATT_VALUE)];

    if WideSameText(sName, XNS_GT_AUTO_ACCEPT_REQ) then
      sets^.AutoAcceptRequests := WideSameText(sValue, XVAL_TRUE)
    else
    if WideSameText(sName, XNS_GT_AUTO_ACCEPT_SUG) then
      sets^.AutoAcceptSuggestions := WideSameText(sValue, XVAL_TRUE)
    else
    if WideSameText(sName, XNS_GT_ARCHIVING_ENABLED) then
      sets^.ArchivingEnabled := WideSameText(sValue, XVAL_TRUE)
    else
    if DuringLogin and WideSameText(sName, XNS_GT_MAIL_NOTIFS) then
      sets^.MailNotifications := WideSameText(sValue, XVAL_TRUE);
  end;//for

  if DuringLogin then
  begin
    if (sets^.AutoAcceptSuggestions <> Settings.AutoAcceptSuggestions) or
       (sets^.ArchivingEnabled <> Settings.ArchivingEnabled) or
       (sets^.MailNotifications <> Settings.MailNotifications) then
    begin
      Send_SetUserSettings(Username, False);
    end;
  end;
end;

{*****************************************************************}
procedure TGoogleTalk.Send_GetSharedStatus(const Username: string);
begin
  FSocket.Send(XmlTagIqTo(XVAL_GET, Username, FSocket.NewSeqNum(SEQT_GTALK_GET_SHARED_ST),
               MakeTag(XNS_QUERY, '',
                       XmlAtt(XATT_XMLNS, XNS_GT_SHARED_STATUS) + THE_SPC +
                       XmlAtt(XATT_GT_VERSION, GT_SHARED_ST_VER) ) ) );
end;

{*****************************************************************}
procedure TGoogleTalk.Rcvd_SharedStatus(XmlNode: TXmlNode);
var Node: TXmlNode;
    sName: string;
    i, j: Integer;
begin
  SetLength(SharedStatus.StatusLists, 0);
  Finalize(SharedStatus);
  ZeroMemory(@SharedStatus, SizeOf(TSharedStatus));

  for i := 0 to XmlNode.ChildContainerCount-1 do
  begin
    Node := XmlNode.ChildContainers[i];

    sName := UTF8ToUnicodeString(Node.Name);

    if WideSameText(sName, XNS_STATUS) then
      SharedStatus.StatusAdv  := Node.ValueUnicode
    else
    if WideSameText(sName, XNS_SHOW) then
      SharedStatus.StatusShow := Node.ValueUnicode
    else
    if WideSameText(sName, XNS_GT_INVISIBLE) then
      SharedStatus.Invisible := WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_VALUE)], XVAL_TRUE)
    else
    if WideSameText(sName, XNS_GT_STATUS_LIST) then
    begin
      //add status-list
      SetLength(SharedStatus.StatusLists, Length(SharedStatus.StatusLists)+1);

      //parse statuses
      with SharedStatus.StatusLists[Length(SharedStatus.StatusLists)-1] do
      begin
        ListName := Node.AttributeValueByNameWide[UTF8Encode(XATT_SHOW)];

        for j := 0 to Node.ChildContainerCount-1 do
        begin
          if WideSameText(UTF8ToUnicodeString(Node.ChildContainers[j].Name), XNS_STATUS) then
          begin
            SetLength(Statuses, Length(Statuses)+1);
            Statuses[Length(Statuses)-1] := Node.ChildContainers[j].ValueUnicode;
          end;
        end;//for
      end;//with
    end;
  end;//for
end;

{*****************************************************************}
function TGoogleTalk.Send_GoggleSharedStatus(const Username, StatusShow: string): Boolean;
var sTags, sList: string;
    i, j: Integer;
begin
  Result := False;

  if WideSameText(StatusShow, PRES_SHOW_ONLINE) then
  begin
    SharedStatus.Invisible  := False;
    SharedStatus.StatusAdv  := '';
    SharedStatus.StatusShow := '';

    Result := True;
  end
  else
  if WideSameText(StatusShow, PRES_SHOW_DND) then
  begin
    SharedStatus.Invisible  := False;
    SharedStatus.StatusAdv  := '';
    SharedStatus.StatusShow := PRES_SHOW_DND;

    Result := True;
  end
  else
  if WideSameText(StatusShow, PRES_SHOW_INVIS_SPC) then
  begin
    SharedStatus.Invisible  := True;
    SharedStatus.StatusAdv  := '';
    SharedStatus.StatusShow := '';

    Result := True;
  end
  else
  begin
    if SharedStatus.Invisible then
    begin
      SharedStatus.Invisible := False;
      Result := True;
    end;

    if (SharedStatus.StatusAdv <> '') then
    begin
      SharedStatus.StatusAdv  := '';
      Result := True;
    end;

      if (SharedStatus.StatusShow <> '') then
    begin
      SharedStatus.StatusShow := '';
      Result := True;
    end;
  end;

  if not Result then Exit;

  sTags := MakeTag(XNS_STATUS, SharedStatus.StatusAdv) +
           MakeTag(XNS_SHOW, SharedStatus.StatusShow);

  for i := 0 to Length(SharedStatus.StatusLists)-1 do
  begin
    sList := '';

    for j := 0 to Length(SharedStatus.StatusLists[i].Statuses)-1 do
      sList := sList + MakeTag(XNS_STATUS, SharedStatus.StatusLists[i].Statuses[j]);//for

    sTags := sTags +
             MakeTag(XNS_GT_STATUS_LIST, sList, XmlAtt(XATT_SHOW, SharedStatus.StatusLists[i].ListName));
  end;//for

  sTags := sTags +
           MakeTag(XNS_GT_INVISIBLE, '', XmlAtt(XATT_VALUE, LowerCase(BoolToStr(SharedStatus.Invisible, True))));

  sTags := MakeTag(XNS_QUERY, sTags, XmlAtt(XATT_XMLNS, XNS_GT_SHARED_STATUS) + THE_SPC +
                                     XmlAtt(XATT_GT_VERSION, GT_SHARED_ST_VER) );

  FSocket.Send(XmlTagIqTo(XVAL_SET, Username, FSocket.NewSeqNum(SEQT_GTALK_SET_SHARED_ST), sTags));
end;

{*****************************************************************}
procedure TGoogleTalk.Send_GetNewMail(const OnlyUnreadCount: Boolean; const ResetLastTime: Boolean = False);
var sAtts: string;
    iSeqType: DWord;
begin
  sAtts := XmlAtt(XATT_XMLNS, XNS_GT_MAIL_NOTIFY);

  if ResetLastTime then
  begin
    FCurMailTid  := '';
    FCurMailTime := '';
  end;

  if (FCurMailTime <> '') then
    sAtts := sAtts + THE_SPC + XmlAtt(XATT_GT_NEWER_THAN_TIME, FCurMailTime);

  if (FCurMailTid <> '') then
    sAtts := sAtts + THE_SPC + XmlAtt(XATT_GT_NEWER_THAN_TID, FCurMailTid);

  if OnlyUnreadCount then
    iSeqType := SEQT_GTALK_GET_UNREAD_MAIL
  else
    iSeqType := SEQT_GTALK_GET_NEW_MAIL;

  FSocket.Send(XmlTagIq(XVAL_GET, FSocket.NewSeqNum(iSeqType), MakeTag(XNS_QUERY, '', sAtts)) );
end;

{*****************************************************************}
procedure TGoogleTalk.Rcvd_NewMail(XmlNode: TXmlNode; var MailNotif: TExtMailNotif; const OnlyUnreadCount: Boolean);
var Node: TXmlNode;
    i: integer;
begin
  Finalize(MailNotif);
  ZeroMemory(@MailNotif, SizeOf(TExtMailNotif));

  FCurMailTime := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_GT_RESULT_TIME)];

  MailNotif.TotalUnread := StrToIntDef(XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_GT_TOTAL_MATCHED)], 0);

  if not OnlyUnreadCount then
    MailNotif.ClickUrl := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_GT_URL)];

  for i := 0 to XmlNode.ChildContainerCount-1 do
  begin
    Node := XmlNode.ChildContainers[i];

    //find first mail thread info tag
    if WideSameText(UTF8ToUnicodeString(Node.Name), XNS_GT_MAIL_THREAD_INFO) then
    begin
      //save first msg tid
      if (i = 0) then
      begin
        FCurMailTid := Node.AttributeValueByNameWide[UTF8Encode(XATT_GT_TID)];
        if not OnlyUnreadCount then
        begin
          if (Node.AttributeValueByNameWide[UTF8Encode(XATT_GT_URL)] <> '') then
            MailNotif.ClickUrl := ConvertTidToHex(FCurMailTid, Node.AttributeValueByNameWide[UTF8Encode(XATT_GT_URL)]);

          GetMailMsg(Node, MailNotif);
        end;

        Break;
      end;
    end;
  end;//for
end;

{*****************************************************************}
procedure TGoogleTalk.GetMailMsg(XmlNode: TXmlNode; var MailNotif: TExtMailNotif);
var Node: TXmlNode;
    i: integer;
begin
  Node := XmlNode.NodeByName(UTF8Encode(XNS_GT_SENDERS));
  if (Node <> nil) then
  begin
    for i := 0 to Node.ChildContainerCount-1 do
    begin
      if WideSameText(UTF8ToUnicodeString(Node.ChildContainers[i].Name), XNS_GT_SENDER) then
      begin
        if (StrToInt64Def(Node.ChildContainers[i].AttributeValueByNameWide[UTF8Encode(XATT_GT_UNREAD)], 0) > 0) then
        begin
          MailNotif.SenderName  := Node.ChildContainers[i].AttributeValueByNameWide[UTF8Encode(XATT_NAME)];
          MailNotif.SenderEmail := Node.ChildContainers[i].AttributeValueByNameWide[UTF8Encode(XATT_GT_ADDRESS)];

          Break;
        end;
      end;
    end;//for
  end;

 if (MailNotif.SenderEmail = '') and (MailNotif.SenderName = '') then Exit;

 MailNotif.NewMailRcvd := True;

  Node := XmlNode.NodeByName(UTF8Encode(XNS_SUBJECT));
  if (Node <> nil) then
    MailNotif.MailSubject := Node.ValueUnicode;

  Node := XmlNode.NodeByName(UTF8Encode(XNS_GT_SNIPPET));
  if (Node <> nil) then
    MailNotif.MailText := Node.ValueUnicode;
end;


end.
