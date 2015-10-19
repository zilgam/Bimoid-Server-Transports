// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_proto;

interface

uses Windows, Classes, SysUtils, u_ext_info, u_obimp_const, u_obimp_codes, u_params, h_jabber, u_jabber, OverbyteIcsMimeUtils,
     u_convert, {$IFDEF GTALK}u_gtalk,{$ENDIF} u_roster, u_roster_info, u_privacy, u_privacy_list;

type
  //Transport protocol class
  TXmpProto = class(TInterfacedObject, IExtensionTP)
  private
    FClient         : TJabberClient;
    FOpts           : TOptionsArray;
    FMoreInfo       : TExtMoreInfo;
    FMoreInfoGot    : Boolean;
    FFirstOptsRcvd  : Boolean;
    FTimerMsecs     : Integer;
    procedure GetObimpClientInfo;
    procedure SendOwnPresInfo(const ReqID: DWord);
    procedure LoadContacts;
    procedure FillContactAtts(var tc: TTranspContact; aItem: pRosterItem; aList: TPrivacyList);
    procedure CheckPrivUpdContacts;
    procedure CheckIgnoredNilUpd;
  protected
    {$IFDEF DEBUG}
    procedure OnJabber_PktLog(Sender: TObject; const Incoming: Boolean; const XmlTag: string);
    {$ENDIF}
    procedure OnJabber_LoginDone(Sender: TObject);
    procedure OnJabber_LoginFailed(Sender: TObject; const ErrMsg: string);
    procedure OnJabber_CntArrived(Sender: TObject; aItem: pRosterItem; const Resource: string; OnlyAvatarChanged: Boolean);
    procedure OnJabber_CntDeparted(Sender: TObject; aItem: pRosterItem; const Resource: string; OnlyAvatarChanged: Boolean);
    procedure OnJabber_PrivacyChanged(Sender: TObject);
    procedure OnJabber_RosterChanged(Sender: TObject; aItem: pRosterItem; RosterOper: Byte);
    procedure OnJabber_AuthRequest(Sender: TObject; const Jid: string);
    procedure OnJabber_AuthReply(Sender: TObject; const Jid: string; const Granted: Boolean);
    procedure OnJabber_AuthRevoke(Sender: TObject; const Jid: string);
    procedure OnJabber_IncMsg(Sender: TObject; const IncMsg: TXmppIncMsg);
    procedure OnJabber_MsgAck(Sender: TObject; const XmppAck: TXmppMsgAck);
    procedure OnJabber_ChatState(Sender: TObject; const ChatState: TXmppChatState);
    procedure OnJabber_VCardRcvd(Sender: TObject; const vCard: TExtUserDets; const Found: Boolean);
    procedure OnJabber_VCardUpdRes(Sender: TObject; const vCard: TExtUserDets; const Success: Boolean);
    procedure OnJabber_AvatarUpdRes(Sender: TObject; const ReqID: DWord; const Success: Boolean);
    procedure OnJabber_AvatarRcvd(Sender: TObject; const Jid: string; const avik: TAvatarRec);
    procedure OnJabber_Notify(Sender: TObject; const NotifType: Word; const JidSender, Msg, ClickUrl: string);
    procedure OnJabber_MailNotify(Sender: TObject; MailNotif: TExtMailNotif);
    procedure OnJabber_MailUrlDone(Sender: TObject; const MailUrl: string; const ReqID: DWord);
    procedure OnJabber_OwnPresInfo(Sender: TObject);
  public
    UniqID     : DWord;
    EventsTP   : IEventsTP;

    constructor Create(AUniqID: DWord; AEventsTP: IEventsTP);
    destructor  Destroy; override;

    {==== Interface functions ====}
    procedure TimerTick; stdcall;
    procedure GetTransportOpts(var ExtOpts: TExtOptions); stdcall;
    function  TransportSettings(ExtTranspSet: pExtTranspSettings): LongBool; stdcall;
    procedure TransportManage(ExtTranspMan: pExtTranspManage); stdcall;
    procedure AddContact(TranspContact: pTranspContact); stdcall;
    procedure UpdContact(TranspContact: pTranspContact); stdcall;
    procedure DelContact(TranspContact: pTranspContact); stdcall;
    procedure SendAuthRequest(AuthMsg: pExtAuthMsg); stdcall;
    procedure SendAuthReply(AuthMsg: pExtAuthMsg); stdcall;
    procedure SendAuthRevoke(AuthMsg: pExtAuthMsg); stdcall;
    procedure SendInstantMsg(InstantMsg: pExtMsg); stdcall;
    procedure SendMsgAck(MsgAck: pExtMsgAck); stdcall;
    procedure SendNotif(ExtNotif: pExtNotif); stdcall;
    procedure DetailsRequest(ExtDetsReq: pExtDetsReq); stdcall;
    procedure SearchRequest(ExtSrchReq: pExtSrchReq); stdcall;
    procedure OwnDetailsUpdate(ExtUserDets: pExtUserDets); stdcall;
    procedure OwnAvatarSet(AvatarPic: pExtAvatarPic); stdcall;
    procedure OwnPresInfoRequest(ReqOwnPresInfo: pExtReqOwnPresInfo); stdcall;
    procedure OwnMailUrlRequest(ReqOwnMailUrl: pExtReqOwnMailUrl); stdcall;
  end;

implementation

{ TXmpProto }
{***************************************************************}
constructor TXmpProto.Create(AUniqID: DWord; AEventsTP: IEventsTP);
begin
  //save UniqID for events interface callbacks
  UniqID   := AUniqID;
  //save events interface, will be used for protocol callbacks
  EventsTP := AEventsTP;

  //create client
  FClient  := TJabberClient.Create;
  {$IFDEF DEBUG}
  FClient.OnPktLog          := OnJabber_PktLog;
  {$ENDIF}
  FClient.OnLoginDone       := OnJabber_LoginDone;
  FClient.OnLoginFailed     := OnJabber_LoginFailed;
  FClient.OnCntArrived      := OnJabber_CntArrived;
  FClient.OnCntDeparted     := OnJabber_CntDeparted;
  FClient.OnPrivacyChanged  := OnJabber_PrivacyChanged;
  FClient.OnAuthRequest     := OnJabber_AuthRequest;
  FClient.OnAuthReply       := OnJabber_AuthReply;
  FClient.OnAuthRevoke      := OnJabber_AuthRevoke;
  FClient.OnIncMsg          := OnJabber_IncMsg;
  FClient.OnMsgAck          := OnJabber_MsgAck;
  FClient.OnChatState       := OnJabber_ChatState;
  FClient.OnVCardRcvd       := OnJabber_VCardRcvd;
  FClient.OnVCardUpdRes     := OnJabber_VCardUpdRes;
  FClient.OnAvatarUpdRes    := OnJabber_AvatarUpdRes;
  FClient.OnAvatarRcvd      := OnJabber_AvatarRcvd;
  FClient.OnJabberNotify    := OnJabber_Notify;
  FClient.OnMailNotify      := OnJabber_MailNotify;
  FClient.OnMailUrlDone     := OnJabber_MailUrlDone;
  FClient.OnOwnPresInfo     := OnJabber_OwnPresInfo;

  FClient.Roster.OnRosterChanged := OnJabber_RosterChanged;
end;

{***************************************************************}
destructor TXmpProto.Destroy;
begin
  FClient.Logoff;
  FClient.Free;

  inherited;
end;

{***************************************************************}
procedure TXmpProto.TimerTick;
begin
  {$IFDEF GTALK}
  //we have TAuthThread in u_gtalk_auth, manual synchronization required for DLL
  CheckSynchronize;
  {$ENDIF}

  //pass below only full 1 second, not 100 msecs
  Inc(FTimerMsecs);
  if (FTimerMsecs < 10) then Exit;
  FTimerMsecs := 0;

  if (FClient.ConnectState in [csConnecting, csAuthDone]) then
  begin
    //use timer ticks to check connection timeout
    if (FClient.ConnectTicks >= (CONNECT_ATTEMPT_TIMEOUT-1)) then
    begin
      FClient.ConnectTicks := 0;
      FClient.Logoff;

      EventsTP.TransportStateChanged(UniqID, TP_STATE_CON_FAILED);
    end
    else
      FClient.ConnectTicks := FClient.ConnectTicks + 1;
  end;

  if (FClient.ConnectState = csOnline) then
  begin
    //use timer ticks to send keep alives
    if (FClient.AliveTicks >= (XMPP_KEEP_ALIVE_INTERVAL-1)) then
    begin
      FClient.AliveTicks := 0;
      FClient.SendKeepAlive;
    end
    else
      FClient.AliveTicks := FClient.AliveTicks + 1;
  end;
end;

{***************************************************************}
procedure TXmpProto.GetTransportOpts(var ExtOpts: TExtOptions);
var i, j: integer;
begin
  //GetTransportOpts called once on instance creation.

  //get more info once
  GetObimpClientInfo;

  //get def opts
  FOpts := ExtDefOpts;

  //set language strings
  for i := 0 to Length(FOpts)-1 do
  begin
    case FOpts[i].OptID of
     {===============================================}
      OPT_ID_RESOURCE_NAME  :
        begin
          FOpts[i].OptName := GetLI(FMoreInfo.LangCode, LI_ADDITIONAL) + OPT_DIV +
                              GetLI(FMoreInfo.LangCode, LI_SESSION) + OPT_DIV +
                              GetLI(FMoreInfo.LangCode, LI_RESOURCE);
        end;
     {===============================================}
      OPT_ID_PRIORITY       :
        begin
          FOpts[i].OptName := GetLI(FMoreInfo.LangCode, LI_ADDITIONAL) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_SESSION) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_PRIORITY) + VAL_DIV;

          for j := -128 to 127 do FOpts[i].OptName := FOpts[i].OptName + IntToStr(j) + VAL_DIV;
        end;
     {===============================================}
      {$IFDEF GTALK}
     {===============================================}
      OPT_ID_SAVE_HIST  :
        begin
          FOpts[i].OptName := GetLI(FMoreInfo.LangCode, LI_ADDITIONAL) + OPT_DIV +
                              GetLI_GTalk(FMoreInfo.LangCode, LI_GT_GOOGLE_TALK) + OPT_DIV +
                              GetLI_GTalk(FMoreInfo.LangCode, LI_GT_SAVE_CHAT_HIST);
        end;
     {===============================================}
      OPT_ID_MAIL_NOTIF :
        begin
          FOpts[i].OptName := GetLI(FMoreInfo.LangCode, LI_ADDITIONAL) + OPT_DIV +
                              GetLI_GTalk(FMoreInfo.LangCode, LI_GT_GOOGLE_TALK) + OPT_DIV +
                              GetLI_GTalk(FMoreInfo.LangCode, LI_GT_MAIL_NOTIF);
        end;
     {===============================================}
      {$ENDIF}
     {===============================================}
    end;//case
  end;//for

  {$IFDEF XMPP}
  ExtOpts.SettingsFlags := 0;
  {$ENDIF}

  {$IFDEF GTALK}
  ExtOpts.SettingsFlags := TP_SF_HIDE_SRV_PARAMS;
  {$ENDIF}

  ExtOpts.OptionsCount  := Length(FOpts);
  ExtOpts.OptionsArray  := @FOpts[0];
end;

{***************************************************************}
function TXmpProto.TransportSettings(ExtTranspSet: pExtTranspSettings): LongBool;
var bReconnectRequired, bUpdateStatus: Boolean;
    i: integer;
    iVal: ShortInt;
    sni: TExtShowNotifInfo;
    {$IFDEF GTALK}
    bUpdateSettings, bOpt: Boolean;
    mn: TExtMailNotif;
    {$ENDIF}
begin
  Result := True;

  bReconnectRequired := False;
  bUpdateStatus      := False;
  {$IFDEF GTALK}
  bUpdateSettings    := False;
  {$ENDIF}

  with FClient do
  begin
    {$IFDEF XMPP}
    Server   := Trim(ExtTranspSet^.SrvHost);
    Port     := IntToStr(ExtTranspSet^.SrvPort);
    Username := ExtTranspSet^.Account;
    Password := ExtTranspSet^.Password;

    SecureConnect  := False;
    UseCompression := True;
    UseStartTLS    := True;
    {$ENDIF}

    {$IFDEF GTALK}
    Server   := SRV_HOST_GTALK;
    Port     := IntToStr(SRV_PORT_GTALK);
    Username := ExtTranspSet^.Account;
    Password := ExtTranspSet^.Password;

    SecureConnect  := False;
    UseCompression := True;
    UseStartTLS    := True;
    {$ENDIF}
  end;//with

  //get options
  if (ExtTranspSet^.Options.OptionsCount > 0) then
  begin
    for i := 0 to ExtTranspSet^.Options.OptionsCount-1 do
    begin
      with ExtTranspSet^.Options do
      case OptionsArray^[i].OptID of
       {======================================================}
        OPT_ID_RESOURCE_NAME :
          begin
            if not bReconnectRequired then
              bReconnectRequired := FFirstOptsRcvd and (FClient.ConnectState <> csOffline) and (FClient.Resource <> OptionsArray^[i].OptValue);

            FClient.Resource := OptionsArray^[i].OptValue;
          end;
       {======================================================}
        OPT_ID_PRIORITY      :
          begin
            iVal := -128 + GetOptByte(OptionsArray^[i].OptValue);
            if FFirstOptsRcvd then
              bUpdateStatus := FClient.Priority <> iVal;

            FClient.Priority := iVal;
          end;
       {======================================================}
        {$IFDEF GTALK}
       {======================================================}
        OPT_ID_SAVE_HIST     :
          begin
            bOpt := GetOptBool(OptionsArray^[i].OptValue);
            if (bOpt <> FClient.GTalk.Settings.ArchivingEnabled) then
            begin
              FClient.GTalk.Settings.ArchivingEnabled := bOpt;
              if FFirstOptsRcvd then
                bUpdateSettings := True;
            end;
          end;
       {======================================================}
        OPT_ID_MAIL_NOTIF    :
          begin
            FClient.GTalk.Settings.MailNotifications := GetOptBool(OptionsArray^[i].OptValue);
          end;
       {======================================================}
        {$ENDIF}
       {======================================================}
      end;//case
    end;//for
  end;

  {$IFDEF GTALK}
  if bUpdateSettings and (FClient.ConnectState = csOnline) then
    FClient.GTalk.Send_SetUserSettings(FClient.Username, False);

  if FFirstOptsRcvd and (FClient.ConnectState = csOnline) then
  begin
    if FClient.GTalk.Settings.MailNotifications then
      FClient.GTalk.Send_GetNewMail(True, True)
    else
    begin
      ZeroMemory(@mn, SizeOf(TExtMailNotif));
      OnJabber_MailNotify(FClient, mn);
    end;
  end;
  {$ENDIF}

  if bUpdateStatus and (FClient.ConnectState = csOnline) then
  begin
    FClient.UpdateStatus(FClient.StatusShow, FClient.StatusAdvText);
    SendOwnPresInfo(0);
  end;

  if bReconnectRequired then
  begin
    //show recconect required notification on client side
    ZeroMemory(@sni, SizeOf(TExtShowNotifInfo));

    sni.AutoClose   := True;
    sni.IsWarning   := False;
    sni.TitleText   := GetLI(FMoreInfo.LangCode, LI_INFORMATION);
    sni.ContentText := GetLI(FMoreInfo.LangCode, LI_TRANSPORT) + THE_SPC + ExtParams.AccountIDsName + THE_SPC +
                       FClient.Username + THE_CLN + THE_SPC +
                       GetLI(FMoreInfo.LangCode, LI_RECONNECT_REQUIRED);

    EventsTP.ShowNotificaton(UniqID, @sni);
  end;

  FFirstOptsRcvd := True;
end;

{***************************************************************}
procedure TXmpProto.GetObimpClientInfo;
var ProxyInfo: TExtProxyInfo;
begin
  if FMoreInfoGot then Exit;
  FMoreInfoGot := True;

  ZeroMemory(@FMoreInfo, SizeOf(TExtMoreInfo));
  EventsTP.GetMoreInfo(UniqID, FMoreInfo);

  FClient.ClientName  := FMoreInfo.ClientName;
  FClient.ClientVer   := Int64ToVer(FMoreInfo.ClientVer);
  FClient.ClientLang  := FMoreInfo.LangCode;
  FClient.ClientOS    := FMoreInfo.ClientOS;

  //get proxy info
  ZeroMemory(@ProxyInfo, SizeOf(TExtProxyInfo));
  EventsTP.GetProxyInfo(UniqID, ProxyInfo);

  FClient.ProxyInfo := ProxyInfo;
end;

{***************************************************************}
procedure TXmpProto.SendOwnPresInfo(const ReqID: DWord);
var opi: TExtOwnPresenceInfo;
begin
  ZeroMemory(@opi, SizeOf(TExtOwnPresenceInfo));
  opi.ReqID := ReqID;

  opi.CurInstCount  := FClient.OwnPresItem.GetResCount;
  opi.SrvAddedDescr := GetResourceDescr(@FClient.OwnPresItem, FMoreInfo.LangCode);
{
  if (FClient.CurResource <> '') then
    opi.SrvAddedDescr := '<L=606>Resource</L>: ' + FClient.CurResource + CRLF +
                         '<L=727>Priority</L>: ' + IntToStr(FClient.Priority);}


  EventsTP.OwnPresenceInfoRcvd(UniqID, @opi);
end;

{***************************************************************}
procedure TXmpProto.TransportManage(ExtTranspMan: pExtTranspManage);
var sStatus: string;
begin
  case ExtTranspMan^.ManCode of
   {===================================================}
    TP_CON_MAN_CONNECT,
    TP_CON_MAN_STATUS:
      begin
        ConvObimpStatus(ExtTranspMan^.Status, sStatus);

        //this params can be always updated if offline, they will be saved in proto
        FClient.UpdateStatus(sStatus);

        //login or reply
        if (ExtTranspMan^.ManCode = TP_CON_MAN_CONNECT) then
        begin
          //login if offline
          if (FClient.ConnectState = csOffline) then
            FClient.Login;
        end
        else
        begin
          //notify server about status update only if online
          if (FClient.ConnectState = csOnline) then
            EventsTP.TransportStateChanged(UniqID, TP_STATE_STATUS_CHANGED);
        end;
      end;
   {===================================================}
    TP_CON_MAN_DISCONNECT:
      begin
        //logoff client first
        FClient.Logoff;

        //notify server
        EventsTP.TransportStateChanged(UniqID, TP_STATE_LOGGEDOFF);
      end;
   {===================================================}
  end;//case
end;

{***************************************************************}
procedure TXmpProto.AddContact(TranspContact: pTranspContact);
var aItem: pRosterItem;
    {$IFDEF GTALK}
    sGTalk_T: string;
    {$ENDIF}
begin
  {$IFDEF XMPP}
  if (TranspContact^.PrivacyType = CL_PRIV_TYPE_IGNORE_NOT_IN_LIST) then
  begin
    //add to ignore item if not exists
    if FClient.Privacy.Supported then
    begin
      if FClient.Privacy.Bim_UpdJidPrivItem(LIST_BIMOID, TranspContact^.AccountName, PRIV_BIM_IGNORE) then
        FClient.Privacy.Bim_SendUpdateList(LIST_BIMOID, FClient.StatusShow);
    end;

    aItem := FClient.Roster.GetRosterItem(TranspContact^.AccountName);
    //delete contact item from CL if exists
    if (aItem <> nil) then
      FClient.Roster.Send_DelRosterItem(aItem^.JID);
  end
  else
  begin
    //update privacy lists if needed
    if FClient.Privacy.Supported then
    begin
      if FClient.Privacy.Bim_UpdJidPrivItem(LIST_BIMOID, TranspContact^.AccountName, PRIV_BIM_NONE) then
        FClient.Privacy.Bim_SendUpdateList(LIST_BIMOID, FClient.StatusShow);
    end;

    //add new contact
    FClient.Roster.Send_AddRosterItem(TranspContact^.AccountName, TranspContact^.ContactName, TranspContact^.GroupName);
  end;
  {$ENDIF}

  {$IFDEF GTALK}
  //add new contact
  sGTalk_T := '';
  if (TranspContact^.PrivacyType = CL_PRIV_TYPE_IGNORE_NOT_IN_LIST) then sGTalk_T := GT_BLOCKED;

  aItem := FClient.Roster.GetRosterItem(TranspContact^.AccountName);
  if (aItem <> nil) then
    FClient.Roster.Send_UpdRosterItem(aItem^.JID, aItem^.Name, aItem^.GetFirstGroupName, sGTalk_T)
  else
    FClient.Roster.Send_AddRosterItem(TranspContact^.AccountName, TranspContact^.ContactName, '', sGTalk_T);
  {$ENDIF}
end;

{***************************************************************}
procedure TXmpProto.UpdContact(TranspContact: pTranspContact);
var aItem: pRosterItem;
    PrivType: Byte;
    {$IFDEF GTALK}
    sGTalk_T: string;
    {$ENDIF}
begin
  PrivType := PRIV_BIM_NONE;

  case TranspContact^.PrivacyType of
    CL_PRIV_TYPE_NONE               : PrivType := PRIV_BIM_NONE;
    CL_PRIV_TYPE_VISIBLE_LIST       : PrivType := PRIV_BIM_VISIBLE;
    CL_PRIV_TYPE_INVISIBLE_LIST     : PrivType := PRIV_BIM_INVISIBLE;
    CL_PRIV_TYPE_IGNORE_LIST,
    CL_PRIV_TYPE_IGNORE_NOT_IN_LIST : PrivType := PRIV_BIM_IGNORE;
  end;//case

  aItem := FClient.Roster.GetRosterItem(TranspContact^.AccountName);

  //if contact doesnt exist (if it was moved to ignore and now returned back to cl (Bimoid IM doesnt do it))
  if (aItem = nil) then
  begin
    if (TranspContact^.PrivacyType <> CL_PRIV_TYPE_IGNORE_NOT_IN_LIST) then
    begin
      if FClient.Privacy.Supported then
      begin
        if FClient.Privacy.Bim_UpdJidPrivItem(LIST_BIMOID, TranspContact^.AccountName, PrivType) then
          FClient.Privacy.Bim_SendUpdateList(LIST_BIMOID, FClient.StatusShow);
      end;

      //return contact to list
      {$IFDEF GTALK}
      FClient.Roster.Send_AddRosterItem(TranspContact^.AccountName, TranspContact^.ContactName, '', '');
      {$ENDIF}

      {$IFDEF XMPP}
      FClient.Roster.Send_AddRosterItem(TranspContact^.AccountName, TranspContact^.ContactName, TranspContact^.GroupName);
      {$ENDIF}
    end;
    Exit;
  end;

  {$IFDEF XMPP}
  //delete contact if ignore not in list
  if (TranspContact^.PrivacyType = CL_PRIV_TYPE_IGNORE_NOT_IN_LIST) then
    FClient.Roster.Send_DelRosterItem(aItem^.JID)
  else
  begin
    //update contacts name, ignore group changes
    if (aItem^.Name <> TranspContact^.ContactName) then
      FClient.Roster.Send_UpdRosterItem(aItem^.JID, TranspContact^.ContactName, aItem^.GetFirstGroupName);
  end;

  //update privacy lists if changed
  if FClient.Privacy.Supported then
  begin
    if FClient.Privacy.Bim_UpdJidPrivItem(LIST_BIMOID, TranspContact^.AccountName, PrivType) then
    begin
      FClient.Privacy.Bim_SendUpdateList(LIST_BIMOID, FClient.StatusShow);

      if (PrivType in [PRIV_BIM_INVISIBLE, PRIV_BIM_IGNORE]) then
        FClient.SendPresTo(aItem^.JID, PRES_TYPE_UNAVAILABLE)
      else
        FClient.UpdatePresence(aItem^.JID);
    end;
  end;
  {$ENDIF}

  {$IFDEF GTALK}
  sGTalk_T := FClient.GTalk.CheckItemPrivacy(aItem, PrivType);
  if (aItem^.Name <> TranspContact^.ContactName) or not WideSameText(aItem^.GTalk_T, sGTalk_T) then
  begin
    FClient.Roster.Send_UpdRosterItem(aItem^.JID, TranspContact^.ContactName, aItem^.GetFirstGroupName, sGTalk_T);
  end;
  {$ENDIF}
end;

{***************************************************************}
procedure TXmpProto.DelContact(TranspContact: pTranspContact);
var aItem: pRosterItem;
begin
  aItem := FClient.Roster.GetRosterItem(TranspContact^.AccountName);

  {$IFDEF XMPP}
  if (aItem <> nil) then
    FClient.Roster.Send_DelRosterItem(aItem^.JID);

  //delete privacy item if exist
  if FClient.Privacy.Supported then
  begin
    if FClient.Privacy.Bim_UpdJidPrivItem(LIST_BIMOID, TranspContact^.AccountName, PRIV_BIM_NONE) then
      FClient.Privacy.Bim_SendUpdateList(LIST_BIMOID, FClient.StatusShow);
  end;
  {$ENDIF}

  {$IFDEF GTALK}
  if (aItem <> nil) then
  begin
    //add ignored to CL else delete item
    if WideSameText(aItem^.GTalk_T, GT_BLOCKED) then
      FClient.Roster.Send_UpdRosterItem(aItem^.JID, TranspContact^.ContactName, aItem^.GetFirstGroupName, '')
    else
    //set hidden flag
      FClient.Roster.Send_UpdRosterItem(aItem^.JID, TranspContact^.ContactName, aItem^.GetFirstGroupName, GT_HIDDEN);
  end;
  {$ENDIF}
end;

{***************************************************************}
procedure TXmpProto.SendAuthRequest(AuthMsg: pExtAuthMsg);
begin
  FClient.SendPresTo(AuthMsg^.Account, PRES_TYPE_SUBSCRIBE);
end;

{***************************************************************}
procedure TXmpProto.SendAuthReply(AuthMsg: pExtAuthMsg);
begin
  case AuthMsg^.AuthReplyCode of
    AUTH_REPLY_GRANTED : FClient.SendPresTo(AuthMsg^.Account, PRES_TYPE_SUBSCRIBED);
    AUTH_REPLY_DENIED  : FClient.SendPresTo(AuthMsg^.Account, PRES_TYPE_UNSUBSCRIBED);
  end;//case
end;

{***************************************************************}
procedure TXmpProto.SendAuthRevoke(AuthMsg: pExtAuthMsg);
begin
  FClient.SendPresTo(AuthMsg^.Account, PRES_TYPE_UNSUBSCRIBED);
end;

{***************************************************************}
procedure TXmpProto.SendInstantMsg(InstantMsg: pExtMsg);
var XmppMsg: TXmppOutMsg;
begin
  ConvObimpOutMsg(FClient, InstantMsg, XmppMsg);
  FClient.SendMsg(XmppMsg);
end;

{***************************************************************}
procedure TXmpProto.SendMsgAck(MsgAck: pExtMsgAck);
var XmppAck: TXmppMsgAck;
begin
  if ConvObimpMsgAck(FClient, MsgAck, XmppAck) then
    FClient.SendMsgAck(XmppAck);
end;

{***************************************************************}
procedure TXmpProto.SendNotif(ExtNotif: pExtNotif);
var ChatState: TXmppChatState;
begin
  if (ExtNotif^.NotifType = NOTIF_TYPE_USER_TYPING) then
  begin
    if ConvObimpNotif(FClient, ExtNotif, ChatState) then
      FClient.SendChatState(ChatState);
  end;
end;

{***************************************************************}
procedure TXmpProto.DetailsRequest(ExtDetsReq: pExtDetsReq);
var vCard: TExtUserDets;
begin
  if not FClient.RequestVCard(ExtDetsReq^.Account, ExtDetsReq^.ReqID) then
  begin
    ZeroMemory(@vCard, SizeOf(TExtUserDets));

    vCard.Account := ExtDetsReq^.Account;
    vCard.ReqID   := ExtDetsReq^.ReqID;

    EventsTP.DetailsReply(UniqID, @vCard, DETAILS_RES_SERVICE_TEMP_UNAVAILABLE);
  end;
end;

{***************************************************************}
procedure TXmpProto.SearchRequest(ExtSrchReq: pExtSrchReq);
begin

end;

{***************************************************************}
procedure TXmpProto.OwnDetailsUpdate(ExtUserDets: pExtUserDets);
var odur: TExtOwnDetsUpdReply;
begin
  if not FClient.UpdateVCard(ExtUserDets^, ExtUserDets^.ReqID) then
  begin
    ZeroMemory(@odur, SizeOf(TExtOwnDetsUpdReply));

    odur.UpdResultCode := UPD_DETAILS_RES_SERVICE_TEMP_UNAVAILABLE;
    odur.ReqID         := ExtUserDets^.ReqID;

    EventsTP.OwnDetailsUpdReply(UniqID, @odur);
  end;
end;

{***************************************************************}
procedure TXmpProto.OwnAvatarSet(AvatarPic: pExtAvatarPic);
var oasr: TExtOwnAvatarSetReply;
begin
  //if set new avatar then get avatar file data
  if (AvatarPic^.PicData <> nil) and (AvatarPic^.PicDataLen > 0) then
  begin
    if not FClient.UpdateAvatar(BIN_IMG_TYPE_PNG, Base64Encode(AvatarPic^.PicData, AvatarPic^.PicDataLen), AvatarPic^.ReqID) then
    begin
      ZeroMemory(@oasr, SizeOf(TExtOwnAvatarSetReply));

      oasr.SetResultCode := AVATAR_SET_TEMP_UNAVAILABLE;
      oasr.ReqID         := AvatarPic^.ReqID;
      EventsTP.OwnAvatarSetReply(UniqID, @oasr);
    end;
  end
  else
  begin
    //if avatar removed
    if not FClient.UpdateAvatar('', '', AvatarPic^.ReqID) then
    begin
      ZeroMemory(@oasr, SizeOf(TExtOwnAvatarSetReply));

      oasr.SetResultCode := AVATAR_SET_TEMP_UNAVAILABLE;
      oasr.ReqID         := AvatarPic^.ReqID;
      EventsTP.OwnAvatarSetReply(UniqID, @oasr);
    end;
  end;
end;

{***************************************************************}
procedure TXmpProto.OwnPresInfoRequest(ReqOwnPresInfo: pExtReqOwnPresInfo);
begin
  //will not be called, because params set to False
  //SendOwnPresInfo(ReqOwnPresInfo^.ReqID);
end;

{***************************************************************}
procedure TXmpProto.OwnMailUrlRequest(ReqOwnMailUrl: pExtReqOwnMailUrl);
begin
  {$IFDEF GTALK}
  FClient.GTalk.GAuth.IssueAuthTokenThread(FClient.Username, FClient.Password, True, nil, ReqOwnMailUrl^.ReqID);
  {$ENDIF}
end;

{***************************************************************}
{$IFDEF DEBUG}
procedure TXmpProto.OnJabber_PktLog(Sender: TObject; const Incoming: Boolean; const XmlTag: string);
begin
  if Incoming then
    OutDebugStr('RECV: ' + XmlTag)
  else
    OutDebugStr('SEND: ' + XmlTag);
end;
{$ENDIF}

{***************************************************************}
procedure TXmpProto.OnJabber_LoginDone(Sender: TObject);
begin
  //client should load contacts only once per session
  LoadContacts;

  //notify server about loggedin
  EventsTP.TransportStateChanged(UniqID, TP_STATE_LOGGEDIN);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_LoginFailed(Sender: TObject; const ErrMsg: string);
begin
  EventsTP.TransportStateChanged(UniqID, ConvProtoConErrorCode(ErrMsg));
end;

{***************************************************************}
procedure TXmpProto.OnJabber_CntArrived(Sender: TObject; aItem: pRosterItem; const Resource: string; OnlyAvatarChanged: Boolean);
var cs: TExtContactStatus;
begin
  if not ConvProtoBuddyOnline(aItem, Resource, cs, FMoreInfo.LangCode) then Exit;

  //avatar check
  if ( (cs.AvatarHashHex <> '') and not EventsTP.AvatarExists(UniqID, PWideChar(cs.AvatarHashHex)) ) or
     ( (cs.AvatarHashHex = '') and (aItem^.AvatarSha1Hex <> '') ) then
  begin
    cs.AvatarHashHex := '';

    if not aItem^.AvatarWaiting then
    begin
      //set flag to prevent requests duplicates (for example if more than 1 resource connected of same jid)
      aItem^.AvatarWaiting := True;
      FClient.RequestAvatar(aItem^.JID);
    end;

    //if avatar not exist and only avatar changed then dont send ContactOnline
    if OnlyAvatarChanged then Exit;
  end;

  {$IFDEF GTALK}
  if WideSameText(aItem^.GTalk_T, GT_HIDDEN) then Exit;
  {$ENDIF}

  EventsTP.ContactOnline(UniqID, @cs);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_CntDeparted(Sender: TObject; aItem: pRosterItem; const Resource: string; OnlyAvatarChanged: Boolean);
var cs: TExtContactStatus;
begin
  ConvProtoBuddyOffline(aItem, cs);

  {$IFDEF GTALK}
  if WideSameText(aItem^.GTalk_T, GT_HIDDEN) then Exit;
  {$ENDIF}

  EventsTP.ContactOffline(UniqID, @cs);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_PrivacyChanged(Sender: TObject);
begin
  CheckPrivUpdContacts;
  CheckIgnoredNilUpd;
end;

{***************************************************************}
procedure TXmpProto.LoadContacts;
var etc: TExtTranspContacts;
    tc: array of TTranspContact;
    aItem: pRosterItem;
    aList: TPrivacyList;
    aPrivItem: pPrivacyItem;
    i: integer;
    List: TStringList;
begin
  aList := nil;

  if FClient.Privacy.Supported then
    aList := FClient.Privacy.GetList(LIST_BIMOID);

  //create accounts list for fast accs search
  List            := TStringList.Create;
  List.Sorted     := True;
  List.Duplicates := dupIgnore;
  try
  //create and fill contacts array
  aItem := nil;
  i := 0;
  while FClient.Roster.EnumItems(i, aItem) do
  begin
    {$IFDEF GTALK}
    if WideSameText(aItem^.GTalk_T, GT_HIDDEN) then
      Continue;
    {$ENDIF}

    SetLength(tc, Length(tc)+1);
    FillContactAtts(tc[Length(tc)-1], aItem, aList);
    List.Add(tc[Length(tc)-1].AccountName);
  end;//while

  //add ignored "not in list" items
  if (aList <> nil) then
  begin
    aPrivItem := nil;
    i := 0;
    while aList.EnumListItems(i, aPrivItem) do
    begin
      if (aPrivItem^.ItemType = PRIV_ITEM_JID) and (List.IndexOf(aPrivItem^.Value) < 0) then
      begin
        SetLength(tc, Length(tc)+1);

        with tc[Length(tc)-1] do
        begin
          AccountName := aPrivItem^.Value;
          ContactName := '';
          GroupName   := '';
          PrivacyType := CL_PRIV_TYPE_IGNORE_NOT_IN_LIST;
          AuthFlag    := False;

          FClient.Privacy.BimIgnNilList.Add(AccountName);
        end;//with
      end;
    end;//while
  end;

  finally
    List.Free;
  end;

  //send contacts to server
  etc.ContactsCount := Length(tc);

  //if there are no contacts then array can be nil
  if (etc.ContactsCount > 0) then
    etc.ContactsArray := @tc[0]
  else
    etc.ContactsArray := nil;

  //if needed to delete old contacts, then second parameter must be True
  EventsTP.AddContacts(UniqID, True, @etc);
end;

{***************************************************************}
procedure TXmpProto.FillContactAtts(var tc: TTranspContact; aItem: pRosterItem; aList: TPrivacyList);
var aPrivItem: pPrivacyItem;
begin
  ZeroMemory(@tc, SizeOf(TTranspContact));

  tc.AccountName := aItem^.JID;
  tc.ContactName := aItem^.Name;

  if (Length(aItem^.Groups) > 0) then
    tc.GroupName := aItem^.Groups[0];

  tc.PrivacyType := CL_PRIV_TYPE_NONE;
  tc.AuthFlag    := FClient.Roster.IsAuthRequired(aItem);

  if (aList <> nil) then
  begin
    aPrivItem := aList.GetPrivItem(PRIV_ITEM_JID, aItem^.JID);
    if (aPrivItem <> nil) then
    begin
      case FClient.Privacy.Bim_GetJidPrivacy(aPrivItem) of
        PRIV_BIM_VISIBLE    : tc.PrivacyType := CL_PRIV_TYPE_VISIBLE_LIST;
        PRIV_BIM_INVISIBLE  : tc.PrivacyType := CL_PRIV_TYPE_INVISIBLE_LIST;
        PRIV_BIM_IGNORE     : tc.PrivacyType := CL_PRIV_TYPE_IGNORE_LIST;
      end;
    end;
  end;

  {$IFDEF GTALK}
  if WideSameText(aItem^.GTalk_T, GT_BLOCKED) then
    tc.PrivacyType := CL_PRIV_TYPE_IGNORE_NOT_IN_LIST;
  {$ENDIF}

  //save privacy type
  aItem^.PrivType := tc.PrivacyType;
end;

{***************************************************************}
procedure TXmpProto.CheckPrivUpdContacts;
var aList: TPrivacyList;
    aItem: pRosterItem;
    aPrivItem: pPrivacyItem;
    i: integer;
    etc: TExtTranspContacts;
    tc: array of TTranspContact;
begin
  aList := FClient.Privacy.GetList(LIST_BIMOID);

  aItem := nil;
  i := 0;
  while FClient.Roster.EnumItems(i, aItem) do
  begin
    //if privacy list cleared totally
    if (aList = nil) then
    begin
      if (aItem^.PrivType in [CL_PRIV_TYPE_VISIBLE_LIST..CL_PRIV_TYPE_IGNORE_LIST]) then
      begin
        SetLength(tc, Length(tc)+1);
        FillContactAtts(tc[Length(tc)-1], aItem, aList);
      end;
    end
    else //if privacy changed only for some contacts
    begin
      aPrivItem := aList.GetPrivItem(PRIV_ITEM_JID, aItem^.JID);
      if (aPrivItem = nil) then
      begin
        if (aItem^.PrivType in [CL_PRIV_TYPE_VISIBLE_LIST..CL_PRIV_TYPE_IGNORE_LIST]) then
        begin
          SetLength(tc, Length(tc)+1);
          FillContactAtts(tc[Length(tc)-1], aItem, aList);
        end;
      end
      else
      begin
        case FClient.Privacy.Bim_GetJidPrivacy(aPrivItem) of
         {==========================================================}
          PRIV_BIM_NONE       :
            begin
              if (aItem^.PrivType <> CL_PRIV_TYPE_NONE) then
              begin
                SetLength(tc, Length(tc)+1);
                FillContactAtts(tc[Length(tc)-1], aItem, aList);
              end;
            end;
         {==========================================================}
          PRIV_BIM_VISIBLE    :
            begin
              if (aItem^.PrivType <> CL_PRIV_TYPE_VISIBLE_LIST) then
              begin
                SetLength(tc, Length(tc)+1);
                FillContactAtts(tc[Length(tc)-1], aItem, aList);
              end;
            end;
         {==========================================================}
          PRIV_BIM_INVISIBLE  :
            begin
              if (aItem^.PrivType <> CL_PRIV_TYPE_INVISIBLE_LIST) then
              begin
                SetLength(tc, Length(tc)+1);
                FillContactAtts(tc[Length(tc)-1], aItem, aList);
              end;
            end;
         {==========================================================}
          PRIV_BIM_IGNORE     :
            begin
              if (aItem^.PrivType <> CL_PRIV_TYPE_IGNORE_LIST) then
              begin
                SetLength(tc, Length(tc)+1);
                FillContactAtts(tc[Length(tc)-1], aItem, aList);
              end;
            end;
         {==========================================================}
        end;
      end;
    end;
  end;//while

  //send updated contacts to server
  etc.ContactsCount := Length(tc);
  if (etc.ContactsCount > 0) then
  begin
    etc.ContactsArray := @tc[0];
    EventsTP.UpdContacts(UniqID, @etc);
  end;
end;

{***************************************************************}
procedure TXmpProto.CheckIgnoredNilUpd;
var aList: TPrivacyList;
    sAcc: string;
    aPrivItem: pPrivacyItem;
    i: integer;
    etc1, etc2: TExtTranspContacts;
    tc1, tc2: array of TTranspContact;
begin
  aList := FClient.Privacy.GetList(LIST_BIMOID);

  //1. check for deleted ignored not in list contacts
  for i := FClient.Privacy.BimIgnNilList.Count-1 downto 0 do
  begin
    sAcc := FClient.Privacy.BimIgnNilList.Strings[i];

    if (aList = nil) then
    begin
      SetLength(tc1, Length(tc1)+1);
      with tc1[Length(tc1)-1] do
      begin
        AccountName := sAcc;
        ContactName := '';
        GroupName   := '';
        PrivacyType := CL_PRIV_TYPE_IGNORE_NOT_IN_LIST;
        AuthFlag    := False;
      end;//with

      FClient.Privacy.BimIgnNilList.Delete(i);
    end
    else
    begin
      aPrivItem := aList.GetPrivItem(PRIV_ITEM_JID, sAcc);

      if (aPrivItem = nil) or (FClient.Privacy.Bim_GetJidPrivacy(aPrivItem) <> PRIV_BIM_IGNORE) then
      begin
        SetLength(tc1, Length(tc1)+1);
        with tc1[Length(tc1)-1] do
        begin
          AccountName := sAcc;
          ContactName := '';
          GroupName   := '';
          PrivacyType := CL_PRIV_TYPE_IGNORE_NOT_IN_LIST;
          AuthFlag    := False;
        end;//with

        FClient.Privacy.BimIgnNilList.Delete(i);
      end;
    end;
  end;//for

  //send deleted ignored nil contacts to server
  etc1.ContactsCount := Length(tc1);
  if (etc1.ContactsCount > 0) then
  begin
    etc1.ContactsArray := @tc1[0];
    EventsTP.DelContacts(UniqID, @etc1);
  end;

  //2. check for added ignored not in list contacts
  if (aList <> nil) then
  begin
    aPrivItem := nil;
    i := 0;
    while aList.EnumListItems(i, aPrivItem) do
    begin
      if (aPrivItem^.ItemType = PRIV_ITEM_JID) and (FClient.Privacy.Bim_GetJidPrivacy(aPrivItem) = PRIV_BIM_IGNORE) and
        (FClient.Roster.GetRosterItem(aPrivItem^.Value) = nil) and (FClient.Privacy.BimIgnNilList.IndexOf(aPrivItem^.Value) < 0) then
      begin
        SetLength(tc2, Length(tc2)+1);

        with tc2[Length(tc2)-1] do
        begin
          AccountName := aPrivItem^.Value;
          ContactName := '';
          GroupName   := '';
          PrivacyType := CL_PRIV_TYPE_IGNORE_NOT_IN_LIST;
          AuthFlag    := False;

          FClient.Privacy.BimIgnNilList.Add(AccountName);
        end;//with
      end;
    end;//while
  end;

  //send added ignored nil contacts to server
  etc2.ContactsCount := Length(tc2);
  if (etc2.ContactsCount > 0) then
  begin
    etc2.ContactsArray := @tc2[0];
    EventsTP.AddContacts(UniqID, False, @etc2);
  end;
end;

{***************************************************************}
procedure TXmpProto.OnJabber_RosterChanged(Sender: TObject; aItem: pRosterItem; RosterOper: Byte);
var etc: TExtTranspContacts;
    tc: array of TTranspContact;
begin
  SetLength(tc, 1);
  FillContactAtts(tc[0], aItem, FClient.Privacy.GetList(LIST_BIMOID));

  etc.ContactsCount := 1;
  etc.ContactsArray := @tc[0];

  //send changes
  case RosterOper of
   {=====================================================}
    ROSTER_OPER_ADD:
      begin
        EventsTP.AddContacts(UniqID, False, @etc);

        //manually notify if has any online resource
        if (aItem^.GetResCount > 0) then
          OnJabber_CntArrived(Self, aItem, aItem^.ResArray[0].Resource, False);
      end;
   {=====================================================}
    ROSTER_OPER_UPD: EventsTP.UpdContacts(UniqID, @etc);
   {=====================================================}
    ROSTER_OPER_DEL:
      begin
        {$IFDEF XMPP}
        //if contacts was removed and it was ignored then move it to ignore not in list
        if (tc[0].PrivacyType = CL_PRIV_TYPE_IGNORE_LIST) then
        begin
          tc[0].PrivacyType := CL_PRIV_TYPE_IGNORE_NOT_IN_LIST;
          EventsTP.UpdContacts(UniqID, @etc);
        end
        else
          EventsTP.DelContacts(UniqID, @etc);
        {$ENDIF}

        {$IFDEF GTALK}
        EventsTP.DelContacts(UniqID, @etc);
        {$ENDIF}
      end;
   {=====================================================}
  end;//case
end;

{***************************************************************}
procedure TXmpProto.OnJabber_AuthRequest(Sender: TObject; const Jid: string);
var ExtAuthMsg: TExtAuthMsg;
begin
  ZeroMemory(@ExtAuthMsg, SizeOf(TExtAuthMsg));
  ExtAuthMsg.Account := Jid;
  EventsTP.AuthRequestRcvd(UniqID, @ExtAuthMsg);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_AuthReply(Sender: TObject; const Jid: string; const Granted: Boolean);
var ExtAuthMsg: TExtAuthMsg;
begin
  ZeroMemory(@ExtAuthMsg, SizeOf(TExtAuthMsg));

  ExtAuthMsg.Account := Jid;

  if Granted then
    ExtAuthMsg.AuthReplyCode := AUTH_REPLY_GRANTED
  else
    ExtAuthMsg.AuthReplyCode := AUTH_REPLY_DENIED;

  EventsTP.AuthReplyRcvd(UniqID, @ExtAuthMsg);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_AuthRevoke(Sender: TObject; const Jid: string);
var ExtAuthMsg: TExtAuthMsg;
begin
  ZeroMemory(@ExtAuthMsg, SizeOf(TExtAuthMsg));
  ExtAuthMsg.Account := Jid;
  EventsTP.AuthRevokeRcvd(UniqID, @ExtAuthMsg);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_IncMsg(Sender: TObject; const IncMsg: TXmppIncMsg);
var ExtMsg: TExtMsg;
begin
  ConvProtoIncMsg(FClient, IncMsg, ExtMsg);
  EventsTP.InstantMsgRcvd(UniqID, @ExtMsg);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_MsgAck(Sender: TObject; const XmppAck: TXmppMsgAck);
var MsgAck: TExtMsgAck;
begin
  if ConvProtoMsgAck(FClient, XmppAck, MsgAck) then
    EventsTP.MsgAckRcvd(UniqID, @MsgAck);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_ChatState(Sender: TObject; const ChatState: TXmppChatState);
var ExtNotif: TExtNotif;
begin
  if ConvProtoEventMsg(ChatState, ExtNotif) then
    EventsTP.NotifRcvd(UniqID, @ExtNotif);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_VCardRcvd(Sender: TObject; const vCard: TExtUserDets; const Found: Boolean);
var iResCode: Word;
begin
  iResCode := DETAILS_RES_SUCCESS;
  if not Found then
    iResCode := DETAILS_RES_NOT_FOUND;

  EventsTP.DetailsReply(UniqID, @vCard, iResCode);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_VCardUpdRes(Sender: TObject; const vCard: TExtUserDets; const Success: Boolean);
var odur: TExtOwnDetsUpdReply;
begin
  ZeroMemory(@odur, SizeOf(TExtOwnDetsUpdReply));

  if Success then
    odur.UpdResultCode := UPD_DETAILS_RES_SUCCESS
  else
    odur.UpdResultCode := UPD_DETAILS_RES_NOT_ALLOWED;

  odur.ReqID := vCard.ReqID;

  EventsTP.OwnDetailsUpdReply(UniqID, @odur);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_AvatarUpdRes(Sender: TObject; const ReqID: DWord; const Success: Boolean);
var oasr: TExtOwnAvatarSetReply;
begin
  ZeroMemory(@oasr, SizeOf(TExtOwnAvatarSetReply));

  if Success then
    oasr.SetResultCode := AVATAR_SET_SUCCESS
  else
    oasr.SetResultCode := AVATAR_SET_NOT_ALLOWED;

  oasr.ReqID := ReqID;

  EventsTP.OwnAvatarSetReply(UniqID, @oasr);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_AvatarRcvd(Sender: TObject; const Jid: string; const avik: TAvatarRec);
var AvatarRes: TExtAvatarRes;
    AvatarPic: TExtAvatarPic;
    aItem: pRosterItem;
    aResInfo: pResInfo;
    sBuf: RawByteString;
begin
  aItem := FClient.Roster.GetRosterItem(Jid);

  //reset waiting flag
  if (aItem <> nil) then
    aItem^.AvatarWaiting := False;


  if (Length(avik.AvatarBase64) > 0) then
  begin
    sBuf := Base64Decode(avik.AvatarBase64);
    SetLength(sBuf, Length(sBuf));

    AvatarPic.PicData    := @sBuf[1];
    AvatarPic.PicDataLen := Length(sBuf);
    AvatarPic.HashHex    := RawToHex(MD5Raw(sBuf));
    AvatarPic.ReqID      := 0;

    //check existance or save new
    ZeroMemory(@AvatarRes, SizeOf(TExtAvatarRes));
    if not EventsTP.AvatarExists(UniqID, PWideChar(AvatarPic.HashHex)) then
      EventsTP.AvatarRcvd(UniqID, @AvatarPic, AvatarRes)
    else
    begin
      AvatarRes.Failed  := False;
      AvatarRes.HashHex := AvatarPic.HashHex;
    end;

    if not AvatarRes.Failed then
    begin
      //send buddy arrived with avatar info
      if (aItem <> nil) and not WideSameText(aItem^.AvatarMd5Hex, AvatarRes.HashHex) and //check if already got it
        (aItem^.GetResCount > 0) and // if its still online
        WideSameText(aItem^.AvatarSha1Hex, avik.AvatarSha1Hex) then //loading avatar only if its hash exists in presence
      begin
        aResInfo := aItem^.GetHighestPriorityRes;
        if (aResInfo <> nil) then
        begin
          aItem^.AvatarMd5Hex := AvatarRes.HashHex;
          OnJabber_CntArrived(FClient, aItem, aResInfo^.Resource, True);
        end;
      end;

      //if this is our account avatar then send own hash notify
      if WideSameText(FClient.Username, Jid) then
        EventsTP.OwnAvatarHashNotify(UniqID, PWideChar(AvatarRes.HashHex));
    end;
  end
  else
  begin
    //if own avatar is missing then delete it
    if WideSameText(FClient.Username, Jid) then
      EventsTP.OwnAvatarHashNotify(UniqID, nil);
  end;
end;

{***************************************************************}
procedure TXmpProto.OnJabber_Notify(Sender: TObject; const NotifType: Word; const JidSender, Msg, ClickUrl: string);
var sni: TExtShowNotifInfo;
    s: string;
begin
  ZeroMemory(@sni, SizeOf(TExtShowNotifInfo));
  sni.AutoClose   := True;
  sni.IsWarning   := False;

  s := '';
  case NotifType of
   {===================================================}
    XMPP_NOTIF_MSG_FAILED     :
      begin
        sni.TitleText := GetLI(FMoreInfo.LangCode, LI_INFORMATION);

        s := GetLI(FMoreInfo.LangCode, LI_NOTIF_MSG_SEND_FAILED);

        if (JidSender <> '') then
          s := Trim(s + CRLF + GetLI(FMoreInfo.LangCode, LI_RECEIVER) + THE_CLN + JidSender);

        if (Msg <> '') then
          s := s + CRLF + '(' + Msg + ')';
      end;
   {===================================================}
  end;//case

  sni.ContentText := GetLI(FMoreInfo.LangCode, LI_TRANSPORT) + THE_SPC + ExtParams.AccountIDsName + THE_SPC +
                     FClient.Username + CRLF + s;

  EventsTP.ShowNotificaton(UniqID, @sni);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_MailNotify(Sender: TObject; MailNotif: TExtMailNotif);
begin
  EventsTP.MailNotification(UniqID, @MailNotif);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_MailUrlDone(Sender: TObject; const MailUrl: string; const ReqID: DWord);
var omu: TExtOwnMailUrlInfo;
begin
  ZeroMemory(@omu, SizeOf(TExtOwnMailUrlInfo));

  omu.ReqID   := ReqID;
  omu.MailUrl := MailUrl;

  EventsTP.OwnMailUrlRcvd(UniqID, @omu);
end;

{***************************************************************}
procedure TXmpProto.OnJabber_OwnPresInfo(Sender: TObject);
begin
  SendOwnPresInfo(0);
end;


end.

