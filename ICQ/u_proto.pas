// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_proto;

interface

uses Windows, Classes, SysUtils, ExtCtrls, DateUtils, u_ext_info, u_obimp_const, u_obimp_codes, u_params, u_convert,
     h_oscar, u_oscar_cli, u_oscar_fb, u_oscar_item, u_oscar_avatars, u_oscar_hlp, OverbyteIcsMD5;

type
  //Transport protocol class
  TIcqProto = class(TInterfacedObject, IExtensionTP)
  private
    FClient         : TOscarClient;
    FAvatarMS       : TAvatarMS;
    FListDetsReq    : TStringList;
    FMoreInfo       : TExtMoreInfo;
    FMoreInfoGot    : Boolean;
    FUnsNotfiedOnce : Boolean;
    FOpts           : TOptionsArray;
    FTimerMsecs     : Integer;
    procedure GetObimpClientInfo;
    procedure LoadContacts;
    procedure FillContactAtts(var tc: TTranspContact; oItem: pOscarItem);
    procedure ProcessDetsRequests;
  protected
    procedure OnOscar_LoginDone(Sender: TObject);
    procedure OnOscar_LoginFailed(Sender: TObject; const ErrCode: DWord);
    procedure OnOscar_PauseCmd(Sender: TObject);
    procedure OnOscar_ResumeCmd(Sender: TObject);
    procedure OnOscar_Migrated(Sender: TObject);
    procedure OnOscar_BuddyArrived(Sender: TObject; oItem: pOscarItem; OnlyAvatarChanged: Boolean);
    procedure OnOscar_BuddyDeparted(Sender: TObject; oItem: pOscarItem; LastPerSnac: Boolean);
    procedure OnOscar_IcbmIncMsg(Sender: TObject; const IcbmMsg: TIcbmIncMsg);
    procedure OnOscar_IcbmAck(Sender: TObject; const IcbmAck: TIcbmAck);
    procedure OnOscar_IcbmEvent(Sender: TObject; const IcbmEvent: TIcbmEvent);
    procedure OnOscar_AuthRequest(Sender: TObject; const AuthMsg: TOscarAuthMsg; const OfflineMsg: Boolean);
    procedure OnOscar_AuthResponse(Sender: TObject; const AuthMsg: TOscarAuthMsg; const OfflineMsg: Boolean);
    procedure OnOscar_OwnNickInfo(Sender: TObject; const ReqID: DWord);
    procedure OnOscar_ItemOper(Sender: TObject; CmdType: Word; oItem: pOscarItem);
    procedure OnOscar_FailedOper(Sender: TObject; CmdType: Word; oItem: pOscarItem; Code: Word);
    procedure OnOscar_ImdReply(Sender: TObject; const ImdReply: TImdReply);
    procedure OnOscar_ImdUpdReply(Sender: TObject; const ImdUpdReply: TImdReply);
    procedure OnOscar_DownloadReply(Sender: TObject; const BartReplyCode: Byte; const Bart: TOscarBart);
    procedure OnOscar_UploadReply(Sender: TObject; const BartReplyCode: Byte; const Bart: TOscarBart);
    procedure OnOscar_OwnBartInfo(Sender: TObject);
    procedure OnOscar_ImdUpdDeny(Sender: TObject; const ObimpReqID: DWord; const DetsUpd: Boolean);
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


{ TIcqProto }
{***************************************************************}
constructor TIcqProto.Create(AUniqID: DWord; AEventsTP: IEventsTP);
begin
  //save UniqID for events interface callbacks
  UniqID   := AUniqID;
  //save events interface, will be used for protocol callbacks
  EventsTP := AEventsTP;

  //create client
  FClient                 := TOscarClient.Create;
  FClient.OnLoginDone     := OnOscar_LoginDone;
  FClient.OnLoginFailed   := OnOscar_LoginFailed;
  FClient.OnPauseCmd      := OnOscar_PauseCmd;
  FClient.OnResumeCmd     := OnOscar_ResumeCmd;
  FClient.OnMigrated      := OnOscar_Migrated;
  FClient.OnBuddyArrived  := OnOscar_BuddyArrived;
  FClient.OnBuddyDeparted := OnOscar_BuddyDeparted;
  FClient.OnIcbmIncMsg    := OnOscar_IcbmIncMsg;
  FClient.OnIcbmAck       := OnOscar_IcbmAck;
  FClient.OnIcbmEvent     := OnOscar_IcbmEvent;
  FClient.OnAuthRequest   := OnOscar_AuthRequest;
  FClient.OnAuthResponse  := OnOscar_AuthResponse;
  FClient.OnOwnNickInfo   := OnOscar_OwnNickInfo;
  FClient.OnImdReply      := OnOscar_ImdReply;
  FClient.OnImdUpdReply   := OnOscar_ImdUpdReply;
  FClient.OnOwnBartInfo   := OnOscar_OwnBartInfo;
  FClient.OnMdirUpdDeny   := OnOscar_ImdUpdDeny;

  FClient.OscarCL.OnItemOper   := OnOscar_ItemOper;
  FClient.OscarCL.OnFailedOper := OnOscar_FailedOper;

  FClient.Avatars.OnDownloadReply := OnOscar_DownloadReply;
  FClient.Avatars.OnUploadReply   := OnOscar_UploadReply;

  FListDetsReq := TStringList.Create;
end;

{***************************************************************}
destructor TIcqProto.Destroy;
begin
  FListDetsReq.Free;
  if Assigned(FAvatarMS) then FreeAndNil(FAvatarMS);
  FClient.Logoff;
  FClient.Free;
  inherited;
end;

{***************************************************************}
procedure TIcqProto.TimerTick;
begin
  //pass below only full 1 second, not 100 msecs
  Inc(FTimerMsecs);
  if (FTimerMsecs < 10) then Exit;
  FTimerMsecs := 0;

  if (FClient.ConnectState = CONNECT_STATE_CONNECTING) then
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

  if (FClient.ConnectState = CONNECT_STATE_ONLINE) then
  begin
    //request details if needed
    ProcessDetsRequests;

    //use timer ticks to send keep alives
    if (FClient.AliveTicks >= (OSCAR_KEEP_ALIVE_INTERVAL-1)) then
    begin
      FClient.AliveTicks := 0;
      FClient.SendKeepAlive;
    end
    else
      FClient.AliveTicks := FClient.AliveTicks + 1;
  end;

  //barts
  with FClient do
  begin
    if (Avatars.ConnectState = CONNECT_STATE_ONLINE) then
    begin
      //get queued barts if available
      Avatars.ProcessBartsQueue;

      if (Avatars.AliveTicks >= (OSCAR_KEEP_ALIVE_INTERVAL-1)) then
      begin
        Avatars.AliveTicks := 0;
        Avatars.SendKeepAlive;
      end
      else
        Avatars.AliveTicks := Avatars.AliveTicks + 1;
    end;
  end;//with
end;

{***************************************************************}
procedure TIcqProto.GetTransportOpts(var ExtOpts: TExtOptions);
var i: Integer;
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
      OPT_ID_SHOW_ST_SEARCH : FOpts[i].OptName := GetLI(FMoreInfo.LangCode, LI_PRIVACY) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_PRIVACY) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_SHOW_ST_SEARCH);

      OPT_ID_USERS_MUSTAUTH : FOpts[i].OptName := GetLI(FMoreInfo.LangCode, LI_PRIVACY) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_PRIVACY) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_MUST_AUTH_ON_ADD);

      OPT_ID_PRIVACY_LEVEL  : FOpts[i].OptName := GetLI(FMoreInfo.LangCode, LI_PRIVACY) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_PRIVACY) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_PRIVACY_LEVEL) + VAL_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_LOW) + VAL_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_MEDIUM) + VAL_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_HIGH);

      OPT_ID_DISABLE_MULTIS : FOpts[i].OptName := GetLI(FMoreInfo.LangCode, LI_PRIVACY) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_OPTIONS) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_DISABLE_MULTISES);

      OPT_ID_ALLOW_UNS_UPDT : FOpts[i].OptName := GetLI(FMoreInfo.LangCode, LI_PRIVACY) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_OPTIONS) + OPT_DIV +
                                                  GetLI(FMoreInfo.LangCode, LI_ALLOW_UNS_UPDATE);
    end;//case
  end;//for

  ExtOpts.SettingsFlags := 0;
  ExtOpts.OptionsCount  := Length(FOpts);
  ExtOpts.OptionsArray  := @FOpts[0];
end;

{***************************************************************}
function TIcqProto.TransportSettings(ExtTranspSet: pExtTranspSettings): LongBool;
var s: string;
    i: integer;
begin
  Result := True;

  //no multisession, should be removed after some time
  s := ExtTranspSet^.SrvHost;
  i := Pos('/nomc', LowerCase(s));
  if (i > 0) then Delete(s, i, Length(s));

  with FClient do
  begin
    LoginServer   := Trim(s);
    LoginPort     := IntToStr(ExtTranspSet^.SrvPort);

    LoginAccount  := ExtTranspSet^.Account;
    LoginPassword := AnsiString(ExtTranspSet^.Password);
  end;//with

  //get options
  if (ExtTranspSet^.Options.OptionsCount > 0) then
  begin
    for i := 0 to ExtTranspSet^.Options.OptionsCount-1 do
    begin
      with ExtTranspSet^.Options do
      case OptionsArray^[i].OptID of
        OPT_ID_SHOW_ST_SEARCH : FClient.WebAware        := GetOptBool(OptionsArray^[i].OptValue);
        OPT_ID_USERS_MUSTAUTH : FClient.AuthRequired    := GetOptBool(OptionsArray^[i].OptValue);
        OPT_ID_PRIVACY_LEVEL  : FClient.PrivacyLevel    := GetOptByte(OptionsArray^[i].OptValue);
        OPT_ID_DISABLE_MULTIS : FClient.NoMultiSes      := GetOptBool(OptionsArray^[i].OptValue);
        OPT_ID_ALLOW_UNS_UPDT : FClient.AllowUnsUpd     := GetOptBool(OptionsArray^[i].OptValue);
      end;//case
    end;//for

    FClient.UpdateMdirPrivacy;
  end;
end;

{***************************************************************}
procedure TIcqProto.TransportManage(ExtTranspMan: pExtTranspManage);
var iStatus: DWord;
    iPrivStatus: Byte;
    iMoodNum: Integer;
    iMoodName: string;
    sCaps: RawByteString;
begin
  case ExtTranspMan^.ManCode of
   {===================================================}
    TP_CON_MAN_CONNECT,
    TP_CON_MAN_STATUS:
      begin
        ConvObimpStatus(ExtTranspMan^.Status, ExtTranspMan^.StPicID, iStatus, iPrivStatus, iMoodNum, iMoodName, sCaps);

        //this params can be always updated if offline, they will be saved in proto
        FClient.UpdateCaps(sCaps);
        FClient.UpdatePrivStatus(iPrivStatus);
        FClient.UpdateStatus(iStatus);

        //moods
        if (ExtTranspMan^.StPicID > 0) then
        begin
          iMoodNum  := StPicToMoodNum(ExtTranspMan^.StPicID);
          iMoodName := ExtTranspMan^.StPicDesc;
        end;
        FClient.UpdateMood(iMoodNum, iMoodName);

        //login or reply
        if (ExtTranspMan^.ManCode = TP_CON_MAN_CONNECT) then
        begin
          //login if offline
          if (FClient.ConnectState = CONNECT_STATE_OFFLINE) then
            FClient.Login;
        end
        else
        begin
          //notify server about status update only if online
          if (FClient.ConnectState = CONNECT_STATE_ONLINE) then
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
procedure TIcqProto.GetObimpClientInfo;
var ProxyInfo: TExtProxyInfo;
begin
  if FMoreInfoGot then Exit;
  FMoreInfoGot := True;

  ZeroMemory(@FMoreInfo, SizeOf(TExtMoreInfo));
  EventsTP.GetMoreInfo(UniqID, FMoreInfo);

  FClient.ImdLang     := GetLI(FMoreInfo.LangCode, LI_IMD_LANG_COUNTRY);
  FClient.ClientName  := FMoreInfo.ClientName;
  FClient.ClientVer   := FMoreInfo.ClientVer;

  //get proxy info
  ZeroMemory(@ProxyInfo, SizeOf(TExtProxyInfo));
  EventsTP.GetProxyInfo(UniqID, ProxyInfo);

  FClient.ProxyInfo := ProxyInfo;
end;

{***************************************************************}
procedure TIcqProto.OnOscar_LoginDone(Sender: TObject);
var oItem: pOscarItem;
begin
  //clear previous session details queued requests if they are.
  FListDetsReq.Clear;

  //client should load contacts only once per session
  LoadContacts;

  //notify server about loggedin
  EventsTP.TransportStateChanged(UniqID, TP_STATE_LOGGEDIN);

  //notify about own avatar hash if its empty
  oItem := FClient.OscarCL.GetBartItem(IntToStr(BART_BUDDY_ICON));
  if (oItem = nil) then
    EventsTP.OwnAvatarHashNotify(UniqID, nil);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_LoginFailed(Sender: TObject; const ErrCode: DWord);
begin
  EventsTP.TransportStateChanged(UniqID, ConvProtoConErrorCode(ErrCode));
end;

{***************************************************************}
procedure TIcqProto.OnOscar_PauseCmd(Sender: TObject);
begin
  EventsTP.TransportStateChanged(UniqID, TP_STATE_SRV_PAUSED);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_ResumeCmd(Sender: TObject);
begin
  EventsTP.TransportStateChanged(UniqID, TP_STATE_SRV_RESUMED);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_Migrated(Sender: TObject);
begin
  EventsTP.TransportStateChanged(UniqID, TP_STATE_SRV_MIGRATED);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_BuddyArrived(Sender: TObject; oItem: pOscarItem; OnlyAvatarChanged: Boolean);
var cs: TExtContactStatus;
begin
  ConvProtoBuddyOnline(oItem, cs, FMoreInfo.LangCode);

  //avatar check
  if (cs.AvatarHashHex <> '') and not EventsTP.AvatarExists(UniqID, PWideChar(cs.AvatarHashHex)) then
  begin
    cs.AvatarHashHex := '';

    FClient.Avatars.AvatarDownloadReq(OscAcc(oItem^.Name), oItem^.NickInfo.Barts.AvatarFlags, oItem^.NickInfo.Barts.AvatarHash);

    if (FClient.Avatars.ConnectState = CONNECT_STATE_OFFLINE) and not FClient.Avatars.BartRequested then
    begin
      FClient.Avatars.BartRequested := True;
      FClient.RequestService(FAM_BART);
    end;

    //if avatar not exist and only avatar changed then dont send ContactOnline
    if OnlyAvatarChanged then Exit;
  end;

  EventsTP.ContactOnline(UniqID, @cs);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_BuddyDeparted(Sender: TObject; oItem: pOscarItem; LastPerSnac: Boolean);
var cs: TExtContactStatus;
begin
  ConvProtoBuddyOffline(oItem, cs);
  EventsTP.ContactOffline(UniqID, @cs);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_IcbmIncMsg(Sender: TObject; const IcbmMsg: TIcbmIncMsg);
var ExtMsg: TExtMsg;
begin
  if (Length(IcbmMsg.MsgText) > 0) then
  begin
    ConvProtoIncMsg(FClient, IcbmMsg, ExtMsg);
    EventsTP.InstantMsgRcvd(UniqID, @ExtMsg);
  end;
end;

{***************************************************************}
procedure TIcqProto.OnOscar_IcbmAck(Sender: TObject; const IcbmAck: TIcbmAck);
var MsgAck: TExtMsgAck;
begin
  if ConvProtoMsgAck(FClient, IcbmAck, MsgAck) then
    EventsTP.MsgAckRcvd(UniqID, @MsgAck);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_IcbmEvent(Sender: TObject; const IcbmEvent: TIcbmEvent);
var ExtNotif: TExtNotif;
begin
  ConvProtoEventMsg(IcbmEvent, ExtNotif);
  EventsTP.NotifRcvd(UniqID, @ExtNotif);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_AuthRequest(Sender: TObject; const AuthMsg: TOscarAuthMsg; const OfflineMsg: Boolean);
var ExtAuthMsg: TExtAuthMsg;
begin
  ConvProtoAuthRequest(AuthMsg, ExtAuthMsg, OfflineMsg);
  EventsTP.AuthRequestRcvd(UniqID, @ExtAuthMsg);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_AuthResponse(Sender: TObject; const AuthMsg: TOscarAuthMsg; const OfflineMsg: Boolean);
var ExtAuthMsg: TExtAuthMsg;
begin
  ConvProtoAuthReply(AuthMsg, ExtAuthMsg, OfflineMsg);
  EventsTP.AuthReplyRcvd(UniqID, @ExtAuthMsg);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_OwnNickInfo(Sender: TObject; const ReqID: DWord);
var opi: TExtOwnPresenceInfo;
begin
  ConvProtoPresInfo(FClient, ReqID, opi, FMoreInfo.LangCode);
  EventsTP.OwnPresenceInfoRcvd(UniqID, @opi);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_ItemOper(Sender: TObject; CmdType: Word; oItem: pOscarItem);
var etc: TExtTranspContacts;
    tc: array of TTranspContact;
begin
  //ItemOper is only for real buddies
  SetLength(tc, 1);
  FillContactAtts(tc[0], oItem);

  etc.ContactsCount := 1;
  etc.ContactsArray := @tc[0];

  //send changes
  case CmdType of
   {=====================================================}
    CMD_FEEDBAG_INSERT_ITEMS: EventsTP.AddContacts(UniqID, False, @etc);
   {=====================================================}
    CMD_FEEDBAG_UPDATE_ITEMS: EventsTP.UpdContacts(UniqID, @etc);
   {=====================================================}
    CMD_FEEDBAG_DELETE_ITEMS:
      begin
        //if buddy removed self and it was ignored then move it to ignore not in list
        if (tc[0].PrivacyType = CL_PRIV_TYPE_IGNORE_LIST) then
        begin
          tc[0].PrivacyType := CL_PRIV_TYPE_IGNORE_NOT_IN_LIST;
          EventsTP.UpdContacts(UniqID, @etc);
        end
        else
          EventsTP.DelContacts(UniqID, @etc);
      end;
   {=====================================================}
  end;//case
end;

{***************************************************************}
procedure TIcqProto.OnOscar_FailedOper(Sender: TObject; CmdType: Word; oItem: pOscarItem; Code: Word);
var etc: TExtTranspContacts;
    tc: array of TTranspContact;
    sni: TExtShowNotifInfo;
    aItem: TOscarItem;
    bIgnoreNil: Boolean;
begin
  bIgnoreNil := False;

  //if privacy item error, then get its buddy item
  if IsPrivacyItem(oItem) then
  begin
    aItem := oItem^;

    oItem := FClient.OscarCL.GetBuddyItem(oItem^.Name);
    if (oItem <> nil) then
      CmdType := CMD_FEEDBAG_UPDATE_ITEMS
    else
    begin
      //if error during moving to ignore list
      if (aItem.ClassID = FB_CLSID_IGNORE) then
      begin
        CmdType    := CMD_FEEDBAG_INSERT_ITEMS;
        oItem      := @aItem;
        bIgnoreNil := True;
      end;
    end;
  end;

  if IsRealBuddy(oItem) or bIgnoreNil then
  begin
    SetLength(tc, 1);
    FillContactAtts(tc[0], oItem);

    etc.ContactsCount := 1;
    etc.ContactsArray := @tc[0];

    //make rollback operations
    case CmdType of
     {=====================================================}
      CMD_FEEDBAG_INSERT_ITEMS: EventsTP.DelContacts(UniqID, @etc); //delete inserted item
     {=====================================================}
      CMD_FEEDBAG_UPDATE_ITEMS: EventsTP.UpdContacts(UniqID, @etc); //rollback item update
     {=====================================================}
      CMD_FEEDBAG_DELETE_ITEMS: EventsTP.AddContacts(UniqID, False, @etc); //insert back deleted item
     {=====================================================}
    end;//case
  end;

  //show error notification in client side
  ZeroMemory(@sni, SizeOf(TExtShowNotifInfo));

  sni.AutoClose   := True;
  sni.IsWarning   := True;
  sni.TitleText   := GetLI(FMoreInfo.LangCode, LI_INFORMATON);
  sni.ContentText := GetLI(FMoreInfo.LangCode, LI_TRANSPORT) + THE_SPC + ExtParams.AccountIDsName + THE_SPC +
                     string(FClient.LoginAccount) + THE_CLN + THE_SPC;

  case Code of
   {=====================================================}
    FB_STATUS_CODE_OVER_ROW_LIMIT: sni.ContentText := sni.ContentText + GetLI(FMoreInfo.LangCode, LI_LIMIT_REACHED);
   {=====================================================}
    else
      sni.ContentText := sni.ContentText + GetLI(FMoreInfo.LangCode, LI_CL_OPER_FAILED) + THE_SPC + '(0x' + IntToHex(Code, 4) + ')';
   {=====================================================}
  end;//case

  EventsTP.ShowNotificaton(UniqID, @sni);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_ImdReply(Sender: TObject; const ImdReply: TImdReply);
var ExtUserDets: TExtUserDets;
    ExtSrchReply: TExtSrchReply;
    ResCode: Word;
    i: integer;
begin
  if (ImdReply.ReqType = REQ_TYPE_SEARCH) then
  begin
    if (ImdReply.Results > 0) then
    begin
      for i := 0 to ImdReply.Results-1 do
      begin
        ConvProtoSearchReply(ImdReply, i, ExtSrchReply, ResCode);
        EventsTP.SearchReply(UniqID, @ExtSrchReply, ResCode);
      end;//for
    end
    else
    begin
      ConvProtoSearchReply(ImdReply, -1, ExtSrchReply, ResCode);
      EventsTP.SearchReply(UniqID, @ExtSrchReply, ResCode);
    end;
  end
  else
  begin
    if not ConvProtoImdReply(ImdReply, ExtUserDets, ResCode) then Exit;

    if (ImdReply.ReqType = REQ_TYPE_OWN_KEEP_INFO) and FClient.ImdMan.OwnImdRequested then
    begin
      FClient.ImdMan.OwnImdRequested := False;
      ExtUserDets.ReqID := FClient.ImdMan.OwnImdRequestID;
    end;

    EventsTP.DetailsReply(UniqID, @ExtUserDets, ResCode);
  end;
end;

{***************************************************************}
procedure TIcqProto.OnOscar_ImdUpdReply(Sender: TObject; const ImdUpdReply: TImdReply);
var OwnDetsUpdReply: TExtOwnDetsUpdReply;
begin
  case ImdUpdReply.ReqType of
   {===============================================}
    REQ_TYPE_UPDATE_OWN_INFO :
      begin
        if not ConvProtoImdUpdReply(ImdUpdReply, OwnDetsUpdReply) then Exit;
        EventsTP.OwnDetailsUpdReply(UniqID, @OwnDetsUpdReply);
      end;
   {===============================================}
    REQ_TYPE_UPDATE_OPTS     :
      begin
        if (ImdUpdReply.ImdCode = IMD_CODE_SUCCESS) then
        begin
          //if webaware changed then update status
          if (FClient.OscarInfo.ImdInfo.ExposeStatus <> FClient.WebAware) then
          begin
            FClient.OscarInfo.ImdInfo.ExposeStatus := FClient.WebAware;
            FClient.UpdateStatus(FClient.CurrentStatus);
          end;

          //set new opts
          FClient.OscarInfo.ImdInfo.AuthRequired := FClient.AuthRequired;
          FClient.OscarInfo.ImdInfo.PrivacyLevel := FClient.PrivacyLevel;
        end
        else
        begin
          //set back opts
          FClient.WebAware     := FClient.OscarInfo.ImdInfo.ExposeStatus;
          FClient.AuthRequired := FClient.OscarInfo.ImdInfo.AuthRequired;
          FClient.PrivacyLevel := FClient.OscarInfo.ImdInfo.PrivacyLevel;
        end;
      end;
   {===============================================}
  end;//case
end;

{***************************************************************}
procedure TIcqProto.OnOscar_ImdUpdDeny(Sender: TObject; const ObimpReqID: DWord; const DetsUpd: Boolean);
var OwnDetsUpdReply: TExtOwnDetsUpdReply;
    sni: TExtShowNotifInfo;
begin
  //these all for unsearchable accounts
  if DetsUpd then
  begin
    ZeroMemory(@OwnDetsUpdReply, SizeOf(TExtOwnDetsUpdReply));

    OwnDetsUpdReply.UpdResultCode := UPD_DETAILS_RES_NOT_ALLOWED;
    OwnDetsUpdReply.ReqID         := ObimpReqID;

    EventsTP.OwnDetailsUpdReply(UniqID, @OwnDetsUpdReply);
  end
  else
  begin
    //if user updates privacy settings then notify only once
    if FUnsNotfiedOnce then Exit;
    FUnsNotfiedOnce := True;
  end;

  //show deny notification in client side
  ZeroMemory(@sni, SizeOf(TExtShowNotifInfo));

  sni.AutoClose   := True;
  sni.IsWarning   := False;
  sni.TitleText   := GetLI(FMoreInfo.LangCode, LI_INFORMATON);
  sni.ContentText := GetLI(FMoreInfo.LangCode, LI_TRANSPORT) + THE_SPC + ExtParams.AccountIDsName + THE_SPC +
                     string(FClient.LoginAccount) + THE_CLN + THE_SPC +
                     GetLI(FMoreInfo.LangCode, LI_UNSEARCHABLE);

  EventsTP.ShowNotificaton(UniqID, @sni);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_DownloadReply(Sender: TObject; const BartReplyCode: Byte; const Bart: TOscarBart);
var AvatarRes: TExtAvatarRes;
    AvatarPic: TExtAvatarPic;
    oItem: pOscarItem;
begin
  //Only PNG, JPG, GIF, BMP files accepted
  if (BartReplyCode = BART_REPLY_CODE_SUCCESS) and
     (Bart.BartType = BART_BUDDY_ICON) and (Bart.BartFlag <= BART_FLAG_CUSTOM) and
     (Length(Bart.BartFile) > 0) then
  begin
    AvatarPic.PicData    := @Bart.BartFile[1];
    AvatarPic.PicDataLen := Length(Bart.BartFile);
    AvatarPic.HashHex    := RawToHex(Bart.BartHash);
    AvatarPic.ReqID      := 0;

    ZeroMemory(@AvatarRes, SizeOf(TExtAvatarRes));
    EventsTP.AvatarRcvd(UniqID, @AvatarPic, AvatarRes);

    if not AvatarRes.Failed then
    begin
      //send buddy arrived with avatar info
      oItem := FClient.OscarCL.GetBuddyItem(Bart.Account);
      if (oItem <> nil) and oItem^.NickInfo.Online and (oItem^.NickInfo.Barts.AvatarHash = Bart.BartHash) then
        OnOscar_BuddyArrived(FClient, oItem, True);

      //if this is our account avatar then send own hash notify
      if SameOscAcc(FClient.LoginAccount, Bart.Account) then
        EventsTP.OwnAvatarHashNotify(UniqID, PWideChar(AvatarRes.HashHex));
    end;
  end;
end;

{***************************************************************}
procedure TIcqProto.OnOscar_UploadReply(Sender: TObject; const BartReplyCode: Byte; const Bart: TOscarBart);
var oasr: TExtOwnAvatarSetReply;
begin
  if (FAvatarMS = nil) then Exit;

  ConvProtoAvatarUploadRelpy(BartReplyCode, Bart, FAvatarMS.AvatarReqID, oasr);
  EventsTP.OwnAvatarSetReply(UniqID, @oasr);

  //delete avatar bart on error
  if (BartReplyCode <> BART_REPLY_CODE_SUCCESS) then
    FClient.OscarCL.DeleteBartItem(BART_BUDDY_ICON);

  //free stream
  FreeAndNil(FAvatarMS);
end;

{***************************************************************}
procedure TIcqProto.OnOscar_OwnBartInfo(Sender: TObject);
var sHash: WideString;
    sRaw: RawByteString;
begin
  if (FClient.OscarInfo.AvatarFlags = BART_FLAG_CUSTOM) and (Length(FClient.OscarInfo.AvatarHash) = 16) then
  begin
    //avatar is OK, check if we have it
    sHash := RawToHex(FClient.OscarInfo.AvatarHash);

    //if avatar exists then just notify server about it
    if EventsTP.AvatarExists(UniqID, PWideChar(sHash)) then
      EventsTP.OwnAvatarHashNotify(UniqID, PWideChar(sHash))
    else
    begin
      //else request avatar file
      FClient.Avatars.AvatarDownloadReq(OscAcc(FClient.LoginAccount), FClient.OscarInfo.AvatarFlags, FClient.OscarInfo.AvatarHash);

      if (FClient.Avatars.ConnectState = CONNECT_STATE_OFFLINE) and not FClient.Avatars.BartRequested then
      begin
        FClient.Avatars.BartRequested := True;
        FClient.RequestService(FAM_BART);
      end;
    end;
  end
  else
  if ((FClient.OscarInfo.AvatarFlags and BART_FLAG_UNKNOWN) = BART_FLAG_UNKNOWN) and (Length(FClient.OscarInfo.AvatarHash) = 16) then
  begin
    //avatar is missing on bart server, need to upload if we have it or delete if we dont have it
    if Assigned(FAvatarMS) and (FClient.OscarInfo.AvatarHash = HexToRaw(String(StreamMD5(FAvatarMS, nil, nil, 0, FAvatarMS.Size)))) then
    begin
      FAvatarMS.Position := 0;
      SetLength(sRaw, FAvatarMS.Size);
      FAvatarMS.Read(sRaw[1], Length(sRaw));

      //upload avatar file
      FClient.Avatars.AvatarUploadReq(OscAcc(FClient.LoginAccount), FClient.OscarInfo.AvatarHash, sRaw);

      if (FClient.Avatars.ConnectState = CONNECT_STATE_OFFLINE) and not FClient.Avatars.BartRequested then
      begin
        FClient.Avatars.BartRequested := True;
        FClient.RequestService(FAM_BART);
      end;
    end
    else
    begin
      FClient.OscarCL.DeleteBartItem(BART_BUDDY_ICON);
      EventsTP.OwnAvatarHashNotify(UniqID, nil);
    end;
  end
  else
  begin
    //avatar is missing or something wrong, delete it
    FClient.OscarCL.DeleteBartItem(BART_BUDDY_ICON);
    EventsTP.OwnAvatarHashNotify(UniqID, nil);
  end;
end;

{***************************************************************}
procedure TIcqProto.LoadContacts;
var etc: TExtTranspContacts;
    tc: array of TTranspContact;
    oItem: pOscarItem;
    i: integer;
    List: TStringList;
begin
  //create accounts list for fast accs search
  List            := TStringList.Create;
  List.Sorted     := True;
  List.Duplicates := dupIgnore;
  try
  //create and fill contacts array
  oItem := nil;
  i := 0;
  while FClient.OscarCL.EnumBuddyItems(i, oItem) do
  begin
    SetLength(tc, Length(tc)+1);
    FillContactAtts(tc[Length(tc)-1], oItem);
    List.Add(tc[Length(tc)-1].AccountName);
  end;//while

  //add ignored "not in list" items
  oItem := nil;
  i := 0;
  while FClient.OscarCL.EnumClassItems(i, oItem, FB_CLSID_IGNORE) do
  begin
    if (List.IndexOf(OscAcc(oItem^.Name)) < 0) then
    begin
      SetLength(tc, Length(tc)+1);
      FillContactAtts(tc[Length(tc)-1], oItem);
    end;
  end;//while

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
procedure TIcqProto.FillContactAtts(var tc: TTranspContact; oItem: pOscarItem);
var ps: TOscarPrivSet;
begin
  ZeroMemory(@tc, SizeOf(TTranspContact));

  tc.AccountName := OscAcc(oItem^.Name);

  if (oItem^.ClassID = FB_CLSID_BUDDY) then
  begin
    tc.ContactName := oItem^.AttStrValue(FB_ATT_ALIAS);
    tc.GroupName   := FClient.OscarCL.GetGroupNameByGroupID(oItem^.GroupID);
    tc.PrivacyType := CL_PRIV_TYPE_NONE;
    tc.AuthFlag    := oItem^.AttExists(FB_ATT_PENDING_AUTH);

    //get and set privacy
    ps := FClient.OscarCL.GetBuddyPrivacy(oItem^.Name);

    if (privIgnore in ps) then
      tc.PrivacyType := CL_PRIV_TYPE_IGNORE_LIST
    else
    if (privDeny in ps) then
      tc.PrivacyType := CL_PRIV_TYPE_INVISIBLE_LIST
    else
    if (privPermit in ps) then
      tc.PrivacyType := CL_PRIV_TYPE_VISIBLE_LIST;
  end
  else
  if (oItem^.ClassID = FB_CLSID_IGNORE) then
  begin
    tc.PrivacyType := CL_PRIV_TYPE_IGNORE_NOT_IN_LIST;
  end;
end;

{***************************************************************}
procedure TIcqProto.AddContact(TranspContact: pTranspContact);
var oItem: pOscarItem;
    ps: TOscarPrivSet;
begin
  if (TranspContact^.PrivacyType = CL_PRIV_TYPE_IGNORE_NOT_IN_LIST) then
  begin
    //add to ignore item if not exists
    FClient.OscarCL.UpdateBuddyPrivacy(nil, [privIgnore], TranspContact^.AccountName);

    //delete contact item from CL if exists
    oItem := FClient.OscarCL.GetBuddyItem(TranspContact^.AccountName);
    if (oItem <> nil) then
      FClient.OscarCL.DeleteBuddy(oItem);
  end
  else
  begin
    case TranspContact^.PrivacyType of
      CL_PRIV_TYPE_NONE               : ps := [];
      CL_PRIV_TYPE_VISIBLE_LIST       : ps := [privPermit];
      CL_PRIV_TYPE_INVISIBLE_LIST     : ps := [privDeny];
      CL_PRIV_TYPE_IGNORE_LIST        : ps := [privIgnore];
    end;//case

    //update privacy lists
    FClient.OscarCL.UpdateBuddyPrivacy(nil, ps, TranspContact^.AccountName);

    //add new buddy
    FClient.OscarCL.AddBuddy(TranspContact^.AccountName, TranspContact^.ContactName, TranspContact^.GroupName);
  end;
end;

{***************************************************************}
procedure TIcqProto.UpdContact(TranspContact: pTranspContact);
var oItem: pOscarItem;
    ps: TOscarPrivSet;
begin
  case TranspContact^.PrivacyType of
    CL_PRIV_TYPE_NONE               : ps := [];
    CL_PRIV_TYPE_VISIBLE_LIST       : ps := [privPermit];
    CL_PRIV_TYPE_INVISIBLE_LIST     : ps := [privDeny];
    CL_PRIV_TYPE_IGNORE_LIST,
    CL_PRIV_TYPE_IGNORE_NOT_IN_LIST : ps := [privIgnore];
  end;//case

  oItem := FClient.OscarCL.GetBuddyItem(TranspContact^.AccountName);

  //if buddy doesnt exist (if it was moved to ignore and now returned back to cl (Bimoid IM doesnt do it))
  if (oItem = nil) then
  begin
    if (TranspContact^.PrivacyType <> CL_PRIV_TYPE_IGNORE_NOT_IN_LIST) and not FClient.OscarCL.OperBusy then
    begin
      FClient.OscarCL.UpdateBuddyPrivacy(nil, ps, TranspContact^.AccountName);
      //return buddy to list
      FClient.OscarCL.AddBuddy(TranspContact^.AccountName, TranspContact^.ContactName, TranspContact^.GroupName);
    end;
    Exit;
  end;

  //delete buddy if ignore not in list
  if (TranspContact^.PrivacyType = CL_PRIV_TYPE_IGNORE_NOT_IN_LIST) then
    FClient.OscarCL.DeleteBuddy(oItem)
  else
    //we can update contacts name if it was changed, but will not do any groups opers
    FClient.OscarCL.UpdateBuddyAlias(oItem, TranspContact^.ContactName);

  //update privacy lists if changed
  FClient.OscarCL.UpdateBuddyPrivacy(nil, ps, TranspContact^.AccountName);
end;

{***************************************************************}
procedure TIcqProto.DelContact(TranspContact: pTranspContact);
var oItem: pOscarItem;
begin
  oItem := FClient.OscarCL.GetBuddyItem(TranspContact^.AccountName);
  if (oItem <> nil) then
    FClient.OscarCL.DeleteBuddy(oItem);

  //delete privacy items if exist
  FClient.OscarCL.UpdateBuddyPrivacy(nil, [], TranspContact^.AccountName);
end;

{***************************************************************}
procedure TIcqProto.SendAuthRequest(AuthMsg: pExtAuthMsg);
var oAuthMsg: TOscarAuthMsg;
begin
  ConvObimpAuthRequest(AuthMsg, oAuthMsg);
  FClient.SendAuthRequest(oAuthMsg);
end;

{***************************************************************}
procedure TIcqProto.SendAuthReply(AuthMsg: pExtAuthMsg);
var oAuthMsg: TOscarAuthMsg;
begin
  ConvObimpAuthReply(AuthMsg, oAuthMsg);
  FClient.SendAuthRespond(oAuthMsg);
end;

{***************************************************************}
procedure TIcqProto.SendAuthRevoke(AuthMsg: pExtAuthMsg);
begin
  FClient.SendRemoveMe(AuthMsg^.Account);
end;

{***************************************************************}
procedure TIcqProto.SendInstantMsg(InstantMsg: pExtMsg);
var IcbmMsg: TIcbmOutMsg;
begin
  ConvObimpOutMsg(FClient, InstantMsg, IcbmMsg);
  FClient.SendIcbmMsg(IcbmMsg);
end;

{***************************************************************}
procedure TIcqProto.SendMsgAck(MsgAck: pExtMsgAck);
var IcbmEvent: TIcbmEvent;
begin
  if ConvObimpMsgAck(FClient, MsgAck, IcbmEvent) then
    FClient.SendIcbmEvent(IcbmEvent);
end;

{***************************************************************}
procedure TIcqProto.SendNotif(ExtNotif: pExtNotif);
var IcbmEvent: TIcbmEvent;
begin
  if (ExtNotif^.NotifType = NOTIF_TYPE_USER_TYPING) then
  begin
    ConvObimpNotif(ExtNotif, IcbmEvent);
    FClient.SendIcbmEvent(IcbmEvent);
  end;
end;

{***************************************************************}
procedure TIcqProto.DetailsRequest(ExtDetsReq: pExtDetsReq);
var ExtUserDets: TExtUserDets;
begin
  //Do not request details until feedbag oper finished else there will be adding contacts errors.
  //Bimoid server updates contact name after receiving details, but at this time adding operation
  //might not be finished. All details requests will be processed after feedbag oper will be finished.
  if FClient.OscarCL.OperBusy then
  begin
    FListDetsReq.Add(IntToStr(ExtDetsReq^.ReqID) + THE_SPC + ExtDetsReq^.Account);
    Exit;
  end;

  if SameOscAcc(ExtDetsReq^.Account, FClient.LoginAccount) then
  begin
    //dont request imdinfo for own account, because it can be already requested
    if (FClient.OscarInfo.ImdInfo.Account = '') then
    begin
      //if already requested own info and waiting for answer, then dont send request again
      //and just save ReqID to return it on imd reply
      if not FClient.ImdMan.OwnImdRequested then
        FClient.RequestMdirInfo(ExtDetsReq^.Account, REQ_TYPE_OWN_KEEP_INFO, IMD_INFO_LEVEL_FULL, ExtDetsReq^.ReqID)
      else
        FClient.ImdMan.OwnImdRequestID := ExtDetsReq^.ReqID;
    end
    else
    begin
      ConvProtoImdInfo(@FClient.OscarInfo.ImdInfo, ExtUserDets, ExtDetsReq^.ReqID);
      EventsTP.DetailsReply(UniqID, @ExtUserDets, DETAILS_RES_SUCCESS);
    end;
  end
  else
    FClient.RequestMdirInfo(ExtDetsReq^.Account, REQ_TYPE_CNT_DETAILS, IMD_INFO_LEVEL_FULL, ExtDetsReq^.ReqID);
end;

{***************************************************************}
procedure TIcqProto.SearchRequest(ExtSrchReq: pExtSrchReq);
var ImdSrch: TImdSrchReq;
    SrchReply: TExtSrchReply;
begin
  if not ConvObimpSearch(ExtSrchReq, ImdSrch) or not FClient.SearchMdir(ImdSrch, ExtSrchReq^.ReqID) then
  begin
    ZeroMemory(@SrchReply, SizeOf(TExtSrchReply));
    SrchReply.ReqID := ExtSrchReq^.ReqID;

    EventsTP.SearchReply(UniqID, @SrchReply, SEARCH_RES_BAD_REQUEST);
  end;
end;

{***************************************************************}
procedure TIcqProto.OwnDetailsUpdate(ExtUserDets: pExtUserDets);
var ImdInfo: TImdInfo;
begin
  if ConvObimpDetsUpdate(FClient, ExtUserDets, ImdInfo) then
    FClient.UpdateMdirInfo(ImdInfo, ExtUserDets^.ReqID);
end;

{***************************************************************}
procedure TIcqProto.OwnAvatarSet(AvatarPic: pExtAvatarPic);
var bUpdated: Boolean;
    iConvRes: Word;
    oasr: TExtOwnAvatarSetReply;
begin
  //if set new avatar then get avatar file data
  if (AvatarPic^.PicData <> nil) and (AvatarPic^.PicDataLen > 0) then
  begin
    bUpdated := False;
    iConvRes := AVATAR_SET_OTHER_ERROR;
    if Assigned(FAvatarMS) then FreeAndNil(FAvatarMS);

    FAvatarMS := TAvatarMS.Create;
    try
      FAvatarMS.AvatarReqID := AvatarPic^.ReqID;

      FAvatarMS.Position := 0;
      FAvatarMS.Write(AvatarPic^.PicData^, AvatarPic^.PicDataLen);

      //OSCAR doesnt support PNG avatars, so convert it to jpeg
      iConvRes := ConvObimpPngToJpeg(FAvatarMS);

      //add/update bart item hash first
      if (iConvRes = AVATAR_SET_SUCCESS) then
      begin
        bUpdated := True;
        FClient.OscarCL.UpdateBartItem(BART_BUDDY_ICON, BART_FLAG_CUSTOM, HexToRaw(String(StreamMD5(FAvatarMS, nil, nil, 0, FAvatarMS.Size))));
      end;
    finally
      if not bUpdated then
      begin
        FreeAndNil(FAvatarMS);
        //notify server
        ZeroMemory(@oasr, SizeOf(TExtOwnAvatarSetReply));
        oasr.SetResultCode := iConvRes;
        oasr.ReqID         := AvatarPic^.ReqID;
        EventsTP.OwnAvatarSetReply(UniqID, @oasr);
      end;
    end;
  end
  else
  begin
    //if avatar removed, remove bart item
    FClient.OscarCL.DeleteBartItem(BART_BUDDY_ICON);

    //notify server
    ZeroMemory(@oasr, SizeOf(TExtOwnAvatarSetReply));
    oasr.SetResultCode := AVATAR_SET_SUCCESS;
    oasr.ReqID         := AvatarPic^.ReqID;
    EventsTP.OwnAvatarSetReply(UniqID, @oasr);
  end;

  //delete big icon if exists and doesnt matter what we are doing, setting or removing avatar
  FClient.OscarCL.DeleteBartItem(BART_BUDDY_ICON_BIG);
end;

{***************************************************************}
procedure TIcqProto.OwnPresInfoRequest(ReqOwnPresInfo: pExtReqOwnPresInfo);
begin
  //should be sent using ReqOwnPresInfo^.ReqID, but there are some proto restrictions
  FClient.RequestNickInfo;
end;

{***************************************************************}
procedure TIcqProto.OwnMailUrlRequest(ReqOwnMailUrl: pExtReqOwnMailUrl);
begin
  //nothing
end;

{***************************************************************}
procedure TIcqProto.ProcessDetsRequests;
var ExtDetsReq: TExtDetsReq;
    s: string;
    i: integer;
begin
  if FClient.OscarCL.OperBusy or (FListDetsReq.Count = 0) then Exit;

  s := FListDetsReq.Strings[0];
  FListDetsReq.Delete(0);
  i := Pos(THE_SPC, s);
  if (i < 1) then Exit;

  ZeroMemory(@ExtDetsReq, SizeOf(TExtDetsReq));
  ExtDetsReq.ReqID   := StrToIntDef(Copy(s, 1, i-1), 0); Delete(s, 1, i);
  ExtDetsReq.Account := s;

  DetailsRequest(@ExtDetsReq);
end;


end.
