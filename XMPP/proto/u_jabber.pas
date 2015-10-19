// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_jabber;

interface

uses Windows, Classes, SysUtils, OverbyteIcsWSocket, OverbyteIcsMimeUtils, OverbyteIcsSha1,
     h_jabber, u_jabber_hlp, u_xmppsock, NativeXML, u_auth, u_roster, u_roster_info, u_privacy,
     {$IFDEF GTALK}u_gtalk, u_gtalk_auth, {$ENDIF} u_ext_info;

type
  TConnectState   = (csOffline, csConnecting, csAuthDone, csOnline);
  TStreamFeatures = set of (sfStartTLS, sfCompressZlib, sfRegister, sfBind, sfSession);
  TSrvDiscoInfo   = set of (sdi_Ping, sdi_VCardTemp, sdi_MsgOffline);

  TOnContactPres  = procedure (Sender: TObject; aItem: pRosterItem; const Resource: string; OnlyAvatarChanged: Boolean) of object;
  TOnAuthEvent    = procedure (Sender: TObject; const Jid: string) of object;
  TOnAuthReply    = procedure (Sender: TObject; const Jid: string; const Granted: Boolean) of object;
  TOnIncMsg       = procedure (Sender: TObject; const IncMsg: TXmppIncMsg) of object;
  TOnMsgAck       = procedure (Sender: TObject; const MsgAck: TXmppMsgAck) of object;
  TOnChatState    = procedure (Sender: TObject; const ChatState: TXmppChatState) of object;
  TOnVCardRcvd    = procedure (Sender: TObject; const vCard: TExtUserDets; const Found: Boolean) of object;
  TOnAvatarRcvd   = procedure (Sender: TObject; const Jid: string; const avik: TAvatarRec) of object;
  TOnAvatarUpd    = procedure (Sender: TObject; const ReqID: DWord; const Success: Boolean) of object;
  TOnJabberNotify = procedure (Sender: TObject; const NotifType: Word; const JidSender, Msg, ClickUrl: string) of object;
  TOnMailNotify   = procedure (Sender: TObject; MailNotif: TExtMailNotif) of object;
  TOnMailUrlDone  = procedure (Sender: TObject; const MailUrl: string; const ReqID: DWord) of object;

  TJabberClient = class
  private
    FSocket             : TXmppSocket;
    FAuth               : TXmppAuth;
    FRoster             : TXmppRoster;
    FPrivacy            : TXmppPrivacy;
    FStreamFeatures     : TStreamFeatures;
    FSrvDiscoInfo       : TSrvDiscoInfo;
    FOwnPresItem        : TRosterItem;
    FPort               : string;
    FPassword           : string;
    FServer             : string;
    FUsername           : string;
    FAccount            : string;
    FDomain             : string;
    FResource           : string;
    FNewResource        : string;
    FStreamID           : string;
    FClientName         : string;
    FClientVer          : string;
    FClientOS           : string;
    FClientLang         : Word;
    FConnectState       : TConnectState;
    FConnectTicks       : DWord;
    FTrigger_Sock       : Boolean;
    FSecureConnect      : Boolean;
    FUseSrvQuery        : Boolean;
    FUseCompression     : Boolean;
    FUseStartTLS        : Boolean;
    FStatusShow         : string;
    FStatusAdvText      : string;
    FPriority           : ShortInt;
    FOwnVCard           : TExtUserDets;
    FTempVCard          : TExtUserDets;
    FOwnAvatar          : TAvatarRec;
    FTempAvatar         : TAvatarRec;

    FOnLoginDone        : TNotifyEvent;
    FOnLoginFailed      : TOnErrorMsg;
    FOnContactArrived   : TOnContactPres;
    FOnContactDeparted  : TOnContactPres;
    FOnPrivacyChanged   : TNotifyEvent;
    FOnAuthRequest      : TOnAuthEvent;
    FOnAuthReply        : TOnAuthReply;
    FOnAuthRevoke       : TOnAuthEvent;
    FOnIncMsg           : TOnIncMsg;
    FOnMsgAck           : TOnMsgAck;
    FOnChatState        : TOnChatState;
    FOnVCardRcvd        : TOnVCardRcvd;
    FOnVCardUpd         : TOnVCardRcvd;
    FOnAvatarUpd        : TOnAvatarUpd;
    FOnAvatarRcvd       : TOnAvatarRcvd;
    FOnJabberNotify     : TOnJabberNotify;
    FOnMailNotify       : TOnMailNotify;
    FOnMailUrlDone      : TOnMailUrlDone;
    FOnOwnPresInfo      : TNotifyEvent;

    {$IFDEF DEBUG}
    function  GetPktLog: TOnPktLog;
    procedure SetPktLog(const Value: TOnPktLog);
    {$ENDIF}

    function  GetAliveTicks: DWord;
    procedure SetAliveTicks(const Value: DWord);
    function  GetProxyInfo: TExtProxyInfo;
    procedure SetProxyInfo(const Value: TExtProxyInfo);
    function  GetAccInfo: Boolean;
    procedure SocketClose(const Abort: Boolean = False);
    procedure ResetData;
    function  ParseNodeXML(XmlNode: TXmlNode): Boolean;
    function  ProcessLogin(XmlNode: TXmlNode): Boolean;
    function  ProcessOnline(XmlNode: TXmlNode): Boolean;
    procedure ConnectFailed(Reason: string);
    procedure ParseOwnIQ(XmlNode: TXmlNode; ASeq: pXmppSeq);
    procedure ParseExtIQ(XmlNode: TXmlNode);
    procedure ParseSrvDiscoInfo(XmlNode: TXmlNode);
    procedure ParseCliDiscoInfo(const JidCanonic: string; XmlNode: TXmlNode);
    procedure ParsePresence(XmlNode: TXmlNode);
    procedure ParseOwnPresInfo(const APresType, AResource: string; XmlNode: TXmlNode);
    procedure ParseMessage(XmlNode: TXmlNode);
    procedure ParseClientVersion(const JidCanonic: string; XmlNode: TXmlNode);
    procedure ParseVCard(XmlNode: TXmlNode; SuccessOper: Boolean; const Jid: string; ReqID: DWord);
    procedure ParseVCardAvatar(const Jid: string; Node: TXmlNode);
    function  ParseErrorStanza(XmlNode: TXmlNode): string;
    procedure Send_HELLO;
    procedure Send_BYE;
    function  TryNextAuthMechanism: Boolean;
    procedure ClearAvatarRec(Avik: pAvatarRec);
    procedure DoJabberNotify(const NotifType: Word; const JidSender, Msg, ClickUrl: string);
    procedure UpdOwnResPresInfo;
    {$IFDEF GTALK}
    procedure DoMailNotify(const MailNotif: TExtMailNotif);
    {$ENDIF}
  protected
    procedure OnSocketConnect(Sender: TObject);
    procedure OnSocketInitedTLS(Sender: TObject);
    procedure OnSocketClosed(Sender: TObject);
    procedure OnSocketError(Sender: TObject; ErrCode: integer; Msg: string);
    procedure OnSocketXmlRcvd(Sender: TObject; const XmlTag: string);
    {$IFDEF GTALK}
    procedure OnAuthTokenDone(Sender: TObject; User, Token: string; MailUrl: Boolean; MailNotif: TExtMailNotif; ReqID: DWord; OwnUrlReq: Boolean);
    {$ENDIF}
  public
    {$IFDEF GTALK}
    GTalk      : TGoogleTalk;
    {$ENDIF}

    CookieMan  : TCookieMan;
    constructor Create;
    destructor  Destroy; override;

    function  Login: Boolean;
    procedure Logoff;
    procedure SendKeepAlive;
    procedure UpdateStatus(const StatusShow: string; const StatusAdvText: string = '');
    procedure UpdatePresence(const Jid: string = '');
    procedure SendPresTo(const Jid, PresType: string);
    procedure SendMsg(const XmppMsg: TXmppOutMsg);
    procedure SendMsgAck(const XmppAck: TXmppMsgAck);
    procedure SendChatState(const ChatState: TXmppChatState);
    function  RequestVCard(const Jid: string; const ReqID: DWord): Boolean;
    function  UpdateVCard(const vCard: TExtUserDets; const ReqID: DWord; const AvatarUpdate: Boolean = False): Boolean;
    function  RequestAvatar(const Jid: string): Boolean;
    function  UpdateAvatar(const ImgType: string; const BinVal: AnsiString; const ReqID: DWord): Boolean;

    property Roster           : TXmppRoster      read FRoster;
    property Privacy          : TXmppPrivacy     read FPrivacy;
    property CurResource      : string           read FResource;
    property ConnectState     : TConnectState    read FConnectState;
    property ConnectTicks     : DWord            read FConnectTicks      write FConnectTicks;
    property AliveTicks       : DWord            read GetAliveTicks      write SetAliveTicks;
    property Server           : string           read FServer            write FServer;
    property Port             : string           read FPort              write FPort;
    property Username         : string           read FUsername          write FUsername;
    property Password         : string           read FPassword          write FPassword;
    property ProxyInfo        : TExtProxyInfo    read GetProxyInfo       write SetProxyInfo;
    property SecureConnect    : Boolean          read FSecureConnect     write FSecureConnect;
    property UseCompression   : Boolean          read FUseCompression    write FUseCompression;
    property UseStartTLS      : Boolean          read FUseStartTLS       write FUseStartTLS;
    property Resource         : string           read FNewResource        write FNewResource;
    property Priority         : ShortInt         read FPriority          write FPriority;
    property StatusShow       : string           read FStatusShow        write FStatusShow;
    property StatusAdvText    : string           read FStatusAdvText     write FStatusAdvText;
    property ClientName       : string           read FClientName        write FClientName;
    property ClientVer        : string           read FClientVer         write FClientVer;
    property ClientOS         : string           read FClientOS          write FClientOS;
    property ClientLang       : Word             read FClientLang        write FClientLang;
    property OwnPresItem      : TRosterItem      read FOwnPresItem;

    property OnLoginDone      : TNotifyEvent     read FOnLoginDone       write FOnLoginDone;
    property OnLoginFailed    : TOnErrorMsg      read FOnLoginFailed     write FOnLoginFailed;
    property OnCntArrived     : TOnContactPres   read FOnContactArrived  write FOnContactArrived;
    property OnCntDeparted    : TOnContactPres   read FOnContactDeparted write FOnContactDeparted;
    property OnPrivacyChanged : TNotifyEvent     read FOnPrivacyChanged  write FOnPrivacyChanged;
    property OnAuthRequest    : TOnAuthEvent     read FOnAuthRequest     write FOnAuthRequest;
    property OnAuthReply      : TOnAuthReply     read FOnAuthReply       write FOnAuthReply;
    property OnAuthRevoke     : TOnAuthEvent     read FOnAuthRevoke      write FOnAuthRevoke;
    property OnIncMsg         : TOnIncMsg        read FOnIncMsg          write FOnIncMsg;
    property OnMsgAck         : TOnMsgAck        read FOnMsgAck          write FOnMsgAck;
    property OnChatState      : TOnChatState     read FOnChatState       write FOnChatState;
    property OnVCardRcvd      : TOnVCardRcvd     read FOnVCardRcvd       write FOnVCardRcvd;
    property OnVCardUpdRes    : TOnVCardRcvd     read FOnVCardUpd        write FOnVCardUpd;
    property OnAvatarUpdRes   : TOnAvatarUpd     read FOnAvatarUpd       write FOnAvatarUpd;
    property OnAvatarRcvd     : TOnAvatarRcvd    read FOnAvatarRcvd      write FOnAvatarRcvd;
    property OnJabberNotify   : TOnJabberNotify  read FOnJabberNotify    write FOnJabberNotify;
    property OnMailNotify     : TOnMailNotify    read FOnMailNotify      write FOnMailNotify;
    property OnMailUrlDone    : TOnMailUrlDone   read FOnMailUrlDone     write FOnMailUrlDone;
    property OnOwnPresInfo    : TNotifyEvent     read FOnOwnPresInfo     write FOnOwnPresInfo;

    {$IFDEF DEBUG}
    property OnPktLog         : TOnPktLog        read GetPktLog          write SetPktLog;
    {$ENDIF}
  end;

implementation

{ TJabberClient }
{*****************************************************************}
constructor TJabberClient.Create;
begin
  FSocket                    := TXmppSocket.Create;
  FSocket.OnNetConnected     := OnSocketConnect;
  FSocket.OnNetInitedTLS     := OnSocketInitedTLS;
  FSocket.OnNetClosed        := OnSocketClosed;
  FSocket.OnNetError         := OnSocketError;
  FSocket.OnXmlRcvd          := OnSocketXmlRcvd;

  FAuth     := TXmppAuth.Create;
  FRoster   := TXmppRoster.Create(FSocket);
  FPrivacy  := TXmppPrivacy.Create(FSocket);

  //helpers create
  CookieMan := TCookieMan.Create;

  {$IFDEF GTALK}
  GTalk := TGoogleTalk.Create(FSocket);
  GTalk.GAuth.OnThreadTokenDone := OnAuthTokenDone;
  {$ENDIF}
end;

{*****************************************************************}
destructor TJabberClient.Destroy;
begin
  {$IFDEF GTALK}
  GTalk.Free;
  {$ENDIF}

  CookieMan.Free;

  FRoster.Free;
  FPrivacy.Free;
  FAuth.Free;

  FSocket.Free;
  inherited;
end;

{*****************************************************************}
function TJabberClient.GetAliveTicks: DWord;
begin
  Result := FSocket.AliveTicks;
end;

{*****************************************************************}
procedure TJabberClient.SetAliveTicks(const Value: DWord);
begin
  FSocket.AliveTicks := Value;
end;

{*****************************************************************}
function TJabberClient.GetAccInfo: Boolean;
var i: integer;
    s: string;
begin
  Result := False;

  FUsername := Trim(FUsername);
  FServer   := Trim(FServer);
  FPort     := Trim(FPort);

  FAccount  := '';
  FDomain   := '';
  FResource := FNewResource;

  FUseSrvQuery := False;

  if (FUsername = '') or (FPassword = '') or (FPort = '') then Exit;

  s := FUsername;

  //get account
  i := Pos('@', s);
  if (i = 0) then Exit;
  FAccount := Copy(s, 1, i-1);
  Delete(s, 1, i);

  //get domain
  if (s = '') then Exit;
  i := Pos('/', s); //if canonical name with resource
  if (i > 0) then
  begin
    FDomain := Copy(s, 1, i-1);
    Delete(s, 1, i);

    if (FResource = '') then
      FResource := s;

    FUsername := FAccount + '@' + FDomain;
  end
  else
  begin
    FDomain := s;
    if (FResource = '') then
      FResource := 'BimTP';
  end;

  //for SRV record request if no server specified
  if (FServer = '') then
    FUseSrvQuery := True;

  Result := True;
end;

{$IFDEF DEBUG}
{*****************************************************************}
function TJabberClient.GetPktLog: TOnPktLog;
begin
  Result := FSocket.OnPktLog;
end;

{*****************************************************************}
procedure TJabberClient.SetPktLog(const Value: TOnPktLog);
begin
  FSocket.OnPktLog := Value;
end;
{$ENDIF}

{*****************************************************************}
function TJabberClient.GetProxyInfo: TExtProxyInfo;
begin
  Result := FSocket.ProxyInfo;
end;

{*****************************************************************}
procedure TJabberClient.SetProxyInfo(const Value: TExtProxyInfo);
begin
  FSocket.ProxyInfo := Value;

  {$IFDEF GTALK}
  GTalk.GAuth.ProxyInfo := Value;
  {$ENDIF}
end;

{*****************************************************************}
function TJabberClient.Login: Boolean;
begin
  Result := False;
  if not GetAccInfo then
  begin
    ConnectFailed(XHLP_BAD_USERNAME);
    Exit;
  end;
  Result := True;

  if (FSocket.Socket.State <> wsClosed) then
  begin
    FTrigger_Sock := True;
    SocketClose(True);
  end;

  ResetData;

  if FUseSrvQuery then
    FSocket.Server := FDomain
  else
    FSocket.Server := FServer;

  FSocket.Port      := FPort;
  FSocket.SecureSSL := FSecureConnect;

  FConnectState := csConnecting;

  FSocket.Connect(FUseSrvQuery);
end;

{*****************************************************************}
procedure TJabberClient.Logoff;
begin
  if (FConnectState = csOffline) then
    Exit;

  if (FConnectState = csOnline) then
    Send_BYE;

  FTrigger_Sock := True;
  SocketClose;
end;

{*****************************************************************}
procedure TJabberClient.ResetData;
begin
  FTrigger_Sock := False;

  FStreamID := '';
  FStreamFeatures := [];
  FSrvDiscoInfo   := [];

  FConnectTicks := 0;

  ClearAvatarRec(@FOwnAvatar);
  ClearAvatarRec(@FTempAvatar);

  Finalize(FOwnVCard);
  ZeroMemory(@FOwnVCard, SizeOf(TExtUserDets));
  Finalize(FTempVCard);
  ZeroMemory(@FTempVCard, SizeOf(TExtUserDets));

  FAuth.ResetData;
  FRoster.ResetData;
  FPrivacy.ResetData;

  Finalize(FOwnPresItem);
  ZeroMemory(@FOwnPresItem, SizeOf(TRosterItem));

  {$IFDEF GTALK}
  GTalk.ResetData;
  {$ENDIF}
end;

{*****************************************************************}
procedure TJabberClient.ClearAvatarRec(Avik: pAvatarRec);
begin
  Finalize(Avik^);
  ZeroMemory(Avik, SizeOf(TAvatarRec));
end;

{*****************************************************************}
procedure TJabberClient.SocketClose(const Abort: Boolean = False);
begin
  FConnectState := csOffline;

  if (FSocket.Socket.State <> wsClosed) then
    FSocket.Disconnect(Abort);
end;

{*****************************************************************}
procedure TJabberClient.OnSocketClosed(Sender: TObject);
begin
  if FTrigger_Sock then Exit;
  FTrigger_Sock := True;

  SocketClose;
  if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, 'Socket closed');
end;

{*****************************************************************}
procedure TJabberClient.OnSocketError(Sender: TObject; ErrCode: integer; Msg: string);
begin
  if FTrigger_Sock then Exit;
  FTrigger_Sock := True;

  SocketClose;
  if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, 'Socket error ' + IntToStr(ErrCode));
end;

{*****************************************************************}
procedure TJabberClient.OnSocketConnect(Sender: TObject);
begin
  Send_HELLO;
end;

{*****************************************************************}
procedure TJabberClient.OnSocketInitedTLS(Sender: TObject);
begin
  Send_HELLO;
end;

{*****************************************************************}
procedure TJabberClient.OnSocketXmlRcvd(Sender: TObject; const XmlTag: string);
var XML: TNativeXml;
    i: integer;
begin
  XML := TNativeXml.Create(nil);
  try
    try
      XML.ReadFromString(UTF8Encode(XmlTag));
    except Exit; end;

    if not XML.IsEmpty then
    begin
      for i := 0 to XML.RootNodes.Count-1 do
      begin
        if not ParseNodeXML(XML.RootNodes.Items[i]) then
          Break;
      end;//for
    end;
  finally
    XML.Free;
  end;
end;

{*****************************************************************}
procedure TJabberClient.ConnectFailed(Reason: string);
begin
  FTrigger_Sock := True;
  SocketClose;

  if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, Reason);
end;

{*****************************************************************}
function TJabberClient.ParseNodeXML(XmlNode: TXmlNode): Boolean;
begin
  Result := False;
  if (XmlNode = nil) then Exit;

  //reset connection timeout value on every new XML rcvd
  if (ConnectState in [csConnecting, csAuthDone]) then
    FConnectTicks := 0;

  //reset alive ticks on every new XML rcvd after login
  if (ConnectState = csOnline) then
    FSocket.AliveTicks := 0;

  case FConnectState of
    csOffline    :;
    csConnecting : Result := ProcessLogin(XmlNode);
    csAuthDone,
    csOnline     : Result := ProcessOnline(XmlNode);
  end;//case
end;

{*****************************************************************}
function TJabberClient.ProcessLogin(XmlNode: TXmlNode): Boolean;
var Node: TXmlNode;
    i: integer;
    sName, s: string;
begin
  Result := False;
  sName  := UTF8ToUnicodeString(XmlNode.Name);

  //===============================================
  //******* stream:error
  if (sName = XNS_STREAM_ERROR) then
  begin
    s := XHLP_NO_REASON;
    if (XmlNode.ChildContainerCount > 0) then
      s := UTF8ToUnicodeString( XmlNode.ChildContainers[0].Name );

    ConnectFailed(s);
    Exit;
  end
  else
  //===============================================
  //******* xmppbye, end
  if (sName = XNS_BYE) then
  begin
    ConnectFailed(XHLP_STREAM_CLOSED);
    Exit;
  end
  else
  //===============================================
  //******* stream:stream, start
  if (sName = XNS_STREAM_STREAM) then
  begin
    //get stream ID
    FStreamID := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_ID)];
  end
  else
  //===============================================
  //******* stream:features
  if (sName = XNS_STREAM_FEATURES) then
  begin
    //starttls
    Node := XmlNode.NodeByName(XNS_START_TLS);
    if (Node <> nil) then
    begin
      FStreamFeatures := FStreamFeatures + [sfStartTLS];

      if (Node.NodeByName(UTF8Encode(XNS_REQUIRED)) <> nil) then
      begin
        FSocket.Send(XmlTag(XTAG_START_TLS_EMPTY));
        Exit;
      end;
    end;

    if not FSocket.Socket.SslEnable and FUseStartTLS and (sfStartTLS in FStreamFeatures) then
    begin
      FSocket.Send(XmlTag(XTAG_START_TLS_EMPTY));
      Exit;
    end;

    //register
    Node := XmlNode.NodeByName(UTF8Encode(XNS_REGISTER));
    if (Node <> nil) then
      FStreamFeatures := FStreamFeatures + [sfRegister];

    //XEP-0138: Stream Compression
    Node := XmlNode.NodeByName(UTF8Encode(XNS_COMPRESSION));
    if (Node <> nil) then
    begin
      for i := 0 to Node.ChildContainerCount-1 do
      begin
        if not WideSameText(UTF8ToUnicodeString(Node.ChildContainers[i].Name), XNS_METHOD) then Continue;

        if WideSameText(Node.ChildContainers[i].ValueUnicode, XVAL_ZLIB) then
          FStreamFeatures := FStreamFeatures + [sfCompressZlib];
      end;//for
    end;

    {compression should not be used for TLS/SSL connections}
    if not FSocket.Socket.SslEnable and FUseCompression and (sfCompressZlib in FStreamFeatures) and not FSocket.StreamCompressed then
    begin
      FSocket.Send(XmlTag(XTAG_COMPRESS, XVAL_ZLIB));
      Exit;
    end;

    //mechanisms
    Node := XmlNode.NodeByName(UTF8Encode(XNS_MECHANISMS));
    if (Node <> nil) then
    begin
      for i := 0 to Node.ChildContainerCount-1 do
      begin
        if not WideSameText(UTF8ToUnicodeString(Node.ChildContainers[i].Name), XNS_MECHANISM) then Continue;

        if WideSameText(Node.ChildContainers[i].ValueUnicode, XVAL_PLAIN) then
          FAuth.Mechanisms := FAuth.Mechanisms + [amPlain]
        else
        {$IFDEF GTALK}
        if WideSameText(Node.ChildContainers[i].ValueUnicode, XVAL_XGOOGLE_TOKEN) then
          FAuth.Mechanisms := FAuth.Mechanisms + [amXGoogleToken]
        else
        {$ENDIF}
        if WideSameText(Node.ChildContainers[i].ValueUnicode, XVAL_SCRAM_SHA1) then
          FAuth.Mechanisms := FAuth.Mechanisms + [amScramSha1]
        else
        if WideSameText(Node.ChildContainers[i].ValueUnicode, XVAL_DIGEST_MD5) then
          FAuth.Mechanisms := FAuth.Mechanisms + [amDigestMD5]
        else
        if WideSameText(Node.ChildContainers[i].ValueUnicode, XVAL_CRAM_MD5) then
          FAuth.Mechanisms := FAuth.Mechanisms + [amCramMD5];
      end;//for
    end
    else
    begin
      ConnectFailed(XHLP_NO_AUTH_MECHANISMS);
      Exit;
    end;

    if (FAuth.Mechanisms = []) then
    begin
      ConnectFailed(XHLP_SUP_AUTH_NOT_FOUND);
      Exit;
    end;

    //to try other mechanisms on fail
    FAuth.MechsRemain := FAuth.Mechanisms;

    TryNextAuthMechanism;
  end
  else
  //===============================================
  //******* TLS
  if (sName = XNS_PROCEED) then
  begin
    if WideSameText(XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_URI + XURI_XMPP_TLS) then
      FSocket.StartTLS;
  end
  else
  //===============================================
  //******* compression
  if (sName = XNS_COMPRESSED) then
  begin
    FSocket.StreamCompressed := True;
    Send_HELLO;
  end
  else
  //===============================================
  //******* challenge
  if (sName = XNS_CHALLENGE) then
  begin
    case FAuth.CurAuthMech of
      amPlain     : ;
      amScramSha1 :
        begin
          s := FAuth.GenScramSha1Response(XmlNode.ValueUnicode, FAccount, FPassword);
          if (s <> '') then
            FSocket.Send(s)
          else
          begin
            ConnectFailed(XHLP_NO_REASON);
            Exit;
          end;
        end;
      amDigestMD5 : FSocket.Send(FAuth.GenDigestMD5Response( UTF8ToUnicodeString( RawByteString(Base64Decode(XmlNode.ValueUnicode))), FAccount, FPassword, FDomain));
      amCramMD5   : FSocket.Send(FAuth.GenCramMD5Response(RawByteString(Base64Decode(XmlNode.ValueUnicode)), AnsiString(FAccount), AnsiString(FPassword)));
    end;
  end
  else
  //===============================================
  //******* success
  if (sName = XNS_SUCCESS) then
  begin
    case FAuth.CurAuthMech of
      amPlain     : ;
      amScramSha1 :
        begin
          if not FAuth.ValidateScramSha1Signature(XmlNode.ValueUnicode) then
          begin
            ConnectFailed(XHLP_NO_REASON);
            Exit;
          end;
        end;
      amDigestMD5 :;
      amCramMD5   :;
    end;

    FConnectState := csAuthDone;

    //start stream again
    Send_HELLO;
  end
  else
  //===============================================
  //******* failure
  if (sName = XNS_FAILURE) then
  begin
    {$IFDEF GTALK}
    //google closes connection after first auth attempt,
    //so we cant even try other auth mechanisms
    FAuth.MechsRemain := [];
    {$ENDIF}

    if not TryNextAuthMechanism then
    begin
      s := XHLP_NO_REASON;
      if (XmlNode.ChildContainerCount > 0) then
        s := UTF8ToUnicodeString( XmlNode.ChildContainers[0].Name );

      //disconnect
      ConnectFailed(s);
      Exit;
    end;
  end;

  //allow processing next XML Nodes
  Result := True;
end;

{*****************************************************************}
function TJabberClient.TryNextAuthMechanism: Boolean;
begin
  Result := True;

  if (amScramSha1 in FAuth.MechsRemain) then
  begin
    FAuth.CurAuthMech := amScramSha1;
    FAuth.MechsRemain := FAuth.MechsRemain - [amScramSha1];
    FSocket.Send(XmlTag(XTAG_AUTH_MECHANISM2, XVAL_SCRAM_SHA1, FAuth.GenScramSha1ClientMsg(FAccount, True)));
  end
  else
  if (amDigestMD5 in FAuth.MechsRemain) then
  begin
    FAuth.CurAuthMech := amDigestMD5;
    FAuth.MechsRemain := FAuth.MechsRemain - [amDigestMD5];
    FSocket.Send(XmlTag(XTAG_AUTH_MECHANISM1, XVAL_DIGEST_MD5));
  end
  else
  if (amCramMD5 in FAuth.MechsRemain) then
  begin
    FAuth.CurAuthMech := amCramMD5;
    FAuth.MechsRemain := FAuth.MechsRemain - [amCramMD5];
    FSocket.Send(XmlTag(XTAG_AUTH_MECHANISM1, XVAL_CRAM_MD5));
  end
  else
  if (amPlain in FAuth.MechsRemain) then
  begin
    FAuth.CurAuthMech := amPlain;
    FAuth.MechsRemain := FAuth.MechsRemain - [amPlain];
    FSocket.Send(XmlTag(XTAG_AUTH_MECHANISM2, XVAL_PLAIN,
                        String( Base64Encode( UTF8Encode(FUsername) + AnsiChar(#0) +
                                              UTF8Encode(FAccount) + AnsiChar(#0) +
                                              UTF8Encode(FPassword) ) ) ));
  end
  else
  {$IFDEF GTALK}
  //blocking operation, reserved, will never be used because google server disconnects after first auth mechanism fails
  if (amXGoogleToken in FAuth.MechsRemain) then
  begin
    FAuth.CurAuthMech := amXGoogleToken;
    FAuth.MechsRemain := FAuth.MechsRemain - [amXGoogleToken];
    FSocket.Send(XmlTag(XTAG_AUTH_MECHANISM2, XVAL_XGOOGLE_TOKEN,
                 String( Base64Encode(AnsiChar(#0) + UTF8Encode(FUsername) + AnsiChar(#0) +
                                      AnsiString(GTalk.GAuth.IssueAuthToken(FUsername, FPassword)))) ) );
  end
  else
  {$ENDIF}
    Result := False;
end;

{*****************************************************************}
function TJabberClient.ProcessOnline(XmlNode: TXmlNode): Boolean;
var Node: TXmlNode;
    sName, s: string;
    i: integer;
    aSeq: pXmppSeq;
begin
  Result := False;
  sName  := UTF8ToUnicodeString(XmlNode.Name);

  //===============================================
  //******* stream:error
  if (sName = XNS_STREAM_ERROR) then
  begin
    s := XHLP_NO_REASON;
    if (XmlNode.ChildContainerCount > 0) then
      s := UTF8ToUnicodeString( XmlNode.ChildContainers[0].Name );

    ConnectFailed(s);
    Exit;
  end
  else
  //===============================================
  //******* xmppbye, end
  if (sName = XNS_BYE) then
  begin
    ConnectFailed(XHLP_STREAM_CLOSED);
    Exit;
  end
  else
  //===============================================
  //******* stream:stream, start
  if (sName = XNS_STREAM_STREAM) then
  begin
    //get stream ID
    FStreamID := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_ID)];
  end
  else
  //===============================================
  //******* stream:features
  if (sName = XNS_STREAM_FEATURES) then
  begin
    //check for resource binding support
    Node := XmlNode.NodeByName(UTF8Encode(XNS_BIND));
    if (Node <> nil) then
    begin
      FStreamFeatures := FStreamFeatures + [sfBind];

      //bind resource
      if (FResource = '') then
        s := XmlTagIq(XVAL_SET, FSocket.NewSeqNum(SEQT_BIND), XmlTag(XTAG_BIND_EMPTY))
      else
        s := XmlTagIq(XVAL_SET, FSocket.NewSeqNum(SEQT_BIND), XmlTag(XTAG_BIND, XmlTag(XTAG_RESOURCE, FResource)));

      FSocket.Send(s);
    end;

    //check for IM sessions support
    if (XmlNode.NodeByName(UTF8Encode(XNS_SESSION)) <> nil) then
      FStreamFeatures := FStreamFeatures + [sfSession];
  end
  else
  //===============================================
  //******* iq
  if (sName = XNS_IQ) then
  begin
    //check request id
    s := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_ID)];

    i := Pos(SEQ_ID_BIMOID + FSocket.SeqUID + SEQ_DIV, s);
    if (i = 1) then
    begin
      Delete(s, 1, Length(SEQ_ID_BIMOID + FSocket.SeqUID + SEQ_DIV));
      i := StrToIntDef(s, -1);
      if (i > -1) and (i <= SEQ_MAX_NUM) then
      begin
        aSeq := FSocket.GetSeqNum(i);
        if (aSeq <> nil) then
        begin
          try
            ParseOwnIQ(XmlNode, aSeq);
          finally
            FSocket.DelSeqNum(aSeq^.SeqNum);
          end;
        end;
      end;
    end
    else
      ParseExtIQ(XmlNode);
  end
  else
  //===============================================
  //******* presence
  if (sName = XNS_PRESENCE) then
  begin
    ParsePresence(XmlNode);
  end
  else
  //===============================================
  //******* message
  if (sName = XNS_MESSAGE) then
  begin
    ParseMessage(XmlNode);
  end;

  //allow processing next XML Nodes
  Result := True;
end;

{*****************************************************************}
procedure TJabberClient.ParseOwnIQ(XmlNode: TXmlNode; ASeq: pXmppSeq);
var Node: TXmlNode;
    sSender, sRecver, s: string;
    bSuccess: Boolean;
    {$IFDEF GTALK}
    MailNotif: TExtMailNotif;
    {$ENDIF}
begin
  sSender := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_FROM)];
  sRecver := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_TO)];

  bSuccess := LowerCase(XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_TYPE)]) = XVAL_RESULT;
  //cant check rcvr here because resource is not binded at this time
  //bRcvrOk  := (sRecver = '') or WideSameText(sRecver, CanonicName(FUsername, FResource)) or WideSameText(sRecver, FUsername);

  //commands results
  case ASeq^.SeqType of
   {========================================================}
    SEQT_BIND:
      begin
        Node := XmlNode.NodeByName(UTF8Encode(XNS_BIND));
        if (Node <> nil) then
        begin
          Node := Node.NodeByName(UTF8Encode(XNS_JID));
          if (Node <> nil) then
          begin
            s := Node.ValueUnicode;
            if (s <> '') then
              CanonicToNorm(s, FUsername, FAccount, FDomain, FResource);
          end;
        end;

        //send session support after login
        if (sfSession in FStreamFeatures) then
          FSocket.Send(XmlTagIq(XVAL_SET, FSocket.NewSeqNum(SEQT_SESSION), XmlTag(XTAG_SESSION_EMPTY)));
      end;
   {========================================================}
    SEQT_SESSION:
      begin
        //other after login requests
        FSocket.StartCollect;
        try
          {$IFDEF XMPP}
          //XEP-0030: Service Discovery, request server discovery info
          FSocket.Send(XmlTagIqQuery(XVAL_GET, CanonicName(FUsername, FResource), FDomain, FSocket.NewSeqNum(SEQT_SRV_DISCO_INFO), XNS_JABBER_DISCO_INFO));

          //request roster groups delimiter, not needed for Bimoid Transport
          //FSocket.Send(XmlTagIqQuery(XVAL_GET, FSocket.NewSeqNum(SEQT_ROSTER_DELIM), XNS_JABBER_IQ_PRIVATE, XmlTag(XTAG_ROSTER_EMPTY, XNS_ROSTER_DELIMITER) ) );

          //request privacy lists
          FSocket.Send(XmlTagIqQuery(XVAL_GET, FSocket.NewSeqNum(SEQT_PRIVACY), XNS_JABBER_IQ_PRIVACY));
          {$ENDIF}

          {$IFDEF GTALK}
          FPrivacy.Supported := False;

          GTalk.Send_SetUserSettings(FUsername, True);
          GTalk.Send_GetSharedStatus(FUsername);
          GTalk.Send_GetUserSettings(FUsername, True);

          if not FRoster.RequestedOnce then
            FRoster.Send_RequestRoster;
          {$ENDIF}
        finally
          FSocket.EndCollect;
        end;
      end;
   {========================================================}
    SEQT_SRV_DISCO_INFO:
      begin
        if bSuccess then
        begin
          Node := XmlNode.NodeByName(UTF8Encode(XNS_QUERY));
          if (Node <> nil) and WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_JABBER_DISCO_INFO) then
            ParseSrvDiscoInfo(Node);
        end;
      end;
   {========================================================}
    SEQT_CLI_DISCO_INFO:
      begin
        if bSuccess then
        begin
          Node := XmlNode.NodeByName(UTF8Encode(XNS_QUERY));
          if (Node <> nil) and WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_JABBER_DISCO_INFO) then
            ParseCliDiscoInfo(ASeq^.StrVal, Node);
        end;
      end;
   {========================================================}
    SEQT_CLIENT_PING:
      begin
        //server returned ping result
        if not bSuccess then
          FSrvDiscoInfo := FSrvDiscoInfo - [sdi_Ping];
      end;
   {========================================================}
    SEQT_ROSTER_DELIM:
      begin
        //not needed and not used for bimoid transport
        {Node := XmlNode.NodeByName(UTF8Encode(XNS_QUERY));
        if (Node <> nil) and WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_JABBER_IQ_PRIVATE) then
        begin
          Node := Node.NodeByName(UTF8Encode(XNS_ROSTER));
          if (Node <> nil) and WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_ROSTER_DELIMITER) then
          begin
            if bSuccess then
              FRoster.GroupsDelim.GroupsDelimiter := Node.ValueUnicode
            else
              FRoster.GroupsDelim.Supported := False;
          end;
        end;}
      end;
   {========================================================}
    SEQT_PRIVACY:
      begin
        FSocket.StartCollect;
        try

        //parse privacy lists
        Node := XmlNode.NodeByName(UTF8Encode(XNS_QUERY));
        if (Node <> nil) and WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_JABBER_IQ_PRIVACY) then
        begin
          if bSuccess then
          begin
            FPrivacy.Rcvd_Lists(Node);

            if FPrivacy.ListExists(LIST_BIMOID) then
              FPrivacy.Send_RequestList(LIST_BIMOID, SEQT_REQUEST_LIST);
          end
          else
            FPrivacy.Supported := False;
        end;

        if ( not FPrivacy.Supported or not FPrivacy.ListExists(LIST_BIMOID) ) and not FRoster.RequestedOnce then
            FRoster.Send_RequestRoster;

        finally
          FSocket.EndCollect;
        end;
      end;
   {========================================================}
    SEQT_REQUEST_LIST:
      begin
        if bSuccess then
        begin
          Node := XmlNode.NodeByName(UTF8Encode(XNS_QUERY));
          if (Node <> nil) and WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_JABBER_IQ_PRIVACY) then
            FPrivacy.Rcvd_ItemsList(ASeq^.StrVal, Node);
        end;

        if not FRoster.RequestedOnce then FRoster.Send_RequestRoster;
      end;
   {========================================================}
    SEQT_REQ_PUSHED_LIST:
      begin
        Node := XmlNode.NodeByName(UTF8Encode(XNS_QUERY));
        if (Node <> nil) and WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_JABBER_IQ_PRIVACY) then
        begin
          if bSuccess then
          begin
            FPrivacy.Rcvd_ItemsList(ASeq^.StrVal, Node);
            if Assigned(FOnPrivacyChanged) then FOnPrivacyChanged(Self);
          end
          else
          begin
            Node := XmlNode.NodeByName(UTF8Encode(XNS_ERROR));
            if (Node <> nil) then
            begin
              if (Node.NodeByName(UTF8Encode(XNS_ITEM_NOT_FOUND)) <> nil) then
              begin
                FPrivacy.DelList(ASeq^.StrVal);
                if Assigned(FOnPrivacyChanged) then FOnPrivacyChanged(Self);
              end;
            end;
          end;
        end;
      end;
   {========================================================}
    SEQT_ACTIVE_LIST:
      begin
        if bSuccess then
          FPrivacy.Rcvd_ActiveListSet(ASeq^.StrVal);

        UpdatePresence;
      end;
   {========================================================}
    SEQT_UPDCUR_LIST:
      begin
        if not WideSameText(FPrivacy.ListActive, LIST_BIMOID) and FPrivacy.ListExists(LIST_BIMOID) then
          FPrivacy.Send_SetActiveList(LIST_BIMOID)
        else
          UpdatePresence;
      end;
   {========================================================}
    SEQT_ROSTER:
      begin
        if bSuccess then
        begin
          Node := XmlNode.NodeByName(UTF8Encode(XNS_QUERY));
          if (Node <> nil) and WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_JABBER_IQ_ROSTER) then
            FRoster.Rcvd_Roster(Node);
        end;

        //change connect state after rcving roster
        FConnectState := csOnline;
        if Assigned(FOnLoginDone) then FOnLoginDone(Self);

        UpdateStatus(FStatusShow, FStatusAdvText);
      end;
   {========================================================}
    SEQT_ROSTER_OPER:
      begin
        //Most jabber servers reply with result even if they actually did not accept roster oper. They just dont send roster push.
        if not bSuccess then
          FRoster.CancelRosterOper(ASeq^.StrVal, ASeq^.IntTag, pRosterItem(ASeq^.Data));
      end;
   {========================================================}
    SEQT_CLIENT_VER:
      begin
        if bSuccess then
        begin
          Node := XmlNode.NodeByName(UTF8Encode(XNS_QUERY));
          if (Node <> nil) and WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_JABBER_IQ_VERSION) then
            ParseClientVersion(ASeq^.StrVal, Node);
        end;
      end;
   {========================================================}
    SEQT_REQ_VCARD:
      begin
        ParseVCard(XmlNode.NodeByName(UTF8Encode(XNS_VCARD)), bSuccess, ASeq^.StrVal, DWord(ASeq^.IntTag));
      end;
   {========================================================}
    SEQT_UPD_VCARD:
      begin
        //update own vcard if success
        if bSuccess then
        begin
          FOwnVCard := FTempVCard;
          //clear temp to save mem
          Finalize(FTempVCard);
          ZeroMemory(@FTempVCard , SizeOf(TExtUserDets));
        end;

        FOwnVCard.ReqID := DWord(ASeq^.IntTag);

        if Assigned(FOnVCardUpd) then FOnVCardUpd(Self, FOwnVCard, bSuccess);
      end;
   {========================================================}
    SEQT_UPD_VCARD_AVATAR:
      begin
        //update own avatar if success
        if bSuccess then
        begin
          FOwnAvatar := FTempAvatar;
          UpdatePresence;
        end;

        ClearAvatarRec(@FTempAvatar);

        if Assigned(FOnAvatarUpd) then FOnAvatarUpd(Self, DWord(ASeq^.IntTag), bSuccess);
      end;
   {========================================================}
    SEQT_REQ_VCARD_AVATAR:
      begin
        if bSuccess then
        begin
          Node := XmlNode.NodeByName(UTF8Encode(XNS_VCARD));
          if (Node <> nil) then
            ParseVCardAvatar(ASeq^.StrVal, Node.NodeByName(UTF8Encode(XNS_PHOTO)))
          else
            ParseVCardAvatar(ASeq^.StrVal, nil);
        end;
      end;
   {========================================================}
   {$IFDEF GTALK}
   {========================================================}
    SEQT_GTALK_GET_SET_LOGIN,
    SEQT_GTALK_GET_SETTING:
      begin
        if bSuccess then
        begin
          Node := XmlNode.NodeByName(UTF8Encode(XNS_GT_USER_SETTING));
          if (Node <> nil) then
            GTalk.Rcvd_Setting(Node, FUsername, ASeq^.SeqType = SEQT_GTALK_GET_SET_LOGIN);
        end;

        if (ASeq^.SeqType = SEQT_GTALK_GET_SET_LOGIN) then
          GTalk.Send_GetNewMail(True, True);
      end;
   {========================================================}
    SEQT_GTALK_SET_SETTING:
      begin
        //
      end;
   {========================================================}
    SEQT_GTALK_GET_SHARED_ST:
      begin
        if bSuccess then
        begin
          Node := XmlNode.NodeByName(UTF8Encode(XNS_QUERY));
          if (Node <> nil) and WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_GT_SHARED_STATUS) then
            GTalk.Rcvd_SharedStatus(Node);
        end;
      end;
   {========================================================}
    SEQT_GTALK_SET_SHARED_ST:
      begin
        if not WideSameText(FStatusShow, PRES_SHOW_DND) then //to prevent duplicating double sending
          UpdatePresence;
      end;
   {========================================================}
    SEQT_GTALK_GET_UNREAD_MAIL,
    SEQT_GTALK_GET_NEW_MAIL:
      begin
        if bSuccess then
        begin
          Node := XmlNode.NodeByName(UTF8Encode(XNS_GT_MAILBOX));
          if (Node <> nil) and WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)], XNS_GT_MAIL_NOTIFY) then
          begin
            GTalk.Rcvd_NewMail(Node, MailNotif, ASeq^.SeqType = SEQT_GTALK_GET_UNREAD_MAIL);

            if GTalk.Settings.MailNotifications then
            begin
              MailNotif.RcvrEmail := FUsername;

              if (ASeq^.SeqType = SEQT_GTALK_GET_UNREAD_MAIL) then
                DoMailNotify(MailNotif)
              else
                GTalk.GAuth.IssueAuthTokenThread(FUsername, FPassword, True, @MailNotif);
            end;
          end;
        end;
      end;
   {========================================================}
   {$ENDIF}
   {========================================================}
  end;//case
end;

{*****************************************************************}
procedure TJabberClient.ParseExtIQ(XmlNode: TXmlNode);
var Node: TXmlNode;
    sNS, sName, sSender, sRecver, sType, sID, s, sNode: string;
    bRcvrOk, bGet, bSet, bHandled: Boolean;
begin
  sSender := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_FROM)];
  sRecver := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_TO)];
  sType   := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_TYPE)];
  sID     := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_ID)];

  bRcvrOk := (sRecver = '') or WideSameText(sRecver, CanonicName(FUsername, FResource)) or WideSameText(sRecver, FUsername);
  if not bRcvrOk then Exit;

  //get first child node
  if (XmlNode.ChildContainerCount = 0) then Exit;
  Node := XmlNode.ChildContainers[0];
  if (Node = nil) then Exit;

  sName := UTF8ToUnicodeString(Node.Name);
  sNS   := LowerCase(Node.AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)]);
  bGet  := WideSameText(sType, XVAL_GET);
  bSet  := WideSameText(sType, XVAL_SET);

  //will reply only to "get" and "set" requests
  if not bGet and not bSet then Exit;

  bHandled := True;

  if WideSameText(sName, XNS_QUERY) then
  begin
    //====== client disco info ========================
    if bGet and WideSameText(sNS, XNS_JABBER_DISCO_INFO) then
    begin
      sNode := Node.AttributeValueByNameWide[UTF8Encode(XATT_NODE)];

      s := FClientName;
      if (s = '') then s := SEQ_ID_BIMOID;

      s := XmlTag(XTAG_IDENTITY_EMPTY, [XVAL_CLIENT, XVAL_PC, s]) +
           XmlTag(XTAG_FEATURE_EMPTY, XNS_JABBER_DISCO_INFO) +
           XmlTag(XTAG_FEATURE_EMPTY, XNS_JABBER_IQ_PRIVACY) +
           XmlTag(XTAG_FEATURE_EMPTY, XNS_JABBER_IQ_VERSION) +
           XmlTag(XTAG_FEATURE_EMPTY, XNS_JABBER_CHAT_STATES) +
           XmlTag(XTAG_FEATURE_EMPTY, XNS_XMPP_RECEIPTS) +
           XmlTag(XTAG_FEATURE_EMPTY, XNS_XMPP_PING);

      if (sNode = '') then
        s := XmlTag(XTAG_QUERY, XNS_JABBER_DISCO_INFO, s)
      else
        s := XmlTag(XTAG_QUERY_NODE, [XNS_JABBER_DISCO_INFO, sNode, s]);

      FSocket.Send(XmlTagIq(XVAL_RESULT, CanonicName(FUsername, FResource), sSender, sID, s));
    end
    else
    //====== XEP-0092: Software Version ===============
    if bGet and WideSameText(sNS, XNS_JABBER_IQ_VERSION) then
    begin
      s := XmlTag(XTAG_NAME, SEQ_ID_BIMOID);
      if (FClientName <> '') then s := XmlTag(XTAG_NAME, FClientName);
      if (FClientVer  <> '') then s := s + XmlTag(XTAG_VERSION, FClientVer) else s := s + XmlTag(XTAG_VERSION, '0.0.0.0');
      if (FClientOS   <> '') then s := s + XmlTag(XTAG_OS, FClientOS);

      FSocket.Send(XmlTagIqQuery(XVAL_RESULT, CanonicName(FUsername, FResource), sSender, sID, XNS_JABBER_IQ_VERSION, s));
    end
    else
    //====== privacy list push ========================
    if bSet and WideSameText(sNS, XNS_JABBER_IQ_PRIVACY) then
    begin
      Node := Node.NodeByName(UTF8Encode(XNS_LIST));
      if (Node <> nil) then
      begin
        //we are interested only in bimoid list pushes here
        s := Node.AttributeValueByNameWide[UTF8Encode(XATT_NAME)];
        if WideSameText(s, LIST_BIMOID) then
        begin
          if not FSocket.IsSeqTypeExists(SEQT_UPDCUR_LIST, s) then
            FPrivacy.Send_RequestList(s, SEQT_REQ_PUSHED_LIST);
        end;
      end;
      //send push result
      FSocket.Send(XmlTagIq(XVAL_RESULT, CanonicName(FUsername, FResource), sSender, sID));
    end
    else
    //====== roster push ==============================
    if bSet and WideSameText(sNS, XNS_JABBER_IQ_ROSTER) then
    begin
      Node := Node.NodeByName(UTF8Encode(XNS_ITEM));
      if (Node <> nil) then
        FRoster.Rcvd_RosterItemOper(Node);

      //send push result
      FSocket.Send(XmlTagIq(XVAL_RESULT, CanonicName(FUsername, FResource), sSender, sID));
    end
    {$IFDEF GTALK}
    else
    //====== GTALK ======================================
    if bSet and WideSameText(sNS, XNS_GT_SHARED_STATUS) then
    begin
      GTalk.Rcvd_SharedStatus(Node);
      //send push result
      FSocket.Send(XmlTagIq(XVAL_RESULT, sID, MakeTag(XNS_QUERY, '', XmlAtt(XATT_XMLNS, XNS_GT_SHARED_STATUS))));
    end
    {$ENDIF}
    else
    //====== unhandled ================================
      bHandled := False;
  end
  else
  //====== XEP-0199: XMPP Ping ========================
  if bGet and WideSameText(sName, XNS_PING) then
  begin
    if WideSameText(sNS, XNS_XMPP_PING) then
      FSocket.Send(XmlTagIq(XVAL_RESULT, CanonicName(FUsername, FResource), sSender, sID));
  end
  {$IFDEF GTALK}
  else
  //====== GTALK ======================================
  if bSet and WideSameText(sName, XNS_GT_USER_SETTING) then
  begin
    if WideSameText(sNS, XNS_GT_GOOGLE_SETTING) then
    begin
      GTalk.Rcvd_Setting(Node, FUsername, False);
      //send push result
      FSocket.Send(XmlTagIq(XVAL_RESULT, sID, XmlTag(XTAG_GT_USER_SETTING_EMPTY)));
    end;
  end
  else
  if bSet and WideSameText(sName, XNS_GT_NEW_MAIL) then
  begin
    if WideSameText(sNS, XNS_GT_MAIL_NOTIFY) then
    begin
      //send push result first
      FSocket.Send(XmlTag(XTAG_IQ_EMPTY, XVAL_RESULT, sID));
      //check mailbox
      GTalk.Send_GetNewMail(False);
    end;
  end
  {$ENDIF}
  else
  //====== unhandled ==================================
    bHandled := False;


  //====== return service unavailable for unhandled ===
  if not bHandled then
    FSocket.Send(XmlTagIq(XVAL_ERROR, CanonicName(FUsername, FResource), sSender, sID,
                 XmlTag(XTAG_ERROR, XVAL_CANCEL, XmlTag(XTAG_SVC_UNAVAIL))));
end;

{*****************************************************************}
procedure TJabberClient.ParseSrvDiscoInfo(XmlNode: TXmlNode);
var i: Integer;
begin
  FSrvDiscoInfo := [];
  for i := 0 to XmlNode.ChildContainerCount-1 do
  begin
    //features
    if WideSameText(UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name), XNS_FEATURE) then
    begin
      if WideSameText(XmlNode.ChildContainers[i].AttributeValueByNameWide[XATT_VAR], XNS_XMPP_PING) then
        FSrvDiscoInfo := FSrvDiscoInfo + [sdi_Ping]
      else
      if WideSameText(XmlNode.ChildContainers[i].AttributeValueByNameWide[XATT_VAR], XNS_VCARD_TEMP) then
        FSrvDiscoInfo := FSrvDiscoInfo + [sdi_VCardTemp]
      else
      if WideSameText(XmlNode.ChildContainers[i].AttributeValueByNameWide[XATT_VAR], XNS_MSG_OFFLINE) then
        FSrvDiscoInfo := FSrvDiscoInfo + [sdi_MsgOffline];
    end;
  end;//for
end;

{*****************************************************************}
procedure TJabberClient.ParseCliDiscoInfo(const JidCanonic: string; XmlNode: TXmlNode);
var aItem: pRosterItem;
    aResInfo: pResInfo;
    sUser, sAcc, sDomain, sResource: string;
    i: Integer;
begin
  CanonicToNorm(JidCanonic, sUser, sAcc, sDomain, sResource);

  aItem := Roster.GetRosterItem(sUser);
  if (aItem = nil) then Exit;

  aResInfo := aItem^.GetRes(sResource);
  if (aResInfo = nil) then Exit;

  aResInfo^.DiscoInfo := [];

  for i := 0 to XmlNode.ChildContainerCount-1 do
  begin
    //features
    if WideSameText(UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name), XNS_FEATURE) then
    begin
      if WideSameText(XmlNode.ChildContainers[i].AttributeValueByNameWide[XATT_VAR], XNS_XMPP_RECEIPTS) then
        aResInfo^.DiscoInfo := aResInfo^.DiscoInfo + [cdi_Receipts]
      else
      if WideSameText(XmlNode.ChildContainers[i].AttributeValueByNameWide[XATT_VAR], XNS_JABBER_CHAT_STATES) then
        aResInfo^.DiscoInfo := aResInfo^.DiscoInfo + [cdi_ChatStates]
      else
      if WideSameText(XmlNode.ChildContainers[i].AttributeValueByNameWide[XATT_VAR], XNS_JABBER_IQ_VERSION) then
        aResInfo^.DiscoInfo := aResInfo^.DiscoInfo + [cdi_ClientVer];
    end
    else
    //identity
    if WideSameText(UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name), XNS_IDENTITY) then
    begin
      if WideSameText(XmlNode.ChildContainers[i].AttributeValueByNameWide[XATT_CATEGORY], XVAL_CLIENT) then
      begin
        aResInfo^.ClientVer.Ver  := '';
        aResInfo^.ClientVer.OS   := '';
        aResInfo^.ClientVer.Name := XmlNode.ChildContainers[i].AttributeValueByNameWide[XATT_NAME];
      end;
    end;
  end;//for

  //if can get full client version
  if (cdi_ClientVer in aResInfo^.DiscoInfo) then
    FSocket.Send(XmlTagIqQuery(XVAL_GET, CanonicName(FUsername, FResource), JidCanonic, FSocket.NewSeqNum(SEQT_CLIENT_VER, JidCanonic), XNS_JABBER_IQ_VERSION))
  else
  //notify client name
  if (aResInfo^.ClientVer.Name <> '') then
  begin
    if Assigned(FOnContactArrived) then FOnContactArrived(Self, aItem, sResource, False);
  end;
end;

{*****************************************************************}
procedure TJabberClient.ParseClientVersion(const JidCanonic: string; XmlNode: TXmlNode);
var aItem: pRosterItem;
    aResInfo: pResInfo;
    sUser, sAcc, sDomain, sResource: string;
    i: Integer;
begin
  CanonicToNorm(JidCanonic, sUser, sAcc, sDomain, sResource);

  aItem := Roster.GetRosterItem(sUser);
  if (aItem = nil) then Exit;

  aResInfo := aItem^.GetRes(sResource);
  if (aResInfo = nil) then Exit;

  Finalize(aResInfo^.ClientVer);
  ZeroMemory(@aResInfo^.ClientVer, SizeOf(TClientVer));

  for i := 0 to XmlNode.ChildContainerCount-1 do
  begin
    //features
    if WideSameText(UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name), XNS_NAME) then
      aResInfo^.ClientVer.Name := XmlNode.ChildContainers[i].ValueUnicode
    else
    if WideSameText(UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name), XNS_VERSION) then
      aResInfo^.ClientVer.Ver  := XmlNode.ChildContainers[i].ValueUnicode
    else
    if WideSameText(UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name), XNS_OS) then
      aResInfo^.ClientVer.OS   := XmlNode.ChildContainers[i].ValueUnicode;
  end;//for

  //notify client
  if Assigned(FOnContactArrived) then FOnContactArrived(Self, aItem, sResource, False);
end;

{*****************************************************************}
procedure TJabberClient.ParsePresence(XmlNode: TXmlNode);
var Node: TXmlNode;
    sSender, sRecver, sType, sUser, sAcc, sDomain, sResource, sStatusShow, sStatusText: string;
    bRcvrOk, bOwnRes: Boolean;
    aItem: pRosterItem;
    iPriority: ShortInt;
    aResInfo: pResInfo;
begin
  sSender := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_FROM)];
  sRecver := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_TO)];
  sType   := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_TYPE)];

  bRcvrOk := (sRecver = '') or WideSameText(sRecver, CanonicName(FUsername, FResource)) or WideSameText(sRecver, FUsername);
  if not bRcvrOk then Exit;

  bOwnRes := CanonicName(FUsername, FResource) = sSender;

  CanonicToNorm(sSender, sUser, sAcc, sDomain, sResource);

  //if own pres info (own or connected from other resource)
  if WideSameText(FUsername, sUser) then
    ParseOwnPresInfo(sType, sResource, XmlNode);

  sStatusShow := '';
  sStatusText := '';
  iPriority   := 0;

  aItem := FRoster.GetRosterItem(sUser);

  //=== online presence info =============
  if (sType = PRES_TYPE_NONE) then
  begin
    if (aItem <> nil) then
    begin
      if WideSameText(XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_SHOW)], PRES_SHOW_INVIS_SPC) then
        sStatusShow := PRES_SHOW_INVIS_SPC;

      Node := XmlNode.NodeByName(UTF8Encode(XNS_SHOW));
      if (Node <> nil) then sStatusShow := Node.ValueUnicode;

      Node := XmlNode.NodeByName(UTF8Encode(XNS_STATUS));
      if (Node <> nil) then sStatusText := Node.ValueUnicode;

      Node := XmlNode.NodeByName(UTF8Encode(XNS_PRIORITY));
      if (Node <> nil) then iPriority := StrToIntDef(Node.ValueUnicode, 0);

      //overflow check
      if not aItem^.ResExists(sResource) and (aItem^.GetResCount >= XMPP_MAX_RESINFO_COUNT) then Exit;

      //get client disco info, most server ignore own session resource disco info request, so added this check
      if not bOwnRes and (aItem^.GetRes(sResource) = nil) then
        FSocket.Send(XmlTagIqQuery(XVAL_GET, CanonicName(FUsername, FResource), sSender, FSocket.NewSeqNum(SEQT_CLI_DISCO_INFO, sSender), XNS_JABBER_DISCO_INFO));

      //add/update resource info
      aItem^.UpdRes(sResource, sStatusShow, sStatusText, iPriority, True);

      //manually filling own resource disco info
      if bOwnRes then
      begin
        aResInfo := aItem^.GetRes(sResource);
        if (aResInfo <> nil) then
        begin
          aResInfo^.DiscoInfo := [cdi_Receipts, cdi_ChatStates, cdi_ClientVer];

          aResInfo^.ClientVer.Name := FClientName;
          aResInfo^.ClientVer.Ver  := FClientVer;
          aResInfo^.ClientVer.OS   := FClientOS;
        end;
      end;

      //searching avatar hash, vcard avatar XEP-0153: vCard-Based Avatars
      Node := XmlNode.NodeByAttributeValue(UTF8Encode(XNS_X), UTF8Encode(XATT_XMLNS), UTF8Encode(XNS_VCARD_TEMP_X_UPDATE));
      if (Node <> nil) then
      begin
        Node := Node.NodeByName(UTF8Encode(XNS_PHOTO));
        if (Node <> nil) then
        begin
          if not WideSameText(aItem^.AvatarSha1Hex, Node.ValueUnicode) then
          begin
            aItem^.AvatarSha1Hex := Node.ValueUnicode;
            aItem^.AvatarMd5Hex  := '';
          end;
        end
        else
        begin
          aItem^.AvatarSha1Hex := '';
          aItem^.AvatarMd5Hex  := '';
        end;
      end
      else
      begin
        //if no vcard:temp:x:update node exists then check that this is only one resource that left online and reset avatar
        if (aItem^.GetResCount = 1) then
        begin
          aItem^.AvatarSha1Hex := '';
          aItem^.AvatarMd5Hex  := '';
        end;
      end;

      if Assigned(FOnContactArrived) then FOnContactArrived(Self, aItem, sResource, False);
    end;
  end
  else
  //=== offline presence info =============
  if WideSameText(sType, PRES_TYPE_UNAVAILABLE) then
  begin
    if (aItem <> nil) then
    begin
      if not aItem^.ResExists(sResource) then Exit;
      aItem^.DelRes(sResource);

      if (aItem^.GetResCount > 0) then
      begin
        //if one of resources went offline, then use first online resource
        if Assigned(FOnContactArrived) then FOnContactArrived(Self, aItem, aItem^.ResArray[0].Resource, False);
      end
      else
      begin
        //reset avatar info too
        aItem^.AvatarSha1Hex := '';
        aItem^.AvatarMd5Hex  := '';

        if Assigned(FOnContactDeparted) then FOnContactDeparted(Self, aItem, sResource, False);
      end;
    end;
  end
  else
  if WideSameText(sType, PRES_TYPE_SUBSCRIBE) then
  begin
    //auth request
    if Assigned(FOnAuthRequest) then FOnAuthRequest(Self, sUser);
  end
  else
  if WideSameText(sType, PRES_TYPE_SUBSCRIBED) then
  begin
    //auth reply granted
    if Assigned(FOnAuthReply) then FOnAuthReply(Self, sUser, True);
  end
  else
  if WideSameText(sType, PRES_TYPE_UNSUBSCRIBE) then
  begin
    //contact unsubscribed from our presence, but we dont care about it
  end
  else
  if WideSameText(sType, PRES_TYPE_UNSUBSCRIBED) then
  begin
    //auth reply and revoke, depending on contact subscription state
    if (aItem <> nil) then
    begin
      if WideSameText(aItem^.PrevAsk, PRES_TYPE_SUBSCRIBE) then
      begin
        if Assigned(FOnAuthReply) then FOnAuthReply(Self, sUser, False);
      end
      else
      begin
        if Assigned(FOnAuthRevoke) then FOnAuthRevoke(Self, sUser);
      end;
    end;
  end;
end;

{*****************************************************************}
procedure TJabberClient.UpdOwnResPresInfo;
begin
  //add/upd own resource too
  FOwnPresItem.UpdRes(FResource, FStatusShow, FStatusAdvText, FPriority, True);
  if Assigned(FOnOwnPresInfo) then FOnOwnPresInfo(Self);
end;

{*****************************************************************}
procedure TJabberClient.ParseOwnPresInfo(const APresType, AResource: string; XmlNode: TXmlNode);
var Node: TXmlNode;
    sStatusShow, sStatusText: string;
    iPriority: ShortInt;
begin
  sStatusShow := '';
  sStatusText := '';
  iPriority   := 0;

  if (APresType = PRES_TYPE_NONE) then
  begin
    if WideSameText(XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_SHOW)], PRES_SHOW_INVIS_SPC) then
      sStatusShow := PRES_SHOW_INVIS_SPC;

    Node := XmlNode.NodeByName(UTF8Encode(XNS_SHOW));
    if (Node <> nil) then sStatusShow := Node.ValueUnicode;

    Node := XmlNode.NodeByName(UTF8Encode(XNS_STATUS));
    if (Node <> nil) then sStatusText := Node.ValueUnicode;

    Node := XmlNode.NodeByName(UTF8Encode(XNS_PRIORITY));
    if (Node <> nil) then iPriority := StrToIntDef(Node.ValueUnicode, 0);

    //overflow check
    if not FOwnPresItem.ResExists(AResource) and (FOwnPresItem.GetResCount >= XMPP_MAX_RESINFO_COUNT) then Exit;

    //add/update resource info
    FOwnPresItem.UpdRes(AResource, sStatusShow, sStatusText, iPriority, True);
  end
  else
  //=== offline presence info =============
  if WideSameText(APresType, PRES_TYPE_UNAVAILABLE) then
  begin
    if not FOwnPresItem.ResExists(AResource) then Exit;
    FOwnPresItem.DelRes(AResource);
  end
  else
    Exit;

  if Assigned(FOnOwnPresInfo) then FOnOwnPresInfo(Self);
end;

{*****************************************************************}
function TJabberClient.ParseErrorStanza(XmlNode: TXmlNode): string;
var sNS, sName, sType, sInfo: string;
    i: integer;
begin
  //get error type
  sType := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_TYPE)];

  //get error info
  sInfo := '';
  for i := 0 to XmlNode.ChildContainerCount-1 do
  begin
    sNS   := XmlNode.ChildContainers[i].AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)];
    sName := UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name);

    if WideSameText(sNS, XNS_URI + XNS_XMPP_STANZAS) then
    begin
      if not WideSameText(sName, XNS_TEXT) then
        sInfo := sName;
    end;
  end;//for

  Result := sType;

  if (sInfo <> '') then
  begin
    if (Result = '') then
      Result := sInfo
    else
      Result := Result + THE_CLN + sInfo;
  end;
end;

{*****************************************************************}
procedure TJabberClient.ParseMessage(XmlNode: TXmlNode);
var sSender, sRecver, sType, sID, sUser, sAcc, sDomain, sResource,
    sSubj, {sThread,} sBody, sNS, sChatState, sAckCmd, sAckID, sTime: string;
    Node: TXmlNode;
    bRcvrOk, bOffline: Boolean;
    im: TXmppIncMsg;
    ma: TXmppMsgAck;
    cs: TXmppChatState;
    i: Integer;
    aItem: pRosterItem;
begin
  sSender := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_FROM)];
  sRecver := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_TO)];
  sType   := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_TYPE)];
  sID     := XmlNode.AttributeValueByNameWide[UTF8Encode(XATT_ID)];

  bRcvrOk := (sRecver = '') or WideSameText(sRecver, CanonicName(FUsername, FResource)) or WideSameText(sRecver, FUsername);
  if not bRcvrOk then Exit;

  CanonicToNorm(sSender, sUser, sAcc, sDomain, sResource);

  aItem := FRoster.GetRosterItem(sUser);

  if (sType = '') then sType := MSG_TYPE_NORMAL;

  //notify about error
  if WideSameText(sType, MSG_TYPE_ERROR) then
  begin
    Node := XmlNode.NodeByName(UTF8Encode(XNS_ERROR));
    if (Node <> nil) then
    begin
      sBody := ParseErrorStanza(Node);
      if (sBody <> '') then
        DoJabberNotify(XMPP_NOTIF_MSG_FAILED, sSender, sBody, '');
    end;
    Exit;
  end;

  //ignore unsupported
  if WideSameText(sType, MSG_TYPE_GROUPCHAT) or WideSameText(sType, MSG_TYPE_HEADLINE) then
    Exit;

  sSubj      := '';
  //sThread    := '';
  sBody      := '';
  sChatState := '';
  sAckCmd    := '';
  sAckID     := '';
  sTime      := '';
  bOffline   := False;

  for i := 0 to XmlNode.ChildContainerCount-1 do
  begin
    sNS := XmlNode.ChildContainers[i].AttributeValueByNameWide[UTF8Encode(XATT_XMLNS)];

    //XEP-0085: Chat State Notifications
    if WideSameText(sNS, XNS_JABBER_CHAT_STATES) then
      sChatState := UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name)
    else
    //XEP-0184: Message Delivery Receipts
    if WideSameText(sNS, XNS_XMPP_RECEIPTS) then
    begin
      sAckCmd := UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name);
      sAckID  := XmlNode.ChildContainers[i].AttributeValueByNameWide[UTF8Encode(XATT_ID)];
    end
    else
    //XEP-0203: Delayed Delivery
    if WideSameText(sNS, XNS_XMPP_DELAY) then
    begin
      if WideSameText(XmlNode.ChildContainers[i].ValueUnicode, OFFLINE_STORAGE) then //cant say is this needed or not
      begin
        bOffline := True;
        sTime    := XmlNode.ChildContainers[i].AttributeValueByNameWide[UTF8Encode(XATT_STAMP)];
      end;
    end
    else
    //get other msg tags data
    if WideSameText(UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name), XNS_BODY) then
      sBody := sBody + XmlNode.ChildContainers[i].ValueUnicode + CRLF
    else
    if WideSameText(UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name), XNS_SUBJECT) then
      sSubj := sSubj + XmlNode.ChildContainers[i].ValueUnicode + CRLF;
    //else
    //if WideSameText(UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name), XNS_THREAD) then
    //  sThread := XmlNode.ChildContainers[i].ValueUnicode;
  end;//for

  sBody := Trim(sBody);
  sSubj := Trim(sSubj);

  //if incoming message
  if (sBody <> '') then
  begin
    if (aItem <> nil) then aItem^.LastResource := sResource;
    ZeroMemory(@im, SizeOf(TXmppIncMsg));

    im.Jid         := sUser;
    im.Resource    := sResource;
    im.Subj        := sSubj;
    im.Body        := sBody;
    im.AckID       := sID;
    im.AckRequired := (sID <> '') and WideSameText(sAckCmd, XNS_REQUEST);
    im.IsOffline   := bOffline;

    if (sTime <> '') then
      im.OfflineDT := Iso8601ToUnix(sTime);
    if (im.OfflineDT = 0) then
      im.IsOffline := False;

    if Assigned(FOnIncMsg) then FOnIncMsg(Self, im);
  end
  else
  //if msg ack
  if WideSameText(sAckCmd, XNS_RECEIVED) then
  begin
    if (aItem <> nil) then aItem^.LastResource := sResource;
    ZeroMemory(@ma, SizeOf(TXmppMsgAck));

    ma.Jid      := sUser;
    ma.Resource := sResource;

    if (sAckID <> '') then
      ma.AckID := sAckID
    else
      ma.AckID := sID; //some clients adding ID to message TAG (it is not right)

    if Assigned(FOnMsgAck) then FOnMsgAck(Self, ma);
  end;

  //if has chatstate
  if (sChatState <> '') then
  begin
    if (aItem <> nil) then aItem^.LastResource := sResource;
    ZeroMemory(@cs, SizeOf(TXmppChatState));

    cs.Jid       := sUser;
    cs.Resource  := sResource;
    cs.ChatState := sChatState;

    if Assigned(FOnChatState) then FOnChatState(Self, cs);
  end;
end;

{*****************************************************************}
procedure TJabberClient.ParseVCard(XmlNode: TXmlNode; SuccessOper: Boolean; const Jid: string; ReqID: DWord);
var vCard: TExtUserDets;
    Node: TXmlNode;
    i: Integer;
    bHome, bWork, bCell, bFax: Boolean;
begin
  ZeroMemory(@vCard, SizeOf(TExtUserDets));

  vCard.Account := Jid;
  vCard.ReqID   := ReqID;

  try
  if not SuccessOper or (XmlNode = nil) or (XmlNode.ChildContainerCount = 0) then
    Exit;

  Node := XmlNode.NodeByName(UTF8Encode(XNS_NICKNAME));
  if (Node <> nil) then vCard.NickName := Node.ValueUnicode;

  //names
  Node := XmlNode.NodeByName(UTF8Encode(XNS_N));
  if (Node <> nil) then
  begin
    for i := 0 to Node.ChildContainerCount-1 do
    begin
      if WideSameText(XNS_GIVEN, UTF8ToUnicodeString(Node.ChildContainers[i].Name)) then
        vCard.FirstName := Node.ChildContainers[i].ValueUnicode
      else
      if WideSameText(XNS_MIDDLE, UTF8ToUnicodeString(Node.ChildContainers[i].Name)) then
        vCard.FirstName := vCard.FirstName + THE_SPC + Node.ChildContainers[i].ValueUnicode
      else
      if WideSameText(XNS_FAMILY, UTF8ToUnicodeString(Node.ChildContainers[i].Name)) then
        vCard.LastName := Node.ChildContainers[i].ValueUnicode;
    end;//for

    vCard.FirstName := Trim(vCard.FirstName);
  end;

  Node := XmlNode.NodeByName(UTF8Encode(XNS_FN));
  if (Node <> nil) and (vCard.FirstName = '') then
    vCard.FirstName := Node.ValueUnicode;


  //home address
  for i := 0 to XmlNode.ChildContainerCount-1 do
  begin
    if WideSameText(XNS_ADR, UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name)) then
    begin
      bHome := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_HOME)) <> nil;
      bWork := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_WORK)) <> nil;

      if bHome or (not bHome and not bWork) then
      begin
        Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_REGION));
        if (Node <> nil) then vCard.RegionState := Node.ValueUnicode;

        Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_LOCALITY));
        if (Node <> nil) then vCard.City := Node.ValueUnicode;

        Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_PCODE));
        if (Node <> nil) then vCard.ZipCode := Node.ValueUnicode;

        Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_EXTADD));
        if (Node = nil) then
          Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_EXTADR));

        if (Node <> nil) and (Node.ValueUnicode <> '') then
        begin
          if (vCard.Address = '') then
            vCard.Address := Node.ValueUnicode
          else
            vCard.Address := vCard.Address + THE_COM + THE_SPC + Node.ValueUnicode;
        end;

        Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_STREET));
        if (Node <> nil) and (Node.ValueUnicode <> '') then
        begin
          if (vCard.Address = '') then
            vCard.Address := Node.ValueUnicode
          else
            vCard.Address := vCard.Address + THE_COM + THE_SPC + Node.ValueUnicode;
        end;

        Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_CTRY));
        if (Node = nil) then
          Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_COUNTRY));

        if (Node <> nil) and (Node.ValueUnicode <> '') then
        begin
          if (vCard.Address = '') then
            vCard.Address := Node.ValueUnicode
          else
            vCard.Address := vCard.Address + THE_COM + THE_SPC + Node.ValueUnicode;
        end;

        Break;
      end;
    end;
  end;//for

  //birthday
  Node := XmlNode.NodeByName(UTF8Encode(XNS_BDAY));
  if (Node <> nil) then vCard.Birthday := VCardDateToUnix(Node.ValueUnicode);

  //homepage
  Node := XmlNode.NodeByName(UTF8Encode(XNS_URL));
  if (Node <> nil) then vCard.HomePage := Node.ValueUnicode;

  //about
  Node := XmlNode.NodeByName(UTF8Encode(XNS_DESC));
  if (Node <> nil) then vCard.About := Node.ValueUnicode;

  //emails, phones, company
  for i := 0 to XmlNode.ChildContainerCount-1 do
  begin
    if WideSameText(XNS_EMAIL, UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name)) then
    begin
      Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_USERID));
      if (Node <> nil) then
      begin
        if (vCard.Email1 = '') then
          vCard.Email1 := Node.ValueUnicode
        else
        if (vCard.Email2 = '') then
          vCard.Email2 := Node.ValueUnicode;
      end;
    end
    else
    if WideSameText(XNS_TEL, UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name)) then
    begin
      Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_NUMBER));
      if (Node <> nil) then
      begin
        bHome := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_HOME)) <> nil;
        bWork := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_WORK)) <> nil;
        bCell := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_CELL)) <> nil;
        bFax  := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_FAX)) <> nil;

        if (vCard.FaxNum = '') and bFax then
          vCard.FaxNum    := Node.ValueUnicode
        else
        if (vCard.CellPhone = '') and bCell then
          vCard.CellPhone := Node.ValueUnicode
        else
        if (vCard.HomePhone = '') and bHome then
          vCard.HomePhone := Node.ValueUnicode
        else
        if (vCard.WorkPhone = '') and bWork then
          vCard.WorkPhone := Node.ValueUnicode;
      end;
    end
    else
    if WideSameText(XNS_ORG, UTF8ToUnicodeString(XmlNode.ChildContainers[i].Name)) then
    begin
      Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_ORGNAME));
      if (Node <> nil) and (vCard.Company = '') then
        vCard.Company := Node.ValueUnicode;

      Node := XmlNode.ChildContainers[i].NodeByName(UTF8Encode(XNS_ORGUNIT));
      if (Node <> nil) and (vCard.DivDept = '') then
        vCard.DivDept := Node.ValueUnicode;
    end;
  end;//for

  //position
  Node := XmlNode.NodeByName(UTF8Encode(XNS_TITLE));
  if (Node <> nil) then vCard.Position := Node.ValueUnicode;

  //photo
  ParseVCardAvatar(vCard.Account, XmlNode.NodeByName(UTF8Encode(XNS_PHOTO)));

  finally
    //save owner vcard
    if WideSameText(FUsername, vCard.Account) then
      FOwnVCard := vCard;

    if Assigned(FOnVCardRcvd) then FOnVCardRcvd(Self, vCard, SuccessOper);
  end;
end;

{*****************************************************************}
procedure TJabberClient.ParseVCardAvatar(const Jid: string; Node: TXmlNode);
var avik: TAvatarRec;
    i: Integer;
begin
  ClearAvatarRec(@avik);

  if (Node <> nil) then
  begin
    for i := 0 to Node.ChildContainerCount-1 do
    begin
      if WideSameText(UTF8ToUnicodeString(Node.ChildContainers[i].Name), XNS_TYPE) then
        avik.ImgType := Node.ChildContainers[i].ValueUnicode
      else
      if WideSameText(UTF8ToUnicodeString(Node.ChildContainers[i].Name), XNS_BINVAL) then
        avik.AvatarBase64 := AnsiString(Node.ChildContainers[i].ValueUnicode);
    end;//for

    if (avik.AvatarBase64 <> '') then
    begin
      try
        avik.AvatarSha1Hex := SHA1toHex(SHA1ofStr(Base64Decode(avik.AvatarBase64)));
      except
      end;
    end;
  end;

  //save owner vcard
  if WideSameText(FUsername, Jid) then
    FOwnAvatar := avik;

  if Assigned(FOnAvatarRcvd) then FOnAvatarRcvd(Self, Jid, avik);
end;

{*****************************************************************}
procedure TJabberClient.Send_HELLO;
begin
  FSocket.Send(XmlTag(XTAG_HEADER) + XmlTag(XTAG_INIT_STREAM, FDomain, XATT_LANG));
end;

{*****************************************************************}
procedure TJabberClient.Send_BYE;
begin
  FSocket.Send(XmlTag(XTAG_PRESENCE_TYPE_EMPTY, PRES_TYPE_UNAVAILABLE) + XmlTag(XTAG_END_STREAM));
end;

{*****************************************************************}
procedure TJabberClient.SendKeepAlive;
begin
  if (ConnectState = csOnline) then
  begin
    if (sdi_Ping in FSrvDiscoInfo) then
      FSocket.Send(XmlTagIq(XVAL_GET, CanonicName(FUsername, FResource), FDomain, FSocket.NewSeqNum(SEQT_CLIENT_PING), XmlTag(XTAG_PING)))
    else
      FSocket.Send(' ');
  end;
end;

{*****************************************************************}
procedure TJabberClient.UpdatePresence(const Jid: string = '');
var s: string;
begin
  if (FConnectState <> csOnline) then Exit;

  s := XmlTag(XTAG_PRIORITY, IntToStr(FPriority));

  if (FStatusShow <> '') and (FStatusShow <> PRES_SHOW_INVIS_SPC) and (FStatusShow <> PRES_SHOW_IFA_SPC) then
    s := s + XmlTag(XTAG_SHOW, FStatusShow);

  if (FStatusAdvText <> '') then
    s := s + XmlTag(XTAG_STATUS, XmppHTMLEncode(FStatusAdvText));

  //if (sdi_VCardTemp in FSrvDiscoInfo) then
    s := s + XmlTag(XTAG_X, XNS_VCARD_TEMP_X_UPDATE, MakeTag(XNS_PHOTO, FOwnAvatar.AvatarSha1Hex));

  {$IFDEF XMPP}
  if (FStatusShow = PRES_SHOW_INVIS_SPC) or (FStatusShow = PRES_SHOW_IFA_SPC) then
  begin
    if FPrivacy.Supported then
    begin
      if (Jid <> '') then
        FSocket.Send(XmlTag(XTAG_PRESENCE_SHOW_TO, [Jid, PRES_SHOW_INVIS_SPC, s]))
      else
        FSocket.Send(XmlTag(XTAG_PRESENCE_SHOW, PRES_SHOW_INVIS_SPC, s));
    end;
  end
  else
  begin
    if (Jid <> '') then
      FSocket.Send(XmlTag(XTAG_PRESENCE_TO, Jid, s))
    else
      FSocket.Send(XmlTag(XTAG_PRESENCE, s));
  end;
  {$ENDIF}


  {$IFDEF GTALK}
  if (Jid <> '') then
    FSocket.Send(XmlTag(XTAG_PRESENCE_TO, Jid, s))
  else
    FSocket.Send(XmlTag(XTAG_PRESENCE, s));
  {$ENDIF}
end;

{*****************************************************************}
procedure TJabberClient.UpdateStatus(const StatusShow: string; const StatusAdvText: string = '');
begin
  FStatusShow    := StatusShow;
  FStatusAdvText := StatusAdvText;
  if (FConnectState <> csOnline) then Exit;

  FSocket.StartCollect;
  try

  {$IFDEF XMPP}
  if (StatusShow = PRES_SHOW_INVIS_SPC) or (StatusShow = PRES_SHOW_IFA_SPC) then
    FSocket.Send(XmlTag(XTAG_PRESENCE_TYPE, PRES_TYPE_UNAVAILABLE, XmlTag(XTAG_PRIORITY, IntToStr(FPriority))));

  if not FPrivacy.Supported or not FPrivacy.Bim_SendUpdateList(LIST_BIMOID, FStatusShow) then
  begin
    if not WideSameText(FPrivacy.ListActive, LIST_BIMOID) and FPrivacy.ListExists(LIST_BIMOID) then
      FPrivacy.Send_SetActiveList(LIST_BIMOID)
    else
      UpdatePresence;
  end;
  {$ENDIF}

  {$IFDEF GTALK}
  if not GTalk.Send_GoggleSharedStatus(FUsername, FStatusShow) then
    UpdatePresence;
  {$ENDIF}

  finally
    FSocket.EndCollect;
    //notify own pres info changed
    UpdOwnResPresInfo;
  end;
end;

{*****************************************************************}
procedure TJabberClient.SendPresTo(const Jid, PresType: string);
begin
  FSocket.Send(XmlTag(XTAG_PRESENCE_TYTO_EMPTY, Jid, PresType));
end;

{*****************************************************************}
procedure TJabberClient.SendMsg(const XmppMsg: TXmppOutMsg);
var sAtts, sTags: string;
begin
  //msg attributes
  sAtts := XmlAtt(XATT_TO, CanonicName(XmppMsg.Jid, XmppMsg.Resource)) + THE_SPC +
           XmlAtt(XATT_TYPE, MSG_TYPE_CHAT) + THE_SPC;

  if XmppMsg.AckRequired then
    sAtts := sAtts + XmlAtt(XATT_ID, XmppMsg.AckID) + THE_SPC;

  sAtts := Trim(sAtts);

  //msg tags
  sTags := XmlTag(XTAG_BODY, XmppHTMLEncode(XmppMsg.Body));

  if XmppMsg.AckRequired then
    sTags := sTags + XmlTag(XTAG_REQUEST);

  if XmppMsg.ChatStateActive then
    sTags := sTags + XmlTag(XTAG_CHAT_STATE, XNS_ACTIVE);

  FSocket.Send(XmlTag(XTAG_MESSAGE, sAtts, sTags));
end;

{*****************************************************************}
procedure TJabberClient.SendMsgAck(const XmppAck: TXmppMsgAck);
var sAtts: string;
begin
  sAtts := XmlAtt(XATT_TO, CanonicName(XmppAck.Jid, XmppAck.Resource)) + THE_SPC +
           XmlAtt(XATT_TYPE, MSG_TYPE_CHAT) + THE_SPC +
           XmlAtt(XATT_ID, XmppAck.AckID); //some clients waiting ID in the message attributes (it is not right),

  FSocket.Send(XmlTag(XTAG_MESSAGE, sAtts, XmlTag(XTAG_RECEIVED, XmppAck.AckID)));
end;

{*****************************************************************}
procedure TJabberClient.SendChatState(const ChatState: TXmppChatState);
var sAtts: string;
begin
  sAtts := XmlAtt(XATT_TO, CanonicName(ChatState.Jid, ChatState.Resource)) + THE_SPC +
           XmlAtt(XATT_TYPE, MSG_TYPE_CHAT);

  FSocket.Send(XmlTag(XTAG_MESSAGE, sAtts, XmlTag(XTAG_CHAT_STATE, ChatState.ChatState)));
end;

{*****************************************************************}
function TJabberClient.RequestVCard(const Jid: string; const ReqID: DWord): Boolean;
begin
  //should not check vcard support because servers can be different
  {Result := False;
  if not (sdi_VCardTemp in FSrvDiscoInfo) then Exit;}

  Result := True;

  if WideSameText(Jid, FUsername) then
    FSocket.Send(XmlTagIq(XVAL_GET, FSocket.NewSeqNum(SEQT_REQ_VCARD, Jid, ReqID), XmlTag(XTAG_VCARD_EMPTY)))
  else
    FSocket.Send(XmlTagIq(XVAL_GET, CanonicName(FUsername, FResource), Jid, FSocket.NewSeqNum(SEQT_REQ_VCARD, Jid, ReqID), XmlTag(XTAG_VCARD_EMPTY)));
end;

{*****************************************************************}
function TJabberClient.UpdateVCard(const vCard: TExtUserDets; const ReqID: DWord; const AvatarUpdate: Boolean = False): Boolean;
var sTags: string;
    iSeq: DWord;
    avik: pAvatarRec;
begin
  Result := False;
  if {not (sdi_VCardTemp in FSrvDiscoInfo) or} not WideSameText(vCard.Account, FUsername) then Exit;

  Result := True;

  //save temp new vcard
  FTempVCard := vCard;

  {$IFDEF XMPP}
  //generate vCard
  sTags := MakeTag(XNS_FN) +

           //name
           MakeTag(XNS_N,
                   MakeTag(XNS_FAMILY, XmppHTMLEncode(vCard.LastName)) +
                   MakeTag(XNS_GIVEN, XmppHTMLEncode(vCard.FirstName)) +
                   MakeTag(XNS_MIDDLE) ) +
           MakeTag(XNS_NICKNAME, XmppHTMLEncode(vCard.NickName)) +
           MakeTag(XNS_URL, XmppHTMLEncode(vCard.HomePage)) +
           MakeTag(XNS_BDAY, UnixToVCardDate(vCard.Birthday)) +

           //address
           MakeTag(XNS_ADR,
                   MakeTag(XNS_HOME) +
                   MakeTag(XNS_EXTADD) +
                   MakeTag(XNS_STREET, XmppHTMLEncode(vCard.Address)) +
                   MakeTag(XNS_LOCALITY, XmppHTMLEncode(vCard.City)) +
                   MakeTag(XNS_REGION, XmppHTMLEncode(vCard.RegionState)) +
                   MakeTag(XNS_PCODE, XmppHTMLEncode(vCard.ZipCode)) +
                   MakeTag(XNS_CTRY) ) +

           //organization
           MakeTag(XNS_ORG,
                   MakeTag(XNS_ORGNAME, XmppHTMLEncode(vCard.Company)) +
                   MakeTag(XNS_ORGUNIT, XmppHTMLEncode(vCard.DivDept)) ) +
           MakeTag(XNS_TITLE, XmppHTMLEncode(vCard.Position)) +
           MakeTag(XNS_ROLE);

  //emails
  if (vCard.Email1 <> '') then
    sTags := sTags +
             MakeTag(XNS_EMAIL, XmppHTMLEncode(vCard.Email1) + MakeTag(XNS_USERID, XmppHTMLEncode(vCard.Email1)));

  if (vCard.Email2 <> '') then
    sTags := sTags +
             MakeTag(XNS_EMAIL, XmppHTMLEncode(vCard.Email2) + MakeTag(XNS_USERID, XmppHTMLEncode(vCard.Email2)));

  //phones
  if (vCard.HomePhone <> '') then
    sTags := sTags +
             MakeTag(XNS_TEL, MakeTag(XNS_HOME) + MakeTag(XNS_NUMBER, XmppHTMLEncode(vCard.HomePhone)));

  if (vCard.WorkPhone <> '') then
    sTags := sTags +
             MakeTag(XNS_TEL, MakeTag(XNS_WORK) + MakeTag(XNS_NUMBER, XmppHTMLEncode(vCard.WorkPhone)));

  if (vCard.CellPhone <> '') then
    sTags := sTags +
             MakeTag(XNS_TEL, MakeTag(XNS_HOME) + MakeTag(XNS_CELL) + MakeTag(XNS_NUMBER, XmppHTMLEncode(vCard.CellPhone)));

  if (vCard.FaxNum <> '') then
    sTags := sTags +
             MakeTag(XNS_TEL, MakeTag(XNS_HOME) + MakeTag(XNS_FAX) + MakeTag(XNS_NUMBER, XmppHTMLEncode(vCard.FaxNum)));

  //description
  sTags := sTags +
           MakeTag(XNS_DESC, XmppHTMLEncode(vCard.About));
  {$ENDIF}

  {$IFDEF GTALK}
  sTags := '';
  {$ENDIF}

  //photo/avatar
  if not AvatarUpdate then
    avik := @FOwnAvatar
  else
    avik := @FTempAvatar;

  if (avik^.AvatarBase64 = '') then
    sTags := sTags +
             MakeTag(UpperCase(XNS_PHOTO))
  else
    sTags := sTags +
             MakeTag(UpperCase(XNS_PHOTO),
                     MakeTag(UpperCase(XNS_TYPE), avik^.ImgType) +
                     MakeTag(XNS_BINVAL, String(avik^.AvatarBase64)));


  if not AvatarUpdate then
    iSeq := SEQT_UPD_VCARD
  else
    iSeq := SEQT_UPD_VCARD_AVATAR;

  FSocket.Send(XmlTagIq(XVAL_SET, FSocket.NewSeqNum(iSeq, FUsername, ReqID), XmlTag(XTAG_VCARD, sTags)));
end;

{*****************************************************************}
function TJabberClient.RequestAvatar(const Jid: string): Boolean;
begin
//  Result := False;

//  if (sdi_VCardTemp in FSrvDiscoInfo) then
//  begin
    Result := True;

    if WideSameText(Jid, FUsername) then
      FSocket.Send(XmlTagIq(XVAL_GET, FSocket.NewSeqNum(SEQT_REQ_VCARD_AVATAR, Jid), XmlTag(XTAG_VCARD_EMPTY)))
    else
      FSocket.Send(XmlTagIq(XVAL_GET, CanonicName(FUsername, FResource), Jid, FSocket.NewSeqNum(SEQT_REQ_VCARD_AVATAR, Jid), XmlTag(XTAG_VCARD_EMPTY)));
//  end;
end;

{*****************************************************************}
function TJabberClient.UpdateAvatar(const ImgType: string; const BinVal: AnsiString; const ReqID: DWord): Boolean;
begin
//  Result := False;

//  if (sdi_VCardTemp in FSrvDiscoInfo) then
//  begin
    FTempAvatar.ImgType       := ImgType;
    FTempAvatar.AvatarSha1Hex := '';
    if (BinVal <> '') then
      FTempAvatar.AvatarSha1Hex := SHA1toHex(SHA1ofStr(BinVal));

    FTempAvatar.AvatarBase64 := BinVal;

    //set acc name if details not rcvd
    FOwnVCard.Account := FUsername;

    Result := UpdateVCard(FOwnVCard, ReqID, True);
//  end;
end;

{*****************************************************************}
procedure TJabberClient.DoJabberNotify(const NotifType: Word; const JidSender, Msg, ClickUrl: string);
begin
  if Assigned(FOnJabberNotify) then FOnJabberNotify(Self, NotifType, JidSender, Msg, ClickUrl);
end;

{$IFDEF GTALK}
{*****************************************************************}
procedure TJabberClient.DoMailNotify(const MailNotif: TExtMailNotif);
begin
  if Assigned(FOnMailNotify) then FOnMailNotify(Self, MailNotif);
end;

{*****************************************************************}
procedure TJabberClient.OnAuthTokenDone(Sender: TObject; User, Token: string; MailUrl: Boolean; MailNotif: TExtMailNotif; ReqID: DWord; OwnUrlReq: Boolean);
begin
  if MailUrl and WideSameText(User, FUsername) then //usernames must match anyway, but who knows))
  begin
    if OwnUrlReq then
    begin
      if (Token = '') then Token := GOOGLE_URL_MAIL;
      if Assigned(FOnMailUrlDone) then FOnMailUrlDone(Self, Token, ReqID);
    end
    else
    begin
      if (Token <> '') then
        MailNotif.ClickUrl := Token;

      DoMailNotify(MailNotif);
    end;
  end;
end;
{$ENDIF}





end.
