// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_oscar_cli;

interface

uses Windows, Messages, Classes, SysUtils, OverbyteIcsWSocket, OverbyteIcsMD5,
     u_oscar_sock, h_oscar, u_oscar_caps, u_oscar_fb, u_oscar_item, u_oscar_timer, u_oscar_hlp, u_oscar_avatars, u_ext_info;

type
  TOnSomeCode    = procedure (Sender: TObject; const SomeCode: DWord) of object;
  TOnBuddyEvent  = procedure (Sender: TObject; oItem: pOscarItem; HelpBool: Boolean) of object;
  TOnIcbmIncMsg  = procedure (Sender: TObject; const IcbmMsg: TIcbmIncMsg) of object;
  TOnIcbmAck     = procedure (Sender: TObject; const IcbmAck: TIcbmAck) of object;
  TOnIcbmEvent   = procedure (Sender: TObject; const IcbmEvent: TIcbmEvent) of object;
  TOnAuthMsg     = procedure (Sender: TObject; const AuthMsg: TOscarAuthMsg; const OfflineMsg: Boolean) of object;
  TOnImdReply    = procedure (Sender: TObject; const ImdReply: TImdReply) of object;
  TOnMdirUpdDeny = procedure (Sender: TObject; const ObimpReqID: DWord; const DetsUpd: Boolean) of object;

  TOscarClient = class
  private
    FSocket           : TOscarSocket;
    FTimer            : TOscarTimer;
    FAccount          : string;
    FPassword         : AnsiString;
    FServer           : string;
    FPort             : string;
    FOfflineMsgs      : Boolean;
    FWebAware         : Boolean;
    FAuthRequired     : Boolean;
    FPrivacyLevel     : DWord;
    FNoMultiSes       : Boolean;
    FUnsearchable     : Boolean;
    FAllowUnsUpd      : Boolean;
    FConnectState     : Byte;
    FConnectTicks     : DWord;
    FStatus           : DWord;
    FPrivStatus       : Byte;
    FMoodNum          : Integer;
    FMoodName         : string;
    FTrigger_Sock     : Boolean;
    FBosCookie        : RawByteString;
    FAdditCaps        : RawByteString;
    FBuddiesInitDone  : Boolean;
    FMigrating        : Boolean;
    FImdLang          : string;
    FImdOwnRcvd       : Boolean;
    FNewImdInfo       : TImdInfo;
    FClientName       : string;
    FClientVer        : Int64;
    FImdResSkip       : DWord;
    FImdLastMD5       : string;
    FOnLoginDone      : TNotifyEvent;
    FOnLoginFailed    : TOnSomeCode;
    FOnPauseCmd       : TNotifyEvent;
    FOnResumeCmd      : TNotifyEvent;
    FOnMigrated       : TNotifyEvent;
    FOnBuddyArrived   : TOnBuddyEvent;
    FOnBuddyDeparted  : TOnBuddyEvent;
    FOnIcbmIncMsg     : TOnIcbmIncMsg;
    FOnIcbmAck        : TOnIcbmAck;
    FOnIcbmEvent      : TOnIcbmEvent;
    FOnAuthReq        : TOnAuthMsg;
    FOnAuthResp       : TOnAuthMsg;
    FOnOwnNickInfo    : TOnSomeCode;
    FOnImdReply       : TOnImdReply;
    FOnImdUpdReply    : TOnImdReply;
    FOnOwnBartInfo    : TNotifyEvent;
    FOnMdirUpdDeny    : TOnMdirUpdDeny;
    function  GetAliveTicks: DWord;
    procedure SetAliveTicks(const Value: DWord);
    function  GetProxyInfo: TExtProxyInfo;
    procedure SetProxyInfo(const Value: TExtProxyInfo);
    procedure SetConnectParams(AServer, APort: string);
    procedure SocketClose;
    procedure SocketConnect;
    function  RunTimerEvent(OscarTimerCmd: Byte; TimerInterval: DWord): Boolean;
    procedure ResetData(FullReset: Boolean);
    procedure ParseFlapData_1(var Flap: TOscarFlap);
    procedure ParseFlapData_2(var Flap: TOscarFlap);
    procedure ParseFlapData_4(var Flap: TOscarFlap);
    procedure ParseFam_BUCP(var Snac: TOscarSnac);
    procedure ParseFam_OSVC(var Snac: TOscarSnac);
    procedure ParseFam_FEEDBAG(var Snac: TOscarSnac);
    procedure ParseFam_LOCATE(var Snac: TOscarSnac);
    procedure ParseFam_BUDDY(var Snac: TOscarSnac);
    procedure ParseFam_ICBM(var Snac: TOscarSnac);
    procedure ParseFam_PD(var Snac: TOscarSnac);
    procedure ParseFam_MDIR(var Snac: TOscarSnac);
    procedure ParseLoginReply(var Data: RawByteString);
    function  ParseOscarSrvPortCookie(var Data: RawByteString; var Srv, Port: string; var Cookie: RawByteString; var ErrorCode: Word): Word;
    procedure SendPreOnlineSnacs1;
    procedure SendPreOnlineSnacs2;
    function  SendSnac(const Fam, Cmd: Word; const ReqID: DWord = 0; const Data: RawByteString = ''): Boolean;
    procedure Send_HELLO;
    procedure Send_HELLO_COOKIE;
    procedure Send_BYE;
    function  GenClientFamInfo(FamInfoType: Byte): RawByteString;
    function  GenLoginTLVs(Challenge: RawByteString): RawByteString;
    function  GenClientTLVs: RawByteString;
    procedure RequestFeedbag;
    procedure RequestOfflineMsgs;
    function  GenClientCapsArray: RawByteString;
    function  GenClientInfoCap: RawByteString;
    function  GenClientIcbmParams: RawByteString;
    function  GenNickDcInfo: RawByteString;
    procedure CopyNickInfo(const SrcNI: TNickInfo; var DstNI: TNickInfo);
    procedure CheckBuddiesInit;
    function  ParseBuddyInfo(var Snac: TOscarSnac; var NickInfo: TNickInfo): string;
    function  ParseNickInfo(var Snac: TOscarSnac; var NickInfo: TNickInfo): string;
    procedure ParseIcqDcInfo(Data: RawByteString; var DcInfo: TIcqDcInfo);
    procedure ParseBartsInfo(var Barts: TNickBarts; Data: RawByteString);
    function  ParseBartsReply(Data: RawByteString): Boolean;
    function  ParseIcbmImData(Data: RawByteString): string;
    procedure BuddyArrived(oItem: pOscarItem; OnlyAvatarChanged: Boolean);
    procedure BuddyDeparted(oItem: pOscarItem; LastPerSnac: Boolean);
    procedure UpdateMoodBarts(ForceRemove: Boolean);
    function  CheckForIncAlterAck(const IcbmEvent: TIcbmEvent): Boolean;
    procedure ParseImdAddress(ImdInfo: pImdInfo; Data: RawByteString);
    procedure ParseImdPhones(ImdInfo: pImdInfo; Data: RawByteString);
    procedure ParseImdWorkInfo(ImdInfo: pImdInfo; Data: RawByteString);
    procedure ParseImdInterests(ImdInfo: pImdInfo; Data: RawByteString);
    function  GenImdUpdData(const ImdInfo: TImdInfo): RawByteString;
    function  GenImdAddressRaw(const ImdInfo: TImdInfo): RawByteString;
    function  GenImdWorkInfoRaw(const ImdInfo: TImdInfo): RawByteString;
    function  GenImdPhonesRaw(const ImdInfo: TImdInfo): RawByteString;
    function  GenImdInterestsRaw(const ImdInfo: TImdInfo): RawByteString;
    function  GetImdReqMD5(const ImdSrch: TImdSrchReq): string;
  protected
    procedure OnSocketFlapRcvd(Sender: TObject; var Flap: TOscarFlap);
    procedure OnSocketClosed(Sender: TObject);
    procedure OnSocketError(Sender: TObject; ErrCode: integer; Msg: string);
    procedure OnOscarTimer(Sender: TObject);
  public
    OscarInfo  : TOscarInfo;
    OscarCL    : TOscarFB;
    CookieMan  : TCookieMan;
    ImdMan     : TImdMan;
    Avatars    : TAvatarsClient;
    constructor Create;
    destructor  Destroy; override;
    procedure Login;
    procedure Logoff;
    procedure SendKeepAlive;
    procedure RequestNickInfo;
    procedure UpdatePrivStatus(PrivStatus: Byte);
    procedure UpdateStatus(Status: DWord);
    procedure UpdateCaps(AdditCaps: RawByteString);
    procedure UpdateMood(MoodNum: Integer; MoodName: string);
    procedure SendIcbmMsg(const IcbmMsg: TIcbmOutMsg);
    procedure SendIcbmEvent(const IcbmEvent: TIcbmEvent);
    function  CanSendAlterAck(const Account: string): Boolean;
    procedure SendAuthRequest(const AuthReq: TOscarAuthMsg);
    procedure SendAuthRespond(const AuthResp: TOscarAuthMsg);
    procedure SendRemoveMe(const Account: string);
    procedure RequestMdirInfo(const Account: string; const ReqType: Byte; const ImdInfoLevel, ObimpReqID: DWord);
    procedure UpdateMdirInfo(const ImdInfo: TImdInfo; const ObimpReqID: DWord);
    procedure RequestService(const Family: Word);
    function  SearchMdir(const ImdSrch: TImdSrchReq; const ObimpReqID: DWord): Boolean;
    procedure UpdateMdirPrivacy;
    property ConnectState     : Byte             read FConnectState;
    property AliveTicks       : DWord            read GetAliveTicks      write SetAliveTicks;
    property ConnectTicks     : DWord            read FConnectTicks      write FConnectTicks;
    property LoginServer      : string           read FServer            write FServer;
    property LoginPort        : string           read FPort              write FPort;
    property LoginAccount     : string           read FAccount           write FAccount;
    property LoginPassword    : AnsiString       read FPassword          write FPassword;
    property ProxyInfo        : TExtProxyInfo    read GetProxyInfo       write SetProxyInfo;
    property WebAware         : Boolean          read FWebAware          write FWebAware;
    property AuthRequired     : Boolean          read FAuthRequired      write FAuthRequired;
    property PrivacyLevel     : DWord            read FPrivacyLevel      write FPrivacyLevel;
    property NoMultiSes       : Boolean          read FNoMultiSes        write FNoMultiSes;
    property Unsearchable     : Boolean          read FUnsearchable;
    property CurrentStatus    : DWord            read FStatus;
    property AllowUnsUpd      : Boolean          read FAllowUnsUpd       write FAllowUnsUpd;
    property ImdLang          : string           read FImdLang           write FImdLang;
    property ClientName       : string           read FClientName        write FClientName;
    property ClientVer        : Int64            read FClientVer         write FClientVer;
    property OnLoginDone      : TNotifyEvent     read FOnLoginDone       write FOnLoginDone;
    property OnLoginFailed    : TOnSomeCode      read FOnLoginFailed     write FOnLoginFailed;
    property OnPauseCmd       : TNotifyEvent     read FOnPauseCmd        write FOnPauseCmd;
    property OnResumeCmd      : TNotifyEvent     read FOnResumeCmd       write FOnResumeCmd;
    property OnMigrated       : TNotifyEvent     read FOnMigrated        write FOnMigrated;
    property OnBuddyArrived   : TOnBuddyEvent    read FOnBuddyArrived    write FOnBuddyArrived;
    property OnBuddyDeparted  : TOnBuddyEvent    read FOnBuddyDeparted   write FOnBuddyDeparted;
    property OnIcbmIncMsg     : TOnIcbmIncMsg    read FOnIcbmIncMsg      write FOnIcbmIncMsg;
    property OnIcbmAck        : TOnIcbmAck       read FOnIcbmAck         write FOnIcbmAck;
    property OnIcbmEvent      : TOnIcbmEvent     read FOnIcbmEvent       write FOnIcbmEvent;
    property OnAuthRequest    : TOnAuthMsg       read FOnAuthReq         write FOnAuthReq;
    property OnAuthResponse   : TOnAuthMsg       read FOnAuthResp        write FOnAuthResp;
    property OnOwnNickInfo    : TOnSomeCode      read FOnOwnNickInfo     write FOnOwnNickInfo;
    property OnImdReply       : TOnImdReply      read FOnImdReply        write FOnImdReply;
    property OnImdUpdReply    : TOnImdReply      read FOnImdUpdReply     write FOnImdUpdReply;
    property OnOwnBartInfo    : TNotifyEvent     read FOnOwnBartInfo     write FOnOwnBartInfo;
    property OnMdirUpdDeny    : TOnMdirUpdDeny   read FOnMdirUpdDeny     write FOnMdirUpdDeny;
  end;

implementation

{ TOscarClient }
{*****************************************************************}
constructor TOscarClient.Create;
begin
  FSocket                := TOscarSocket.Create;
  FSocket.OnNetFlapRcvd  := OnSocketFlapRcvd;
  FSocket.OnNetClosed    := OnSocketClosed;
  FSocket.OnNetError     := OnSocketError;

  OscarCL           := TOscarFB.Create;
  OscarCL.Socket    := FSocket;
  OscarCL.OscarInfo := @OscarInfo;

  FTimer         := TOscarTimer.Create;
  FTimer.OnTimer := OnOscarTimer;

  //helpers create
  CookieMan  := TCookieMan.Create;
  ImdMan     := TImdMan.Create;
  Avatars    := TAvatarsClient.Create;

  //set defaults
  FStatus     := NI_STATUS_ONLINE;
  FPrivStatus := FB_PD_MODE_DENY_INVIS;
  FMoodNum    := -1;

  FImdLang    := 'en-US';
end;

{*****************************************************************}
destructor TOscarClient.Destroy;
begin
  Avatars.Free;
  ImdMan.Free;
  CookieMan.Free;
  FTimer.Free;
  OscarCL.Free;
  FSocket.Free;
  inherited;
end;

{*****************************************************************}
function TOscarClient.RunTimerEvent(OscarTimerCmd: Byte; TimerInterval: DWord): Boolean;
begin
  Result := False;
  if FTimer.Enabled or (TimerInterval < 1) then Exit;
  Result := True;

  FTimer.OscarCmd := OscarTimerCmd;
  FTimer.Interval := TimerInterval;
  FTimer.Enabled  := True;
end;

{*****************************************************************}
procedure TOscarClient.OnOscarTimer(Sender: TObject);
begin
  //this is events timer, so it should be always disabled
  FTimer.Enabled := False;

  case FTimer.OscarCmd of
   {==============================================}
    OT_CMD_BOS_CONNECT:
      begin
        if (FConnectState = CONNECT_STATE_OFFLINE) then
          SocketConnect;
      end;
   {==============================================}
  end;//case

  //clear last command
  FTimer.OscarCmd := 0;
end;

{*****************************************************************}
procedure TOscarClient.SetConnectParams(AServer, APort: string);
begin
  FSocket.Server := AServer;
  FSocket.Port   := APort;
end;

{*****************************************************************}
procedure TOscarClient.Login;
begin
  ResetData(True);
  SocketClose;
  SetConnectParams(FServer, FPort);
  SocketConnect;
end;

{*****************************************************************}
procedure TOscarClient.Logoff;
begin
  Avatars.Logoff;

  if (FConnectState = CONNECT_STATE_OFFLINE) then
    Exit;

  if (FConnectState = CONNECT_STATE_ONLINE) then
    Send_BYE;

  FTrigger_Sock := True;
  SocketClose;
end;

{*****************************************************************}
procedure TOscarClient.SocketConnect;
begin
  FConnectState := CONNECT_STATE_CONNECTING;
  FSocket.Connect;
end;

{*****************************************************************}
procedure TOscarClient.ResetData(FullReset: Boolean);
begin
  FSocket.ResetData(FMigrating);

  FTrigger_Sock    := False;
  FBuddiesInitDone := False;

  if FullReset then
  begin
    FMigrating    := False;
    FBosCookie    := '';
    FImdOwnRcvd   := False;
    FOfflineMsgs  := False;
    FConnectTicks := 0;

    ImdMan.OwnImdRequested := False;
  end;
end;

{*****************************************************************}
procedure TOscarClient.SocketClose;
begin
  FConnectState := CONNECT_STATE_OFFLINE;
  FSocket.Disconnect;
end;

{*****************************************************************}
procedure TOscarClient.OnSocketClosed(Sender: TObject);
begin
  if FTrigger_Sock then Exit;
  FTrigger_Sock := True;

  SocketClose;
  if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, 0);
end;

{*****************************************************************}
procedure TOscarClient.OnSocketError(Sender: TObject; ErrCode: integer; Msg: string);
begin
  if FTrigger_Sock then Exit;
  FTrigger_Sock := True;

  SocketClose;
  if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, 0);
end;

{*****************************************************************}
procedure TOscarClient.OnSocketFlapRcvd(Sender: TObject; var Flap: TOscarFlap);
begin
  //reset connection timeout value on every new flap rcvd
  if (ConnectState = CONNECT_STATE_CONNECTING) then
    FConnectTicks := 0;

  case Flap.FrameType of
    1: ParseFlapData_1(Flap);
    2: ParseFlapData_2(Flap);
    4: ParseFlapData_4(Flap);
  end;//case
end;

{*****************************************************************}
procedure TOscarClient.ParseFlapData_1(var Flap: TOscarFlap);
var iHello: DWord;
begin
  if (Flap.Data = '') then Exit;

  //check hello
  iHello := ReadDWord(Flap.Data, 4);
  if (iHello = 0) then Exit;

  if (FConnectState = CONNECT_STATE_CONNECTING) then
  begin
    if (FBosCookie = '') then
    begin
      Send_HELLO;
      SendSnac(FAM_BUCP, CMD_BUCP_REQUEST_CHALLENGE, 0, TLV(BUCP_TAG_ACCOUNT, UTF8Encode(FAccount)));
    end
    else
      Send_HELLO_COOKIE;
  end;
end;

{*****************************************************************}
procedure TOscarClient.ParseFlapData_4(var Flap: TOscarFlap);
var iTag, iLen: Word;
    iErrCode: DWord;
begin
  //server bumped us out
  if (FConnectState = CONNECT_STATE_ONLINE) then
  begin
    iErrCode := 0;

    //if only bye, then reply with bye
    if (Flap.Data = '') then
      Send_BYE
    else
    begin
      //else check for login from another place tag (called disconnect)
      while (Flap.Data <> '') do
      begin
        iTag := ReadWord(Flap.Data, 2);
        iLen := ReadWord(Flap.Data, 2);

        case iTag of
         {------------------------------------------------}
          BUCP_TAG_DISCONNECT:
            begin
              //skip disconnect reason code
              ReadWord(Flap.Data, iLen);
              //add our fake reason code
              iErrCode := BUCP_ERR_ANOTHER_PLACE_LOGIN;
            end;
         {------------------------------------------------}
          else
            Delete(Flap.Data, 1, iLen);
         {------------------------------------------------}
        end;//case
      end;//while
    end;

    FTrigger_Sock := True;
    SocketClose;
    if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, iErrCode);
  end;
end;

{*****************************************************************}
procedure TOscarClient.ParseFlapData_2(var Flap: TOscarFlap);
var Snac: TOscarSnac;
    iLen: Word;
begin
  Snac.Fam   := ReadWord(Flap.Data, 2);
  Snac.Cmd   := ReadWord(Flap.Data, 2);
  Snac.Flags := ReadWord(Flap.Data, 2);
  Snac.ReqID := ReadDWord(Flap.Data, 4);

  //removing optional TLV if present
  if ((Snac.Flags and SNAC_FLAG_OPT_TLV_PRESENT) = SNAC_FLAG_OPT_TLV_PRESENT) then
  begin
    iLen := ReadWord(Flap.Data);
    Delete(Flap.Data, 1, 2 + iLen);
  end;

  Snac.Data := Flap.Data;

  case Snac.Fam of
    FAM_OSVC    : ParseFam_OSVC(Snac);
    FAM_LOCATE  : ParseFam_LOCATE(Snac);
    FAM_BUDDY   : ParseFam_BUDDY(Snac);
    FAM_ICBM    : ParseFam_ICBM(Snac);
    FAM_PD      : ParseFam_PD(Snac);
    FAM_FEEDBAG : ParseFam_FEEDBAG(Snac);
    FAM_BUCP    : ParseFam_BUCP(Snac);
    FAM_MDIR    : ParseFam_MDIR(Snac);
  end;//case
end;

{*****************************************************************}
procedure TOscarClient.ParseFam_BUCP(var Snac: TOscarSnac);
var iLen: Word;
begin
  case Snac.Cmd of
   {=====================================================}
    CMD_BUCP_ERROR:
      begin
        if (FConnectState = CONNECT_STATE_CONNECTING) then
        begin
          FTrigger_Sock := True;
          SocketClose;
          //service temp unavailable
          if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, BUCP_ERR_SERVICE_TEMP_UNAVAIL);
        end;
      end;
   {=====================================================}
    CMD_BUCP_REPLY_CHALLENGE:
      begin
        iLen := ReadWord(Snac.Data, 2);
        if (iLen > 0) then
          SendSnac(FAM_BUCP, CMD_BUCP_LOGIN, 0, GenLoginTLVs(ReadStr(Snac.Data, iLen, iLen)))
        else
        begin
          FTrigger_Sock := True;
          SocketClose;
          //service temp unavailable
          if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, BUCP_ERR_SERVICE_TEMP_UNAVAIL);
        end;
      end;
   {=====================================================}
    CMD_BUCP_LOGIN_REPLY:
      begin
        ParseLoginReply(Snac.Data);
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
procedure TOscarClient.ParseLoginReply(var Data: RawByteString);
var iError: Word;
    sSrv, sPort: string;
begin
  FBosCookie := '';
  sSrv       := '';
  sPort      := '';
  iError     := BUCP_ERR_SERVICE_TEMP_UNAVAIL;

  ParseOscarSrvPortCookie(Data, sSrv, sPort, FBosCookie, iError);

  //if authenticated
  if (FBosCookie <> '') and (sSrv <> '') then
  begin
    //close connection
    Send_BYE;
    FTrigger_Sock := True;
    SocketClose;

    //connect to BOS
    SetConnectParams(sSrv, sPort);
    ResetData(False);

    //we cant close connection and start it again at once, so lets use timer event
    RunTimerEvent(OT_CMD_BOS_CONNECT, 100);
  end
  else
  begin
    FTrigger_Sock := True;
    SocketClose;
    //error
    if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, iError);
  end;
end;

{*****************************************************************}
function TOscarClient.ParseOscarSrvPortCookie(var Data: RawByteString; var Srv, Port: string; var Cookie: RawByteString; var ErrorCode: Word): Word;
var iTag, iLen: Word;
begin
  Result := 0;

  while (Data <> '') do
  begin
    iTag := ReadWord(Data, 2);
    iLen := ReadWord(Data, 2);

    case iTag of
     {------------------------------------------------}
      BUCP_TAG_SERVER       : GetOscarSrvPort(UTF8ToUnicodeString(ReadStr(Data, iLen, iLen)), Srv, Port);
     {------------------------------------------------}
      BUCP_TAG_COOKIE       : Cookie    := ReadStr(Data, iLen, iLen);
     {------------------------------------------------}
      BUCP_TAG_LOGIN_ERROR  : ErrorCode := ReadWord(Data, iLen);
     {------------------------------------------------}
      BUCP_TAG_FAMILY       : Result    := ReadWord(Data, iLen); //for service requesting
     {------------------------------------------------}
      else
        Delete(Data, 1, iLen);
     {------------------------------------------------}
    end;//case
  end;//while
end;

{*****************************************************************}
procedure TOscarClient.ParseFam_OSVC(var Snac: TOscarSnac);
var iCount: Word;
    i: integer;
    s: RawByteString;
    aNickInfo: TNickInfo;
    bNiChanged, bFirst: Boolean;
    iNum: Byte;
    IPs: string;
    iError: Word;
    sSrv, sPort: string;
begin
  case Snac.Cmd of
   {=====================================================}
    CMD_OSVC_ERROR:
      begin
        if (FConnectState = CONNECT_STATE_CONNECTING) then
        begin
          FTrigger_Sock := True;
          SocketClose;
          if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, BUCP_ERR_INTERNAL_ERROR);
        end;
      end;
   {=====================================================}
    CMD_OSVC_HOST_ONLINE:
      begin
        if (FConnectState = CONNECT_STATE_CONNECTING) then
          SendSnac(FAM_OSVC, CMD_OSVC_CLIENT_VERSIONS, CMD_OSVC_CLIENT_VERSIONS, GenClientFamInfo(FAM_INFO_TYPE_VERSIONS));
      end;
   {=====================================================}
    CMD_OSVC_HOST_VERSIONS:
      begin
        if (FConnectState = CONNECT_STATE_CONNECTING) then
          SendSnac(FAM_OSVC, CMD_OSVC_RATES_QUERY, CMD_OSVC_RATES_QUERY);
      end;
   {=====================================================}
    CMD_OSVC_RATES_REPLY:
      begin
        if (FConnectState = CONNECT_STATE_CONNECTING) then
        begin
          //get rate groups count
          iCount := ReadWord(Snac.Data);
          s := '';
          for i := 1 to iCount do
            s := s + tb(Word(i));

          //ack rates
          SendSnac(FAM_OSVC, CMD_OSVC_RATES_ADD_SUB, CMD_OSVC_RATES_ADD_SUB, s);

          //request other params and set flags
          SendPreOnlineSnacs1;
        end;
      end;
   {=====================================================}
    CMD_OSVC_NICK_INFO_REPLY:
      begin
        bNiChanged := False;
        bFirst     := True;
        iNum       := 0;
        i          := 0;
        IPs        := '';

        while (Snac.Data <> '') do
        begin
          Finalize(aNickInfo);
          ZeroMemory(@aNickInfo, SizeOf(TNickInfo));

          //first nickinfo describes the overall state
          if bFirst then
          begin
            bFirst := False;

            //parsing overall instances info
            ParseNickInfo(Snac, aNickInfo);

            //get this instance num
            iNum := aNickInfo.ThisInstNum;
          end
          else
          begin
            //trying to find our instance num
            ParseNickInfo(Snac, aNickInfo);

            //if found then update our nickinfo
            if (aNickInfo.MyInstNum = iNum) then
            begin
              //this will help to save traffic
              bNiChanged := IsPresInfoChanged(OscarInfo, aNickInfo);

              //set new nick info
              CopyNickInfo(aNickInfo, OscarInfo.NickInfo);

              //set this inst num value just in case
              OscarInfo.NickInfo.ThisInstNum := iNum;

              //not so important here, but this flag will show that we rcvd nickinfo at leat once
              OscarInfo.NickInfo.Online := True;
            end
            else
            begin
              //get other instances IPs
              IPs := IPs + IntToIP(aNickInfo.RealIP) + CRLF;
            end;

            //count instances
            Inc(i);
          end;
        end;//while

        //if something important changed, then notify OBIMP server
        if bNiChanged or ( (i > 0) and (i <> OscarInfo.InstanceCount) ) or FImdOwnRcvd or
           (Snac.ReqID = CMD_OSVC_NICK_INFO_QUERY) then //if was requested manually then reply
        begin
          FImdOwnRcvd := False;

          //update instance info
          if (i > 0) then
          begin
            OscarInfo.InstanceCount := i;
            OscarInfo.OtherInstIPs  := Trim(IPs);
          end;

          //notify
          if Assigned(FOnOwnNickInfo) then FOnOwnNickInfo(Self, Snac.ReqID);
        end;
      end;
   {=====================================================}
    CMD_OSVC_PAUSE:
      begin
        if (FConnectState = CONNECT_STATE_CONNECTING) then
        begin
          FTrigger_Sock := True;
          SocketClose;
          if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, BUCP_ERR_INTERNAL_ERROR);
        end
        else
        if (FConnectState = CONNECT_STATE_ONLINE) then
        begin
          SendSnac(FAM_OSVC, CMD_OSVC_PAUSE_ACK, CMD_OSVC_PAUSE_ACK, GenClientFamInfo(FAM_INFO_TYPE_FAMILIES));

          FSocket.SendPause;

          //notify paused
          if Assigned(FOnPauseCmd) then FOnPauseCmd(Self);
        end;
      end;
   {=====================================================}
    CMD_OSVC_RESUME:
      begin
        FSocket.SendResume;

        //notify resumed
        if Assigned(FOnResumeCmd) then FOnResumeCmd(Self);
      end;
   {=====================================================}
    CMD_OSVC_MIGRATE_GROUPS:
      begin
        //migrate to other server
        FMigrating := True;

        //delete "all groups" word
        Delete(Snac.Data, 1, 2);

        //parse new BOS data
        ParseLoginReply(Snac.Data);
      end;
   {=====================================================}
    CMD_OSVC_SVC_RESPONSE:
      begin
        s      := '';
        sSrv   := '';
        sPort  := '';
        iError := 0;

        if (Avatars.ConnectState = CONNECT_STATE_OFFLINE) and
           (ParseOscarSrvPortCookie(Snac.Data, sSrv, sPort, s, iError) = FAM_BART) and
           (s <> '') and (sSrv <> '') then
        begin
          Avatars.SvcServer := sSrv;
          Avatars.SvcPort   := sPort;
          Avatars.SvcCookie := s;

          Avatars.Login;
        end;
      end;
   {=====================================================}
    CMD_OSVC_BART_REPLY:
      begin
        //we need here only avatar info
        if ParseBartsReply(Snac.Data) then
        begin
          if Assigned(FOnOwnBartInfo) then FOnOwnBartInfo(Self);
        end;
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
procedure TOscarClient.ParseFam_LOCATE(var Snac: TOscarSnac);
var iTag, iLen: Word;
begin
  case Snac.Cmd of
   {=====================================================}
    CMD_LOCATE_RIGHTS_REPLY:
      begin
        while (Snac.Data <> '') do
        begin
          iTag := ReadWord(Snac.Data, 2);
          iLen := ReadWord(Snac.Data, 2);

          case iTag of
           {------------------------------------------------}
            LOCATE_TAG_RIGHTS_MAX_FULL_CAPS   : OscarInfo.LocateParams.MaxFullCapsAllowed  := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            LOCATE_TAG_RIGHTS_MAX_SHORT_CAPS  : OscarInfo.LocateParams.MaxShortCapsAllowed := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            else
              Delete(Snac.Data, 1, iLen);
           {------------------------------------------------}
          end;//case
        end;//while
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
procedure TOscarClient.ParseFam_BUDDY(var Snac: TOscarSnac);
var iTag, iLen: Word;
    oItem: pOscarItem;
    bInited, bWasOnline, bNiChanged, bOnlyAvatar: Boolean;
    sName: string;
    NickInfo: TNickInfo;
begin
  case Snac.Cmd of
   {=====================================================}
    CMD_BUDDY_RIGHTS_REPLY:
      begin
        while (Snac.Data <> '') do
        begin
          iTag := ReadWord(Snac.Data, 2);
          iLen := ReadWord(Snac.Data, 2);

          case iTag of
           {------------------------------------------------}
            BUDDY_TAG_RIGHTS_MAX_BUDDIES      : OscarInfo.BuddyParams.MaxBuddies       := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            BUDDY_TAG_RIGHTS_MAX_WATCHERS     : OscarInfo.BuddyParams.MaxWatchers      := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            BUDDY_TAG_RIGHTS_MAX_ICQ_BROAD    : OscarInfo.BuddyParams.MaxIcqBroadcast  := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            BUDDY_TAG_RIGHTS_MAX_TMP_BUDDIES  : OscarInfo.BuddyParams.MaxTempBuddies   := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            else
              Delete(Snac.Data, 1, iLen);
           {------------------------------------------------}
          end;//case
        end;//while
      end;
   {=====================================================}
    CMD_BUDDY_REJECTED,
    CMD_BUDDY_ARRIVED,
    CMD_BUDDY_DEPARTED:
      begin
        //set init flag to check if we going to do any ops after all buddies inited
        bInited := False;

        while (Snac.Data <> '') do
        begin
          Finalize(NickInfo);
          ZeroMemory(@NickInfo, SizeOf(TNickInfo));

          //set defs
          SetNickInfoDefs(NickInfo);

          //get buddy info
          sName := ParseBuddyInfo(Snac, NickInfo);

          oItem := OscarCL.GetBuddyItem(sName);
          if (oItem = nil) then Continue;

          //save last state to check below, this will save traffic
          bWasOnline := oItem^.NickInfo.Online;

          //check for changes in online nickinfo, this will save traffic too
          bNiChanged  := False;
          bOnlyAvatar := False;
          if (Snac.Cmd = CMD_BUDDY_ARRIVED) then
            bNiChanged := IsNickInfoChanged(oItem^.NickInfo, NickInfo, bOnlyAvatar);

          //set new buddy info
          CopyNickInfo(NickInfo, oItem^.NickInfo);

          //notify
          if (Snac.Cmd = CMD_BUDDY_ARRIVED) then
          begin
            if bNiChanged then
              BuddyArrived(oItem, bOnlyAvatar)
          end
          else
          begin
            if (Snac.Cmd = CMD_BUDDY_DEPARTED) and oItem^.BuddyInited and bWasOnline then
              BuddyDeparted(oItem, (Snac.Data = ''));
          end;

          //check inited flag
          if not oItem^.BuddyInited then
          begin
            OscarCL.InitBuddy(oItem);
            if not bInited then bInited := True;
          end;
        end;//while

        //check all budies inited or not
        if bInited and not FBuddiesInitDone then
          CheckBuddiesInit;
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
procedure TOscarClient.ParseFam_ICBM(var Snac: TOscarSnac);
var IcbmMsg: TIcbmIncMsg;
    //IcbmAck: TIcbmAck;
    IcbmEvent: TIcbmEvent;
    iTag, iLen: Word;
begin
  case Snac.Cmd of
   {=====================================================}
    CMD_ICBM_PARAM_REPLY:
      begin
        if (Length(Snac.Data) >= 16) then
        begin
          with OscarInfo.IcbmParams do
          begin
            MaxSlots             := ReadWord(Snac.Data, 2);
            IcbmFlags            := ReadDWord(Snac.Data, 4);
            MaxIncomingIcbmLen   := ReadWord(Snac.Data, 2);
            MaxSourceEvil        := ReadWord(Snac.Data, 2);
            MaxDestinationEvil   := ReadWord(Snac.Data, 2);
            MinInterIcbmInterval := ReadDWord(Snac.Data, 4);
          end;//with
        end;
      end;
   {=====================================================}
    CMD_ICBM_MSG_TO_CLIENT:
      begin
        ZeroMemory(@IcbmMsg, SizeOf(TIcbmIncMsg));

        with IcbmMsg do
        begin
          MsgCookie  := ReadQWord(Snac.Data, 8);
          MsgChannel := ReadWord(Snac.Data, 2);

          //we support only 1ch msgs
          if (MsgChannel <> 1) then Exit;

          //parse Nickinfo tags
          Account    := ParseNickInfo(Snac, NickInfo);
        end;//with

        //parse ICBM tags
        while (Snac.Data <> '') do
        begin
          iTag := ReadWord(Snac.Data, 2);
          iLen := ReadWord(Snac.Data, 2);

          case iTag of
           {------------------------------------------------}
            ICBM_TAG_IM_DATA:
              begin
                IcbmMsg.MsgText := ParseIcbmImData(ReadStr(Snac.Data, iLen, iLen));
              end;
           {------------------------------------------------}
            ICBM_TAG_STORE_OFFLINE:
              begin
                IcbmMsg.IsOffline := True;
                if (iLen > 0) then Delete(Snac.Data, 1, iLen); //just in case
              end;
           {------------------------------------------------}
            ICBM_TAG_OFFMSG_SENT_TIME:
              begin
                IcbmMsg.OffMsgTime := ReadDWord(Snac.Data, iLen);
              end;
           {------------------------------------------------}
            else
              Delete(Snac.Data, 1, iLen);
           {------------------------------------------------}
          end;//case
        end;//while

        if Assigned(FOnIcbmIncMsg) then FOnIcbmIncMsg(Self, IcbmMsg);
      end;
   {=====================================================}
    CMD_ICBM_HOST_ACK:
      begin
        { commented, because using Alter Acks
        ZeroMemory(@IcbmAck, SizeOf(TIcbmAck));

        with IcbmAck do
        begin
          MsgCookie  := ReadQWord(Snac.Data, 8);
          MsgChannel := ReadWord(Snac.Data, 2);
          Account    := ReadLnbUTF8(Snac.Data, True, True);
        end;//with

        if Assigned(FOnIcbmAck) then FOnIcbmAck(Self, IcbmAck);
        }
      end;
   {=====================================================}
    CMD_ICBM_CLIENT_EVENT:
      begin
        ZeroMemory(@IcbmEvent, SizeOf(TIcbmEvent));

        with IcbmEvent do
        begin
          MsgCookie  := ReadQWord(Snac.Data, 8);
          MsgChannel := ReadWord(Snac.Data, 2);
          Account    := ReadLnbUTF8(Snac.Data, True, True);
          EventCode  := ReadWord(Snac.Data, 2);
        end;//with

        if not CheckForIncAlterAck(IcbmEvent) then
        begin
          if Assigned(FOnIcbmEvent) then FOnIcbmEvent(Self, IcbmEvent);
        end;
      end;
   {=====================================================}
    CMD_ICBM_OFFLINE_DONE:
      begin
        FOfflineMsgs := False;
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
function TOscarClient.ParseIcbmImData(Data: RawByteString): string;
var iTag, iLen, iEnc: Word;
    sRaw: RawByteString;
    s: string;
begin
  Result := '';

  while (Data <> '') do
  begin
    iTag := ReadWord(Data, 2);
    iLen := ReadWord(Data, 2);

    //look for msg text only
    case iTag of
     {=====================================================}
      IM_DATA_TAG_IM_TEXT: //may be more than one, so collect the result
        begin
          //get whole value to oper with
          sRaw := ReadStr(Data, iLen, iLen);

          //get encoding
          iEnc := ReadWord(sRaw, 2);
          //skip unused
          Delete(sRaw, 1, 2);

          //get msg text
          Result := Result + RawToUniEnc(iEnc, sRaw);
        end;
     {=====================================================}
      else
        Delete(Data, 1, iLen);
     {=====================================================}
    end;//case
  end;//while

  //remove useless spaces and crlf-s at the end
  Result := TrimRight(Result);

  //check for HTML tags
  if (Length(Result) > 12) then
  begin
    s := Trim(LowerCase(Result));
    if (Pos('<html>', s) = 1) and (Pos('</html>', s) > 6) then
      Result := CutHtmlTags(Result);
  end;
end;

{*****************************************************************}
function TOscarClient.CheckForIncAlterAck(const IcbmEvent: TIcbmEvent): Boolean;
var IcbmAck: TIcbmAck;
begin
  Result := False;
  if (IcbmEvent.MsgCookie = 0) or (IcbmEvent.EventCode <> ICBM_EVENT_NONE) then Exit;

  //check that it is real our cookie
  if (CookieMan.GetCookieByOscar(IcbmEvent.Account, IcbmEvent.MsgCookie) = nil) then Exit;

  ZeroMemory(@IcbmAck, SizeOf(TIcbmAck));

  with IcbmAck do
  begin
    MsgCookie  := IcbmEvent.MsgCookie;
    MsgChannel := IcbmEvent.MsgChannel;
    Account    := IcbmEvent.Account;
  end;//with

  Result := True;

  if Assigned(FOnIcbmAck) then FOnIcbmAck(Self, IcbmAck);
end;

{*****************************************************************}
procedure TOscarClient.ParseFam_PD(var Snac: TOscarSnac);
var iTag, iLen: Word;
begin
  case Snac.Cmd of
   {=====================================================}
    CMD_PD_RIGHTS_REPLY:
      begin
        while (Snac.Data <> '') do
        begin
          iTag := ReadWord(Snac.Data, 2);
          iLen := ReadWord(Snac.Data, 2);

          case iTag of
           {------------------------------------------------}
            PD_TAG_RIGHTS_MAX_PERMIT_ENTRIES     : OscarInfo.PdParams.MaxPermitEntries    := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            PD_TAG_RIGHTS_MAX_DENY_ENTRIES       : OscarInfo.PdParams.MaxDenyEntries      := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            PD_TAG_RIGHTS_MAX_TEMP_PERMIT        : OscarInfo.PdParams.MaxTmpPermitEntries := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            else
              Delete(Snac.Data, 1, iLen);
           {------------------------------------------------}
          end;//case
        end;//while

        //send other snacs if was migrating
        if FMigrating then
          SendPreOnlineSnacs2;
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
procedure TOscarClient.ParseFam_FEEDBAG(var Snac: TOscarSnac);
var iTag, iLen: Word;
    AuthMsg: TOscarAuthMsg;
begin
  case Snac.Cmd of
   {=====================================================}
    CMD_FEEDBAG_ERROR:
      begin
        if (FConnectState = CONNECT_STATE_CONNECTING) then
        begin
          FTrigger_Sock := True;
          SocketClose;
          if Assigned(FOnLoginFailed) then FOnLoginFailed(Self, BUCP_ERR_INTERNAL_ERROR);
        end;
      end;
   {=====================================================}
    CMD_FEEDBAG_RIGHTS_REPLY:
      begin
        while (Snac.Data <> '') do
        begin
          iTag := ReadWord(Snac.Data, 2);
          iLen := ReadWord(Snac.Data, 2);

          case iTag of
           {------------------------------------------------}
            FB_TAG_RIGHTS_MAX_CLASS_ATTRS        : OscarInfo.FbParams.MaxClassAttrs         := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            FB_TAG_RIGHTS_MAX_ITEM_ATTRS_SIZE    : OscarInfo.FbParams.MaxItemAllAttrsSize   := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            FB_TAG_RIGHTS_MAX_ITEMS_BY_CLASS     : OscarInfo.FbParams.MaxItemsbyClass       := ReadStr(Snac.Data, iLen, iLen);
           {------------------------------------------------}
            FB_TAG_RIGHTS_MAX_CLIENT_ITEMS       : OscarInfo.FbParams.MaxDevClientItems     := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            FB_TAG_RIGHTS_MAX_ITEM_NAME_LEN      : OscarInfo.FbParams.MaxItemNameLen        := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            FB_TAG_RIGHTS_MAX_RECENT_BUDDIES     : OscarInfo.FbParams.MaxRecentBuddies      := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            FB_TAG_RIGHTS_INTERACTION_BUDDIES    : OscarInfo.FbParams.InteractBuddiesTop    := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            FB_TAG_RIGHTS_INTERACTION_HALF_LIFE  : OscarInfo.FbParams.InteractHalfLifeSecs  := ReadDWord(Snac.Data, iLen);
           {------------------------------------------------}
            FB_TAG_RIGHTS_INTERACTION_MAX_SCORE  : OscarInfo.FbParams.InteractMaxScore      := ReadDWord(Snac.Data, iLen);
           {------------------------------------------------}
            FB_TAG_RIGHTS_MAX_BUDDIES_PER_GROUP  : OscarInfo.FbParams.MaxBuddiesPerGroup    := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            FB_TAG_RIGHTS_MAX_MEGA_BOTS          : OscarInfo.FbParams.MaxBotsBuddies        := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            FB_TAG_RIGHTS_MAX_SMART_GROUPS       : OscarInfo.FbParams.MaxSmartGroups        := ReadWord(Snac.Data, iLen);
           {------------------------------------------------}
            else
              Delete(Snac.Data, 1, iLen);
           {------------------------------------------------}
          end;//case
        end;//while
      end;
   {=====================================================}
    CMD_FEEDBAG_CL_REPLY:
      begin
        OscarCL.RawDataRcvd(Snac.Data);

        //if there are more feedbag SNACs available then wait them all
        if ((Snac.Flags and SNAC_FLAG_MORE_REPLIES) = SNAC_FLAG_MORE_REPLIES) then
          Exit;

        //parse feedbag items
        OscarCL.ParseRawData;

        if (FConnectState = CONNECT_STATE_CONNECTING) then
        begin
          //set params and client ready
          SendSnac(FAM_FEEDBAG, CMD_FEEDBAG_CL_USE, CMD_FEEDBAG_CL_USE);

          //check that root group exists (should be already there)
          OscarCL.CheckOrCreateRootGroup;

          //update privacy status if needed
          OscarCL.UpdatePdInfo(FPrivStatus);

          //request own details
          ImdMan.OwnImdRequested := True;
          RequestMdirInfo(LoginAccount, REQ_TYPE_OWN_KEEP_INFO, IMD_INFO_LEVEL_FULL, REQ_TYPE_OWN_KEEP_INFO);

          //send other snacs
          SendPreOnlineSnacs2;

          //if no buddies in CL then do after buddies init opers from here
          if (OscarCL.GetBuddiesCount = 0) then
            CheckBuddiesInit;
        end;
      end;
   {=====================================================}
    CMD_FEEDBAG_INSERT_ITEMS  : OscarCL.ServerOperRcvd(Snac);
   {=====================================================}
    CMD_FEEDBAG_UPDATE_ITEMS  : OscarCL.ServerOperRcvd(Snac);
   {=====================================================}
    CMD_FEEDBAG_DELETE_ITEMS  : OscarCL.ServerOperRcvd(Snac);
   {=====================================================}
    CMD_FEEDBAG_START_CLUSTER : OscarCL.ServerStartCluster;
   {=====================================================}
    CMD_FEEDBAG_END_CLUSTER   : OscarCL.ServerEndCluster;
   {=====================================================}
    CMD_FEEDBAG_STATUS        : OscarCL.OperStatusRcvd(Snac);
   {=====================================================}
    CMD_FEEDBAG_REQ_AUTH_TO_CLIENT:
      begin
        ZeroMemory(@AuthMsg, SizeOf(TOscarAuthMsg));

        AuthMsg.Account    := ReadLnbUTF8(Snac.Data, True, True);
        AuthMsg.ReasonText := ReadLnwUTF8(Snac.Data, True);

        if Assigned(FOnAuthReq) then FOnAuthReq(Self, AuthMsg, FOfflineMsgs);
      end;
   {=====================================================}
    CMD_FEEDBAG_RESP_AUTH_TO_CLIENT:
      begin
        ZeroMemory(@AuthMsg, SizeOf(TOscarAuthMsg));

        AuthMsg.Account := ReadLnbUTF8(Snac.Data, True, True);
        AuthMsg.Code    := ReadByte(Snac.Data, 1);

        if Assigned(FOnAuthResp) then FOnAuthResp(Self, AuthMsg, FOfflineMsgs);
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
procedure TOscarClient.ParseFam_MDIR(var Snac: TOscarSnac);
var aReq: pImdReq;
    iLen, iTlvCount, iTag: Word;
    ImdReply: TImdReply;
    i, j: integer;
begin
  case Snac.Cmd of
   {=====================================================}
    CMD_MDIR_INFO_REPLY:
      begin
        aReq := ImdMan.GetRequest(Snac.ReqID);
        if (aReq = nil) then Exit;

        ImdReply.ReqType := aReq^.ReqType;
        ImdReply.ReqID   := aReq^.ObimpReqID;
        ImdReply.ImdCode := ReadDWord(Snac.Data, 4);

        //get and del debug str len
        iLen := ReadWord(Snac.Data, 2);
        if (iLen > 0) then Delete(Snac.Data, 1, iLen);

        ImdReply.Matches := ReadDWord(Snac.Data, 4);
        ImdReply.Skipped := ReadDWord(Snac.Data, 4);
        ImdReply.Results := ReadDWord(Snac.Data, 4);

        //buffer overflaw check
        if (ImdReply.Results > MAXWORD) then
          ImdReply.Results := MAXWORD;

        if (ImdReply.Results > 0) then
        begin
          SetLength(ImdReply.ImdData, ImdReply.Results);

          for i := 0 to ImdReply.Results-1 do
          begin
            //set some initial values
            InitImdInfo(@ImdReply.ImdData[i]);

            ImdReply.ImdData[i].Account := ReadLnwUTF8(Snac.Data, True);

            Delete(Snac.Data, 1, 4); //LastUpdate
            Delete(Snac.Data, 1, 4); //Unknown

            iTlvCount := ReadWord(Snac.Data, 2);

            for j := 1 to iTlvCount do
            begin
              iTag := ReadWord(Snac.Data, 2);
              iLen := ReadWord(Snac.Data, 2);

              with ImdReply.ImdData[i] do
              case iTag of
               {--------------------------------------------------------------}
                IMD_TLV_NICKNAME        : Nickname      := ReadStrUTF8(Snac.Data, iLen, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_FIRSTNAME       : Firstname     := ReadStrUTF8(Snac.Data, iLen, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_LASTNAME        : Lastname      := ReadStrUTF8(Snac.Data, iLen, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_VALIDATED_EMAIL : ValidEmail    := ReadStrUTF8(Snac.Data, iLen, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_PENDING_EMAIL   : PendingEmail  := ReadStrUTF8(Snac.Data, iLen, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_BIRTHDAY        : Birthday      := ReadDWord(Snac.Data, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_GENDER          : Gender        := ReadDWord(Snac.Data, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_ADDRESS         : ParseImdAddress(@ImdReply.ImdData[i], ReadStr(Snac.Data, iLen, iLen));
               {--------------------------------------------------------------}
                IMD_TLV_LOGIN_ALIAS     : LoginAlias    := ReadStrUTF8(Snac.Data, iLen, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_LANGUAGE1       : Language1     := ReadStrUTF8(Snac.Data, iLen, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_LANGUAGE2       : Language2     := ReadStrUTF8(Snac.Data, iLen, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_PHONES          : ParseImdPhones(@ImdReply.ImdData[i], ReadStr(Snac.Data, iLen, iLen));
               {--------------------------------------------------------------}
                IMD_TLV_HOMEPAGE        : Homepage      := ReadStrUTF8(Snac.Data, iLen, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_WORK_INFO       : ParseImdWorkInfo(@ImdReply.ImdData[i], ReadStr(Snac.Data, iLen, iLen));
               {--------------------------------------------------------------}
                IMD_TLV_INTERESTS       : ParseImdInterests(@ImdReply.ImdData[i], ReadStr(Snac.Data, iLen, iLen));
               {--------------------------------------------------------------}
                IMD_TLV_ABOUT           : About         := ReadStrUTF8(Snac.Data, iLen, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_STATUS          : OnlineStatus  := ReadDWord(Snac.Data, iLen);
               {--------------------------------------------------------------}
                IMD_TLV_EXPOSE_STATUS   : ExposeStatus  := Boolean(ReadDWord(Snac.Data, iLen));
               {--------------------------------------------------------------}
                IMD_TLV_AUTH_REQUIRED   : AuthRequired  := Boolean(ReadDWord(Snac.Data, iLen));
               {--------------------------------------------------------------}
                IMD_TLV_PRIVACY_LEVEL   : PrivacyLevel  := ReadDWord(Snac.Data, iLen);
               {--------------------------------------------------------------}
                else
                  Delete(Snac.Data, 1, iLen);
               {--------------------------------------------------------------}
              end;//case
            end;//for

            Delete(Snac.Data, 1, 4); //Zeros
          end;//for
        end;

        //save own info
        if (aReq^.ReqType = REQ_TYPE_OWN_KEEP_INFO) then
        begin
          FImdOwnRcvd := True; //to send own presinfo with attached email

          Finalize(OscarInfo.ImdInfo);
          ZeroMemory(@OscarInfo.ImdInfo, SizeOf(TImdInfo));

          //set some initial values
          InitImdInfo(@OscarInfo.ImdInfo);

          //if imdinfo found then set else update imdinfo account just in case
          if (ImdReply.Results > 0) then
          begin
            OscarInfo.ImdInfo := ImdReply.ImdData[0];
            FUnsearchable := False;

           //update privacy for searchable
           UpdateMdirPrivacy;
          end
          else
          begin
            //unsearchable?
            OscarInfo.ImdInfo.Account := FAccount;
            FUnsearchable := True;
          end;
        end
        else
        if (aReq^.ReqType = REQ_TYPE_SEARCH) then
        begin
          if ( (ImdReply.Results + ImdReply.Skipped) < ImdReply.Matches ) then
            FImdResSkip := ImdReply.Results + ImdReply.Skipped
          else
            FImdResSkip := 0;
        end;

        if Assigned(FOnImdReply) then FOnImdReply(Self, ImdReply);

        //del request
        ImdMan.DelRequest(aReq^.OscarReqID);
      end;
   {=====================================================}
    CMD_MDIR_INFO_UPD_REPLY:
      begin
        aReq := ImdMan.GetRequest(Snac.ReqID);
        if (aReq = nil) then Exit;

        //we can use here ImdReply
        ImdReply.ReqType := aReq^.ReqType;
        ImdReply.ReqID   := aReq^.ObimpReqID;
        ImdReply.ImdCode := ReadDWord(Snac.Data, 4);

        //update current ImdInfo to the new one on succes update
        if (ImdReply.ImdCode = IMD_CODE_SUCCESS) and (ImdReply.ReqType = REQ_TYPE_UPDATE_OWN_INFO) then
          OscarInfo.ImdInfo := FNewImdInfo;

        if Assigned(FOnImdUpdReply) then FOnImdUpdReply(Self, ImdReply);

        //del request
        ImdMan.DelRequest(aReq^.OscarReqID);
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
procedure TOscarClient.ParseImdAddress(ImdInfo: pImdInfo; Data: RawByteString);
var iTlvCount, iTag, iLen: Word;
    i: integer;
begin
  ImdInfo^.HomeStreet  := '';
  ImdInfo^.HomeCity    := '';
  ImdInfo^.HomeState   := '';
  ImdInfo^.HomeZip     := '';
  ImdInfo^.HomeCountry := '';

  if (Data = '') then Exit;
  iTlvCount := ReadWord(Data, 2);
  if (iTlvCount = 0) then Exit;

  for i := 1 to iTlvCount do
  begin
    iTag := ReadWord(Data, 2);
    iLen := ReadWord(Data, 2);

    case iTag of
      IMD_ADDRESS_TLV_STREET  : ImdInfo^.HomeStreet  := ReadStrUTF8(Data, iLen, iLen);
      IMD_ADDRESS_TLV_CITY    : ImdInfo^.HomeCity    := ReadStrUTF8(Data, iLen, iLen);
      IMD_ADDRESS_TLV_STATE   : ImdInfo^.HomeState   := ReadStrUTF8(Data, iLen, iLen);
      IMD_ADDRESS_TLV_ZIP     : ImdInfo^.HomeZip     := ReadStrUTF8(Data, iLen, iLen);
      IMD_ADDRESS_TLV_COUNTRY : ImdInfo^.HomeCountry := ReadStrUTF8(Data, iLen, iLen);
      else
        Delete(Data, 1, iLen);
    end;//case
  end;//for
end;

{*****************************************************************}
procedure TOscarClient.ParseImdPhones(ImdInfo: pImdInfo; Data: RawByteString);
var iTlvCount, iTag, iLen: Word;
    i: integer;
    sNumber: string;
    iNumType: DWord;
begin
  ImdInfo^.PhoneHome    := '';
  ImdInfo^.PhoneWork    := '';
  ImdInfo^.PhoneMobile  := '';
  ImdInfo^.PhoneHomeFax := '';
  ImdInfo^.PhoneWorkFax := '';
  ImdInfo^.PhoneOther   := '';

  while (Data <> '') do
  begin
    iTlvCount := ReadWord(Data, 2);
    if (iTlvCount = 0) then Continue;
    sNumber  := '';
    iNumType := 0;

    for i := 1 to iTlvCount do
    begin
      iTag := ReadWord(Data, 2);
      iLen := ReadWord(Data, 2);

      case iTag of
        IMD_PHONE_TLV_NUMBER : sNumber  := ReadStrUTF8(Data, iLen, iLen);
        IMD_PHONE_TLV_TYPE   : iNumType := ReadDWord(Data, iLen);
        else
          Delete(Data, 1, iLen);
      end;//case
    end;//for

    case iNumType of
      IMD_PHONE_HOME     : ImdInfo^.PhoneHome    := sNumber;
      IMD_PHONE_WORK     : ImdInfo^.PhoneWork    := sNumber;
      IMD_PHONE_MOBILE   : ImdInfo^.PhoneMobile  := sNumber;
      IMD_PHONE_HOME_FAX : ImdInfo^.PhoneHomeFax := sNumber;
      IMD_PHONE_WORK_FAX : ImdInfo^.PhoneWorkFax := sNumber;
      IMD_PHONE_OTHER    : ImdInfo^.PhoneOther   := sNumber;
    end;//case
  end;//while
end;

{*****************************************************************}
procedure TOscarClient.ParseImdWorkInfo(ImdInfo: pImdInfo; Data: RawByteString);
var iTlvCount, iTag, iLen: Word;
    i: integer;
begin
  ImdInfo^.WorkPosition  := '';
  ImdInfo^.WorkCompany   := '';
  ImdInfo^.WorkWebsite   := '';
  ImdInfo^.WorkDepart    := '';
  ImdInfo^.WorkIndustry  := 0;
  ImdInfo^.WorkSubIndust := 0;
  ImdInfo^.WorkStartDate := $80000000;
  ImdInfo^.WorkEndDate   := $80000000;
  ImdInfo^.WorkStreet    := '';
  ImdInfo^.WorkCity      := '';
  ImdInfo^.WorkState     := '';
  ImdInfo^.WorkZip       := '';
  ImdInfo^.WorkCountry   := '';

  if (Data = '') then Exit;
  iTlvCount := ReadWord(Data, 2);
  if (iTlvCount = 0) then Exit;

  for i := 1 to iTlvCount do
  begin
    iTag := ReadWord(Data, 2);
    iLen := ReadWord(Data, 2);

    case iTag of
      IMD_WORK_TLV_POSITION      : ImdInfo^.WorkPosition  := ReadStrUTF8(Data, iLen, iLen);
      IMD_WORK_TLV_COMPANY       : ImdInfo^.WorkCompany   := ReadStrUTF8(Data, iLen, iLen);
      IMD_WORK_TLV_WEBSITE       : ImdInfo^.WorkWebsite   := ReadStrUTF8(Data, iLen, iLen);
      IMD_WORK_TLV_DEPARTMENT    : ImdInfo^.WorkDepart    := ReadStrUTF8(Data, iLen, iLen);
      IMD_WORK_TLV_INDUSTRY      : ImdInfo^.WorkIndustry  := ReadDWord(Data, iLen);
      IMD_WORK_TLV_SUB_INDUSTRY  : ImdInfo^.WorkSubIndust := ReadDWord(Data, iLen);
      IMD_WORK_TLV_START_DATE    : ImdInfo^.WorkStartDate := ReadDWord(Data, iLen);
      IMD_WORK_TLV_END_DATE      : ImdInfo^.WorkEndDate   := ReadDWord(Data, iLen);
      IMD_WORK_TLV_STREET        : ImdInfo^.WorkStreet    := ReadStrUTF8(Data, iLen, iLen);
      IMD_WORK_TLV_CITY          : ImdInfo^.WorkCity      := ReadStrUTF8(Data, iLen, iLen);
      IMD_WORK_TLV_STATE         : ImdInfo^.WorkState     := ReadStrUTF8(Data, iLen, iLen);
      IMD_WORK_TLV_ZIP           : ImdInfo^.WorkZip       := ReadStrUTF8(Data, iLen, iLen);
      IMD_WORK_TLV_COUNTRY       : ImdInfo^.WorkCountry   := ReadStrUTF8(Data, iLen, iLen);
      else
        Delete(Data, 1, iLen);
    end;//case
  end;//for
end;

{*****************************************************************}
procedure TOscarClient.ParseImdInterests(ImdInfo: pImdInfo; Data: RawByteString);
var iTlvCount, iTag, iLen: Word;
    i, j: integer;
    sText: string;
    iCode: DWord;
begin
  ImdInfo^.InterestText1 := '';
  ImdInfo^.InterestCode1 := 0;
  ImdInfo^.InterestText2 := '';
  ImdInfo^.InterestCode2 := 0;
  ImdInfo^.InterestText3 := '';
  ImdInfo^.InterestCode3 := 0;
  ImdInfo^.InterestText4 := '';
  ImdInfo^.InterestCode4 := 0;

  j := 0;
  while (Data <> '') do
  begin
    iTlvCount := ReadWord(Data, 2);
    if (iTlvCount = 0) then Continue;
    sText := '';
    iCode := 0;
    Inc(j);

    for i := 1 to iTlvCount do
    begin
      iTag := ReadWord(Data, 2);
      iLen := ReadWord(Data, 2);

      case iTag of
        IMD_INTEREST_TLV_TEXT : sText := ReadStrUTF8(Data, iLen, iLen);
        IMD_INTEREST_TLV_CODE : iCode := ReadDWord(Data, iLen);
        else
          Delete(Data, 1, iLen);
      end;//case
    end;//for

    case j of
     {==============================================}
      1: begin
           ImdInfo^.InterestText1 := sText;
           ImdInfo^.InterestCode1 := iCode;
         end;
     {==============================================}
      2: begin
           ImdInfo^.InterestText2 := sText;
           ImdInfo^.InterestCode2 := iCode;
         end;
     {==============================================}
      3: begin
           ImdInfo^.InterestText3 := sText;
           ImdInfo^.InterestCode3 := iCode;
         end;
     {==============================================}
      4: begin
           ImdInfo^.InterestText4 := sText;
           ImdInfo^.InterestCode4 := iCode;
         end;
     {==============================================}
    end;//case
  end;//while
end;

{*****************************************************************}
function TOscarClient.GetAliveTicks: DWord;
begin
  Result := FSocket.AliveTicks;
end;

{*****************************************************************}
procedure TOscarClient.SetAliveTicks(const Value: DWord);
begin
  FSocket.AliveTicks := Value;
end;

{*****************************************************************}
procedure TOscarClient.Send_HELLO;
begin
  FSocket.SendFlap(1, tb(DWord(1)));
end;

{*****************************************************************}
procedure TOscarClient.Send_HELLO_COOKIE;
begin
  FSocket.SendFlap(1, tb(DWord(1)) +                      //hello
                   TLV(BUCP_TAG_COOKIE, FBosCookie) +  //cookie
                   GenClientTLVs);                     //client info
end;

{*****************************************************************}
procedure TOscarClient.Send_BYE;
begin
  FSocket.SendFlap(4, '');
end;

{*****************************************************************}
procedure TOscarClient.SendKeepAlive;
begin
  FSocket.SendFlap(5, tb(DWord(OSCAR_KEEP_ALIVE_INTERVAL)));
end;

{*****************************************************************}
function TOscarClient.SendSnac(const Fam, Cmd: Word; const ReqID: DWord; const Data: RawByteString): Boolean;
begin
  //send snac
  Result := FSocket.SendSnac(Fam, Cmd, ReqID, Data);
end;

{*****************************************************************}
procedure TOscarClient.RequestNickInfo;
begin
  if (FConnectState = CONNECT_STATE_OFFLINE) then Exit;
  //ReqID > $FFFF will cause disconnect!
  //should use official one
  SendSnac(FAM_OSVC, CMD_OSVC_NICK_INFO_QUERY, CMD_OSVC_NICK_INFO_QUERY);
end;

{*****************************************************************}
procedure TOscarClient.RequestFeedbag;
begin
  OscarCL.ClearListItems;
  SendSnac(FAM_FEEDBAG, CMD_FEEDBAG_CL_QUERY, CMD_FEEDBAG_CL_QUERY);
end;

{*****************************************************************}
procedure TOscarClient.RequestOfflineMsgs;
begin
  FOfflineMsgs := True;
  SendSnac(FAM_ICBM, CMD_ICBM_OFFLINE_RETRIEVE, CMD_ICBM_OFFLINE_RETRIEVE or (GetOscarRndWord shl 16));
end;

{*****************************************************************}
procedure TOscarClient.RequestService(const Family: Word);
begin
  SendSnac(FAM_OSVC, CMD_OSVC_SVC_REQUEST, CMD_OSVC_SVC_REQUEST or (GetOscarRndWord shl 16), tb(Family));
end;

{*****************************************************************}
procedure TOscarClient.SendPreOnlineSnacs1;
begin
  RequestNickInfo;

  if not FMigrating then
  begin
    //some important flags here to init all buddies
    SendSnac(FAM_FEEDBAG, CMD_FEEDBAG_RIGHTS_QUERY, CMD_FEEDBAG_RIGHTS_QUERY,
             TLV(FB_TAG_RIGHTS_FLAGS, tb(FB_RIGHTS_FLAG_AUTHORIZATION_SUPPORTED or
                                         FB_RIGHTS_FLAG_DOMAIN_SN_SUPPORTED or
                                         FB_RIGHTS_FLAG_ICQ_NUM_SUPPORTED)));
    RequestFeedbag;
  end;

  SendSnac(FAM_LOCATE, CMD_LOCATE_RIGHTS_QUERY, CMD_LOCATE_RIGHTS_QUERY);

  SendSnac(FAM_BUDDY, CMD_BUDDY_RIGHTS_QUERY, CMD_BUDDY_RIGHTS_QUERY,
           TLV(BUDDY_TAG_RIGHTS_FLAGS, tb(BUDDY_RIGHTS_FLAG_BART_SUPPORTED or
                                          BUDDY_RIGHTS_FLAG_INITIAL_DEPARTS or
                                          BUDDY_RIGHTS_FLAG_OFFLINE_BART_SUPPORTED or
                                          BUDDY_RIGHTS_FLAG_REJECT_PENDING_BUDDIES)));

  SendSnac(FAM_ICBM, CMD_ICBM_PARAM_QUERY, CMD_ICBM_PARAM_QUERY);

  SendSnac(FAM_PD, CMD_PD_RIGHTS_QUERY, CMD_PD_RIGHTS_QUERY);
end;

{*****************************************************************}
procedure TOscarClient.SendPreOnlineSnacs2;
begin
  SendSnac(FAM_LOCATE, CMD_LOCATE_SET_INFO, CMD_LOCATE_SET_INFO, TLV(LOCATE_TAG_INFO_CAPS, GenClientCapsArray));
  SendSnac(FAM_ICBM, CMD_ICBM_ADD_PARAMS, CMD_ICBM_ADD_PARAMS, GenClientIcbmParams);

  SendSnac(FAM_OSVC, CMD_OSVC_SET_NICKINFO, CMD_OSVC_SET_NICKINFO,
           TLV(OSVC_TAG_NICK_INFO_STATUS, tb(FStatus)) +
           TLV(OSVC_TAG_NICK_INFO_DCINFO, GenNickDcInfo));

  SendSnac(FAM_OSVC, CMD_OSVC_CLIENT_ONLINE, CMD_OSVC_CLIENT_ONLINE, GenClientFamInfo(FAM_INFO_TYPE_TOOLVERS));

  UpdateMoodBarts(False);

  //OK, we are online
  FConnectState := CONNECT_STATE_ONLINE;

  //if was migrating then reset migration flag
  if FMigrating then
  begin
    FMigrating := False;
    FSocket.SendResume; //send paused flaps

    //notify migrated
    if Assigned(FOnMigrated) then FOnMigrated(Self);
  end
  else
  begin
    //notify login done
    if Assigned(FOnLoginDone) then FOnLoginDone(Self);
  end;
end;

{*****************************************************************}
function TOscarClient.GenLoginTLVs(Challenge: RawByteString): RawByteString;
begin
  Result := TLV(BUCP_TAG_ACCOUNT, UTF8Encode(FAccount)) +
            TLV(BUCP_TAG_RESPONSE, MD5Pwd(Challenge, FPassword)) +
            TLV(BUCP_TAG_MACHINE_INFO) +
            GenClientTLVs;
end;

{*****************************************************************}
function TOscarClient.GenClientTLVs: RawByteString;
var sRaw: RawByteString;
begin
  if FNoMultiSes then
    sRaw := TLV(BUCP_TAG_MULTICON, tb([CLIENT_MC_SINGLE]))
  else
    sRaw := TLV(BUCP_TAG_MULTICON, tb([CLIENT_MC_MULTIPLE]));

  Result := TLV(BUCP_TAG_CLIENT_NAME, CLIENT_NAME) +
            TLV(BUCP_TAG_VER_MAJOR,   CLIENT_VER_MAJOR) +
            TLV(BUCP_TAG_VER_MINOR,   CLIENT_VER_MINOR) +
            TLV(BUCP_TAG_VER_POINT,   CLIENT_VER_POINT) +
            TLV(BUCP_TAG_VER_BUILD,   CLIENT_VER_BUILD) +  //important thing for status flags
            TLV(BUCP_TAG_CLIENT_ID,   CLIENT_ID) +
            TLV(BUCP_TAG_LANGUAGE,    CLIENT_LANGUAGE) +
            TLV(BUCP_TAG_COUNTRY,     CLIENT_COUNTRY) +
            TLV(BUCP_TAG_RECONNECT,   tb([CLIENT_RECONNECT])) +
            sRaw;
end;

{*****************************************************************}
function TOscarClient.GenClientFamInfo(FamInfoType: Byte): RawByteString;
begin
  Result := '';

  case FamInfoType of
   {=====================================================}
    FAM_INFO_TYPE_FAMILIES:
      begin
        Result := tb(FAM_PLUGINS) +
                  tb(FAM_OSVC) +
                  tb(FAM_UNK_x24) +
                  tb(FAM_FEEDBAG) +
                  tb(FAM_LOCATE) +
                  tb(FAM_MDIR) +
                  tb(FAM_BUDDY) +
                  tb(FAM_ICQ) +
                  tb(FAM_ICBM) +
                  tb(FAM_PD) +
                  tb(FAM_LOOKUP) +
                  tb(FAM_STATS);
      end;
   {=====================================================}
    FAM_INFO_TYPE_VERSIONS:
      begin
        Result := tb(FAM_PLUGINS) + tb(VER_FAM_PLUGINS) +
                  tb(FAM_OSVC)    + tb(VER_FAM_OSVC) +
                  tb(FAM_UNK_x24) + tb(VER_FAM_x24) +
                  tb(FAM_FEEDBAG) + tb(VER_FAM_FEEDBAG) +
                  tb(FAM_LOCATE)  + tb(VER_FAM_LOCATE) +
                  tb(FAM_MDIR)    + tb(VER_FAM_MDIR) +
                  tb(FAM_BUDDY)   + tb(VER_FAM_BUDDY) +
                  tb(FAM_ICQ)     + tb(VER_FAM_ICQ) +
                  tb(FAM_ICBM)    + tb(VER_FAM_ICBM) +
                  tb(FAM_PD)      + tb(VER_FAM_PD) +
                  tb(FAM_LOOKUP)  + tb(VER_FAM_LOOKUP) +
                  tb(FAM_STATS)   + tb(VER_FAM_STATS);
      end;
   {=====================================================}
    FAM_INFO_TYPE_TOOLVERS:
      begin
        Result := tb(FAM_PLUGINS) + tb(VER_FAM_PLUGINS) + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_OSVC)    + tb(VER_FAM_OSVC)    + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_UNK_x24) + tb(VER_FAM_x24)     + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_FEEDBAG) + tb(VER_FAM_FEEDBAG) + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_LOCATE)  + tb(VER_FAM_LOCATE)  + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_MDIR)    + tb(VER_FAM_MDIR)    + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_BUDDY)   + tb(VER_FAM_BUDDY)   + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_ICQ)     + tb(VER_FAM_ICQ)     + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_ICBM)    + tb(VER_FAM_ICBM)    + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_PD)      + tb(VER_FAM_PD)      + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_LOOKUP)  + tb(VER_FAM_LOOKUP)  + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_STATS)   + tb(VER_FAM_STATS)   + tb(TOOL_ID) + tb(TOOL_VER);
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
function TOscarClient.GenClientCapsArray: RawByteString;
begin
  Result := tb(OscCapsShort[CAP_SHORT_SHORT_CAPS].UUID) +
            tb(OscCapsShort[CAP_SHORT_UNIFICATION].UUID) +
            tb(OscCapsShort[CAP_SHORT_ICQ_UTF8].UUID) +
            tb(OscCapsShort[CAP_SHORT_AVATARS].UUID) +
            tb(OscCapsFull[CAP_FULL_ICQ_CLI_EVENTS].UUID) + //alter/old clients compatibility
            tb(OscCapsOther[CAP_OTHER_ALTER_ACKS].UUID) +
            GenClientInfoCap +
            FAdditCaps;
end;

{*****************************************************************}
function TOscarClient.GenClientInfoCap: RawByteString;
begin
  Result := '';
  if (FClientName = '') then Exit;

  //first 8 bytes is text mark
  if WideSameText(FClientName, 'Bimoid') then
    Result := 'BTP/BIMD'
  else
  if WideSameText(FClientName, 'Bimoid [Android]') then
    Result := 'BTP/BIMA'
  else
  if WideSameText(FClientName, 'Bimoid Mobile') then
    Result := 'BTP/BIMM'
  else
    Result := 'BTP/BIMU';

  //last 8 bytes is version
  Result := UTF8Encode(Result) + tb(FClientVer);

  //just in case check cap length
  if (Length(Result) <> 16) then
    Result := '';
end;

{*****************************************************************}
function TOscarClient.GenClientIcbmParams: RawByteString;
begin
  with OscarInfo.IcbmParams do
  begin
    //set needed params
    IcbmFlags := ICBM_PARAM_FLAG_CH_MSGS_ALLOWED or
                 ICBM_PARAM_FLAG_MISSED_CALLS_ENABLED or
                 ICBM_PARAM_FLAG_EVENTS_ALLOWED or
                 ICBM_PARAM_FLAG_OFFLINE_MSGS_ALLOWED or
                 ICBM_PARAM_FLAG_UNK_800; //useful

    MaxIncomingIcbmLen   := ICBM_PARAM_MAX_INC_MSG_LEN;
    MaxSourceEvil        := OSCAR_MAX_EVIL;
    MaxDestinationEvil   := OSCAR_MAX_EVIL;
    MinInterIcbmInterval := ICBM_PARAM_MIN_INTERVAL;

    //return result
    Result := tb($0000) +  //set for all channels
              tb(IcbmFlags) +
              tb(MaxIncomingIcbmLen) +
              tb(MaxSourceEvil) +
              tb(MaxDestinationEvil) +
              tb(MinInterIcbmInterval);
  end;//with
end;

{*****************************************************************}
function TOscarClient.GenNickDcInfo: RawByteString;
begin
  with OscarInfo.IcqDcInfo do
  begin
    //maybe useless nowadays?
    IntIP     := 0;
    IntPort   := 0;
    ConFlag   := 0;
    ConVer    := 9;
    Cookie    := GetOscarRndDWord;
    WebPort   := 0;
    Features  := 0;
    Time1     := 0;
    Time2     := 0;
    Time3     := 0;
    NotUsed   := 0;

    Result := tb(IntIP) + tb(IntPort) + tb([ConFlag]) +
              tb(ConVer) +
              tb(Cookie) +
              tb(WebPort) + tb(Features) +
              tb(Time1) + tb(Time2) + tb(Time3) +
              tb(NotUsed);
  end;//with
end;

{*****************************************************************}
procedure TOscarClient.CheckBuddiesInit;
begin
  if not OscarCL.AllBuddiesInited then Exit;
  FBuddiesInitDone := True;

  //ok, do any operations after all buddies inited
  RequestOfflineMsgs;
end;

{*****************************************************************}
procedure TOscarClient.CopyNickInfo(const SrcNI: TNickInfo; var DstNI: TNickInfo);
var Barts: TNickBarts;
begin
  //keep barts info, because it cant just be copied
  Barts := DstNI.Barts;

  //assign records
  DstNI := SrcNI;

  //check barts changes
  if not DstNI.Barts.AvatarHashAdded then
  begin
    DstNI.Barts.AvatarHashAdded := Barts.AvatarHashAdded;
    DstNI.Barts.AvatarFlags     := Barts.AvatarFlags;
    DstNI.Barts.AvatarHash      := Barts.AvatarHash;
  end;

  if not DstNI.Barts.StatusStrAdded then
  begin
    DstNI.Barts.StatusStrAdded := Barts.StatusStrAdded;
    DstNI.Barts.StatusStr      := Barts.StatusStr;
  end;

  if not DstNI.Barts.MoodNumAdded then
  begin
    DstNI.Barts.MoodNumAdded := Barts.MoodNumAdded;
    DstNI.Barts.MoodNum      := Barts.MoodNum;
  end;

  if not DstNI.Barts.MoodStrAdded then
  begin
    DstNI.Barts.MoodStrAdded := Barts.MoodStrAdded;
    DstNI.Barts.MoodStr      := Barts.MoodStr;
  end;
end;

{*****************************************************************}
function TOscarClient.ParseBuddyInfo(var Snac: TOscarSnac; var NickInfo: TNickInfo): string;
begin
  Result := '';
  if (Snac.Data = '') then Exit;

  //rejected notifications containing only accs array
  if (Snac.Cmd = CMD_BUDDY_REJECTED) then
  begin
    //get account name
    Result := ReadLnbUTF8(Snac.Data, True, True);
  end
  else
  begin
    if (Snac.Cmd = CMD_BUDDY_ARRIVED) or (Snac.Cmd = CMD_BUDDY_DEPARTED) then
    begin
      //arrived and departed notificatons containing nickinfo
      Result := ParseNickInfo(Snac, NickInfo);

      //set online flag if arrived
      NickInfo.Online := Snac.Cmd = CMD_BUDDY_ARRIVED;
    end
    else
      Snac.Data := '';
  end;
end;

{*****************************************************************}
function TOscarClient.ParseNickInfo(var Snac: TOscarSnac; var NickInfo: TNickInfo): string;
var iLen, iTlvCount, iTag: Word;
    i: Integer;
begin
  //get account name
  Result := ReadLnbUTF8(Snac.Data, True, True);

  //get warning level
  NickInfo.WarnLevel := ReadWord(Snac.Data, 2);

  //get buddy info TLV count
  iTlvCount := ReadWord(Snac.Data, 2);
  if (iTlvCount = 0) then Exit;

  //parse buddy TLVs
  for i := 1 to iTlvCount do
  begin
    iTag := ReadWord(Snac.Data, 2);
    iLen := ReadWord(Snac.Data, 2);

    case iTag of
     {==============================================================================}
      OSVC_TAG_NICK_INFO_NICK_FLAGS        : NickInfo.NickFlags   := ReadWord(Snac.Data, iLen);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_SIGNON_TOD        : NickInfo.SignonTime  := ReadDWord(Snac.Data, iLen);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_IDLE_MINS         : NickInfo.IdleMins    := ReadDWord(Snac.Data, iLen);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_MEMBER_SINCE      : NickInfo.MemberSince := ReadDWord(Snac.Data, iLen);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_STATUS            : NickInfo.Status      := ReadDWord(Snac.Data, iLen);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_CLIENTTYPE        : NickInfo.ClientType  := ReadWord(Snac.Data, iLen);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_REALIPADDRESS     : NickInfo.RealIP      := ReadDWord(Snac.Data, iLen);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_DCINFO            : ParseIcqDcInfo(ReadStr(Snac.Data, iLen, iLen), NickInfo.DcInfo);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_OSCAR_CAPS        : NickInfo.CapsFull    := ReadStr(Snac.Data, iLen, iLen);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_OSCAR_ONL_SECS    : NickInfo.OnlineSecs  := ReadDWord(Snac.Data, iLen);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_SHORT_OSCAR_CAPS  : NickInfo.CapsShort   := ReadStr(Snac.Data, iLen, iLen);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_BART_INFO         : ParseBartsInfo(NickInfo.Barts, ReadStr(Snac.Data, iLen, iLen));
     {==============================================================================}
      OSVC_TAG_NICK_INFO_MY_INSTANCE_NUM   : NickInfo.MyInstNum   := ReadByte(Snac.Data, iLen);
     {==============================================================================}
      OSVC_TAG_NICK_INFO_THIS_INSTANCE_NUM : NickInfo.ThisInstNum := ReadByte(Snac.Data, iLen);
     {==============================================================================}
      else
        Delete(Snac.Data, 1, iLen);
     {==============================================================================}
    end;//case
  end;//for
end;

{*****************************************************************}
procedure TOscarClient.ParseIcqDcInfo(Data: RawByteString; var DcInfo: TIcqDcInfo);
begin
  if (Length(Data) = 37) then with DcInfo do
  begin
    IntIP    := ReadDWord(Data, 4);
    IntPort  := ReadDWord(Data, 4);
    ConFlag  := ReadByte(Data, 1);
    ConVer   := ReadWord(Data, 2);
    Cookie   := ReadDWord(Data, 4);
    WebPort  := ReadDWord(Data, 4);
    Features := ReadDWord(Data, 4);
    Time1    := ReadDWord(Data, 4);
    Time2    := ReadDWord(Data, 4);
    Time3    := ReadDWord(Data, 4);
    NotUsed  := ReadWord(Data, 2);
  end;//with
end;

{*****************************************************************}
procedure TOscarClient.ParseBartsInfo(var Barts: TNickBarts; Data: RawByteString);
var iBartType: Word;
    iBartFlags: Byte;
    iLen: Byte;
    sRaw: RawByteString;
begin
  while (Data <> '') do
  begin
    iBartType  := ReadWord(Data, 2);
    iBartFlags := ReadByte(Data, 1);

    iLen       := ReadByte(Data, 1);
    sRaw       := ReadStr(Data, iLen, iLen);

    case iBartType of
     {===========================================================}
      BART_BUDDY_ICON   :
        begin
          Barts.AvatarHashAdded := True;
          Barts.AvatarFlags     := iBartFlags;
          Barts.AvatarHash      := GetBartData(iBartFlags, sRaw, True);
        end;
     {===========================================================}
      BART_STATUS_STR   :
        begin
          Barts.StatusStrAdded := True;
          Barts.StatusStr      := UTF8ToUnicodeString(GetBartData(iBartFlags, sRaw));
        end;
     {===========================================================}
      BART_ICQ_MOOD_NUM :
        begin
          Barts.MoodNumAdded := True;
          Barts.MoodNum      := MoodStrIDToMoodNum( UTF8ToUnicodeString(GetBartData(iBartFlags, sRaw)) );
        end;
     {===========================================================}
      BART_ICQ_MOOD_STR :
        begin
          Barts.MoodStrAdded := True;
          Barts.MoodStr      := UTF8ToUnicodeString(GetBartData(iBartFlags, sRaw));
        end;
     {===========================================================}
    end;//case
  end;//while
end;

{*****************************************************************}
function TOscarClient.ParseBartsReply(Data: RawByteString): Boolean;
var iBartType: Word;
    iBartFlags: Byte;
    iLen: Byte;
    sRaw: RawByteString;
begin
  Result := False;

  while (Data <> '') do
  begin
    iBartType  := ReadWord(Data, 2);
    iBartFlags := ReadByte(Data, 1);

    iLen       := ReadByte(Data, 1);
    sRaw       := ReadStr(Data, iLen, iLen);

    case iBartType of
     {===========================================================}
      BART_BUDDY_ICON   :
        begin
          OscarInfo.AvatarFlags := iBartFlags;
          OscarInfo.AvatarHash  := sRaw;

          Result := True;
        end;
     {===========================================================}
    end;//case
  end;//while
end;

{*****************************************************************}
procedure TOscarClient.BuddyArrived(oItem: pOscarItem; OnlyAvatarChanged: Boolean);
begin
  if Assigned(FOnBuddyArrived) then FOnBuddyArrived(Self, oItem, OnlyAvatarChanged);
end;

{*****************************************************************}
procedure TOscarClient.BuddyDeparted(oItem: pOscarItem; LastPerSnac: Boolean);
begin
  if Assigned(FOnBuddyDeparted) then FOnBuddyDeparted(Self, oItem, LastPerSnac);
end;

{*****************************************************************}
procedure TOscarClient.UpdatePrivStatus(PrivStatus: Byte);
begin
  if (FPrivStatus = PrivStatus) then Exit;
  FPrivStatus := PrivStatus;

  if (FConnectState = CONNECT_STATE_ONLINE) then
    OscarCL.UpdatePdInfo(FPrivStatus);
end;

{*****************************************************************}
procedure TOscarClient.UpdateStatus(Status: DWord);
begin
  if FWebAware then
    Status := Status or NI_STATUS_FL_WEBAWARE
  else
    Status := Status and not NI_STATUS_FL_WEBAWARE;

  if (FStatus = Status) then Exit;
  FStatus := Status;

  if (FConnectState = CONNECT_STATE_ONLINE) then
    SendSnac(FAM_OSVC, CMD_OSVC_SET_NICKINFO, CMD_OSVC_SET_NICKINFO, TLV(OSVC_TAG_NICK_INFO_STATUS, tb(FStatus)) );
end;

{*****************************************************************}
procedure TOscarClient.UpdateCaps(AdditCaps: RawByteString);
begin
  if (FAdditCaps = AdditCaps) then Exit;
  FAdditCaps := AdditCaps;

  if (FConnectState = CONNECT_STATE_ONLINE) then
    SendSnac(FAM_LOCATE, CMD_LOCATE_SET_INFO, CMD_LOCATE_SET_INFO, TLV(LOCATE_TAG_INFO_CAPS, GenClientCapsArray));
end;

{*****************************************************************}
procedure TOscarClient.UpdateMood(MoodNum: Integer; MoodName: string);
begin
  if (FMoodNum = MoodNum) and WideSameStr(FMoodName, MoodName) then Exit;

  FMoodNum  := MoodNum;
  FMoodName := MoodName;

  if (FConnectState = CONNECT_STATE_ONLINE) then
    UpdateMoodBarts(True);
end;

{*****************************************************************}
procedure TOscarClient.UpdateMoodBarts(ForceRemove: Boolean);
var sBarts: RawByteString;
begin
  if (FMoodNum = -1) and not ForceRemove then Exit;

  sBarts := '';

  //if set new mood
  if (FMoodNum > -1) then
  begin
    if (Length(FMoodName) > 0) then
      sBarts := GenBartData(BART_ICQ_MOOD_STR, BART_FLAG_DATA, UTF8EncodeMaxEncLen(FMoodName, MOOD_STR_MAX_ENC_LEN))
    else
      sBarts := GenBartData(BART_ICQ_MOOD_STR, 0, '');

    sBarts := sBarts +
              GenBartData(BART_ICQ_MOOD_NUM, 0, MoodNumToStrID(FMoodNum));
  end
  else
  begin
    //if mood removed
    sBarts := GenBartData(BART_ICQ_MOOD_STR, 0, '') +
              GenBartData(BART_ICQ_MOOD_NUM, 0, '');
  end;

  //remove old mood str that is not used in new clients
  if OscarInfo.NickInfo.Barts.StatusStrAdded then
  begin
    OscarInfo.NickInfo.Barts.StatusStrAdded := False;
    sBarts := sBarts +
              GenBartData(BART_STATUS_STR, 0, '')
  end;

  SendSnac(FAM_OSVC, CMD_OSVC_SET_NICKINFO, CMD_OSVC_SET_NICKINFO, TLV(OSVC_TAG_NICK_INFO_BART_INFO, sBarts));
end;

{*****************************************************************}
procedure TOscarClient.SendIcbmMsg(const IcbmMsg: TIcbmOutMsg);
var sRaw: RawByteString;
begin
  if (FConnectState <> CONNECT_STATE_ONLINE) then Exit;

  //nowadays only msg channel 1 used for txt msgs
  if (IcbmMsg.MsgChannel <> 1) then Exit;

  //msg data TLV first
  sRaw := TLV(ICBM_TAG_IM_DATA,
              TLV(IM_DATA_TAG_CAPS, tb([$01])) +    //always 1
              TLV(IM_DATA_TAG_IM_TEXT, tb(ICBM_IM_ENCODING_UNICODE) +
                                       tb($0000) +  //unused
                                       StrToRawUniBE(IcbmMsg.MsgText, ICBM_MAX_1CH_ENC_MSG_LEN)) );

  //add additional TLVs
  if IcbmMsg.ReqHostAck then
    sRaw := sRaw + TLV(ICBM_TAG_REQUEST_HOST_ACK);

  if IcbmMsg.Store then
    sRaw := sRaw + TLV(ICBM_TAG_STORE_OFFLINE);

  //send msg
  SendSnac(FAM_ICBM, CMD_ICBM_MSG_TO_HOST, CMD_ICBM_MSG_TO_HOST or (GetOscarRndWord shl 16),
           tb(IcbmMsg.MsgCookie) +
           tb(IcbmMsg.MsgChannel) +
           LnbValUTF8(IcbmMsg.Account) +
           sRaw);
end;

{*****************************************************************}
procedure TOscarClient.SendIcbmEvent(const IcbmEvent: TIcbmEvent);
begin
  if (FConnectState <> CONNECT_STATE_ONLINE) then Exit;

  //msg channel 1 used for events
  if (IcbmEvent.MsgChannel <> 1) then Exit;

  SendSnac(FAM_ICBM, CMD_ICBM_CLIENT_EVENT, CMD_ICBM_CLIENT_EVENT,
           tb(IcbmEvent.MsgCookie) +
           tb(IcbmEvent.MsgChannel) +
           LnbValUTF8(IcbmEvent.Account) +
           tb(IcbmEvent.EventCode));
end;

{*****************************************************************}
function TOscarClient.CanSendAlterAck(const Account: string): Boolean;
var oItem: pOscarItem;
begin
  Result := False;

  oItem := OscarCL.GetBuddyItem(Account);
  if (oItem = nil) then Exit;

  //status check
  if ( (FPrivStatus = FB_PD_MODE_PERMIT_VIS) and not (privPermit in OscarCL.GetBuddyPrivacy(Account)) ) or
     (FPrivStatus = FB_PD_MODE_DENY_ALL) then Exit;

  //cap check
  if not HasOscarCap(OscCapsOther[CAP_OTHER_ALTER_ACKS].UUID, oItem^.NickInfo.CapsFull) then
    Exit;

  Result := True;
end;

{*****************************************************************}
procedure TOscarClient.SendAuthRequest(const AuthReq: TOscarAuthMsg);
begin
  if (FConnectState <> CONNECT_STATE_ONLINE) then Exit;

  SendSnac(FAM_FEEDBAG, CMD_FEEDBAG_REQ_AUTH_TO_HOST, CMD_FEEDBAG_REQ_AUTH_TO_HOST or (GetOscarRndWord shl 16),
           LnbValUTF8(AuthReq.Account) +
           LnwValUTF8(AuthReq.ReasonText) +
           tb($0000));
end;

{*****************************************************************}
procedure TOscarClient.SendAuthRespond(const AuthResp: TOscarAuthMsg);
begin
  if (FConnectState <> CONNECT_STATE_ONLINE) then Exit;

  SendSnac(FAM_FEEDBAG, CMD_FEEDBAG_RESP_AUTH_TO_HOST, CMD_FEEDBAG_RESP_AUTH_TO_HOST or (GetOscarRndWord shl 16),
           LnbValUTF8(AuthResp.Account) +
           tb([AuthResp.Code]) +
           tb($0000) + //unused reason
           tb($0000));
end;

{*****************************************************************}
procedure TOscarClient.SendRemoveMe(const Account: string);
begin
  if (FConnectState <> CONNECT_STATE_ONLINE) then Exit;

  SendSnac(FAM_FEEDBAG, CMD_FEEDBAG_REMOVE_ME, CMD_FEEDBAG_REMOVE_ME or (GetOscarRndWord shl 16),
           LnbValUTF8(Account));
end;

{*****************************************************************}
procedure TOscarClient.RequestMdirInfo(const Account: string; const ReqType: Byte; const ImdInfoLevel, ObimpReqID: DWord);
var sRaw: RawByteString;
    oItem: pOscarItem;
    aReq: pImdReq;
begin
  if (FConnectState = CONNECT_STATE_OFFLINE) then Exit;

  //add request info
  aReq := ImdMan.AddRequest(ReqType, ImdMan.NewOscarReqID(CMD_MDIR_INFO_QUERY), ObimpReqID);

  //get contact item private key if available
  sRaw := '';
  if (ReqType = REQ_TYPE_CNT_DETAILS) then
  begin
    oItem := OscarCL.GetBuddyItem(Account);
    if (oItem <> nil) and oItem^.AttExists(FB_ATT_MDIR_KEY) then
    begin
      if oItem^.AttExists(FB_ATT_MDIR_DATE) then
        sRaw := TLV($0002, tb(DWord(OleTimeToUnix(oItem^.AttQWordValue(FB_ATT_MDIR_DATE) + $3C00000000{45 hours}))))
      else
        sRaw := TLV($0002, tb(DWord($80000000)));

      sRaw := sRaw +
              TLV($0003, oItem^.AttRawValue(FB_ATT_MDIR_KEY));
    end;
  end;


  //search TLV
  sRaw := tb($0001) + //items count
          TLV(IMD_SEARCH_ACCOUNT,
              TLV($0001, UTF8Encode(Account)) +
              sRaw );

  //header
  sRaw := LnwValUTF8(FImdLang) +
          tb([$00]) +
          tb([$00]) +
          tb($0000) +
          tb($0001) +
          tb(ImdInfoLevel) +
          tb($0000) +
          tb($0001) +
          sRaw +
          tb($0000) +
          tb($0000) +
          tb(DWord($00000000)) + //skip
          tb(DWord($00000001)) + //limit
          TLV($0001, tb(DWord($00000001)));

  SendSnac(FAM_MDIR, CMD_MDIR_INFO_QUERY, aReq^.OscarReqID, sRaw);
end;

{*****************************************************************}
function TOscarClient.GetImdReqMD5(const ImdSrch: TImdSrchReq): string;
begin
  Result := StrMD5(ImdSrch.Account + ImdSrch.Email + ImdSrch.Nickname + ImdSrch.Firstname + ImdSrch.Lastname +
                   IntToStr(ImdSrch.Gender) + ImdSrch.HomeCity + ImdSrch.HomeCountry + ImdSrch.Language + ImdSrch.Keywords +
                   IntToStr(ImdSrch.AgeMin) + IntToStr(ImdSrch.AgeMax) + IntToStr(ImdSrch.OnlineOnly));
end;

{*****************************************************************}
function TOscarClient.GetProxyInfo: TExtProxyInfo;
begin
  Result := FSocket.ProxyInfo;
end;

{*****************************************************************}
procedure TOscarClient.SetProxyInfo(const Value: TExtProxyInfo);
begin
  FSocket.ProxyInfo := Value;
end;

{*****************************************************************}
function TOscarClient.SearchMdir(const ImdSrch: TImdSrchReq; const ObimpReqID: DWord): Boolean;
var Pkt, s, sRaw, sLast: RawByteString;
    aReq: pImdReq;
    iCount: Word;
    sMD5: string;
begin
  Result := False;
  if (FConnectState = CONNECT_STATE_OFFLINE) then Exit;

  //get MD5 hash of the request to search next contacts by skipping results
  sMD5 := GetImdReqMD5(ImdSrch);
  if (FImdLastMD5 <> sMD5) then
    FImdResSkip := 0;
  FImdLastMD5 := sMD5;

  Pkt    := '';
  sRaw   := '';
  sLast  := '';
  iCount := 0;

  //===== if search by account ==============================
  if (ImdSrch.Account <> '') then
  begin
    Inc(iCount);

    sRaw  := TLV(IMD_SEARCH_ACCOUNT,
                 TLV($0001, UTF8Encode(ImdSrch.Account)));

    sLast := TLV($0001, tb(DWord($00000001)));
  end
  else
  //===== if search by email ================================
  if (ImdSrch.Email <> '') then
  begin
    Inc(iCount);

    sRaw  := TLV(IMD_SEARCH_EMAIL, UTF8Encode(ImdSrch.Email));

    sLast := TLV($0001, tb(DWord($00000001)));
  end
  else
  //===== if search by other params =========================
  begin
    //IMD_SEARCH_PROFILE
    if (ImdSrch.Nickname <> '') or (ImdSrch.Firstname <> '') or (ImdSrch.Lastname <> '') or (ImdSrch.Language <> '') then
    begin
      Inc(iCount);
      s := '';

      if (ImdSrch.Firstname <> '') then
        s := s + tb($0003) + //tlv count
             TLV($0001, tb(DWord(IMD_TLV_FIRSTNAME))) +
             TLV($0002, tb(DWord($00000001))) +
             TLV($0003, UTF8Encode(ImdSrch.Firstname));

      if (ImdSrch.Lastname <> '') then
        s := s + tb($0003) + //tlv count
             TLV($0001, tb(DWord(IMD_TLV_LASTNAME))) +
             TLV($0002, tb(DWord($00000001))) +
             TLV($0003, UTF8Encode(ImdSrch.Lastname));

      if (ImdSrch.Nickname <> '') then
        s := s + tb($0003) + //tlv count
             TLV($0001, tb(DWord(IMD_TLV_LOGIN_ALIAS))) +
             TLV($0002, tb(DWord($00000001))) +
             TLV($0003, UTF8Encode(ImdSrch.Nickname));

      if (ImdSrch.Language <> '') then
        s := s + tb($0003) + //tlv count
             TLV($0001, tb(DWord(IMD_TLV_LANGUAGE1))) +
             TLV($0002, tb(DWord($00000001))) +
             TLV($0003, UTF8Encode(ImdSrch.Language));

      sRaw := sRaw +
              TLV(IMD_SEARCH_PROFILE, s);
    end;

    //IMD_SEARCH_PERSONAL
    if (ImdSrch.Gender > 0) or (ImdSrch.OnlineOnly > 0) then
    begin
      Inc(iCount);
      s := '';

      if (ImdSrch.OnlineOnly > 0) then
        s := s + tb($0003) + //tlv count
             TLV($0001, tb(DWord(IMD_TLV_STATUS))) +
             TLV($0002, tb(DWord($00000003))) +
             TLV($0003, tb(ImdSrch.OnlineOnly));

      if (ImdSrch.Gender > 0) then
        s := s + tb($0003) + //tlv count
             TLV($0001, tb(DWord(IMD_TLV_GENDER))) +
             TLV($0002, tb(DWord($00000003))) +
             TLV($0003, tb(ImdSrch.Gender));

      sRaw := sRaw +
              TLV(IMD_SEARCH_PERSONAL, s);
    end;

    //IMD_SEARCH_AGE_RANGE
    if (ImdSrch.AgeMin > 0) and (ImdSrch.AgeMax > 0) and (ImdSrch.AgeMin <= ImdSrch.AgeMax) then
    begin
      Inc(iCount);

      s := TLV($0002, tb(ImdSrch.AgeMin));

      if (ImdSrch.AgeMax = 255) then
        s := s + TLV($0003, tb(DWord($00000078))) //120 years
      else
        s := s + TLV($0003, tb(ImdSrch.AgeMax));

      sRaw := sRaw +
              TLV(IMD_SEARCH_AGE_RANGE, s);
    end;

    //IMD_SEARCH_COUNTRY
    if (ImdSrch.HomeCity <> '') or (ImdSrch.HomeCountry <> '') then
    begin
      Inc(iCount);
      s := '';

      if (ImdSrch.HomeCity <> '') then
        s := s + TLV(IMD_ADDRESS_TLV_CITY, UTF8Encode(ImdSrch.HomeCity));

      if (ImdSrch.HomeCountry <> '') then
        s := s + TLV(IMD_ADDRESS_TLV_COUNTRY, UTF8Encode(ImdSrch.HomeCountry));

      sRaw := sRaw +
              TLV(IMD_SEARCH_COUNTRY, s);
    end;

    //IMD_SEARCH_KEYWORD
    if (ImdSrch.Keywords <> '') then
    begin
      Inc(iCount);

      sRaw := sRaw +
              TLV(IMD_SEARCH_KEYWORD, UTF8Encode(ImdSrch.Keywords));
    end;
  end;

  if (sRaw = '') then Exit;
  Result := True;

  //header
  Pkt := LnwValUTF8(FImdLang) +
         tb([$00]) +
         tb([$00]) +
         tb($0000) +
         tb($0001) +
         tb(DWord(IMD_INFO_LEVEL_MID)) +
         tb($0000) +
         tb($0001) +
         tb(iCount) + //search items count
         sRaw +
         tb($0000) +
         tb($0000) +
         tb(DWord(FImdResSkip)) + //skip count
         tb(DWord(IMD_MAX_RESULTS_COUNT)) + //limit
         sLast;

  //add request info
  aReq := ImdMan.AddRequest(REQ_TYPE_SEARCH, ImdMan.NewOscarReqID(CMD_MDIR_INFO_QUERY), ObimpReqID);

  SendSnac(FAM_MDIR, CMD_MDIR_INFO_QUERY, aReq^.OscarReqID, Pkt);
end;

{*****************************************************************}
function TOscarClient.GenImdAddressRaw(const ImdInfo: TImdInfo): RawByteString;
begin
  Result := tb($0005) + //tlv count
            TLV(IMD_ADDRESS_TLV_STREET,  UTF8Encode(ImdInfo.HomeStreet)) +
            TLV(IMD_ADDRESS_TLV_CITY,    UTF8Encode(ImdInfo.HomeCity)) +
            TLV(IMD_ADDRESS_TLV_STATE,   UTF8Encode(ImdInfo.HomeState)) +
            TLV(IMD_ADDRESS_TLV_ZIP,     UTF8Encode(ImdInfo.HomeZip)) +
            TLV(IMD_ADDRESS_TLV_COUNTRY, UTF8Encode(ImdInfo.HomeCountry));
end;

{*****************************************************************}
function TOscarClient.GenImdWorkInfoRaw(const ImdInfo: TImdInfo): RawByteString;
begin
  Result := tb($000D) + //tlv count
            TLV(IMD_WORK_TLV_POSITION,     UTF8Encode(ImdInfo.WorkPosition)) +
            TLV(IMD_WORK_TLV_COMPANY,      UTF8Encode(ImdInfo.WorkCompany)) +
            TLV(IMD_WORK_TLV_WEBSITE,      UTF8Encode(ImdInfo.WorkWebsite)) +
            TLV(IMD_WORK_TLV_DEPARTMENT,   UTF8Encode(ImdInfo.WorkDepart)) +
            TLV(IMD_WORK_TLV_INDUSTRY,     tb(ImdInfo.WorkIndustry)) +
            TLV(IMD_WORK_TLV_SUB_INDUSTRY, tb(ImdInfo.WorkSubIndust)) +
            TLV(IMD_WORK_TLV_START_DATE,   tb(ImdInfo.WorkStartDate)) +
            TLV(IMD_WORK_TLV_END_DATE,     tb(ImdInfo.WorkEndDate)) +
            TLV(IMD_WORK_TLV_STREET,       UTF8Encode(ImdInfo.WorkStreet)) +
            TLV(IMD_WORK_TLV_CITY,         UTF8Encode(ImdInfo.WorkCity)) +
            TLV(IMD_WORK_TLV_STATE,        UTF8Encode(ImdInfo.WorkState)) +
            TLV(IMD_WORK_TLV_ZIP,          UTF8Encode(ImdInfo.WorkZip)) +
            TLV(IMD_WORK_TLV_COUNTRY,      UTF8Encode(ImdInfo.WorkCountry));
end;

{*****************************************************************}
function TOscarClient.GenImdPhonesRaw(const ImdInfo: TImdInfo): RawByteString;
begin
  Result := '';

  if (ImdInfo.PhoneHome <> '') then
    Result := Result + tb($0002) + //tlv count
              TLV(IMD_PHONE_TLV_NUMBER, UTF8Encode(ImdInfo.PhoneHome)) +
              TLV(IMD_PHONE_TLV_TYPE,   tb(DWord(IMD_PHONE_HOME)));

  if (ImdInfo.PhoneWork <> '') then
    Result := Result + tb($0002) + //tlv count
              TLV(IMD_PHONE_TLV_NUMBER, UTF8Encode(ImdInfo.PhoneWork)) +
              TLV(IMD_PHONE_TLV_TYPE,   tb(DWord(IMD_PHONE_WORK)));

  if (ImdInfo.PhoneMobile <> '') then
    Result := Result + tb($0002) + //tlv count
              TLV(IMD_PHONE_TLV_NUMBER, UTF8Encode(ImdInfo.PhoneMobile)) +
              TLV(IMD_PHONE_TLV_TYPE,   tb(DWord(IMD_PHONE_MOBILE)));

  if (ImdInfo.PhoneHomeFax <> '') then
    Result := Result + tb($0002) + //tlv count
              TLV(IMD_PHONE_TLV_NUMBER, UTF8Encode(ImdInfo.PhoneHomeFax)) +
              TLV(IMD_PHONE_TLV_TYPE,   tb(DWord(IMD_PHONE_HOME_FAX)));

  if (ImdInfo.PhoneWorkFax <> '') then
    Result := Result + tb($0002) + //tlv count
              TLV(IMD_PHONE_TLV_NUMBER, UTF8Encode(ImdInfo.PhoneWorkFax)) +
              TLV(IMD_PHONE_TLV_TYPE,   tb(DWord(IMD_PHONE_WORK_FAX)));

  if (ImdInfo.PhoneOther <> '') then
    Result := Result + tb($0002) + //tlv count
              TLV(IMD_PHONE_TLV_NUMBER, UTF8Encode(ImdInfo.PhoneOther)) +
              TLV(IMD_PHONE_TLV_TYPE,   tb(DWord(IMD_PHONE_OTHER)));
end;

{*****************************************************************}
function TOscarClient.GenImdInterestsRaw(const ImdInfo: TImdInfo): RawByteString;
begin
  Result := '';

  if (ImdInfo.InterestText1 <> '') then
    Result := Result + tb($0002) + //tlv count
              TLV(IMD_INTEREST_TLV_TEXT, UTF8Encode(ImdInfo.InterestText1)) +
              TLV(IMD_INTEREST_TLV_CODE, tb(ImdInfo.InterestCode1));

  if (ImdInfo.InterestText2 <> '') then
    Result := Result + tb($0002) + //tlv count
              TLV(IMD_INTEREST_TLV_TEXT, UTF8Encode(ImdInfo.InterestText2)) +
              TLV(IMD_INTEREST_TLV_CODE, tb(ImdInfo.InterestCode2));

  if (ImdInfo.InterestText3 <> '') then
    Result := Result + tb($0002) + //tlv count
              TLV(IMD_INTEREST_TLV_TEXT, UTF8Encode(ImdInfo.InterestText3)) +
              TLV(IMD_INTEREST_TLV_CODE, tb(ImdInfo.InterestCode3));

  if (ImdInfo.InterestText4 <> '') then
    Result := Result + tb($0002) + //tlv count
              TLV(IMD_INTEREST_TLV_TEXT, UTF8Encode(ImdInfo.InterestText4)) +
              TLV(IMD_INTEREST_TLV_CODE, tb(ImdInfo.InterestCode4));
end;

{*****************************************************************}
function TOscarClient.GenImdUpdData(const ImdInfo: TImdInfo): RawByteString;
var sRaw: RawByteString;
    iTlvCount: Word;
begin
  Result := '';

  sRaw      := '';
  iTlvCount := 0;

  //=== SORTED BY TLV TAGS
  //firstname
  if not WideSameStr(ImdInfo.Firstname, OscarInfo.ImdInfo.Firstname) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_FIRSTNAME, UTF8Encode(ImdInfo.Firstname));
  end;

  //lastname
  if not WideSameStr(ImdInfo.Lastname, OscarInfo.ImdInfo.Lastname) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_LASTNAME, UTF8Encode(ImdInfo.Lastname));
  end;

  //gender
  if (ImdInfo.Gender <> OscarInfo.ImdInfo.Gender) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_GENDER, tb(ImdInfo.Gender));
  end;

  //address
  if not WideSameStr(ImdInfo.HomeStreet, OscarInfo.ImdInfo.HomeStreet) or
     not WideSameStr(ImdInfo.HomeCity, OscarInfo.ImdInfo.HomeCity) or
     not WideSameStr(ImdInfo.HomeState, OscarInfo.ImdInfo.HomeState) or
     not WideSameStr(ImdInfo.HomeZip, OscarInfo.ImdInfo.HomeZip) or
     not WideSameStr(ImdInfo.HomeCountry, OscarInfo.ImdInfo.HomeCountry) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_ADDRESS, GenImdAddressRaw(ImdInfo));
  end;

  //login alias and nickname as same things, but when updating it, only ALIAS should be updated, nickname will be changed on server too
  if not WideSameStr(ImdInfo.LoginAlias, OscarInfo.ImdInfo.LoginAlias) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_LOGIN_ALIAS, UTF8Encode(ImdInfo.LoginAlias));
  end;

  //homepage
  if not WideSameStr(ImdInfo.Homepage, OscarInfo.ImdInfo.Homepage) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_HOMEPAGE, UTF8Encode(ImdInfo.Homepage));
  end;

  //language1
  if not WideSameStr(ImdInfo.Language1, OscarInfo.ImdInfo.Language1) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_LANGUAGE1, UTF8Encode(ImdInfo.Language1));
  end;

  //work info
  if not WideSameStr(ImdInfo.WorkPosition, OscarInfo.ImdInfo.WorkPosition) or
     not WideSameStr(ImdInfo.WorkCompany, OscarInfo.ImdInfo.WorkCompany) or
     not WideSameStr(ImdInfo.WorkDepart, OscarInfo.ImdInfo.WorkDepart) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_WORK_INFO, GenImdWorkInfoRaw(ImdInfo));
  end;

  //about
  if not WideSameStr(ImdInfo.About, OscarInfo.ImdInfo.About) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_ABOUT, UTF8Encode(ImdInfo.About));
  end;

  //birthday
  if (ImdInfo.Birthday <> OscarInfo.ImdInfo.Birthday) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_BIRTHDAY, tb(ImdInfo.Birthday));
  end;

  //language2
  if not WideSameStr(ImdInfo.Language2, OscarInfo.ImdInfo.Language2) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_LANGUAGE2, UTF8Encode(ImdInfo.Language2));
  end;

  //phones
  if not WideSameStr(ImdInfo.PhoneHome, OscarInfo.ImdInfo.PhoneHome) or
     not WideSameStr(ImdInfo.PhoneWork, OscarInfo.ImdInfo.PhoneWork) or
     not WideSameStr(ImdInfo.PhoneMobile, OscarInfo.ImdInfo.PhoneMobile) or
     not WideSameStr(ImdInfo.PhoneWorkFax, OscarInfo.ImdInfo.PhoneWorkFax) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_PHONES, GenImdPhonesRaw(ImdInfo));
  end;

  //interests
  if not WideSameStr(ImdInfo.InterestText1, OscarInfo.ImdInfo.InterestText1) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_INTERESTS, GenImdInterestsRaw(ImdInfo));
  end;

  //add tlv count
  if (sRaw <> '') then
    Result := tb(iTlvCount) + sRaw;
end;

{*****************************************************************}
procedure TOscarClient.UpdateMdirInfo(const ImdInfo: TImdInfo; const ObimpReqID: DWord);
var aReq: pImdReq;
    sRaw: RawByteString;
begin
  if FUnsearchable and not FAllowUnsUpd then
  begin
    if Assigned(FOnMdirUpdDeny) then FOnMdirUpdDeny(Self, ObimpReqID, True);
    Exit;
  end;

  if (FConnectState <> CONNECT_STATE_ONLINE) then Exit;

  //add request info
  aReq := ImdMan.AddRequest(REQ_TYPE_UPDATE_OWN_INFO, ImdMan.NewOscarReqID(CMD_MDIR_INFO_UPD_QUERY), ObimpReqID);

  //gen tlvs with tlvcount
  sRaw := GenImdUpdData(ImdInfo);

  //nothing to change?
  if (sRaw = '') then Exit;

  //save new info to set after result rcv
  FNewImdInfo := ImdInfo;

  //header
  sRaw := LnwValUTF8(ImdInfo.Account) +
          tb(DWord($00000000)) + //Zeros
          tb(DWord($00000000)) + //Zeros
          sRaw +
          tb(DWord($00000000)); //Zeros

  SendSnac(FAM_MDIR, CMD_MDIR_INFO_UPD_QUERY, aReq^.OscarReqID, sRaw);
end;

{*****************************************************************}
procedure TOscarClient.UpdateMdirPrivacy;
var sRaw: RawByteString;
    iTlvCount: Word;
    iBool: DWord;
    aReq: pImdReq;
begin
  if (FConnectState <> CONNECT_STATE_ONLINE) then Exit;

  sRaw      := '';
  iTlvCount := 0;

  //=== SORTED BY TLV TAGS
  //privacy level
  if (FPrivacyLevel <> OscarInfo.ImdInfo.PrivacyLevel) and (FPrivacyLevel <= IMD_PRIV_LEVEL_HIGH) then
  begin
    Inc(iTlvCount);
    sRaw := sRaw + TLV(IMD_TLV_PRIVACY_LEVEL, tb(FPrivacyLevel));
  end;

  //webaware
  if (FWebAware <> OscarInfo.ImdInfo.ExposeStatus) then
  begin
    Inc(iTlvCount);
    if FWebAware then iBool := 1 else iBool := 0;
    sRaw := sRaw + TLV(IMD_TLV_EXPOSE_STATUS, tb(iBool));
  end;

  //auth reqãired
  if (FAuthRequired <> OscarInfo.ImdInfo.AuthRequired) then
  begin
    Inc(iTlvCount);
    if FAuthRequired then iBool := 1 else iBool := 0;
    sRaw := sRaw + TLV(IMD_TLV_AUTH_REQUIRED, tb(iBool));
  end;

  //nothing to change?
  if (sRaw = '') then Exit;

  if FUnsearchable and not FAllowUnsUpd then
  begin
    if Assigned(FOnMdirUpdDeny) then FOnMdirUpdDeny(Self, 0, False);
    Exit;
  end;

  //add tlv count
  sRaw := tb(iTlvCount) + sRaw;

  //add request info
  aReq := ImdMan.AddRequest(REQ_TYPE_UPDATE_OPTS, ImdMan.NewOscarReqID(CMD_MDIR_INFO_UPD_QUERY), 0);

  //header
  sRaw := LnwValUTF8(OscarInfo.ImdInfo.Account) +
          tb(DWord($00000000)) + //Zeros
          tb(DWord($00000000)) + //Zeros
          sRaw +
          tb(DWord($00000000)); //Zeros

  SendSnac(FAM_MDIR, CMD_MDIR_INFO_UPD_QUERY, aReq^.OscarReqID, sRaw);
end;


end.
