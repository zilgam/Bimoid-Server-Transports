// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_oscar_avatars;

interface

uses Windows, Messages, Classes, SysUtils, OverbyteIcsWSocket, u_oscar_sock, h_oscar;

const
  BART_REQ_TYPE_DOWNLOAD = 1;
  BART_REQ_TYPE_UPLOAD   = 2;
  BART_NOAVATAR_HASH_HEX = '0201D20472';

type
  TOscarBart = record
    ReqType    : Byte;
    Account    : string;
    BartType   : Word;
    BartFlag   : Byte;
    BartHash   : RawByteString;
    BartFile   : RawByteString;
  end;
  pOscarBart = ^TOscarBart;

  TOnBartReply = procedure (Sender: TObject; const BartReplyCode: Byte; const Bart: TOscarBart) of object;

  TAvatarsClient = class
  private
    FSocket           : TOscarSocket;
    FListQueue        : TList;
    FServer           : string;
    FPort             : string;
    FBosCookie        : RawByteString;
    FConnectState     : Byte;
    FTrigger_Sock     : Boolean;
    FQueueBusy        : Boolean;
    FBartRequested    : Boolean;
    FOnDownloadReply  : TOnBartReply;
    FOnUploadReply    : TOnBartReply;
    function  GetAliveTicks: DWord;
    procedure SetAliveTicks(const Value: DWord);
    procedure SetConnectParams(AServer, APort: string);
    procedure SocketClose;
    procedure SocketConnect;
    procedure ResetData;
    procedure LoginFailed(const ErrCode: Word);
    procedure ParseFlapData_1(var Flap: TOscarFlap);
    procedure ParseFlapData_2(var Flap: TOscarFlap);
    procedure ParseFlapData_4(var Flap: TOscarFlap);
    procedure ParseFam_OSVC(var Snac: TOscarSnac);
    procedure ParseFam_BART(var Snac: TOscarSnac);
    procedure SendPreOnlineSnacs;
    function  SendSnac(const Fam, Cmd: Word; const ReqID: DWord = 0; const Data: RawByteString = ''): Boolean;
    procedure Send_HELLO_COOKIE;
    procedure Send_BYE;
    function  GenClientFamInfo(FamInfoType: Byte): RawByteString;
    function  GenClientTLVs: RawByteString;
    procedure ClearBartsQueue;
    function  GetOscarBart(const ReqType: Byte; const Account: string; const BartHash: RawByteString): pOscarBart;
  protected
    procedure OnSocketFlapRcvd(Sender: TObject; var Flap: TOscarFlap);
    procedure OnSocketClosed(Sender: TObject);
    procedure OnSocketError(Sender: TObject; ErrCode: integer; Msg: string);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Login;
    procedure Logoff;
    procedure SendKeepAlive;
    procedure AvatarDownloadReq(const Account: string; const AvatarFlags: Byte; const BartHash: RawByteString);
    procedure AvatarUploadReq(const Account: string; const BartHash, BartFile: RawByteString);
    procedure ProcessBartsQueue;
    property ConnectState     : Byte             read FConnectState;
    property AliveTicks       : DWord            read GetAliveTicks      write SetAliveTicks;
    property SvcServer        : string           read FServer            write FServer;
    property SvcPort          : string           read FPort              write FPort;
    property SvcCookie        : RawByteString    read FBosCookie         write FBosCookie;
    property BartRequested    : Boolean          read FBartRequested     write FBartRequested;
    property OnDownloadReply  : TOnBartReply     read FOnDownloadReply   write FOnDownloadReply;
    property OnUploadReply    : TOnBartReply     read FOnUploadReply     write FOnUploadReply;
  end;

implementation

{ TAvatarsClient }
{*****************************************************************}
constructor TAvatarsClient.Create;
begin
  FSocket                := TOscarSocket.Create;
  FSocket.OnNetFlapRcvd  := OnSocketFlapRcvd;
  FSocket.OnNetClosed    := OnSocketClosed;
  FSocket.OnNetError     := OnSocketError;

  FListQueue := TList.Create;
end;

{*****************************************************************}
destructor TAvatarsClient.Destroy;
begin
  ClearBartsQueue;
  FListQueue.Free;
  FSocket.Free;
  inherited;
end;

{*****************************************************************}
procedure TAvatarsClient.ClearBartsQueue;
var ob: pOscarBart;
    i: integer;
begin
  if (FListQueue.Count = 0) then Exit;

  for i := FListQueue.Count-1 downto 0 do
  begin
    ob := FListQueue.Items[i];
    Dispose(ob);
    FListQueue.Delete(i);
  end;//for
end;

{*****************************************************************}
procedure TAvatarsClient.SetConnectParams(AServer, APort: string);
begin
  FSocket.Server := AServer;
  FSocket.Port   := APort;
end;

{*****************************************************************}
procedure TAvatarsClient.Login;
begin
  ResetData;
  SocketClose;
  SetConnectParams(FServer, FPort);
  SocketConnect;
end;

{*****************************************************************}
procedure TAvatarsClient.Logoff;
begin
  if (FConnectState = CONNECT_STATE_OFFLINE) then
    Exit;

  if (FConnectState = CONNECT_STATE_ONLINE) then
    Send_BYE;

  FBartRequested := False;

  FTrigger_Sock := True;
  SocketClose;
end;

{*****************************************************************}
procedure TAvatarsClient.LoginFailed(const ErrCode: Word);
begin
  FBartRequested := False;
end;

{*****************************************************************}
procedure TAvatarsClient.SocketConnect;
begin
  FConnectState := CONNECT_STATE_CONNECTING;
  FSocket.Connect;
end;

{*****************************************************************}
procedure TAvatarsClient.ResetData;
begin
  FSocket.ResetData(False);
  FTrigger_Sock  := False;
  FQueueBusy     := False;
end;

{*****************************************************************}
procedure TAvatarsClient.SocketClose;
begin
  FConnectState := CONNECT_STATE_OFFLINE;
  FSocket.Disconnect;
end;

{*****************************************************************}
procedure TAvatarsClient.OnSocketClosed(Sender: TObject);
begin
  if FTrigger_Sock then Exit;
  FTrigger_Sock := True;

  SocketClose;
  LoginFailed(0);
end;

{*****************************************************************}
procedure TAvatarsClient.OnSocketError(Sender: TObject; ErrCode: integer; Msg: string);
begin
  if FTrigger_Sock then Exit;
  FTrigger_Sock := True;

  SocketClose;
  LoginFailed(0);
end;

{*****************************************************************}
procedure TAvatarsClient.OnSocketFlapRcvd(Sender: TObject; var Flap: TOscarFlap);
begin
  case Flap.FrameType of
    1: ParseFlapData_1(Flap);
    2: ParseFlapData_2(Flap);
    4: ParseFlapData_4(Flap);
  end;//case
end;

{*****************************************************************}
procedure TAvatarsClient.ParseFlapData_1(var Flap: TOscarFlap);
var iHello: DWord;
begin
  if (Flap.Data = '') then Exit;

  //check hello
  iHello := ReadDWord(Flap.Data, 4);
  if (iHello = 0) then Exit;

  if (FConnectState = CONNECT_STATE_CONNECTING) then
    Send_HELLO_COOKIE;
end;

{*****************************************************************}
procedure TAvatarsClient.ParseFlapData_4(var Flap: TOscarFlap);
begin
  //server bumped us out
  if (FConnectState = CONNECT_STATE_ONLINE) then
  begin
    Send_BYE;

    FTrigger_Sock := True;
    SocketClose;
    LoginFailed(BUCP_ERR_INTERNAL_ERROR);
  end;
end;

{*****************************************************************}
procedure TAvatarsClient.ParseFlapData_2(var Flap: TOscarFlap);
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
    FAM_BART    : ParseFam_BART(Snac);
  end;//case
end;

{*****************************************************************}
procedure TAvatarsClient.ParseFam_OSVC(var Snac: TOscarSnac);
var iCount: Word;
    i: integer;
    s: RawByteString;
begin
  case Snac.Cmd of
   {=====================================================}
    CMD_OSVC_ERROR:
      begin
        if (FConnectState = CONNECT_STATE_CONNECTING) then
        begin
          FTrigger_Sock := True;
          SocketClose;
          LoginFailed(BUCP_ERR_INTERNAL_ERROR);
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
          SendPreOnlineSnacs;
        end;
      end;
   {=====================================================}
    CMD_OSVC_PAUSE,
    CMD_OSVC_MIGRATE_GROUPS:
      begin
        FTrigger_Sock := True;
        SocketClose;
        LoginFailed(BUCP_ERR_INTERNAL_ERROR);
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
procedure TAvatarsClient.ParseFam_BART(var Snac: TOscarSnac);
var ob: TOscarBart;
    iLen: Word;
    iRes: Byte;
begin
  case Snac.Cmd of
   {=====================================================}
    CMD_BART_UPLOAD_REPLY:
      begin
        FQueueBusy := False;

        ZeroMemory(@ob, SizeOf(TOscarBart));

        iRes := ReadByte(Snac.Data, 1);

        ob.BartType := ReadWord(Snac.Data, 2);
        ob.BartFlag := ReadByte(Snac.Data, 1);
        iLen        := ReadByte(Snac.Data, 1);
        if (iLen > 0) then
          ob.BartHash := ReadStr(Snac.Data, iLen, iLen);

        //notify
        if Assigned(FOnUploadReply) then FOnUploadReply(Self, iRes, ob);
      end;
   {=====================================================}
    CMD_BART_DOWNLOAD_REPLY:
      begin
        FQueueBusy := False;

        ZeroMemory(@ob, SizeOf(TOscarBart));

        ob.Account := ReadLnbUTF8(Snac.Data, True, True);

        //parse queried bart id, parse to skip it
        ob.BartType := ReadWord(Snac.Data, 2);
        ob.BartFlag := ReadByte(Snac.Data, 1);
        iLen        := ReadByte(Snac.Data, 1);
        if (iLen > 0) then
          ob.BartHash := ReadStr(Snac.Data, iLen, iLen);

        //get query result
        iRes := ReadByte(Snac.Data, 1);
        //if success
        if (iRes = 0) then
        begin
          //parse replied bart id that will be used
          ob.BartType := ReadWord(Snac.Data, 2);
          ob.BartFlag := ReadByte(Snac.Data, 1);
          iLen        := ReadByte(Snac.Data, 1);
          if (iLen > 0) then
            ob.BartHash := ReadStr(Snac.Data, iLen, iLen);

          //get bart file len
          iLen := ReadWord(Snac.Data, 2);
          if (iLen > 0) then
          begin
            ob.BartFile := ReadStr(Snac.Data, iLen, iLen);
            SetLength(ob.BartFile, Length(ob.BartFile));
          end;

          //notify
          if Assigned(FOnDownloadReply) then FOnDownloadReply(Self, iRes, ob);
        end;
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
function TAvatarsClient.GetAliveTicks: DWord;
begin
  Result := FSocket.AliveTicks;
end;

{*****************************************************************}
procedure TAvatarsClient.SetAliveTicks(const Value: DWord);
begin
  FSocket.AliveTicks := Value;
end;

{*****************************************************************}
procedure TAvatarsClient.Send_HELLO_COOKIE;
begin
  FSocket.SendFlap(1, tb(DWord(1)) +                   //hello
                   TLV(BUCP_TAG_COOKIE, FBosCookie) +  //cookie
                   GenClientTLVs);                     //client info
end;

{*****************************************************************}
procedure TAvatarsClient.Send_BYE;
begin
  FSocket.SendFlap(4, '');
end;

{*****************************************************************}
procedure TAvatarsClient.SendKeepAlive;
begin
  FSocket.SendFlap(5, tb(DWord(OSCAR_KEEP_ALIVE_INTERVAL)));
end;

{*****************************************************************}
function TAvatarsClient.SendSnac(const Fam, Cmd: Word; const ReqID: DWord; const Data: RawByteString): Boolean;
begin
  //send snac
  Result := FSocket.SendSnac(Fam, Cmd, ReqID, Data);
end;

{*****************************************************************}
procedure TAvatarsClient.SendPreOnlineSnacs;
begin
  SendSnac(FAM_OSVC, CMD_OSVC_CLIENT_ONLINE, CMD_OSVC_CLIENT_ONLINE, GenClientFamInfo(FAM_INFO_TYPE_TOOLVERS));

  //OK, we are online
  FConnectState := CONNECT_STATE_ONLINE;
end;

{*****************************************************************}
function TAvatarsClient.GenClientTLVs: RawByteString;
begin
  Result := TLV(BUCP_TAG_CLIENT_NAME, CLIENT_NAME) +
            TLV(BUCP_TAG_VER_MAJOR,   CLIENT_VER_MAJOR) +
            TLV(BUCP_TAG_VER_MINOR,   CLIENT_VER_MINOR) +
            TLV(BUCP_TAG_VER_POINT,   CLIENT_VER_POINT) +
            TLV(BUCP_TAG_VER_BUILD,   CLIENT_VER_BUILD) +  //important thing for status flags
            TLV(BUCP_TAG_CLIENT_ID,   CLIENT_ID) +
            TLV(BUCP_TAG_LANGUAGE,    CLIENT_LANGUAGE) +
            TLV(BUCP_TAG_COUNTRY,     CLIENT_COUNTRY) +
            TLV(BUCP_TAG_RECONNECT,   tb([CLIENT_RECONNECT])) +
            TLV(BUCP_TAG_MULTICON,    tb([CLIENT_MC_MULTIPLE]));
end;

{*****************************************************************}
function TAvatarsClient.GenClientFamInfo(FamInfoType: Byte): RawByteString;
begin
  Result := '';

  case FamInfoType of
   {=====================================================}
    FAM_INFO_TYPE_FAMILIES:
      begin
        Result := tb(FAM_OSVC) +
                  tb(FAM_BART);
      end;
   {=====================================================}
    FAM_INFO_TYPE_VERSIONS:
      begin
        Result := tb(FAM_OSVC)    + tb(VER_FAM_OSVC) +
                  tb(FAM_BART)    + tb(VER_FAM_BART);
      end;
   {=====================================================}
    FAM_INFO_TYPE_TOOLVERS:
      begin
        Result := tb(FAM_OSVC)    + tb(VER_FAM_OSVC)    + tb(TOOL_ID) + tb(TOOL_VER) +
                  tb(FAM_BART)    + tb(VER_FAM_BART)    + tb(TOOL_ID) + tb(TOOL_VER);
      end;
   {=====================================================}
  end;//case
end;

{*****************************************************************}
function TAvatarsClient.GetOscarBart(const ReqType: Byte; const Account: string; const BartHash: RawByteString): pOscarBart;
var ob: pOscarBart;
    i: integer;
begin
  Result := nil;

  for i := 0 to FListQueue.Count-1 do
  begin
    ob := FListQueue.Items[i];

    if (ob^.ReqType = ReqType) and SameOscAcc(ob^.Account, Account) and (ob^.BartHash = BartHash) then
    begin
      Result := ob;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
procedure TAvatarsClient.AvatarDownloadReq(const Account: string; const AvatarFlags: Byte; const BartHash: RawByteString);
var ob: pOscarBart;
begin
  if (BartHash = '') or (GetOscarBart(BART_REQ_TYPE_DOWNLOAD, Account, BartHash) <> nil) then Exit;

  //add to request queue
  New(ob);
  ZeroMemory(ob, SizeOf(TOscarBart));

  ob^.ReqType  := BART_REQ_TYPE_DOWNLOAD;
  ob^.Account  := Account;
  ob^.BartType := BART_BUDDY_ICON;
  ob^.BartFlag := AvatarFlags;
  ob^.BartHash := BartHash;
  ob^.BartFile := '';

  FListQueue.Add(ob);
end;

{*****************************************************************}
procedure TAvatarsClient.AvatarUploadReq(const Account: string; const BartHash, BartFile: RawByteString);
var ob: pOscarBart;
begin
  if (Account = '') or (BartFile = '') or (BartHash = '') or (GetOscarBart(BART_REQ_TYPE_UPLOAD, Account, BartHash) <> nil) then Exit;

  //add to request queue
  New(ob);
  ZeroMemory(ob, SizeOf(TOscarBart));

  ob^.ReqType  := BART_REQ_TYPE_UPLOAD;
  ob^.Account  := Account;
  ob^.BartType := BART_BUDDY_ICON;
  ob^.BartFlag := BART_FLAG_CUSTOM;
  ob^.BartHash := BartHash;
  ob^.BartFile := BartFile;

  FListQueue.Add(ob);
end;

{*****************************************************************}
procedure TAvatarsClient.ProcessBartsQueue;
var ob: pOscarBart;
begin
  if (ConnectState <> CONNECT_STATE_ONLINE) or
     (FListQueue.Count = 0) or
     FQueueBusy then
  begin
    Exit;
  end;

  ob := FListQueue.Items[0];
  FListQueue.Delete(0);
  try
    FQueueBusy := True;

    if (ob^.ReqType = BART_REQ_TYPE_DOWNLOAD) then
    begin
      //if download
      SendSnac(FAM_BART, CMD_BART_DOWNLOAD, CMD_BART_DOWNLOAD or (GetOscarRndWord shl 16),
               LnbValUTF8(ob^.Account) +
               tb([$01]) + //bart IDs count
               GenBartData(ob^.BartType, ob^.BartFlag, ob^.BartHash));
    end
    else
    begin
      //if upload
      SendSnac(FAM_BART, CMD_BART_UPLOAD, CMD_BART_UPLOAD or (GetOscarRndWord shl 16),
               tb(ob^.BartType) +
               tb(Word(Length(ob^.BartFile))) +
               ob^.BartFile);
    end;

  finally
    Dispose(ob);
  end;
end;





end.
