// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_oscar_sock;

interface

uses Windows, Classes, SysUtils, OverbyteIcsWSocket, h_oscar, u_ext_info;

type
  TOnSocketFlap  = procedure (Sender: TObject; var Flap: TOscarFlap) of object;
  TOnSocketSent  = procedure (Sender: TObject; ErrCode: Word) of object;
  TOnSocketError = procedure (Sender: TObject; ErrCode: integer; Msg: string) of object;

  TOscarSocket = class
  private
    FSocket           : TWSocket;
    FRemServer        : string;
    FRemPort          : string;
    FProxyInfo        : TExtProxyInfo;
    FPktBuf           : RawByteString;
    FOscarSeq         : Word;
    FCollectFlaps     : Boolean;
    FSavedFlaps       : RawByteString;
    FSendPaused       : Boolean;
    FPausedSnacs      : RawByteString;
    FProxyWasSet      : Boolean;
    FOnNetConnected   : TNotifyEvent;
    FOnNetClosed      : TNotifyEvent;
    FOnNetError       : TOnSocketError;
    FOnNetFlapRcvd    : TOnSocketFlap;
    procedure OnSocket_Read(Sender: TObject; ErrCode: Word);
    procedure OnSocket_Connected(Sender: TObject; ErrCode: Word);
    procedure OnSocket_DnsDone(Sender: TObject; ErrCode: Word);
    procedure OnSocket_Closed(Sender: TObject; ErrCode: Word);
    //procedure OnSocket_HttpError(Sender: TObject; ErrCode: Word; TunnelServerAuthTypes: THttpTunnelServerAuthTypes; const Msg: String);
    procedure ResetConnectionParams;
    procedure SetConnectionParams(SetProxy: Boolean);
    function  SendPkt(Data: Pointer; Len: Integer): Integer;
    function  NewSeq: Word;
    procedure ParseData;
    function  IsNextFlapRcvd(var Flap: TOscarFlap): Boolean;
  public
    AliveTicks : DWord;
    constructor Create;
    destructor Destroy; override;
    procedure StartCollect;
    function  EndCollect: Boolean;
    procedure Connect;
    procedure Disconnect;
    procedure ResetData(const Migrating: Boolean);
    procedure SendPause;
    procedure SendResume;
    function  SendFlap(const FrameType: Byte; const Data: RawByteString): Boolean;
    function  SendSnac(const Fam, Cmd: Word; const ReqID: DWord = 0; const Data: RawByteString = ''): Boolean;

    property Server             : string         read FRemServer          write FRemServer;
    property Port               : string         read FRemPort            write FRemPort;
    property ProxyInfo          : TExtProxyInfo  read FProxyInfo          write FProxyInfo;

    property OnNetConnected     : TNotifyEvent   read FOnNetConnected     write FOnNetConnected;
    property OnNetClosed        : TNotifyEvent   read FOnNetClosed        write FOnNetClosed;
    property OnNetFlapRcvd      : TOnSocketFlap  read FOnNetFlapRcvd      write FOnNetFlapRcvd;
    property OnNetError         : TOnSocketError read FOnNetError         write FOnNetError;
  end;

implementation

const
  MAX_SNACS_BUFFER_SIZE = 32768;

{ TCustomSocket }
{***************************************************************}
constructor TOscarSocket.Create;
begin
  FSocket                    := TWSocket.Create(nil);
  FSocket.LineMode           := False;
  FSocket.OnDnsLookupDone    := OnSocket_DnsDone;
  FSocket.OnSessionConnected := OnSocket_Connected;
  FSocket.OnDataAvailable    := OnSocket_Read;
  FSocket.OnSessionClosed    := OnSocket_Closed;
  //FSocket.OnHttpTunnelError  := OnSocket_HttpError;
end;

{***************************************************************}
destructor TOscarSocket.Destroy;
begin
  FSocket.Close;
  FSocket.Free;
  inherited;
end;

{***************************************************************}
procedure TOscarSocket.ResetConnectionParams;
begin
  FSocket.Addr := '';
  FSocket.Port := '';

  FSocket.SocksServer   := '';
  FSocket.SocksPort     := '';
  FSocket.SocksUsercode := '';
  FSocket.SocksPassword := '';
  FSocket.SocksAuthentication := socksNoAuthentication;

  FSocket.HttpTunnelAuthType := htatNone;
  FSocket.HttpTunnelServer   := '';
  FSocket.HttpTunnelPort     := '';
  FSocket.HttpTunnelUsercode := '';
  FSocket.HttpTunnelPassword := '';
end;

{***************************************************************}
procedure TOscarSocket.SetConnectionParams(SetProxy: Boolean);
begin
  ResetConnectionParams;

  FRemServer := Trim(FRemServer);
  FRemPort   := Trim(FRemPort);

  FSocket.Addr := FRemServer;
  FSocket.Port := FRemPort;

  {.$message 'REMOVE SOCKS BEFORE RELEASE'}
  {FSocket.SocksLevel  := '4';
  FSocket.SocksServer := '192.168.218.131';//'192.168.218.150';
  FSocket.SocksPort   := '5190';}

  FProxyWasSet := SetProxy;

  if SetProxy then
  begin
    case FProxyInfo.ProxyType of
    {============================================}
     EXT_PROXY_TYPE_NONE    :;
    {============================================}
     EXT_PROXY_TYPE_HTTP    :
       begin
         FSocket.HttpTunnelAuthType := htatDetect;
         FSocket.HttpTunnelServer   := FProxyInfo.ProxyHost;
         FSocket.HttpTunnelPort     := IntToStr(FProxyInfo.ProxyPort);
         FSocket.HttpTunnelUsercode := FProxyInfo.ProxyUser;
         FSocket.HttpTunnelPassword := FProxyInfo.ProxyPass;
       end;
    {============================================}
     EXT_PROXY_TYPE_SOCKS4,
     EXT_PROXY_TYPE_SOCKS4A,
     EXT_PROXY_TYPE_SOCKS5  :
        begin
          FSocket.SocksServer := FProxyInfo.ProxyHost;
          FSocket.SocksPort   := IntToStr(FProxyInfo.ProxyPort);

          case FProxyInfo.ProxyType of
            EXT_PROXY_TYPE_SOCKS4  : FSocket.SocksLevel := '4';
            EXT_PROXY_TYPE_SOCKS4A : FSocket.SocksLevel := '4A';
            EXT_PROXY_TYPE_SOCKS5  : FSocket.SocksLevel := '5';
          end;//case

          if (FProxyInfo.ProxyUser <> '') or (FProxyInfo.ProxyPass <> '') then
          begin
            FSocket.SocksAuthentication := socksAuthenticateUsercode;
            FSocket.SocksUsercode       := FProxyInfo.ProxyUser;
            FSocket.SocksPassword       := FProxyInfo.ProxyPass;
          end;
        end;
    {============================================}
    end;//case
  end;
end;

{***************************************************************}
procedure TOscarSocket.Connect;
begin
  if (FSocket.State <> wsClosed) then
    FSocket.Close;

  //we doesnt set proxy before dnslookup
  SetConnectionParams(False);

  if (FRemServer = '') or (FRemPort = '') or (StrToIntDef(FRemPort, 0) = 0) then
  begin
    if Assigned(FOnNetError) then FOnNetError(FSocket, 1, 'check server/port');
    Exit;
  end;

  FSocket.DnsLookup(FRemServer);
end;

{***************************************************************}
procedure TOscarSocket.Disconnect;
begin
  if (FSocket.State <> wsClosed) then
    FSocket.Close;
end;

{***************************************************************}
function TOscarSocket.SendPkt(Data: Pointer; Len: Integer): Integer;
begin
  Result := -1;
  if (FSocket.State = wsClosed) then
  begin
    if Assigned(FOnNetError) then FOnNetError(FSocket, 1, 'not connected');
    Exit;
  end;

  try
    Result := FSocket.Send(Data, Len);
  except
    if Assigned(FOnNetError) then FOnNetError(FSocket, 1, 'send error');
  end;
end;

{***************************************************************}
{procedure TOscarSocket.OnSocket_HttpError(Sender: TObject; ErrCode: Word; TunnelServerAuthTypes: THttpTunnelServerAuthTypes; const Msg: String);
begin
  AddLog('Socket_HttpError: code=' + IntToStr(ErrCode) + ' auth=' + IntToStr(Byte(TunnelServerAuthTypes)) + ' msg=' + Msg);
end;}

{***************************************************************}
procedure TOscarSocket.OnSocket_Closed(Sender: TObject; ErrCode: Word);
begin
  if (ErrCode <> 0) then
  begin
    if Assigned(FOnNetError) then FOnNetError(Sender, ErrCode, WSocketErrorDesc(ErrCode));
    Exit;
  end;

  if Assigned(FOnNetClosed) then FOnNetClosed(Sender);
end;

{***************************************************************}
procedure TOscarSocket.OnSocket_DnsDone(Sender: TObject; ErrCode: Word);
begin
  if (ErrCode <> 0) then
  begin
    //if lookup failed and proxy info was not set then try resolve dns after proxy set
    if not FProxyWasSet then
    begin
      Disconnect;
      SetConnectionParams(True);
      FSocket.DnsLookup(FRemServer);
      Exit;
    end;

    if Assigned(FOnNetError) then FOnNetError(Sender, ErrCode, WSocketErrorDesc(ErrCode));
    Exit;
  end;

  //set proxy info
  Disconnect;
  SetConnectionParams(True);

  FSocket.Addr := FSocket.DnsResult;
  try
    FSocket.Connect;
  except
    if Assigned(FOnNetError) then FOnNetError(Sender, FSocket.LastError, '');
  end;
end;

{***************************************************************}
procedure TOscarSocket.OnSocket_Connected(Sender: TObject; ErrCode: Word);
begin
  if (ErrCode <> 0) then
  begin
    if Assigned(FOnNetError) then FOnNetError(Sender, ErrCode, WSocketErrorDesc(ErrCode));
    Exit;
  end;

  if Assigned(FOnNetConnected) then FOnNetConnected(Sender);
end;

{***************************************************************}
procedure TOscarSocket.ResetData(const Migrating: Boolean);
begin
  FCollectFlaps := False;
  FSavedFlaps   := '';
  FSendPaused   := False;

  AliveTicks    := 0;

  if not Migrating then
    FPausedSnacs  := '';

  FOscarSeq     := GetOscarRndWord;
  FPktBuf       := '';
end;

{***************************************************************}
procedure TOscarSocket.OnSocket_Read(Sender: TObject; ErrCode: Word);
var Len, Cnt: integer;
    Buf: RawByteString;
begin
  //get net data, could be faster
  Cnt := FSocket.RcvdCount;
  SetLength(Buf, Cnt);
  Len := FSocket.Receive(@Buf[1], Cnt);
  if (Len <= 0) then Exit;
  SetLength(Buf, Len);

  //collect buffer data
  FPktBuf := FPktBuf + Buf;
  ParseData;
end;

{***************************************************************}
procedure TOscarSocket.ParseData;
var Flap: TOscarFlap;
begin
  if (FPktBuf = '') then
    Exit;

  if not IsNextFlapRcvd(Flap) then
    Exit;

  //collect any replies, they will be sent after whole buffer parsing
  StartCollect;
  try

    if Assigned(FOnNetFlapRcvd) then FOnNetFlapRcvd(Self, Flap);

  finally
    //if there are other flaps remain in packet then handle them too
    if (Length(FPktBuf) > 0) then
      ParseData
    else
      EndCollect;
  end;
end;

{***************************************************************}
function TOscarSocket.IsNextFlapRcvd(var Flap: TOscarFlap): Boolean;
var s: RawByteString;
begin
  Result := False;

  //min flap size is 6
  if (Length(FPktBuf) < 6) then Exit;

  s := Copy(FPktBuf, 1, 6);

  Flap.Signature := ReadByte(s, 1);
  Flap.FrameType := ReadByte(s, 1);
  Flap.Seq       := ReadWord(s, 2);
  Flap.Len       := ReadWord(s, 2);
  Flap.Data      := '';

  //if unknown data rcvd then clear buffer
  if (Flap.Signature <> $2A) then
  begin
    FPktBuf := '';
    Exit;
  end;

  //check that flap fully rcvd
  if ((Length(FPktBuf)-6) < Flap.Len) then Exit;

  Delete(FPktBuf, 1, 6);

  if (Flap.Len > 0) then
    Flap.Data := ReadStr(FPktBuf, Flap.Len, Flap.Len);

  Result := True;
end;

{***************************************************************}
function TOscarSocket.NewSeq: Word;
begin
  Result := FOscarSeq;

  Inc(FOscarSeq, 1);
  if (FOscarSeq = $8001) then
    FOscarSeq := 0;
end;

{***************************************************************}
procedure TOscarSocket.StartCollect;
begin
  if FCollectFlaps then Exit;
  FCollectFlaps := True;
  FSavedFlaps   := '';
end;

{***************************************************************}
function TOscarSocket.EndCollect: Boolean;
var sRaw: RawByteString;
begin
  Result := False;

  FCollectFlaps := False;

  if (FSavedFlaps <> '') then
  begin
    sRaw := FSavedFlaps;

    //clear saved flaps
    FSavedFlaps := '';

    //reset alive tick
    AliveTicks := 0;

    try
      if (FSocket.State <> wsClosed) then
      begin
        SetLength(sRaw, Length(sRaw));
        Result := SendPkt(@sRaw[1], Length(sRaw)) > -1;
      end;
    except end;
  end;
end;

{***************************************************************}
function TOscarSocket.SendFlap(const FrameType: Byte; const Data: RawByteString): Boolean;
var iLen: Word;
    sRaw: RawByteString;
begin
  Result := False;

  iLen := Length(Data);

  //big SNACs will cause diconnect
  if (FrameType = 2) and (iLen > OSCAR_MAX_SNAC_SIZE) then
    Exit;

  //check if send snacs paused
  if FSendPaused and (FrameType = 2) then
  begin
    if (Length(FPausedSnacs) < MAX_SNACS_BUFFER_SIZE) then
      FPausedSnacs := FPausedSnacs + tb(iLen) + Data;

    Result := True;
    Exit;
  end;

  //make flap
  sRaw := tb([$2A, FrameType]) + tb(NewSeq) + tb(iLen) + Data;

  if FCollectFlaps then
  begin
    FSavedFlaps := FSavedFlaps + sRaw;
    Result      := True;
    Exit;
  end;

  //reset alive tick
  AliveTicks := 0;

  try
    if (FSocket.State <> wsClosed) then
    begin
      SetLength(sRaw, Length(sRaw));
      Result := SendPkt(@sRaw[1], Length(sRaw)) > -1;
    end;
  except end;
end;

{***************************************************************}
function TOscarSocket.SendSnac(const Fam, Cmd: Word; const ReqID: DWord; const Data: RawByteString): Boolean;
begin
  Result := SendFlap(2,
                     tb(Fam) +   //family
                     tb(Cmd) +   //command
                     tb($0000) + //flags
                     tb(ReqID) + //request ID
                     Data);
end;

{***************************************************************}
procedure TOscarSocket.SendPause;
begin
  //send pause ack and any other collected flaps
  if FCollectFlaps then EndCollect;

  //start collect paused snacs
  FSendPaused := True;
end;

{***************************************************************}
procedure TOscarSocket.SendResume;
var sRaw: RawByteString;
    iLen: Word;
begin
  FSendPaused := False;

  //send any other collected flaps
  if FCollectFlaps then EndCollect;

  if (Length(FPausedSnacs) = 0) then Exit;

  sRaw := '';

  while (FPausedSnacs <> '') do
  begin
    iLen := ReadWord(FPausedSnacs, 2);
    sRaw := sRaw +
            tb($2A02) + tb(NewSeq) + tb(iLen) + ReadStr(FPausedSnacs, iLen, iLen);
  end;//while

  //reset alive tick
  AliveTicks := 0;

  try
    if (FSocket.State <> wsClosed) then
    begin
      SetLength(sRaw, Length(sRaw));
      SendPkt(@sRaw[1], Length(sRaw));
    end;
  except end;
end;



end.
