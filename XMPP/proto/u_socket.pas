// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_socket;

interface

uses Windows, SysUtils, Classes, OverbyteIcsWSocket, OverbyteIcsDnsQuery, OverbyteIcsSSLEAY, OverbyteIcsLIBEAY, OverbyteIcsMimeUtils,
     u_timer, u_ext_info;

const
  PRX_TYPE_NONE     = 0;
  PRX_TYPE_HTTP     = 1;
  PRX_TYPE_HTTPS    = 2;
  PRX_TYPE_SOCKS4   = 3;
  PRX_TYPE_SOCKS4A  = 4;
  PRX_TYPE_SOCKS5   = 5;

  ERR_NET_NOSERVER      = 0;
  ERR_NET_NOPORT        = 1;
  ERR_NET_CON_FAILED    = 2;

  NET_STR_NO_SERVER     = 'Set remote host address';
  NET_STR_NO_PORT       = 'Set remote port';
  NET_STR_CON_FAILED    = 'Connection failed';
  NET_STR_PR_CON_FAILED = 'Proxy connection failed';
  NET_STR_PR_NOT_AUTHED = 'Proxy authentication failed';

  SRV_XMPP_CLIENT       = '_xmpp-client._tcp.';

  SSL_SUB_DIR = 'ObimpSsl\';

type
  TOnPktLog       = procedure (Sender: TObject; const Incoming: Boolean; const XmlTag: string) of object;
  TOnSocketEvent  = procedure (Sender: TObject) of object;
  TOnSocketData   = procedure (Sender: TObject; Data: RawByteString) of object;
  TOnSocketSent   = procedure (Sender: TObject; ErrCode: Word) of object;
  TOnSocketError  = procedure (Sender: TObject; ErrCode: integer; Msg: string) of object;
  TOnVerifyFailed = procedure (Sender: TObject; ChainInfo: string; const BadHost: Boolean; var ConnectAnyway, SaveTrusted: Boolean) of object;

  TObimpSocket = class
  private
    FRemServer          : string;
    FRemPort            : string;
    FProxyInfo          : TExtProxyInfo;
    FSecureSSL          : Boolean;
    FWaitingTLS         : Boolean;
    FOnNetConnected     : TOnSocketEvent;
    FOnNetClosed        : TOnSocketEvent;
    FOnNetDataAvailable : TOnSocketData;
    FOnNetDataSent      : TOnSocketSent;
    FOnNetError         : TOnSocketError;
    FOnNetInitedTLS     : TNotifyEvent;
    FOnVerifyFailed     : TOnVerifyFailed;
    FOnLoadTrusted      : TNotifyEvent;
    FOnSaveTrusted      : TNotifyEvent;
    {+++ DnsQuery Events ++++++++++++++++++++++++++++++++++++++++++++++++++++++}
    procedure OnDnsTimer(Sender: TObject);
    procedure OnDnsQueryRequestDone(Sender: TObject; Error: Word);
    {+++ Socket Events ++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
    procedure OnInfSockSslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
    procedure OnInfSockSslVerifyPeer(Sender: TObject; var Ok: Integer; Cert: TX509Base);
    procedure OnInfSockRead(Sender: TObject; ErrCode: Word);
    procedure OnInfSockSent(Sender: TObject; ErrCode: Word);
    procedure OnInfSockConnected(Sender: TObject; ErrCode: Word);
    procedure OnInfSockDnsDone(Sender: TObject; ErrCode: Word);
    procedure OnInfSockClosed(Sender: TObject; ErrCode: Word);
    procedure OnInfSockSocksError(Sender: TObject; ErrCode: integer; Msg: string);
    {+++ Helpful Procs ++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
    procedure SetConnectionParams;
    procedure DoNetConnected(const SkipSSL: Boolean);
    procedure DoNetInitedTLS;
  protected
    procedure ResetConnectionParams; virtual;
    procedure DoDataAvailable(var Buf: RawByteString); virtual;
  public
    DnsQuery     : TDnsQuery;
    Socket       : TSslWSocket;
    SslCtx       : TSslContext;
    ListDns      : TStringList;
    DnsTimer     : TBestTimer;
    constructor Create;
    destructor Destroy; override;
    {+++ Public Procs +++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
    procedure Connect(SrvQuery: Boolean);
    procedure Disconnect(const Abort: Boolean = False);
    function  SendNetPkt(const Pkt: RawByteString): integer;
    function  StartTLS: Boolean;
    {+++ Public Props +++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
    property Server    : string         read FRemServer  write FRemServer;
    property Port      : string         read FRemPort    write FRemPort;
    property SecureSSL : Boolean        read FSecureSSL  write FSecureSSL;
    property ProxyInfo : TExtProxyInfo  read FProxyInfo  write FProxyInfo;

    {+++ Events +++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
    property OnNetConnected     : TOnSocketEvent  read FOnNetConnected     write FOnNetConnected;
    property OnNetClosed        : TOnSocketEvent  read FOnNetClosed        write FOnNetClosed;
    property OnNetDataAvailable : TOnSocketData   read FOnNetDataAvailable write FOnNetDataAvailable;
    property OnNetDataSent      : TOnSocketSent   read FOnNetDataSent      write FOnNetDataSent;
    property OnNetError         : TOnSocketError  read FOnNetError         write FOnNetError;
    property OnNetInitedTLS     : TNotifyEvent    read FOnNetInitedTLS     write FOnNetInitedTLS;
    property OnSslVerifyFailed  : TOnVerifyFailed read FOnVerifyFailed     write FOnVerifyFailed;
    property OnSslLoadTrusted   : TNotifyEvent    read FOnLoadTrusted      write FOnLoadTrusted;
    property OnSslSaveTrusted   : TNotifyEvent    read FOnSaveTrusted      write FOnSaveTrusted;
  end;

implementation

{***************************************************************}
function GetDnsServerIP: AnsiString;
type
  PTIP_ADDRESS_STRING = ^TIP_ADDRESS_STRING;
  TIP_ADDRESS_STRING = array[0..15] of AnsiChar;
  PTIP_ADDR_STRING = ^TIP_ADDR_STRING;
  TIP_ADDR_STRING = packed record
    Next      : PTIP_ADDR_STRING;
    IpAddress : TIP_ADDRESS_STRING;
    IpMask    : TIP_ADDRESS_STRING;
    Context   : DWORD;
  end;
  PTFixedInfo = ^TFixedInfo;
  TFixedInfo = packed record
    HostName          : array[1..128 + 4] of AnsiChar;
    DomainName        : array[1..128 + 4] of AnsiChar;
    CurrentDNSServer  : PTIP_ADDR_STRING;
    DNSServerList     : TIP_ADDR_STRING;
    NodeType          : UINT;
    ScopeID           : array[1..256 + 4] of AnsiChar;
    EnableRouting     : UINT;
    EnableProxy       : UINT;
    EnableDNS         : UINT;
  end;
const
  IpHlpDLL = 'iphlpapi.dll';
  CRLF_A   : RawByteString = #13#10;
var
  IpHlpModule : THandle;
  FixedInfo   : PTFixedInfo;
  InfoSize    : Longint;
  PDnsServer  : PTIP_ADDR_STRING;
  err         : integer;
  FuncGetNetworkParams: function (FixedInfo: PTFixedInfo; pOutPutLen: PULONG): DWORD; stdcall;
begin
  Result   := '';
  InfoSize := 0;

  IpHlpModule := LoadLibrary(IpHlpDLL);
  if (IpHlpModule = 0) then
    Exit;

  try
    FuncGetNetworkParams := GetProcAddress(IpHlpModule, 'GetNetworkParams');
    if (@FuncGetNetworkParams = nil) then
      Exit;

    err := FuncGetNetworkParams(nil, @InfoSize);
    if (err <> ERROR_BUFFER_OVERFLOW) then
      Exit;

    GetMem(FixedInfo, InfoSize);
    try
      err := FuncGetNetworkParams(FixedInfo, @InfoSize);
      if (err <> ERROR_SUCCESS) then
        Exit;

      with FixedInfo^ do
      begin
        Result := DnsServerList.IpAddress;

        PDnsServer := DnsServerList.Next;
        while (PDnsServer <> nil) do
        begin
          if (Result <> '') then
            Result := Result + CRLF_A;

          Result := Result + PDnsServer^.IPAddress;

          PDnsServer := PDnsServer.Next;
        end;
    end;
    finally
      FreeMem(FixedInfo);
    end;
  finally
    FreeLibrary(IpHlpModule);
  end;
end;

{ TObimpSocket }
{***************************************************************}
constructor TObimpSocket.Create;
begin
  DnsQuery               := TDnsQuery.Create(nil);
  DnsQuery.OnRequestDone := OnDnsQueryRequestDone;

  Socket                    := TSslWSocket.Create(nil);
  Socket.LineMode           := FALSE;

  Socket.OnDnsLookupDone    := OnInfSockDnsDone;
  Socket.OnSessionConnected := OnInfSockConnected;
  Socket.OnDataAvailable    := OnInfSockRead;
  Socket.OnSessionClosed    := OnInfSockClosed;
  Socket.OnSocksError       := OnInfSockSocksError;
  Socket.OnDataSent         := OnInfSockSent;
  Socket.OnSslVerifyPeer    := OnInfSockSslVerifyPeer;
  Socket.OnSslHandshakeDone := OnInfSockSslHandshakeDone;

  SslCtx                      := TSslContext.Create(nil);
  SslCtx.SslOptions           := [sslOpt_NO_SSLv2];
  SslCtx.SslVersionMethod     := sslV23_CLIENT;
  SslCtx.SslSessionCacheModes := [sslSESS_CACHE_CLIENT];
  SslCtx.SslVerifyPeer        := True;
  SslCtx.SslCAFile            := '';
  SslCtx.SslCAPath            := '';

  Socket.SslContext := SslCtx;

  ListDns := TStringList.Create;

  DnsTimer          := TBestTimer.Create;
  DnsTimer.Interval := 2000;
  DnsTimer.Enabled  := False;
  DnsTimer.OnTimer  := OnDnsTimer;
end;

{***************************************************************}
destructor TObimpSocket.Destroy;
begin
  DnsTimer.Free;

  DnsQuery.CloseConnection;
  DnsQuery.Free;

  ListDns.Free;

  Socket.Close;
  Socket.Free;

  SslCtx.Free;
  inherited;
end;

{***************************************************************}
procedure TObimpSocket.ResetConnectionParams;
begin
  Socket.Addr := '';
  Socket.Port := '';

  Socket.SocksServer         := '';
  Socket.SocksPort           := '';
  Socket.SocksUsercode       := '';
  Socket.SocksPassword       := '';
  Socket.SocksAuthentication := socksNoAuthentication;

  Socket.HttpTunnelAuthType := htatNone;
  Socket.HttpTunnelServer   := '';
  Socket.HttpTunnelPort     := '';
  Socket.HttpTunnelUsercode := '';
  Socket.HttpTunnelPassword := '';

  Socket.SslEnable := False;

  FWaitingTLS := False;
end;

{***************************************************************}
procedure TObimpSocket.SetConnectionParams;
begin
  ResetConnectionParams;

  FRemServer := Trim(FRemServer);
  FRemPort   := Trim(FRemPort);

  Socket.Addr := FRemServer;
  Socket.Port := FRemPort;

  case FProxyInfo.ProxyType of
  {============================================}
   EXT_PROXY_TYPE_NONE    :;
  {============================================}
   EXT_PROXY_TYPE_HTTP    :
     begin
       Socket.HttpTunnelAuthType := htatDetect;
       Socket.HttpTunnelServer   := FProxyInfo.ProxyHost;
       Socket.HttpTunnelPort     := IntToStr(FProxyInfo.ProxyPort);
       Socket.HttpTunnelUsercode := FProxyInfo.ProxyUser;
       Socket.HttpTunnelPassword := FProxyInfo.ProxyPass;
     end;
  {============================================}
   EXT_PROXY_TYPE_SOCKS4,
   EXT_PROXY_TYPE_SOCKS4A,
   EXT_PROXY_TYPE_SOCKS5  :
      begin
        Socket.SocksServer := FProxyInfo.ProxyHost;
        Socket.SocksPort   := IntToStr(FProxyInfo.ProxyPort);

        case FProxyInfo.ProxyType of
          EXT_PROXY_TYPE_SOCKS4  : Socket.SocksLevel := '4';
          EXT_PROXY_TYPE_SOCKS4A : Socket.SocksLevel := '4A';
          EXT_PROXY_TYPE_SOCKS5  : Socket.SocksLevel := '5';
        end;//case

        if (FProxyInfo.ProxyUser <> '') or (FProxyInfo.ProxyPass <> '') then
        begin
          Socket.SocksAuthentication := socksAuthenticateUsercode;
          Socket.SocksUsercode       := FProxyInfo.ProxyUser;
          Socket.SocksPassword       := FProxyInfo.ProxyPass;
        end;
      end;
  {============================================}
  end;//case
end;

{***************************************************************}
procedure TObimpSocket.Connect(SrvQuery: Boolean);
var ErrCode: Word;
    Msg, s: string;
begin
  Disconnect;

  SetConnectionParams;

  if (FRemServer = '') then
  begin
    ErrCode := ERR_NET_NOSERVER;
    Msg     := NET_STR_NO_SERVER;
    if Assigned(FOnNetError) then FOnNetError(Socket, ErrCode, Msg);
    Exit;
  end;

  //to get SRV record
  if SrvQuery and not WSocketIsDottedIP(UTF8Encode(FRemServer)) then
  begin
    if (ListDns.Count = 0) then
      ListDns.Text := Trim( UTF8ToUnicodeString(GetDnsServerIP) );

    if (ListDns.Count > 0) then
    begin
      s := ListDns.Strings[0];
      ListDns.Delete(0);

      DnsTimer.Enabled := True;

      DnsQuery.Addr := s;

      s := SRV_XMPP_CLIENT + FRemServer;

      try
        DnsQuery.SRVLookup(UTF8Encode(s));
      except
        if Assigned(FOnNetError) then FOnNetError(Socket, ERR_NET_CON_FAILED, NET_STR_CON_FAILED);
      end;

      Exit;
    end;
  end;

  if (FRemPort = '') or (StrToIntDef(FRemPort, 0) = 0) then
  begin
    ErrCode := ERR_NET_NOPORT;
    Msg     := NET_STR_NO_PORT;
    if Assigned(FOnNetError) then FOnNetError(Socket, ErrCode, Msg);
    Exit;
  end;

  Socket.DnsLookup(FRemServer);
end;

{***************************************************************}
procedure TObimpSocket.Disconnect(const Abort: Boolean = False);
begin
  DnsTimer.Enabled := False;

  if (Socket.State <> wsClosed) then
  begin
    if Abort then
      Socket.Abort
    else
      Socket.Close;
  end;

  DnsQuery.CloseConnection;
end;

{***************************************************************}
procedure TObimpSocket.OnDnsTimer(Sender: TObject);
begin
  DnsTimer.Enabled := False;
  Connect(ListDns.Count > 0);
end;

{***************************************************************}
procedure TObimpSocket.OnDnsQueryRequestDone(Sender: TObject; Error: Word);
var i, iCurPort, iNewPort: Integer;
    sHost: string;
    bFound: Boolean;
begin
  DnsTimer.Enabled := False;

  sHost    := '';
  iCurPort := StrToIntDef(Trim(FRemPort), 0);
  iNewPort := 0;
  bFound   := False;

  if (Error = 0) and (DnsQuery.ResponseANCount > 0) then
  begin
    if (DnsQuery.AnswerTag[0] > -1) then
    begin
      //find SRV record with needed port
      for i := 0 to DnsQuery.ResponseANCount-1 do
      begin
        if (DnsQuery.SrvRecords[i].PortNum = iCurPort) then
        begin
          sHost    := UTF8ToUnicodeString( DnsQuery.SrvRecords[i].Host );
          iNewPort := DnsQuery.SrvRecords[i].PortNum;

          bFound := True;
          Break;
        end;
      end;//for

      //if needed port not found then use first SRV record
      if not bFound then
      begin
        sHost    := UTF8ToUnicodeString( DnsQuery.SrvRecords[0].Host );
        iNewPort := DnsQuery.SrvRecords[0].PortNum;
      end;
    end;
  end;

  if (sHost <> '') and (iNewPort > 0) then
  begin
    FRemServer := sHost;
    FRemPort   := IntToStr(iNewPort);
    FSecureSSL := False;//SRV records return non-secure ports
  end;

  ListDns.Clear;
  Connect(False);
end;

{***************************************************************}
procedure TObimpSocket.OnInfSockSocksError(Sender: TObject; ErrCode: integer; Msg: string);
begin
  if Assigned(FOnNetError) then FOnNetError(Sender, ErrCode, Msg);
end;

{***************************************************************}
procedure TObimpSocket.OnInfSockClosed(Sender: TObject; ErrCode: Word);
begin
  if (ErrCode <> 0) then
  begin
    if Assigned(FOnNetError) then FOnNetError(Sender, ErrCode, WsocketErrorDesc(ErrCode));
    Exit;
  end;

  if Assigned(FOnNetClosed) then FOnNetClosed(Sender);
end;

{***************************************************************}
procedure TObimpSocket.OnInfSockDnsDone(Sender: TObject; ErrCode: Word);
begin
  if (ErrCode <> 0) then
  begin
    if Assigned(FOnNetError) then FOnNetError(Sender, ErrCode, WsocketErrorDesc(ErrCode));
    Exit;
  end;

  Disconnect;

  //connect to dns reply host
  Socket.Addr := Socket.DnsResult;

  try
    Socket.Connect;
  except
    if Assigned(FOnNetError) then FOnNetError(Socket, Socket.LastError, '');
  end;
end;

{***************************************************************}
procedure TObimpSocket.OnInfSockConnected(Sender: TObject; ErrCode: Word);
begin
  if (ErrCode <> 0) then
  begin
    if Assigned(FOnNetError) then FOnNetError(Sender, ErrCode, WsocketErrorDesc(ErrCode));
    Exit;
  end;

  DoNetConnected(False);
end;

{***************************************************************}
procedure TObimpSocket.OnInfSockSslVerifyPeer(Sender: TObject; var Ok: Integer; Cert: TX509Base);
begin
  //Alternate verification takes place in event HandshakeDone, we accept anything temporarily here.
  //Note that the same certificate may appear multiple times in this event when we set OK to 1 overwriting the real verify result.
  OK := 1;
end;

{***************************************************************}
procedure TObimpSocket.OnInfSockSslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
begin
  //ssl handshake error or session is reused or no verification wanted
  if (ErrCode <> 0) then
    Exit;

  if FWaitingTLS then
  begin
    FWaitingTLS := False;
    DoNetInitedTLS;
  end
  else
    DoNetConnected(True);
end;

{***************************************************************}
procedure TObimpSocket.OnInfSockSent(Sender: TObject; ErrCode: Word);
begin
  if Assigned(FOnNetDataSent) then FOnNetDataSent(Sender, ErrCode);
end;

{***************************************************************}
procedure TObimpSocket.OnInfSockRead(Sender: TObject; ErrCode: Word);
var Len, Cnt    : integer;
    Buf         : RawByteString;
begin
 {### RECEIVING DATA #####################################}
  Cnt := Socket.RcvdCount;
  SetLength(Buf, Cnt);
  Len := Socket.Receive(@Buf[1], Cnt);
  if (Len <= 0) then Exit;
  SetLength(Buf, Len);
 {########################################################}

  DoDataAvailable(Buf);
end;

{***************************************************************}
function TObimpSocket.SendNetPkt(const Pkt: RawByteString): integer;
begin
  Result := -1;
  if (Socket.State = wsClosed) then
  begin
    if Assigned(FOnNetError) then FOnNetError(Socket, ERR_NET_CON_FAILED, NET_STR_CON_FAILED);
    Exit;
  end;

  try
    Result := Socket.SendStr(Pkt);
  except
    if Assigned(FOnNetError) then FOnNetError(Socket, ERR_NET_CON_FAILED, NET_STR_CON_FAILED);
  end;
end;

{***************************************************************}
procedure TObimpSocket.DoNetConnected(const SkipSSL: Boolean);
begin
  if not SkipSSL and FSecureSSL then
  begin
    Socket.SslEnable := True;
    Socket.StartSslHandshake;
  end
  else
    if Assigned(FOnNetConnected) then FOnNetConnected(Socket);
end;

{***************************************************************}
procedure TObimpSocket.DoDataAvailable(var Buf: RawByteString);
begin
  if Assigned(FOnNetDataAvailable) then FOnNetDataAvailable(Socket, Buf);
end;

{***************************************************************}
function TObimpSocket.StartTLS: Boolean;
begin
  Result := False;
  if (Socket.State <> wsConnected) then Exit;
  Result := True;

  FWaitingTLS := True;

  Socket.SslEnable := True;
  Socket.StartSslHandshake;
end;

{***************************************************************}
procedure TObimpSocket.DoNetInitedTLS;
begin
  if Assigned(FOnNetInitedTLS) then FOnNetInitedTLS(Socket);
end;




initialization
  GLIBEAY_DLL_Name := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + SSL_SUB_DIR + 'libbim32.dll';
  GSSLEAY_DLL_Name := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + SSL_SUB_DIR + 'sslbim32.dll';

end.

