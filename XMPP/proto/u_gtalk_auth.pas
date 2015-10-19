// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_gtalk_auth;

interface

uses Windows, SysUtils, Classes, OverbyteIcsWSocket, OverbyteIcsHttpProt, OverbyteIcsUrl, u_ext_info;

const
  GOOGLE_URL_MAIL = 'https://mail.google.com/mail';

type
  TOnAuthDoneEvent = procedure (Sender: TObject; Username, Token: string; MailUrl: Boolean; MailNotif: TExtMailNotif; ReqID: DWord; OwnUrlReq: Boolean) of object;

  TAuthThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Username  : string;
    Password  : string;
    AuthToken : string;
    MailUrl   : Boolean;
    UniqID    : string;
    ProxyInfo : TExtProxyInfo;
    MailNotif : TExtMailNotif;
    ReqID     : DWord;
    OwnUrlReq : Boolean;
  end;

  TGoogleClient = class(TSslHttpCli)
  public
    SslCtx  : TSslContext;
    DataIn  : TMemoryStream;
    DataOut : TMemoryStream;
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
  end;

  TGoogleAuth = class
  private
    FListThreads     : TStringList;
    FUniqSeq         : DWord;
    FProxyInfo       : TExtProxyInfo;
    FOnAuthDoneEvent : TOnAuthDoneEvent;
    procedure ThreadTerminateEvent(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure KillThreads;

    //sync/blocking functions
    function  IssueAuthToken(const Username, Password: string): string;
    function  IssueAuthTokenMailUrl(const Username, Password: string): string;

    //async/non-blocking function
    procedure IssueAuthTokenThread(const Username, Password: string; const ReturnMailUrl: Boolean; const MailNotif: pExtMailNotif = nil; const ReqID: DWord = 0);

    property  ProxyInfo         : TExtProxyInfo    read FProxyInfo       write FProxyInfo;
    property  OnThreadTokenDone : TOnAuthDoneEvent read FOnAuthDoneEvent write FOnAuthDoneEvent;
  end;

implementation

const
  GOOGLE_URL_CLIENT_AUTH    = 'https://www.google.com/accounts/ClientAuth';
  GOOGLE_URL_ISSUE_TOKEN    = 'https://www.google.com/accounts/IssueAuthToken';
  GOOGLE_URL_TOKEN_AUTH     = 'https://www.google.com/accounts/TokenAuth';

  GOOGLE_DOMAIN_GMAIL       = 'gmail.com';
  GOOGLE_DOMAIN_GOOGLEMAIL  = 'googlemail.com';
  GOOGLE_ACC_TYPE_NATIVE    = 'GOOGLE';
  GOOGLE_ACC_TYPE_HOSTED    = 'HOSTED';
  GOOGLE_TRUE               = 'true';

{***************************************************************}
function GetAccountType(const Username: string): string;
var s: string;
begin
  Result := GOOGLE_ACC_TYPE_HOSTED;

  s := LowerCase(Username);
  if (Pos('@' + GOOGLE_DOMAIN_GMAIL, s) > 0) or (Pos('@' + GOOGLE_DOMAIN_GOOGLEMAIL, s) > 0) then
    Result := GOOGLE_ACC_TYPE_NATIVE;
end;

{***************************************************************}
procedure SetClientProxyInfo(Client: TGoogleClient; const ProxyInfo: TExtProxyInfo);
begin
  if (ProxyInfo.ProxyType <> EXT_PROXY_TYPE_NONE) then
  begin
    Client.Proxy         := ProxyInfo.ProxyHost;
    Client.ProxyPort     := IntToStr(ProxyInfo.ProxyPort);
    Client.ProxyUsername := ProxyInfo.ProxyUser;
    Client.ProxyPassword := ProxyInfo.ProxyPass;

    if (ProxyInfo.ProxyType in [EXT_PROXY_TYPE_SOCKS4, EXT_PROXY_TYPE_SOCKS5, EXT_PROXY_TYPE_SOCKS4A]) then
    begin
      Client.SocksServer := ProxyInfo.ProxyHost;
      Client.SocksPort   := IntToStr(ProxyInfo.ProxyPort);

      if (ProxyInfo.ProxyUser <> '') then
      begin
        Client.SocksAuthentication := socksAuthenticateUsercode;
        Client.SocksUsercode := ProxyInfo.ProxyUser;
        Client.SocksPassword := ProxyInfo.ProxyPass;
      end
      else
        Client.SocksAuthentication := socksNoAuthentication;

      case ProxyInfo.ProxyType of
        EXT_PROXY_TYPE_SOCKS4  : Client.SocksLevel  := '4';
        EXT_PROXY_TYPE_SOCKS5  : Client.SocksLevel  := '5';
        EXT_PROXY_TYPE_SOCKS4A : Client.SocksLevel  := '4A';
      end;//case
    end;
  end
  else
  begin
    Client.Proxy         := '';
    Client.ProxyPort     := '';
    Client.ProxyUsername := '';
    Client.ProxyPassword := '';

    Client.SocksServer   := '';
    Client.SocksPort     := '';
    Client.SocksAuthentication := socksNoAuthentication;
  end;
end;

{***************************************************************}
function RunClientAuth(const Username, Password: string; var SID, LSID: string; const ProxyInfo: TExtProxyInfo): Boolean;
var Client: TGoogleClient;
    Buf: RawByteString;
    i: Integer;
    s: string;
begin
  Result := False;

  SID  := '';
  LSID := '';

  Client := TGoogleClient.Create(nil);
  try
    SetClientProxyInfo(Client, ProxyInfo);

    Client.URL := GOOGLE_URL_CLIENT_AUTH;

    Buf := RawByteString(
           'accountType=' + UrlEncode(GetAccountType(Username)) +
           '&Email=' + UrlEncode(Username) +
           '&Passwd=' + UrlEncode(Password) +
           '&skipvpage=' + UrlEncode(GOOGLE_TRUE) +
           '&PersistentCookie=' + UrlEncode(GOOGLE_TRUE)
           );

    SetLength(Buf, Length(Buf));

    Client.DataOut.Write(Buf[1], Length(Buf));
    Client.DataOut.Seek(0, soFromBeginning);

    try
      Client.Post;
    except
      Exit;
    end;

    if (Client.DataIn.Size > 0) then
    begin
      SetLength(Buf, Client.DataIn.Size);

      Client.DataIn.Seek(0, 0);
      Client.DataIn.Read(Buf[1], Length(Buf));

      s := Trim( String(Buf) );

      i := Pos('LSID=', s);
      if (i = 0) then Exit;

      LSID := Copy(s, i+5, Length(s));
      Delete(s, i, Length(s));

      i := Pos('SID=', LSID);
      if (i > 0) then
      begin
        SID := Copy(LSID, i+4, Length(LSID));
        Delete(LSID, i, Length(LSID));
      end
      else
      begin
        i := Pos('SID=', s);
        if (i = 0) then Exit;
        SID := Trim(Copy(s, i+4, Length(s)));
      end;

      Result := True;
    end;

  finally
    Client.Free;
  end;
end;

{***************************************************************}
function RunIssueAuthToken(const SID, LSID: string; const ForXmppLogin: Boolean; var Token: string; const ProxyInfo: TExtProxyInfo): Boolean;
var Client: TGoogleClient;
    Buf: RawByteString;
    svc: string;
begin
  Result := False;
  Token  := '';

  if ForXmppLogin then
    svc := 'mail'
  else
    svc := 'gaia';

  Client := TGoogleClient.Create(nil);
  try
    SetClientProxyInfo(Client, ProxyInfo);

    Client.URL := GOOGLE_URL_ISSUE_TOKEN;

    Buf := RawByteString(
           'SID=' + SID +
           '&LSID=' + LSID +
           '&service=' + UrlEncode(svc) +
           '&Session=' + UrlEncode(GOOGLE_TRUE) +
           '&skipvpage=' + UrlEncode(GOOGLE_TRUE)
           );

    SetLength(Buf, Length(Buf));

    Client.DataOut.Write(Buf[1], Length(Buf));
    Client.DataOut.Seek(0, soFromBeginning);

    try
      Client.Post;
    except
      Exit;
    end;

    if (Client.DataIn.Size > 0) then
    begin
      SetLength(Buf, Client.DataIn.Size);

      Client.DataIn.Seek(0, 0);
      Client.DataIn.Read(Buf[1], Length(Buf));

      Token := Trim( String(Buf) );

      if (Token = '') then Exit;

      Result := True;
    end;

  finally
    Client.Free;
  end;
end;

{ TAuthThread }
{***************************************************************}
procedure TAuthThread.Execute;
var SID, LSID: string;
begin
  AuthToken := '';
  try
    if RunClientAuth(Username, Password, SID, LSID, ProxyInfo) then
    begin
      if Terminated or not RunIssueAuthToken(SID, LSID, not MailUrl, AuthToken, ProxyInfo) then
        AuthToken := '';
    end;
  except
  end;
end;

{ TGoogleClient }
{***************************************************************}
constructor TGoogleClient.Create(Aowner: TComponent);
begin
  inherited;

  DataIn  := TMemoryStream.Create;
  DataOut := TMemoryStream.Create;

  SendStream := DataOut;
  RcvdStream := DataIn;


  SslCtx                      := TSslContext.Create(Self);
  SslCtx.SslVersionMethod     := sslV23_CLIENT;
  SslCtx.SslSessionCacheModes := [sslSESS_CACHE_CLIENT];
  SslCtx.SslVerifyPeer        := False;
  SslCtx.SslOptions           := [sslOpt_MICROSOFT_SESS_ID_BUG,
                                  sslOpt_NETSCAPE_CHALLENGE_BUG,
                                  sslOpt_NETSCAPE_REUSE_CIPHER_CHANGE_BUG,
                                  sslOpt_SSLREF2_REUSE_CERT_TYPE_BUG,
                                  sslOpt_MICROSOFT_BIG_SSLV3_BUFFER,
                                  sslOpt_MSIE_SSLV2_RSA_PADDING,
                                  sslOpt_SSLEAY_080_CLIENT_DH_BUG,
                                  sslOpt_TLS_D5_BUG,
                                  sslOpt_TLS_BLOCK_PADDING_BUG,
                                  sslOpt_TLS_ROLLBACK_BUG,
                                  sslOpt_NO_SSLv2,
                                  sslOpt_NETSCAPE_CA_DN_BUG,
                                  sslOpt_NETSCAPE_DEMO_CIPHER_CHANGE_BUG];

  SslContext := SslCtx;

  Accept          := '';
  Agent           := 'Google Talk';
  Connection      := 'Keep-Alive';
  ContentTypePost := 'application/x-www-form-urlencoded';
  RequestVer      := '1.1';
end;

{***************************************************************}
destructor TGoogleClient.Destroy;
begin
  DataOut.Free;
  DataIn.Free;

  inherited;
end;

{ TGoogleAuth }
{***************************************************************}
constructor TGoogleAuth.Create;
begin
  FListThreads := TStringList.Create;
end;

{***************************************************************}
destructor TGoogleAuth.Destroy;
begin
  KillThreads;
  FListThreads.Free;
  inherited;
end;

{***************************************************************}
procedure TGoogleAuth.KillThreads;
var AuthThread: TAuthThread;
begin
  while (FListThreads.Count > 0) do
  begin
    AuthThread := TAuthThread(FListThreads.Objects[0]);

    //delete thread form list
    FListThreads.Delete(0);

    if not AuthThread.Finished then
    begin
      AuthThread.OnTerminate := nil;

      //do not allow free after thread finish
      AuthThread.FreeOnTerminate := False;

      //it will terminate the thread
      AuthThread.Free;
    end;
  end;//while
end;

{***************************************************************}
function TGoogleAuth.IssueAuthToken(const Username, Password: string): string;
var SID, LSID: string;
begin
  Result := '';
  if not RunClientAuth(Username, Password, SID, LSID, FProxyInfo) then Exit;

  if not RunIssueAuthToken(SID, LSID, True, Result, FProxyInfo) then
    Result := '';
end;

{***************************************************************}
function TGoogleAuth.IssueAuthTokenMailUrl(const Username, Password: string): string;
var SID, LSID, Token: string;
begin
  Result := '';

  if not RunClientAuth(Username, Password, SID, LSID, FProxyInfo) then
    Exit;

  if RunIssueAuthToken(SID, LSID, False, Token, FProxyInfo) then
  begin
    Result := GOOGLE_URL_TOKEN_AUTH +
              '?auth=' + Token +
              '&service=mail' +
              '&continue=' + UrlEncode(GOOGLE_URL_MAIL) +
              '&source=googletalk';
  end;
end;

{***************************************************************}
procedure TGoogleAuth.IssueAuthTokenThread(const Username, Password: string; const ReturnMailUrl: Boolean; const MailNotif: pExtMailNotif = nil; const ReqID: DWord = 0);
var AuthThread: TAuthThread;
begin
  AuthThread := TAuthThread.Create(True);

  AuthThread.FreeOnTerminate := True;
  AuthThread.OnTerminate     := ThreadTerminateEvent;

  Inc(FUniqSeq);

  AuthThread.Username   := Username;
  AuthThread.Password   := Password;
  AuthThread.MailUrl    := ReturnMailUrl;
  AuthThread.UniqID     := IntToStr(FUniqSeq);
  AuthThread.ProxyInfo  := FProxyInfo;

  if (MailNotif <> nil) then
    AuthThread.MailNotif  := MailNotif^;

  AuthThread.ReqID      := ReqID;
  AuthThread.OwnUrlReq  := MailNotif = nil;

  FListThreads.AddObject(AuthThread.UniqID, AuthThread);
  AuthThread.Start;
end;

{***************************************************************}
procedure TGoogleAuth.ThreadTerminateEvent(Sender: TObject);
var AuthThread: TAuthThread;
    i: Integer;
begin
  AuthThread := TAuthThread(Sender);

  //threads control
  i := FListThreads.IndexOf(AuthThread.UniqID);
  if (i > -1) then
    FListThreads.Delete(i);

  //set correct url
  if AuthThread.OwnUrlReq and (AuthThread.MailNotif.ClickUrl = '') then
    AuthThread.MailNotif.ClickUrl := GOOGLE_URL_MAIL;

  if (AuthThread.MailNotif.ClickUrl <> '') then
  begin
    //make https
    if (Pos('http://', LowerCase(AuthThread.MailNotif.ClickUrl)) = 1) then
      Insert('s', AuthThread.MailNotif.ClickUrl, 5);
  end;


  if AuthThread.MailUrl and (AuthThread.AuthToken <> '') then
    AuthThread.AuthToken := GOOGLE_URL_TOKEN_AUTH +
                            '?auth=' + AuthThread.AuthToken +
                            '&service=mail' +
                            '&continue=' + UrlEncode(AuthThread.MailNotif.ClickUrl) +
                            '&source=googletalk';

  if Assigned(FOnAuthDoneEvent) then
    FOnAuthDoneEvent(Self, AuthThread.Username, AuthThread.AuthToken, AuthThread.MailUrl, AuthThread.MailNotif, AuthThread.ReqID, AuthThread.OwnUrlReq);
end;



end.
