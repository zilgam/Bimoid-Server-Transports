// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_auth;

interface

uses Windows, Classes, SysUtils, OverbyteIcsMD5, OverbyteIcsMimeUtils, OverbyteIcsSha1, Math, h_jabber;

type
  TAuthMechanism  = (amPlain, amScramSha1, amDigestMD5, amCramMD5, amXGoogleToken);
  TAuthMechanisms = set of TAuthMechanism;

  TXmppAuth = class
  private
    FScramClientNonce : string;
    FScramServerSign  : string;
  public
    Mechanisms  : TAuthMechanisms; //supported mechanisms
    MechsRemain : TAuthMechanisms; //mechanisms remain on login failed by another mechanism
    CurAuthMech : TAuthMechanism;
    DoneMD5     : Boolean;         //because challenge will be rcvd 2 times
    constructor Create;
    destructor  Destroy; override;
    procedure ResetData;
    function GenDigestMD5Response(challenge, Account, Password, Domain: string): string;
    function GenCramMD5Response(challenge, Account, Password: RawByteString): string;
    function GenScramSha1ClientMsg(Account: string; Base64Encoded: Boolean): string;
    function GenScramSha1Response(challenge, Account, Password: string): string;
    function ValidateScramSha1Signature(challenge: string): Boolean;
  end;


implementation

{ TXmppAuth }
{*******************************************************************}
constructor TXmppAuth.Create;
begin

end;

{*******************************************************************}
destructor TXmppAuth.Destroy;
begin

  inherited;
end;

{*******************************************************************}
procedure TXmppAuth.ResetData;
begin
  Mechanisms  := [];
  MechsRemain := [];
  CurAuthMech := amDigestMD5;
  DoneMD5     := False;

  FScramClientNonce := Base64Encode(GetRndHexBytes(8));
  FScramServerSign  := '';
end;

{*******************************************************************}
function TXmppAuth.GenDigestMD5Response(challenge, Account, Password, Domain: string): string;
var s, sNonce, sQop, sCnonce: string;
    aRes, a1, a2: RawByteString;
    i: integer;
begin
  Result := XTAG_RESPONSE_EMPTY;
  if DoneMD5 or (challenge = '') then Exit;

  //get nonce and qop
  s    := challenge;
  i    := Pos('nonce="', s);
  if (i = 0) then Exit;

  Delete(s, 1, i+6);
  sNonce := Copy(s, 1, Pos('"', s)-1);

  s    := challenge;
  i    := Pos('qop="', s);
  sQop := '';
  if (i > 0) then
  begin
    Delete(s, 1, i+4);
    sQop := Copy(s, 1, Pos('"', s)-1);
  end;

  DoneMD5 := True;

  //gen response, RFC 2617, lets start that crazy raving
  sCnonce := GetRndHexBytes(16);

  //if PassIsHash then
  //  a1 := cw(Password) + UTF8Encode(':' + sNonce + ':' + sCnonce) //if we have only hash
  //else
  a1 := MD5Raw(UTF8Encode(Account + ':' + Domain + ':' + Password)) +
               UTF8Encode(':' + sNonce + ':' + sCnonce);// + ':' + Account + '@' + Server);

  a2 := UTF8Encode('AUTHENTICATE:xmpp/' + Domain);

  aRes := Base64Encode( UTF8Encode(
                        'username="' + Account +'",' +
                        'realm="' + Domain + '",' +
                        'nonce="' + sNonce + '",' +
                        'cnonce="' + sCnonce + '",' +
                        'nc=00000001,' +
                        'qop=' + sQop + ',' +
                        'digest-uri="xmpp/' + Domain + '",' +
                        'charset=utf-8,' +
                        'response=' + RawToHex( MD5Raw( RawByteString(RawToHex(MD5Raw(a1), True)) +
                                                UTF8Encode(':' + sNonce + ':00000001:' + sCnonce + ':' + sQop + ':') +
                                                RawByteString(RawToHex(MD5Raw(a2), True)) ), True )
                       ) );

  Result := XmlTag(XTAG_RESPONSE, String(aRes));
end;

{*******************************************************************}
function TXmppAuth.GenCramMD5Response(challenge, Account, Password: RawByteString): string;
var Digest: TMD5Digest;
begin
  HMAC_MD5(PAnsiChar(challenge)^, Length(challenge), PAnsiChar(Password)^, Length(Password), Digest);
  Result := XmlTag(XTAG_RESPONSE, String(Base64Encode(Account + ' ' + MD5DigestToLowerHexA(Digest))));
end;

{*******************************************************************}
function PBKDF2(pwd: Pointer; pwd_len: Word; salt: Pointer; salt_len: Word; key: PAnsiChar; key_len: Word; iterations: Integer): Integer;
var arrsalt: PByteArray;
    i, j, count, r: Cardinal;
    obuf, d1, d2: SHA1Digest;
begin
  Result := -1;
	if (iterations < 1) or (key_len = 0) or (salt_len = 0) then
		Exit;

  arrsalt := AllocMem(salt_len + 4);
  if (arrsalt = nil) then Exit;

  try
  Move(salt^, arrsalt^, salt_len);

  count := 0;
  while (key_len > 0) do
  begin
    Inc(count);
    arrsalt^[salt_len + 0] := (count shr 24) and $FF;
		arrsalt^[salt_len + 1] := (count shr 16) and $FF;
		arrsalt^[salt_len + 2] := (count shr 8) and $FF;
		arrsalt^[salt_len + 3] := count and $FF;

		HMAC_SHA1(arrsalt^, salt_len + 4, pwd^, pwd_len, d1);
		Move(d1, obuf, SizeOf(obuf));

		for i := 1 to iterations-1 do
    begin
			HMAC_SHA1(d1, SizeOf(d1), pwd^, pwd_len, d2);
			Move(d2, d1, SizeOf(d1));

      for j := 0 to SizeOf(obuf)-1 do
      	obuf[j] := AnsiChar( Ord(obuf[j]) xor Ord(d1[j]) ); //for
		end;//for

		r := Min(key_len, SHA1HashSize);
		Move(obuf, key^, r);
		Inc(key, r);
		Dec(key_len, r);
  end;//while

  Result := 0;

  finally
    FreeMem(arrsalt);
  end;
end;

{*******************************************************************}
function RFC2898DeriveKey(pwd, salt: RawByteString; iterations: Integer): RawByteString;
var sRes: RawByteString;
begin
  Result := '';
  SetLength(sRes, 20);
  if (PBKDF2(PAnsiChar(pwd), Length(pwd), PAnsiChar(salt), Length(salt), PAnsiChar(sRes), Length(sRes), iterations) = 0) then
    Result := sRes;
end;

{*******************************************************************}
function GetCommaValue(str: string; param: string): string;
var s: string;
    i: integer;
begin
  Result := '';

  if (str = '') then Exit;
  if (str[Length(str)] <> ',') then str := str + ',';

  i := Pos(',', str);
  while (i > 0) do
  begin
    s := Copy(str, 1, i-1);
    Delete(str, 1, i);

    if (Pos(param + '=', s) = 1) then
    begin
      Delete(s, 1, 2);
      Result := s;
      Break;
    end;

    i := Pos(',', str);
  end;//while
end;

{*******************************************************************}
function TXmppAuth.GenScramSha1ClientMsg(Account: string; Base64Encoded: Boolean): string;
begin
  Result := 'n,,n=' + Account + ',r=' + FScramClientNonce;
  if Base64Encoded then
    Result := Base64Encode(Result);
end;

{*******************************************************************}
function TXmppAuth.GenScramSha1Response(challenge, Account, Password: string): string;
var s, sDecoded, sNonce, salt,
    cFirstMsgBare, cFinalMsgWoProof,
    authMsg, cFinalMsg: string;
    iters, i: Integer;
    saltedPwd, clientKey, storedKey,
    cSignature, cProof: RawByteString;
begin
  Result := '';
  sDecoded := Base64Decode(challenge);

  //check client nonce and get server nonce(=client nonce + server nonce)
  s := GetCommaValue(sDecoded, 'r');
  if (Copy(s, 1, Length(FScramClientNonce)) <> FScramClientNonce) then Exit;
  sNonce := s;

  //get salt and iterations count
  salt  := GetCommaValue(sDecoded, 's');
  iters := StrToIntDef(GetCommaValue(sDecoded, 'i'), 4096);

  //gen auth msg
  s := GenScramSha1ClientMsg(Account, False);
  cFirstMsgBare    := Copy(s, 4, Length(s)-3);
  cFinalMsgWoProof := 'c=biws,r=' + sNonce;

  authMsg := cFirstMsgBare + ',' + sDecoded + ',' + cFinalMsgWoProof;

  //sign auth msg
  saltedPwd  := RFC2898DeriveKey(UTF8Encode(Password), RawByteString(Base64Decode(salt)), iters);
  if (saltedPwd = '') then Exit;
  clientKey  := HMAC_SHA1_EX(UTF8Encode('Client Key'), saltedPwd);
  storedKey  := SHA1ofStr(clientKey);
  cSignature := HMAC_SHA1_EX(RawByteString(authMsg), storedKey);

  //gen server signature to compare later
  FScramServerSign := string( Base64Encode( HMAC_SHA1_EX(RawByteString(authMsg), HMAC_SHA1_EX(UTF8Encode('Server Key'), saltedPwd)) ) );

  //gen client proof
  SetLength(cProof, Length(clientKey));
  for i := 1 to Length(cProof) do
   	cProof[i] := AnsiChar( Ord(clientKey[i]) xor Ord(cSignature[i]) ); //for

  cFinalMsg := cFinalMsgWoProof + ',p=' + String(Base64Encode(cProof));

  Result := XmlTag(XTAG_RESPONSE, Base64Encode(cFinalMsg));
end;

{*******************************************************************}
function TXmppAuth.ValidateScramSha1Signature(challenge: string): Boolean;
var s, sDecoded: string;
begin
  sDecoded := Base64Decode(challenge);
  s := GetCommaValue(sDecoded, 'v');

  Result := s = FScramServerSign;
end;



end.
