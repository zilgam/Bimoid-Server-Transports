// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_oscar_hlp;

interface

uses Windows, Classes, SysUtils, h_oscar;

type
  TAvatarMS = class(TMemoryStream)
  public
    AvatarReqID: DWord;
  end;

  //Msg cookies helper
  THelpCookie = record
    Account     : string;
    OscarCookie : UInt64;
    ObimpCookie : DWord;
  end;
  pHelpCookie = ^THelpCookie;

  TCookieMan = class
  private
    FListCookies : TList;
    FObimpSeq    : DWord;
    FOscarSeq    : DWord;
    function  ObimpCookieSeq: DWord;
    function  OscarCookieSeq: UInt64;
    procedure ClearListCookies;
    procedure ClearOldCookie;
  public
    constructor Create;
    destructor  Destroy; override;
    function  GetCookieByOscar(const Account: string; OscarCookie: UInt64): pHelpCookie;
    function  GetCookieByObimp(const Account: string; ObimpCookie: DWord): pHelpCookie;
    function  AddOscarCookie(const Account: string; OscarCookie: UInt64): DWord;
    function  AddObimpCookie(const Account: string; ObimpCookie: DWord): UInt64;
    procedure DelCookieByOscar(const Account: string; OscarCookie: UInt64);
    procedure DelCookieByObimp(const Account: string; ObimpCookie: DWord);
  end;


  //MDIR requests helper
  TImdReq = record
    OscarReqID : DWord;
    ObimpReqID : DWord;
    ReqType    : Byte;
  end;
  pImdReq = ^TImdReq;


  TImdMan = class
  private
    FListReq : TList;
    procedure ClearListReq;
    procedure ClearOldRequest;
  public
    OwnImdRequested : Boolean;
    OwnImdRequestID : DWord;
    constructor Create;
    destructor  Destroy; override;
    function  NewOscarReqID(MdirCmd: Word): DWord;
    function  AddRequest(ReqType: Byte; OscarReqID, ObimpReqID: DWord): pImdReq;
    function  GetRequest(OscarReqID: DWord): pImdReq;
    procedure DelRequest(OscarReqID: DWord);
  end;


implementation

{ TCookieMan }
{***************************************************************}
constructor TCookieMan.Create;
begin
  FListCookies := TList.Create;

  FObimpSeq := GetOscarRndDWord;
  FOscarSeq := GetOscarRndDWord;
end;

{***************************************************************}
destructor TCookieMan.Destroy;
begin
  ClearListCookies;
  FListCookies.Free;
  inherited;
end;

{***************************************************************}
procedure TCookieMan.ClearListCookies;
var aCookie: pHelpCookie;
    i: integer;
begin
  for i := FListCookies.Count-1 downto 0 do
  begin
    aCookie := FListCookies.Items[i];
    Dispose(aCookie);
    FListCookies.Delete(i);
  end;//for
end;

{***************************************************************}
procedure TCookieMan.ClearOldCookie;
var aCookie: pHelpCookie;
begin
  if (FListCookies.Count <= 16) then Exit;

  //delete first one
  aCookie := FListCookies.Items[0];
  Dispose(aCookie);
  FListCookies.Delete(0);
end;

{***************************************************************}
function TCookieMan.ObimpCookieSeq: DWord;
begin
  FObimpSeq := FObimpSeq + 1;

  //cookie seq cannot be 0 or 1
  if (FObimpSeq = 0) or (FObimpSeq = 1) then
    FObimpSeq := 2;

  Result := FObimpSeq;
end;

{***************************************************************}
function TCookieMan.OscarCookieSeq: UInt64;
begin
  FOscarSeq := FOscarSeq + 1;

  //cookie seq cannot be 0 or 1
  if (FOscarSeq = 0) or (FOscarSeq = 1) then
    FOscarSeq := 2;

  Result := (UInt64(GetOscarRndDWord) shl 32) or FOscarSeq;
end;

{***************************************************************}
function TCookieMan.GetCookieByObimp(const Account: string; ObimpCookie: DWord): pHelpCookie;
var aCookie: pHelpCookie;
    i: integer;
begin
  Result := nil;

  for i := 0 to FListCookies.Count-1 do
  begin
    aCookie := FListCookies.Items[i];

    if (aCookie^.ObimpCookie = ObimpCookie) and WideSameText(aCookie^.Account, Account) then
    begin
      Result := aCookie;
      Break;
    end;
  end;//for
end;

{***************************************************************}
function TCookieMan.GetCookieByOscar(const Account: string; OscarCookie: UInt64): pHelpCookie;
var aCookie: pHelpCookie;
    i: integer;
begin
  Result := nil;

  for i := 0 to FListCookies.Count-1 do
  begin
    aCookie := FListCookies.Items[i];

    if (aCookie^.OscarCookie = OscarCookie) and WideSameText(aCookie^.Account, Account) then
    begin
      Result := aCookie;
      Break;
    end;
  end;//for
end;

{***************************************************************}
function TCookieMan.AddOscarCookie(const Account: string; OscarCookie: UInt64): DWord;
var aCookie: pHelpCookie;
begin
  New(aCookie);
  aCookie^.Account     := Account;
  aCookie^.OscarCookie := OscarCookie;
  aCookie^.ObimpCookie := ObimpCookieSeq;
  FListCookies.Add(aCookie);

  Result := aCookie^.ObimpCookie;

  ClearOldCookie;
end;

{***************************************************************}
function TCookieMan.AddObimpCookie(const Account: string; ObimpCookie: DWord): UInt64;
var aCookie: pHelpCookie;
begin
  New(aCookie);
  aCookie^.Account     := Account;
  aCookie^.OscarCookie := OscarCookieSeq;
  aCookie^.ObimpCookie := ObimpCookie;
  FListCookies.Add(aCookie);

  Result := aCookie^.OscarCookie;

  ClearOldCookie;
end;

{***************************************************************}
procedure TCookieMan.DelCookieByOscar(const Account: string; OscarCookie: UInt64);
var aCookie: pHelpCookie;
    i: integer;
begin
  for i := FListCookies.Count-1 downto 0 do
  begin
    aCookie := FListCookies.Items[i];

    if (aCookie^.OscarCookie = OscarCookie) and WideSameText(aCookie^.Account, Account) then
    begin
      Dispose(aCookie);
      FListCookies.Delete(i);
      Break;
    end;
  end;//for
end;

{***************************************************************}
procedure TCookieMan.DelCookieByObimp(const Account: string; ObimpCookie: DWord);
var aCookie: pHelpCookie;
    i: integer;
begin
  for i := FListCookies.Count-1 downto 0 do
  begin
    aCookie := FListCookies.Items[i];

    if (aCookie^.ObimpCookie = ObimpCookie) and WideSameText(aCookie^.Account, Account) then
    begin
      Dispose(aCookie);
      FListCookies.Delete(i);
      Break;
    end;
  end;//for
end;

{ TImdMan }
{***************************************************************}
constructor TImdMan.Create;
begin
  FListReq := TList.Create;
end;

{***************************************************************}
destructor TImdMan.Destroy;
begin
  ClearListReq;
  FListReq.Free;
  inherited;
end;

{***************************************************************}
procedure TImdMan.ClearListReq;
var aReq: pImdReq;
    i: integer;
begin
  for i := FListReq.Count-1 downto 0 do
  begin
    aReq := FListReq.Items[i];
    Dispose(aReq);
    FListReq.Delete(i);
  end;//for
end;

{***************************************************************}
function TImdMan.NewOscarReqID(MdirCmd: Word): DWord;
begin
  Result := MdirCmd or (GetOscarRndWord shl 16);

  //get only unique OscarReqID
  while (GetRequest(Result) <> nil) do
    Result := MdirCmd or (GetOscarRndWord shl 16);
end;

{***************************************************************}
function TImdMan.AddRequest(ReqType: Byte; OscarReqID, ObimpReqID: DWord): pImdReq;
var aReq: pImdReq;
begin
  Result := GetRequest(OscarReqID);
  if (Result <> nil) then Exit;

  //del old request if available
  ClearOldRequest;

  New(aReq);
  ZeroMemory(aReq, SizeOf(TImdReq));

  aReq^.OscarReqID := OscarReqID;
  aReq^.ObimpReqID := ObimpReqID;
  aReq^.ReqType    := ReqType;

  FListReq.Add(aReq);

  Result := aReq;
end;

{***************************************************************}
function TImdMan.GetRequest(OscarReqID: DWord): pImdReq;
var aReq: pImdReq;
    i: integer;
begin
  Result := nil;

  for i := 0 to FListReq.Count-1 do
  begin
    aReq := FListReq.Items[i];

    if (aReq^.OscarReqID = OscarReqID) then
    begin
      Result := aReq;
      Break;
    end;
  end;//for
end;

{***************************************************************}
procedure TImdMan.DelRequest(OscarReqID: DWord);
var aReq: pImdReq;
    i: integer;
begin
  for i := FListReq.Count-1 downto 0 do
  begin
    aReq := FListReq.Items[i];

    if (aReq^.OscarReqID = OscarReqID) then
    begin
      Dispose(aReq);
      FListReq.Delete(i);
      Break;
    end;
  end;//for
end;

{***************************************************************}
procedure TImdMan.ClearOldRequest;
var aReq: pImdReq;
begin
  if (FListReq.Count <= 16) then Exit;

  aReq := FListReq.Items[0];
  Dispose(aReq);
  FListReq.Delete(0);
end;


end.
