// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_jabber_hlp;

interface

uses Windows, Classes, SysUtils, h_jabber;

type
  //Msg cookies helper
  THelpCookie = record
    Jid         : string;
    Resource    : string;
    XmppCookie  : string;
    ObimpCookie : DWord;
  end;
  pHelpCookie = ^THelpCookie;

  TCookieMan = class
  private
    FListCookies : TList;
    FObimpSeq    : DWord;
    FXmppSeq     : DWord;
    FSeqUID      : string;
    function  ObimpCookieSeq: DWord;
    function  XmppCookieSeq: string;
    procedure ClearListCookies;
    procedure ClearOldCookie;
  public
    constructor Create;
    destructor  Destroy; override;
    function  GetCookieByXmpp(const Jid: string; XmppCookie: string): pHelpCookie;
    function  GetCookieByObimp(const Jid: string; ObimpCookie: DWord): pHelpCookie;
    function  AddXmppCookie(const Jid, Resource: string; XmppCookie: string): DWord;
    function  AddObimpCookie(const Jid: string; ObimpCookie: DWord): string;
    procedure DelCookieByXmpp(const Jid: string; XmppCookie: string);
    procedure DelCookieByObimp(const Jid: string; ObimpCookie: DWord);
  end;


implementation

{ TCookieMan }
{***************************************************************}
constructor TCookieMan.Create;
begin
  FListCookies := TList.Create;

  FObimpSeq := DWord(Random(MaxInt)) + DWord(-1);
  FXmppSeq  := Random(64);
  FSeqUID   := GetRndHexBytes(2);
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
function TCookieMan.XmppCookieSeq: string;
begin
  FXmppSeq := FXmppSeq + 1;

  Result := SEQ_ID_ACK + FSeqUID + SEQ_DIV + IntToStr(FXmppSeq);
end;

{***************************************************************}
function TCookieMan.GetCookieByObimp(const Jid: string; ObimpCookie: DWord): pHelpCookie;
var aCookie: pHelpCookie;
    i: integer;
begin
  Result := nil;

  for i := 0 to FListCookies.Count-1 do
  begin
    aCookie := FListCookies.Items[i];

    if (aCookie^.ObimpCookie = ObimpCookie) and WideSameText(aCookie^.Jid, Jid) then
    begin
      Result := aCookie;
      Break;
    end;
  end;//for
end;

{***************************************************************}
function TCookieMan.GetCookieByXmpp(const Jid: string; XmppCookie: string): pHelpCookie;
var aCookie: pHelpCookie;
    i: integer;
begin
  Result := nil;

  for i := 0 to FListCookies.Count-1 do
  begin
    aCookie := FListCookies.Items[i];

    if (aCookie^.XmppCookie = XmppCookie) and WideSameText(aCookie^.Jid, Jid) then
    begin
      Result := aCookie;
      Break;
    end;
  end;//for
end;

{***************************************************************}
function TCookieMan.AddObimpCookie(const Jid: string; ObimpCookie: DWord): string;
var aCookie: pHelpCookie;
begin
  New(aCookie);
  ZeroMemory(aCookie, SizeOf(THelpCookie));

  aCookie^.Jid         := Jid;
  aCookie^.Resource    := '';
  aCookie^.XmppCookie  := XmppCookieSeq;
  aCookie^.ObimpCookie := ObimpCookie;
  FListCookies.Add(aCookie);

  Result := aCookie^.XmppCookie;

  ClearOldCookie;
end;

{***************************************************************}
function TCookieMan.AddXmppCookie(const Jid, Resource: string; XmppCookie: string): DWord;
var aCookie: pHelpCookie;
begin
  New(aCookie);
  ZeroMemory(aCookie, SizeOf(THelpCookie));

  aCookie^.Jid         := Jid;
  aCookie^.Resource    := Resource;
  aCookie^.XmppCookie  := XmppCookie;
  aCookie^.ObimpCookie := ObimpCookieSeq;

  FListCookies.Add(aCookie);

  Result := aCookie^.ObimpCookie;

  ClearOldCookie;
end;

{***************************************************************}
procedure TCookieMan.DelCookieByObimp(const Jid: string; ObimpCookie: DWord);
var aCookie: pHelpCookie;
    i: integer;
begin
  for i := FListCookies.Count-1 downto 0 do
  begin
    aCookie := FListCookies.Items[i];

    if (aCookie^.ObimpCookie = ObimpCookie) and WideSameText(aCookie^.Jid, Jid) then
    begin
      Dispose(aCookie);
      FListCookies.Delete(i);
      Break;
    end;
  end;//for
end;

{***************************************************************}
procedure TCookieMan.DelCookieByXmpp(const Jid: string; XmppCookie: string);
var aCookie: pHelpCookie;
    i: integer;
begin
  for i := FListCookies.Count-1 downto 0 do
  begin
    aCookie := FListCookies.Items[i];

    if (aCookie^.XmppCookie = XmppCookie) and WideSameText(aCookie^.Jid, Jid) then
    begin
      Dispose(aCookie);
      FListCookies.Delete(i);
      Break;
    end;
  end;//for
end;


end.
