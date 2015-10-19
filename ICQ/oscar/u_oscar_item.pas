// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_oscar_item;

interface

uses Windows, Classes, SysUtils, h_oscar;

type
  TAttRaw = record
    ID    : Word;
    Value : RawByteString;
  end;
  pAttRaw = ^TAttRaw;

  TAttArray = array of TAttRaw;

  TOscarItem = record
  strict private
    function GetAtt(const AttID: Word): pAttRaw;
    function GetAttIndex(const AttID: Word): Integer;
  public
    Name        : string;
    GroupID     : Word;
    ItemID      : Word;
    ClassID     : Word;
    Atts        : TAttArray;
    //=== for contacts class
    BuddyInited : Boolean;
    NickInfo    : TNickInfo;
    //=== helper funcs ===
    function GenRawItem: RawByteString;
    function GetAttsCount: Integer;
    function GenRawAtts: RawByteString;
    function AddAtt(const AttID: Word; AttVal: RawByteString; const AsFirst: Boolean = False): Boolean;
    function UpdAtt(const AttID: Word; AttVal: RawByteString; AddIfNotExist: Boolean = False): Boolean;
    function DelAtt(const AttID: Word): Boolean;
    function AttExists(const AttID: Word): Boolean;
    function AttRawValue(const AttID: Word): RawByteString;
    function AttStrValue(const AttID: Word): String;
    function AttByteValue(const AttID: Word): Byte;
    function AttWordValue(const AttID: Word): Word;
    function AttDWordValue(const AttID: Word): DWord;
    function AttQWordValue(const AttID: Word): Int64;
  end;
  pOscarItem = ^TOscarItem;

  function  IsRealBuddy(oItem: pOscarItem): Boolean;
  function  IsPrivacyItem(oItem: pOscarItem): Boolean;

implementation

{*****************************************************************}
function IsRealBuddy(oItem: pOscarItem): Boolean;
begin
  Result := False;
  if (oItem = nil) then Exit;

  //Some buddies have attributes that make them actually non-buddy items
  //even if it's class FB_CLSID_BUDDY, so filering them out.
  //Important thing is we must take into account that they may exist in our list
  //and it must be checked before adding/updating items with such item names.

  Result := (oItem^.ClassID = FB_CLSID_BUDDY) and
            (oItem^.Name <> '') and
            (oItem^.GroupID > 0) and
            not oItem^.AttExists(FB_ATT_PHONE_BUDDY) and   //non buddy item, for phone saving purposes
            not oItem^.AttExists(FB_ATT_RECENT) and        //"not in list" recent buddy item
            not oItem^.AttExists(FB_ATT_NIL_BUDDY);        //"not in list" buddy item
end;

{*****************************************************************}
function IsPrivacyItem(oItem: pOscarItem): Boolean;
begin
  Result := False;
  if (oItem = nil) then Exit;

  Result := (oItem^.ClassID = FB_CLSID_PERMIT) or
            (oItem^.ClassID = FB_CLSID_DENY) or
            (oItem^.ClassID = FB_CLSID_IGNORE);
end;

{ TOscarItem }
{*****************************************************************}
function TOscarItem.GenRawItem: RawByteString;
var sRaw: RawByteString;
begin
  sRaw := GenRawAtts;

  Result := LnwValUTF8(Name) +
            tb(GroupID) +
            tb(ItemID) +
            tb(ClassID) +
            tb(Word(Length(sRaw))) +
            sRaw;
end;

{*****************************************************************}
function TOscarItem.GetAtt(const AttID: Word): pAttRaw;
var i: Integer;
begin
  Result := nil;
  if (Length(Atts) = 0) then Exit;

  for i := 0 to Length(Atts)-1 do
  begin
    if (Atts[i].ID = AttID) then
    begin
      Result := @Atts[i];
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarItem.GetAttIndex(const AttID: Word): Integer;
var i: Integer;
begin
  Result := -1;
  if (Length(Atts) = 0) then Exit;

  for i := 0 to Length(Atts)-1 do
  begin
    if (Atts[i].ID = AttID) then
    begin
      Result := i;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarItem.GetAttsCount: Integer;
begin
  Result := Length(Atts);
end;

{*****************************************************************}
function TOscarItem.GenRawAtts: RawByteString;
var i: Integer;
begin
  Result := '';
  if (Length(Atts) = 0) then Exit;

  for i := 0 to Length(Atts)-1 do
    Result := Result + TLV(Atts[i].ID, Atts[i].Value);
end;

{*****************************************************************}
function TOscarItem.AddAtt(const AttID: Word; AttVal: RawByteString; const AsFirst: Boolean = False): Boolean;
var Len, i: integer;
begin
  Result := True;

  Len := Length(Atts);
  i := Len;

  //increase array of attributes
  SetLength(Atts, Len+1);

  if AsFirst and (Len > 0) then
  begin
    Finalize(Atts[Len]);
    Move(Atts[0], Atts[1], Len*SizeOf(TAttRaw));
    Initialize(Atts[0]);
    i := 0;
  end;

  //set new attribute value
  with Atts[i] do
  begin
    ID    := AttID;
    Value := AttVal;
  end;//with
end;

{*****************************************************************}
function TOscarItem.DelAtt(const AttID: Word): Boolean;
var i, iTail: Integer;
begin
  Result := False;
  i := GetAttIndex(AttID);
  if (i < 0) or (i > (Length(Atts)-1)) then Exit;

  Finalize(Atts[i]);

  iTail := Length(Atts) - i;
  if (iTail > 0) then
    Move(Atts[i+1], Atts[i], SizeOf(TAttRaw) * iTail);

  Initialize(Atts[Length(Atts)-1]);
  SetLength(Atts, Length(Atts)-1);

  Result := True;
end;

{*****************************************************************}
function TOscarItem.UpdAtt(const AttID: Word; AttVal: RawByteString; AddIfNotExist: Boolean = False): Boolean;
var AttRaw: pAttRaw;
begin
  Result := False;
  AttRaw := GetAtt(AttID);
  if (AttRaw = nil) then
  begin
    if AddIfNotExist then
    begin
      if not AddAtt(AttID, '') then Exit;
      AttRaw := GetAtt(AttID);
    end
    else
      Exit;
  end;

  AttRaw^.Value := AttVal;
end;

{*****************************************************************}
function TOscarItem.AttExists(const AttID: Word): Boolean;
begin
  Result := GetAtt(AttID) <> nil;
end;

{*****************************************************************}
function TOscarItem.AttRawValue(const AttID: Word): RawByteString;
var AttRaw: pAttRaw;
begin
  Result := '';
  AttRaw := GetAtt(AttID);
  if (AttRaw = nil) then Exit;
  Result := AttRaw^.Value;
end;

{*****************************************************************}
function TOscarItem.AttStrValue(const AttID: Word): String;
var AttRaw: pAttRaw;
begin
  Result := '';
  AttRaw := GetAtt(AttID);
  if (AttRaw = nil) then Exit;
  Result := ReadStrUTF8(AttRaw^.Value, Length(AttRaw^.Value));
end;

{*****************************************************************}
function TOscarItem.AttByteValue(const AttID: Word): Byte;
var AttRaw: pAttRaw;
begin
  Result := 0;
  AttRaw := GetAtt(AttID);
  if (AttRaw = nil) then Exit;
  Result := ReadByte(AttRaw^.Value);
end;

{*****************************************************************}
function TOscarItem.AttWordValue(const AttID: Word): Word;
var AttRaw: pAttRaw;
begin
  Result := 0;
  AttRaw := GetAtt(AttID);
  if (AttRaw = nil) then Exit;
  Result := ReadWord(AttRaw^.Value);
end;

{*****************************************************************}
function TOscarItem.AttDWordValue(const AttID: Word): DWord;
var AttRaw: pAttRaw;
begin
  Result := 0;
  AttRaw := GetAtt(AttID);
  if (AttRaw = nil) then Exit;
  Result := ReadDWord(AttRaw^.Value);
end;

{*****************************************************************}
function TOscarItem.AttQWordValue(const AttID: Word): Int64;
var AttRaw: pAttRaw;
begin
  Result := 0;
  AttRaw := GetAtt(AttID);
  if (AttRaw = nil) then Exit;
  Result := ReadQWord(AttRaw^.Value);
end;

end.
