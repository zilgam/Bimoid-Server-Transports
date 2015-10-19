// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_roster_info;

interface

uses Windows, Classes, SysUtils, h_jabber;

const
  ROSTER_OPER_NON = 0;
  ROSTER_OPER_ADD = 1;
  ROSTER_OPER_UPD = 2;
  ROSTER_OPER_DEL = 3;

type
  TClientDiscoInfo = set of (cdi_Receipts, cdi_ChatStates, cdi_ClientVer);

  TClientVer = record
    Name : string;
    Ver  : string;
    OS   : string;
  end;

  TResInfo = record
    Resource   : string;
    StatusShow : string;
    StatusText : string;
    Priority   : ShortInt;
    DiscoInfo  : TClientDiscoInfo;
    ClientVer  : TClientVer;
  end;
  pResInfo = ^TResInfo;

  TResArray = array of TResInfo;
  TArrayGroups = array of string;

  TRosterItem = record
    JID      : string;
    Name     : string;
    Subscr   : string;
    Ask      : string;
    Groups   : TArrayGroups;

    {$IFDEF GTALK}
    GTalk_T  : string;
    {$ENDIF}

    //=== helpers ===
    ResArray      : TResArray;
    PrivType      : Byte;
    AvatarWaiting : Boolean;
    AvatarSha1Hex : string;
    AvatarMd5Hex  : string;
    PrevAsk       : string;
    LastResource  : string;
    function  GetGroupIndex(const GroupName: string): Integer;
    function  GetFirstGroupName: string;
    function  GetRes(const ResName: string): pResInfo;
    function  GetResIndex(const ResName: string): Integer;
    function  GetResCount: Integer;
    function  AddRes(const ResName, AStatusShow, AStatusText: string; const APriority: ShortInt): pResInfo;
    function  UpdRes(const ResName, AStatusShow, AStatusText: string; const APriority: ShortInt; const AddIfNotExist: Boolean = False): Boolean;
    function  DelRes(const ResName: string): Boolean;
    function  ResExists(const ResName: string): Boolean;
    function  GetHighestPriorityRes: pResInfo;
  end;
  pRosterItem = ^TRosterItem;

implementation

{ TRosterItem }
{*****************************************************************}
function TRosterItem.GetGroupIndex(const GroupName: string): Integer;
var i: Integer;
begin
  Result := -1;
  if (Length(Groups) = 0) then Exit;

  for i := 0 to Length(Groups)-1 do
  begin
    if (Groups[i] = GroupName) then //yes, case sensitive
    begin
      Result := i;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TRosterItem.GetFirstGroupName: string;
begin
  Result := '';
  if (Length(Groups) = 0) then Exit;
  Result := Groups[0];
end;

{*****************************************************************}
function TRosterItem.GetResIndex(const ResName: string): Integer;
var i: Integer;
begin
  Result := -1;
  if (Length(ResArray) = 0) then Exit;

  for i := 0 to Length(ResArray)-1 do
  begin
    if (ResArray[i].Resource = ResName) then
    begin
      Result := i;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TRosterItem.GetRes(const ResName: string): pResInfo;
var i: Integer;
begin
  Result := nil;
  if (Length(ResArray) = 0) then Exit;

  for i := 0 to Length(ResArray)-1 do
  begin
    if (ResArray[i].Resource = ResName) then
    begin
      Result := @ResArray[i];
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TRosterItem.GetHighestPriorityRes: pResInfo;
var i: Integer;
    iPrevPriority: ShortInt;
begin
  Result := nil;
  if (Length(ResArray) = 0) then Exit;

  iPrevPriority := -128;

  for i := 0 to Length(ResArray)-1 do
  begin
    if (ResArray[i].Priority > iPrevPriority) then
    begin
      iPrevPriority := ResArray[i].Priority;
      Result := @ResArray[i];
    end;
  end;//for
end;

{*****************************************************************}
function TRosterItem.GetResCount: Integer;
begin
  Result := Length(ResArray);
end;

{*****************************************************************}
function TRosterItem.ResExists(const ResName: string): Boolean;
begin
  Result := GetRes(ResName) <> nil;
end;

{*****************************************************************}
function TRosterItem.AddRes(const ResName, AStatusShow, AStatusText: string; const APriority: ShortInt): pResInfo;
var Len, i: integer;
begin
  Len := Length(ResArray);
  i := Len;

  //increase resources array
  SetLength(ResArray, Len+1);

  //set new resource info values
  with ResArray[i] do
  begin
    Resource    := ResName;
    Priority    := APriority;
    StatusShow  := AStatusShow;
    StatusText  := AStatusText;
  end;//with

  Result := @ResArray[i];
end;

{*****************************************************************}
function TRosterItem.DelRes(const ResName: string): Boolean;
var i, iTail: Integer;
begin
  Result := False;
  i := GetResIndex(ResName);
  if (i < 0) or (i > (Length(ResArray)-1)) then Exit;

  Finalize(ResArray[i]);

  iTail := Length(ResArray) - i;
  if (iTail > 0) then
    Move(ResArray[i+1], ResArray[i], SizeOf(TResInfo) * iTail);

  Initialize(ResArray[Length(ResArray)-1]);
  SetLength(ResArray, Length(ResArray)-1);

  Result := True;
end;

{*****************************************************************}
function TRosterItem.UpdRes(const ResName, AStatusShow, AStatusText: string; const APriority: ShortInt; const AddIfNotExist: Boolean = False): Boolean;
var ResInfo: pResInfo;
begin
  Result := False;

  ResInfo := GetRes(ResName);
  if (ResInfo = nil) then
  begin
    if AddIfNotExist then
    begin
      ResInfo := AddRes(ResName, AStatusShow, AStatusText, APriority);
      Result  := ResInfo <> nil;
    end;
    Exit;
  end;

  ResInfo^.StatusShow := AStatusShow;
  ResInfo^.StatusText := AStatusText;
  ResInfo^.Priority   := APriority;

  Result := True;
end;



end.
