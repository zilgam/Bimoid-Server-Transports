// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_oscar_fb;

interface

uses Windows, Classes, SysUtils, h_oscar, u_oscar_sock, u_oscar_item;

type
  TOscarPrivSet = set of (privPermit, privDeny, privIgnore);

  TOnItemOper   = procedure (Sender: TObject; CmdType: Word; oItem: pOscarItem) of object;
  TOnFailedOper = procedure (Sender: TObject; CmdType: Word; oItem: pOscarItem; Code: Word) of object;

  TOscarOper = record
    CmdType       : Word;
    ReqID         : DWord; //unique inside list
    ClusterStart  : Boolean;
    ClusterEnd    : Boolean;
    Item          : TOscarItem;
  end;
  pOscarOper = ^TOscarOper;

  TOscarFB = class
  private
    FListItems     : TList;
    FListOpers     : TList;
    FOperSeq       : Word;
    FOperBusy      : Boolean;
    FClusterOpen   : Boolean;
    FRawData       : RawByteString;
    FNewGroupName  : string;
    FOnItemOper    : TOnItemOper;
    FOnFailedOper  : TOnFailedOper;
    procedure ParseItem(oItem: pOscarItem; var Data: RawByteString);
    procedure ParseItemAtts(oItem: pOscarItem; Data: RawByteString);
    function  CreateNewItem(AddToList: Boolean): pOscarItem;
    procedure FreeItem(oItem: pOscarItem);
    procedure SetItemDefaults(oItem: pOscarItem);
    function  NewOperSeq: Word;
    function  GenNewFeedbagID: Word;
    function  GetOperReqID(OperType: Word): DWord;
    function  AddOper(OperType: Word; oItem: pOscarItem; ClusterStart, ClusterEnd: Boolean; const AsNext: Boolean = False): pOscarOper; overload;
    function  AddOper(OperType: Word; Name: string; GroupID: Word; ClassID: Word; ClusterStart, ClusterEnd: Boolean; const AsNext: Boolean = False): pOscarOper; overload;
    function  GetOper(ReqID: DWord): pOscarOper;
    procedure DelOper(ReqID: DWord);
    procedure CopyItem(SrcItem, DstItem: pOscarItem);
    procedure ProcessOperQueue;
    function  GetPrivacyItem(ItemName: string; PrivClassID: Word): pOscarItem;
    procedure DelItemDirect(GroupOrItemID: Word);
    function  ExcludeFromGroupOrder(GroupID, DelItemID: Word; ProcessQueue: Boolean): Boolean;
    function  GetGroupItem(GroupID: Word): pOscarItem;
    function  GetGroupItemByName(GroupName: string): pOscarItem;
    function  ItemsCountPerGroup(GroupID: Word): Word;
    function  GenNewGroupName(GroupName: string): string;
    function  GetFirstNotLimitedGroup: pOscarItem;
    //function  ItemsCountLimited(ClassID: Word): Boolean; works well, but it seems not needed now
    //function  ClassItemsCount(ClassID: Word): Word; works well, but it seems not needed now
    //procedure DeleteRecentBuddy(Account: string);
    //function  GetRecentBuddy(ItemName: string): pOscarItem;
  public
    Socket      : TOscarSocket;
    OscarInfo   : pOscarInfo;
    constructor Create;
    destructor  Destroy; override;
    procedure ClearListItems;
    procedure ClearListOpers;
    procedure RawDataRcvd(const S: RawByteString);
    procedure ParseRawData;
    function  GetItem(GroupOrItemID: Word): pOscarItem;
    function  GetBuddyItem(ItemName: string; const TryNotInited: Boolean = False): pOscarItem;
    function  GetBuddiesCount: Word;
    procedure InitBuddy(oItem: pOscarItem);
    function  AllBuddiesInited: Boolean;
    function  EnumBuddyItems(var iCounter: Integer; var oItem: pOscarItem): Boolean;
    function  EnumClassItems(var iCounter: Integer; var oItem: pOscarItem; const ClassID: Word): Boolean;
    function  GetBuddyPrivacy(ItemName: string): TOscarPrivSet;
    function  GetPdInfoItem: pOscarItem;
    function  GetGroupNameByGroupID(GroupID: Word): string;
    procedure CheckOrCreateRootGroup;
    procedure UpdatePdInfo(PdMode: Byte);
    procedure AddBuddy(Account, ContactName, GroupName: string);
    procedure DeleteBuddy(oItem: pOscarItem);
    procedure UpdateBuddyAlias(oItem: pOscarItem; NewAlias: string);
    procedure UpdateBuddyPrivacy(oItem: pOscarItem; NewPrivSet: TOscarPrivSet; ItemName: string = '');
    procedure OperStatusRcvd(var Snac: TOscarSnac);
    procedure ServerStartCluster;
    procedure ServerEndCluster;
    procedure ServerOperRcvd(var Snac: TOscarSnac);
    function  GetBartItem(ItemName: string): pOscarItem;
    procedure ChangeBuddyPrivacy(ItemName: string; PrivClassID, CmdType: Word);
    procedure UpdateBartItem(BartType: Word; BartFlags: Byte; ItemData: RawByteString);
    procedure DeleteBartItem(BartType: Word);

    property  OperBusy      : Boolean        read FOperBusy;
    property  OnItemOper    : TOnItemOper    read FOnItemOper     write FOnItemOper;
    property  OnFailedOper  : TOnFailedOper  read FOnFailedOper   write FOnFailedOper;
  end;

implementation

{ TOscarFB }
{*****************************************************************}
constructor TOscarFB.Create;
begin
  FListItems := TList.Create;
  FListOpers := TList.Create;

  FOperSeq   := GetOscarRndWord;
end;

{*****************************************************************}
destructor TOscarFB.Destroy;
begin
  ClearListOpers;
  FListOpers.Free;

  ClearListItems;
  FListItems.Free;
  inherited;
end;

{*****************************************************************}
procedure TOscarFB.ClearListItems;
var oItem: pOscarItem;
    i: integer;
begin
  ClearListOpers;

  FRawData := '';

  for i := FListItems.Count-1 downto 0 do
  begin
    oItem := FListItems.Items[i];
    FreeItem(oItem);
    FListItems.Delete(i);
  end;//for
end;

{*****************************************************************}
procedure TOscarFB.ClearListOpers;
var aOper: pOscarOper;
    i: integer;
begin
  FOperBusy    := False;
  FClusterOpen := False;

  for i := FListOpers.Count-1 downto 0 do
  begin
    aOper := FListOpers.Items[i];
    Dispose(aOper);
    FListOpers.Delete(i);
  end;//for
end;

{*****************************************************************}
function TOscarFB.CreateNewItem(AddToList: Boolean): pOscarItem;
begin
  New(Result);

  ZeroMemory(Result, SizeOf(TOscarItem));
  SetItemDefaults(Result);

  if AddToList then
    FListItems.Add(Result);
end;

{*****************************************************************}
procedure TOscarFB.FreeItem(oItem: pOscarItem);
begin
  //free any objects inside oItem if needed
  Dispose(oItem);
end;

{*****************************************************************}
procedure TOscarFB.RawDataRcvd(const S: RawByteString);
begin
  FRawData := FRawData + S;
end;

{*****************************************************************}
procedure TOscarFB.ParseRawData;
var iCount, i: integer;
begin
  while (FRawData <> '') do
  begin
    //delete classes number, always 0
    Delete(FRawData, 1, 1);

    //get items count
    iCount := ReadWord(FRawData, 2);

    //parse items
    for i := 1 to iCount do
      ParseItem(CreateNewItem(True), FRawData);

    //delete last update time
    Delete(FRawData, 1, 4);
  end;//while
end;

{*****************************************************************}
procedure TOscarFB.ParseItem(oItem: pOscarItem; var Data: RawByteString);
var iLen: Word;
begin
  //get item name
  iLen := ReadWord(Data, 2);
  oItem^.Name := ReadStrUTF8(Data, iLen, iLen);

  //get params
  oItem^.GroupID  := ReadWord(Data, 2);
  oItem^.ItemID   := ReadWord(Data, 2);
  oItem^.ClassID  := ReadWord(Data, 2);

  //parse item attributes
  iLen := ReadWord(Data, 2);
  ParseItemAtts(oItem, ReadStr(Data, iLen, iLen));
end;

{*****************************************************************}
procedure TOscarFB.SetItemDefaults(oItem: pOscarItem);
begin
  SetNickInfoDefs(oItem^.NickInfo);
end;

{*****************************************************************}
procedure TOscarFB.ParseItemAtts(oItem: pOscarItem; Data: RawByteString);
var iAtt, iLen: Word;
begin
  while (Data <> '') do
  begin
    iAtt := ReadWord(Data, 2);
    iLen := ReadWord(Data, 2);

    //increase array of attributes
    SetLength(oItem^.Atts, Length(oItem^.Atts)+1);

    //set new attribute value
    with oItem^.Atts[Length(oItem^.Atts)-1] do
    begin
      ID    := iAtt;
      Value := ReadStr(Data, iLen, iLen);
    end;//with
  end; // while
end;

{*****************************************************************}
function TOscarFB.GetItem(GroupOrItemID: Word): pOscarItem;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := nil;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if ( (oItem^.ClassID = FB_CLSID_GROUP) and (oItem^.GroupID = GroupOrItemID) ) or
       ( (oItem^.ClassID <> FB_CLSID_GROUP) and (oItem^.ItemID = GroupOrItemID) ) then
    begin
      Result := oItem;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
{function TOscarFB.ClassItemsCount(ClassID: Word): Word;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := 0;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if (oItem^.ClassID = ClassID) then
      Inc(Result);
  end;//for
end;}

{*****************************************************************}
{function TOscarFB.ItemsCountLimited(ClassID: Word): Boolean;
var sRaw: RawByteString;
    iClassID, iClassCount: Word;
begin
  Result := False;

  sRaw := OscarInfo^.FbParams.MaxItemsbyClass;
  if (sRaw = '') then Exit;

  iClassID := FB_CLSID_BUDDY;

  while (sRaw <> '') do
  begin
    iClassCount := ReadWord(sRaw, 2);

    if (iClassID = ClassID) then
    begin
      Result := ClassItemsCount(ClassID) >= iClassCount;
      Break;
    end;

    Inc(iClassID);
  end;//while
end;}

{*****************************************************************}
function TOscarFB.ItemsCountPerGroup(GroupID: Word): Word;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := 0;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if (oItem^.GroupID = GroupID) then
      Inc(Result);
  end;//for
end;

{*****************************************************************}
function TOscarFB.GetFirstNotLimitedGroup: pOscarItem;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := nil;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if (oItem^.ClassID = FB_CLSID_GROUP) and (oItem^.GroupID > 0) and (oItem^.Name <> '') and not oItem^.AttExists(FB_ATT_RECENT) then
    begin
      if (OscarInfo^.FbParams.MaxBuddiesPerGroup > 0) then
      begin
        if (ItemsCountPerGroup(oItem^.GroupID) < OscarInfo^.FbParams.MaxBuddiesPerGroup) then
        begin
          Result := oItem;
          Break;
        end;
      end
      else
      begin
        Result := oItem;
        Break;
      end;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarFB.GenNewGroupName(GroupName: string): string;
var i: integer;
    s: RawByteString;
begin
  if (Trim(GroupName) = '') then
    GroupName := 'General';

  //check length
  s := UTF8EncodeMaxEncLen(GroupName, OscarInfo^.FbParams.MaxItemNameLen-4); //-4 here to allow adding group number below, if too long name
  GroupName := UTF8ToUnicodeString(s);

  Result := GroupName;
  i := 0;
  while (GetGroupItemByName(Result) <> nil) do
  begin
    Inc(i);
    if (i > MAXWORD) then Break;
    Result := GroupName + IntToStr(i);
  end;//while
end;

{*****************************************************************}
{function TOscarFB.GetRecentBuddy(ItemName: string): pOscarItem;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := nil;
  if (ItemName = '') then Exit;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if (oItem^.ClassID = FB_CLSID_BUDDY) and SameOscAcc(oItem^.Name, ItemName) and oItem^.AttExists(FB_ATT_RECENT) then
    begin
      Result := oItem;
      Break;
    end;
  end;//for
end;}

{*****************************************************************}
function TOscarFB.GetBuddyItem(ItemName: string; const TryNotInited: Boolean = False): pOscarItem;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := nil;
  if (ItemName = '') then Exit;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if IsRealBuddy(oItem) and SameOscAcc(oItem^.Name, ItemName) then
    begin
      Result := oItem;

      //if get first found (yes, can be more if CL corrupted)
      if not TryNotInited then
        Break
      else
      begin
        //Unfortunately, because of some CLs can be corrupted and have two or more SAME buddies
        //we should check every buddy item with same name
        if oItem^.BuddyInited then Continue;
      end;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarFB.GetBuddiesCount: Word;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := 0;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if IsRealBuddy(oItem) then
      Inc(Result);
  end;//for
end;

{*****************************************************************}
procedure TOscarFB.InitBuddy(oItem: pOscarItem);
var sName: string;
begin
  if (oItem = nil) then Exit;
  oItem^.BuddyInited := True;

  //find same buddies in case of corrupted CL and init them too
  sName := oItem^.Name;

  while True do
  begin
    oItem := GetBuddyItem(sName, True);
    if (oItem = nil) then Break;

    if oItem^.BuddyInited then
      Break
    else
      oItem^.BuddyInited := True;
  end;//while
end;

{*****************************************************************}
function TOscarFB.AllBuddiesInited: Boolean;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := True;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if IsRealBuddy(oItem) and not oItem^.BuddyInited then
    begin
      Result := False;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarFB.EnumBuddyItems(var iCounter: Integer; var oItem: pOscarItem): Boolean;
var aItem: pOscarItem;
    i: Integer;
begin
  Result := False;
  oItem  := nil;
  if (FListItems.Count = 0) or (iCounter > Pred(FListItems.Count)) then Exit;

  for i := iCounter to FListItems.Count-1 do
  begin
    aItem := FListItems.Items[i];
    if IsRealBuddy(aItem) then
    begin
      oItem := aItem;

      iCounter := i;
      Inc(iCounter);

      Result := True;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarFB.EnumClassItems(var iCounter: Integer; var oItem: pOscarItem; const ClassID: Word): Boolean;
var aItem: pOscarItem;
    i: Integer;
begin
  Result := False;
  oItem  := nil;
  if (FListItems.Count = 0) or (iCounter > Pred(FListItems.Count)) then Exit;

  for i := iCounter to FListItems.Count-1 do
  begin
    aItem := FListItems.Items[i];

    if (aItem^.ClassID = ClassID) then
    begin
      oItem := aItem;

      iCounter := i;
      Inc(iCounter);

      Result := True;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarFB.GetBuddyPrivacy(ItemName: string): TOscarPrivSet;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := [];

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if ( (oItem^.ClassID = FB_CLSID_PERMIT) or (oItem^.ClassID = FB_CLSID_DENY) or (oItem^.ClassID = FB_CLSID_IGNORE) ) and
      SameOscAcc(oItem^.Name, ItemName) then
    begin
      if (oItem^.ClassID = FB_CLSID_PERMIT) then
        Result := Result + [privPermit]
      else
      if (oItem^.ClassID = FB_CLSID_DENY) then
        Result := Result + [privDeny]
      else
      if (oItem^.ClassID = FB_CLSID_IGNORE) then
        Result := Result + [privIgnore];
    end;
  end;//for
end;

{*****************************************************************}
function TOscarFB.GetPdInfoItem: pOscarItem;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := nil;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if (oItem^.ClassID = FB_CLSID_PD_INFO) then
    begin
      Result := oItem;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarFB.GetPrivacyItem(ItemName: string; PrivClassID: Word): pOscarItem;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := nil;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if (oItem^.ClassID = PrivClassID) and SameOscAcc(oItem^.Name, ItemName) then
    begin
      Result := oItem;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarFB.GetGroupItem(GroupID: Word): pOscarItem;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := nil;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if (oItem^.ClassID = FB_CLSID_GROUP) and (oItem^.GroupID = GroupID) then
    begin
      //root group must be without name
      if (GroupID = 0) and ((oItem^.Name <> '') or (oItem^.ItemID > 0)) then Continue;

      Result := oItem;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarFB.GetGroupItembyName(GroupName: string): pOscarItem;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := nil;

  //!!!exclude root empty group
  if (GroupName = '') then Exit;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if (oItem^.ClassID = FB_CLSID_GROUP) and WideSameText(oItem^.Name, GroupName) then
    begin
      Result := oItem;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarFB.GetGroupNameByGroupID(GroupID: Word): string;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := '';

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if (oItem^.ClassID = FB_CLSID_GROUP) and (oItem^.GroupID = GroupID) then
    begin
      Result := oItem^.Name;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
function TOscarFB.GenNewFeedbagID: Word;
begin
  Result := $7FFF - (GetOscarRndWord div 2);

  while (GetItem(Result) <> nil) do
    Inc(Result);
end;

{*****************************************************************}
procedure TOscarFB.CopyItem(SrcItem, DstItem: pOscarItem);
begin
  //copy static data
  DstItem^ := SrcItem^;

  //we have dynamic atts array, so we have to create new array and copy
  DstItem^.Atts := Copy(SrcItem^.Atts);
end;

{*****************************************************************}
function TOscarFB.NewOperSeq: Word;
begin
  Result := FOperSeq;
  Inc(FOperSeq);
end;

{*****************************************************************}
function TOscarFB.GetOperReqID(OperType: Word): DWord;
begin
  Result := OperType or (DWord(NewOperSeq) shl 16);
end;

{*****************************************************************}
function TOscarFB.AddOper(OperType: Word; oItem: pOscarItem; ClusterStart, ClusterEnd: Boolean; const AsNext: Boolean = False): pOscarOper;
var aOper: pOscarOper;
    i: integer;
begin
  New(aOper);
  ZeroMemory(aOper, SizeOf(TOscarOper));
  SetItemDefaults(@aOper^.Item);

  i := FListOpers.Add(aOper);

  //set as first if needed
  if AsNext and (FListOpers.Count > 1) then
    FListOpers.Exchange(i, 0);

  Result := aOper;

  aOper^.CmdType      := OperType;
  aOper^.ReqID        := GetOperReqID(OperType);
  aOper^.ClusterStart := ClusterStart;
  aOper^.ClusterEnd   := ClusterEnd;

  if (oItem <> nil) then
    CopyItem(oItem, @aOper^.Item);
end;

{*****************************************************************}
function TOscarFB.AddOper(OperType: Word; Name: string; GroupID, ClassID: Word; ClusterStart, ClusterEnd: Boolean; const AsNext: Boolean = False): pOscarOper;
begin
  Result := AddOper(OperType, nil, ClusterStart, ClusterEnd, AsNext);

  if (ClassID = FB_CLSID_GROUP) then
  begin
    Result^.Item.GroupID := GenNewFeedbagID;
    Result^.Item.ItemID  := 0;
  end
  else
  begin
    Result^.Item.GroupID := GroupID;
    Result^.Item.ItemID  := GenNewFeedbagID;
  end;

  Result^.Item.Name     := Name;
  Result^.Item.ClassID  := ClassID;
end;

{*****************************************************************}
function TOscarFB.GetOper(ReqID: DWord): pOscarOper;
var aOper: pOscarOper;
    i: integer;
begin
  Result := nil;

  for i := 0 to FListOpers.Count-1 do
  begin
    aOper := FListOpers.Items[i];
    if (aOper^.ReqID = ReqID) then
    begin
      Result := aOper;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
procedure TOscarFB.DelOper(ReqID: DWord);
var aOper: pOscarOper;
    i: integer;
begin
  for i := FListOpers.Count-1 downto 0 do
  begin
    aOper := FListOpers.Items[i];
    if (aOper^.ReqID = ReqID) then
    begin
      Dispose(aOper);
      FListOpers.Delete(i);
      Break;
    end;
  end;//for

  //reset busy flag after deletion
  FOperBusy := False;
end;

{*****************************************************************}
procedure TOscarFB.ProcessOperQueue;
var aOper: pOscarOper;
    bFailed: Boolean;
begin
  if (FListOpers.Count = 0) or FOperBusy then Exit;

  FOperBusy := True;

  //send next operation
  aOper := FListOpers.Items[0];

  if aOper^.ClusterStart then
  begin
    FClusterOpen := True;
    Socket.StartCollect;
    Socket.SendSnac(FAM_FEEDBAG, CMD_FEEDBAG_START_CLUSTER, CMD_FEEDBAG_START_CLUSTER);
  end;

  bFailed := not Socket.SendSnac(FAM_FEEDBAG, aOper^.CmdType, aOper^.ReqID, aOper^.Item.GenRawItem);

  if aOper^.ClusterStart then
    bFailed := not Socket.EndCollect;

  if bFailed then
    DelOper(aOper^.ReqID);
end;

{*****************************************************************}
procedure TOscarFB.OperStatusRcvd(var Snac: TOscarSnac);
var aOper, aNewOper: pOscarOper;
    oItem, GroupItem: pOscarItem;
    iCode, ItemID, GroupID: Word;
begin
  aOper := GetOper(Snac.ReqID);
  if (aOper = nil) then Exit;

  try
  //get server oper status code
  iCode := ReadWord(Snac.Data, 2);

  //get original item
  if (aOper^.Item.ClassID = FB_CLSID_GROUP) then
    oItem := GetItem(aOper^.Item.GroupID)
  else
    oItem := GetItem(aOper^.Item.ItemID);

  case aOper^.CmdType of
   {=====================================================}
    CMD_FEEDBAG_INSERT_ITEMS:
      begin
        //========== FB_STATUS_CODE_SUCCESS
        if (iCode = FB_STATUS_CODE_SUCCESS) then
        begin
          oItem := CreateNewItem(True);
          //set item data
          CopyItem(@aOper^.Item, oItem);

          if (oItem^.ClassID = FB_CLSID_GROUP) then
          begin
            GroupID := 0;
            ItemID  := oItem^.GroupID;

            GroupItem := GetGroupItem(GroupID);

            //Check that we added normal group,
            //if ItemID = 0, then we added root group and order update is not required
            if (GroupItem <> nil) and (ItemID > 0) then
            begin
              //update order
              aNewOper := AddOper(CMD_FEEDBAG_UPDATE_ITEMS, GroupItem, False, True, True);
              aNewOper^.Item.UpdAtt(FB_ATT_ORDER, aNewOper^.Item.AttRawValue(FB_ATT_ORDER) + tb(ItemID), True);
            end;
          end
          else
          if (oItem^.ClassID = FB_CLSID_BUDDY) then
          begin
            GroupID := oItem^.GroupID;
            ItemID  := oItem^.ItemID;

            GroupItem := GetGroupItem(GroupID);
            if (GroupItem = nil) then
            begin
              //if needed to create new group and insert order
              aNewOper := AddOper(CMD_FEEDBAG_INSERT_ITEMS, FNewGroupName, 0, FB_CLSID_GROUP, False, False, True);
              aNewOper^.Item.GroupID := GroupID;
              aNewOper^.Item.AddAtt(FB_ATT_ORDER, tb(ItemID));
            end
            else
            begin
              //update order
              aNewOper := AddOper(CMD_FEEDBAG_UPDATE_ITEMS, GroupItem, False, True, True);
              aNewOper^.Item.UpdAtt(FB_ATT_ORDER, aNewOper^.Item.AttRawValue(FB_ATT_ORDER) + tb(ItemID), True);
            end;

            //buddy added, if no auth flag, then send info about it, because OBIMP server always adding with auth flag
            if not oItem^.AttExists(FB_ATT_PENDING_AUTH) then
            begin
              if IsRealBuddy(oItem) and Assigned(FOnItemOper) then FOnItemOper(Self, CMD_FEEDBAG_UPDATE_ITEMS, oItem);
            end;
          end;

        end
        //========== FB_STATUS_CODE_AUTH_REQUIRED
        else
        if (iCode = FB_STATUS_CODE_AUTH_REQUIRED) then
        begin
          if (aOper^.Item.ClassID = FB_CLSID_BUDDY) then
          begin
            aNewOper := AddOper(CMD_FEEDBAG_INSERT_ITEMS, @aOper^.Item, False, False, True);
            aNewOper^.Item.AddAtt(FB_ATT_PENDING_AUTH, '', True);
          end;
        end
        //========== Other error
        else
        begin
          //if something went wrong then force cluster ending
          if FClusterOpen and (FListOpers.Count = 1) and not aOper^.ClusterEnd then
            aOper^.ClusterEnd := True;


          //notify only for buddies opers
          if IsRealBuddy(@aOper^.Item) or IsPrivacyItem(@aOper^.Item) then
          begin
            if Assigned(FOnFailedOper) then FOnFailedOper(Self, aOper^.CmdType, @aOper^.Item, iCode);
          end;
        end;
      end;
   {=====================================================}
    CMD_FEEDBAG_UPDATE_ITEMS:
      begin
        if (oItem = nil) then Exit;

        //update item data
        if (iCode = FB_STATUS_CODE_SUCCESS) then
        begin
          //set item data
          CopyItem(@aOper^.Item, oItem);
        end
        else
        begin
          //notify only for buddies opers
          if IsRealBuddy(oItem) and Assigned(FOnFailedOper) then FOnFailedOper(Self, aOper^.CmdType, oItem, iCode);
        end;
      end;
   {=====================================================}
    CMD_FEEDBAG_DELETE_ITEMS:
      begin
        if (oItem = nil) then Exit;

        //delete item
        if (iCode = FB_STATUS_CODE_SUCCESS) then
        begin
          if (oItem^.ClassID = FB_CLSID_GROUP) then
          begin
            GroupID := 0;
            ItemID  := oItem^.GroupID;
          end
          else
          begin
            GroupID := oItem^.GroupID;
            ItemID  := oItem^.ItemID;
          end;

          DelItemDirect(ItemID);

          //remove ItemID from order in it's group
          if not ExcludeFromGroupOrder(GroupID, ItemID, False) and aOper^.ClusterStart then
            aOper^.ClusterEnd := True;
        end
        else
        begin
          //notify only for buddies opers
          if IsRealBuddy(oItem) and Assigned(FOnFailedOper) then FOnFailedOper(Self, aOper^.CmdType, oItem, iCode);
        end;
      end;
   {=====================================================}
  end;//case

  finally
    if aOper^.ClusterEnd then
    begin
      FClusterOpen := False;
      Socket.SendSnac(FAM_FEEDBAG, CMD_FEEDBAG_END_CLUSTER, CMD_FEEDBAG_END_CLUSTER);
    end;

    DelOper(Snac.ReqID);

    //process next queue
    ProcessOperQueue;
  end;
end;

{*****************************************************************}
function TOscarFB.ExcludeFromGroupOrder(GroupID, DelItemID: Word; ProcessQueue: Boolean): Boolean;
var GroupItem: pOscarItem;
    sRaw, sNew: RawByteString;
    ItemID: Word;
    aOper: pOscarOper;
begin
  Result := False;

  GroupItem := GetGroupItem(GroupID);
  if (GroupItem = nil) then Exit;

  //removing ItemID keeping order of the rest items
  sRaw := GroupItem^.AttRawValue(FB_ATT_ORDER);
  sNew := '';

  while (sRaw <> '') do
  begin
    ItemID := ReadWord(sRaw, 2);
    if (ItemID = DelItemID) then Continue;
    sNew := sNew + tb(ItemID);
  end;//while

  //if nothing changed
  if (GroupItem^.AttRawValue(FB_ATT_ORDER) = sNew) then Exit;

  Result := True;

  aOper := AddOper(CMD_FEEDBAG_UPDATE_ITEMS, GroupItem, False, True, True);
  aOper^.Item.UpdAtt(FB_ATT_ORDER, sNew, True);

  if ProcessQueue then
    ProcessOperQueue;
end;

{*****************************************************************}
procedure TOscarFB.ServerStartCluster;
begin
  if not FOperBusy then
    FOperBusy := True;
end;

{*****************************************************************}
procedure TOscarFB.ServerEndCluster;
begin
  if FOperBusy then
  begin
    FOperBusy := False;
    ProcessOperQueue;
  end;
end;

{*****************************************************************}
procedure TOscarFB.ServerOperRcvd(var Snac: TOscarSnac);
var oItem, origItem: pOscarItem;
    bAdded: Boolean;
begin
  //can be more than one oscar feedbag item inside
  while (Snac.Data <> '') do
  begin
    oItem  := CreateNewItem(False);
    bAdded := False;
    try
    ParseItem(oItem, Snac.Data);

    //do changes
    case Snac.Cmd of
     {=====================================================}
      CMD_FEEDBAG_INSERT_ITEMS:
        begin
          //do not free an item, because added to list
          bAdded := True;

          //just add new item to list
          FListItems.Add(oItem);

          //notify only for buddy items
          if IsRealBuddy(oItem) and Assigned(FOnItemOper) then FOnItemOper(Self, Snac.Cmd, oItem);
        end;
     {=====================================================}
      CMD_FEEDBAG_UPDATE_ITEMS:
        begin
          //get original item
          if (oItem^.ClassID = FB_CLSID_GROUP) then
            origItem := GetItem(oItem^.GroupID)
          else
            origItem := GetItem(oItem^.ItemID);

          if (origItem = nil) then Exit;
          //set item data
          CopyItem(oItem, origItem);

          //notify only for buddy items
          if IsRealBuddy(oItem) and Assigned(FOnItemOper) then FOnItemOper(Self, Snac.Cmd, oItem);
        end;
     {=====================================================}
      CMD_FEEDBAG_DELETE_ITEMS:
        begin
          //notify only for buddy items
          if IsRealBuddy(oItem) and Assigned(FOnItemOper) then FOnItemOper(Self, Snac.Cmd, oItem);

          if (oItem^.ClassID = FB_CLSID_GROUP) then
            DelItemDirect(oItem^.GroupID)
          else
            DelItemDirect(oItem^.ItemID);
        end;
     {=====================================================}
    end;//case

    finally
      //free if was not added
      if not bAdded then FreeItem(oItem);
    end;
  end;//while
end;

{*****************************************************************}
procedure TOscarFB.DelItemDirect(GroupOrItemID: Word);
var oItem: pOscarItem;
    i: integer;
begin
  for i := FListItems.Count-1 downto 0 do
  begin
    oItem := FListItems.Items[i];

    if ( (oItem^.ClassID = FB_CLSID_GROUP) and (oItem^.GroupID = GroupOrItemID) ) or
       ( (oItem^.ClassID <> FB_CLSID_GROUP) and (oItem^.ItemID = GroupOrItemID) ) then
    begin
      FreeItem(oItem);
      FListItems.Delete(i);
      Break;
    end;
  end;//for
end;

{*****************************************************************}
procedure TOscarFB.CheckOrCreateRootGroup;
var Root: pOscarItem;
    aOper: pOscarOper;
begin
  Root := GetGroupItem(0);
  if (Root <> nil) then Exit;

  //add root
  aOper := AddOper(CMD_FEEDBAG_INSERT_ITEMS, '', 0, FB_CLSID_GROUP, False, False);
  aOper^.Item.GroupID := 0;

  ProcessOperQueue;
end;

{*****************************************************************}
procedure TOscarFB.UpdatePdInfo(PdMode: Byte);
var oItem: pOscarItem;
    aOper: pOscarOper;
begin
  oItem := GetPdInfoItem;

  if (oItem <> nil) then
  begin
    if not oItem^.AttExists(FB_ATT_PD_MODE) or (Oitem^.AttByteValue(FB_ATT_PD_MODE) <> PdMode) then
    begin
      aOper := AddOper(CMD_FEEDBAG_UPDATE_ITEMS, oItem, False, False);

      //update PdMode according our privacy status
      aOper^.Item.UpdAtt(FB_ATT_PD_MODE, tb([PdMode]), True);

      //update PdMask attribute if exists
      if aOper^.Item.AttExists(FB_ATT_PD_MASK) then
        aOper^.Item.UpdAtt(FB_ATT_PD_MASK, tb($FFFFFFFF));

      ProcessOperQueue;
    end;
  end
  else
  begin
    aOper := AddOper(CMD_FEEDBAG_INSERT_ITEMS, '', 0, FB_CLSID_PD_INFO, False, False);

    //add only PdMode attribute
    aOper^.Item.AddAtt(FB_ATT_PD_MODE, tb([PdMode]));

    ProcessOperQueue;
  end;
end;

{*****************************************************************}
procedure TOscarFB.AddBuddy(Account, ContactName, GroupName: string);
var GroupItem: pOscarItem;
    aOper: pOscarOper;
    GroupID: Word;
begin
  //check existence
  if (GetBuddyItem(Account) <> nil) then Exit;

  //if root group selected then find first group where can be added contacts
  if (GroupName = '') then
    GroupItem := GetFirstNotLimitedGroup
  else
  begin
    //check group item
    GroupItem := GetGroupItemByName(GroupName);
    if (GroupItem <> nil) then
    begin
      //if too many items in this group, then need to create new one
      if (OscarInfo^.FbParams.MaxBuddiesPerGroup > 0) and (ItemsCountPerGroup(GroupItem^.GroupID) >= OscarInfo^.FbParams.MaxBuddiesPerGroup) then
        GroupItem := nil;
    end;
  end;

  FNewGroupName := '';

  if (GroupItem = nil) then
  begin
    GroupID := GenNewFeedbagID;
    //save new group name to create it after adding buddy, yes, it is possible
    FNewGroupName := GenNewGroupName(GroupName);
  end
  else
    GroupID := GroupItem^.GroupID;

  //try delete recent buddy
  //commented because server deletes recent buddy itself
  //DeleteRecentBuddy(Account);

  //add buddy item
  aOper := AddOper(CMD_FEEDBAG_INSERT_ITEMS, Account, GroupID, FB_CLSID_BUDDY, True, False);

  //add alias if its differs from account name
  //Commented WideSameStr, because server will add own alias BUT with wrong encoding :(
  if (ContactName <> '') {and not WideSameStr(Account, ContactName)} then
    aOper^.Item.AddAtt(FB_ATT_ALIAS, UTF8Encode(ContactName));

  //start adding
  ProcessOperQueue;
end;

{*****************************************************************}
procedure TOscarFB.DeleteBuddy(oItem: pOscarItem);
begin
  if (oItem = nil) then Exit;
  AddOper(CMD_FEEDBAG_DELETE_ITEMS, oItem, True, False);
  ProcessOperQueue;
end;

{*****************************************************************}
{procedure TOscarFB.DeleteRecentBuddy(Account: string);
var oItem: pOscarItem;
begin
  oItem := GetRecentBuddy(Account);
  if (oItem = nil) then Exit;
  AddOper(CMD_FEEDBAG_DELETE_ITEMS, oItem, True, False);
  ProcessOperQueue;
end;}

{*****************************************************************}
procedure TOscarFB.UpdateBuddyAlias(oItem: pOscarItem; NewAlias: string);
var aOper: pOscarOper;
begin
  if WideSameStr(NewAlias, oItem^.AttStrValue(FB_ATT_ALIAS)) then Exit;

  aOper := AddOper(CMD_FEEDBAG_UPDATE_ITEMS, oItem, True, True);
  aOper^.Item.UpdAtt(FB_ATT_ALIAS, UTF8EncodeMaxEncLen(NewAlias, FB_MAX_ATT_ALIAS_ENC_LEN), True);
  ProcessOperQueue;
end;

{*****************************************************************}
procedure TOscarFB.UpdateBuddyPrivacy(oItem: pOscarItem; NewPrivSet: TOscarPrivSet; ItemName: string = '');
var ps: TOscarPrivSet;
begin
  if (oItem <> nil) then
    ItemName := oItem^.Name;

  if (ItemName = '') then Exit;

  ps := GetBuddyPrivacy(ItemName);
  if (ps = NewPrivSet) then Exit;

  if (privPermit in NewPrivSet) then
  begin
    if not (privPermit in ps) then ChangeBuddyPrivacy(ItemName, FB_CLSID_PERMIT, CMD_FEEDBAG_INSERT_ITEMS);
  end
  else
  begin
    if (privPermit in ps) then ChangeBuddyPrivacy(ItemName, FB_CLSID_PERMIT, CMD_FEEDBAG_DELETE_ITEMS);
  end;

  if (privDeny in NewPrivSet) then
  begin
    if not (privDeny in ps) then ChangeBuddyPrivacy(ItemName, FB_CLSID_DENY, CMD_FEEDBAG_INSERT_ITEMS);
  end
  else
  begin
    if (privDeny in ps) then ChangeBuddyPrivacy(ItemName, FB_CLSID_DENY, CMD_FEEDBAG_DELETE_ITEMS);
  end;

  if (privIgnore in NewPrivSet) then
  begin
    if not (privIgnore in ps) then ChangeBuddyPrivacy(ItemName, FB_CLSID_IGNORE, CMD_FEEDBAG_INSERT_ITEMS);
  end
  else
  begin
    if (privIgnore in ps) then ChangeBuddyPrivacy(ItemName, FB_CLSID_IGNORE, CMD_FEEDBAG_DELETE_ITEMS);
  end;
end;

{*****************************************************************}
procedure TOscarFB.ChangeBuddyPrivacy(ItemName: string; PrivClassID, CmdType: Word);
var oItem: pOscarItem;
begin
  if (CmdType = CMD_FEEDBAG_INSERT_ITEMS) then
    AddOper(CmdType, ItemName, 0, PrivClassID, False, False)
  else
  begin
    oItem := GetPrivacyItem(ItemName, PrivClassID);
    if (oItem = nil) then Exit;
    AddOper(CmdType, oItem, False, False);
  end;

  ProcessOperQueue;
end;

{*****************************************************************}
function TOscarFB.GetBartItem(ItemName: string): pOscarItem;
var oItem: pOscarItem;
    i: Integer;
begin
  Result := nil;
  if (ItemName = '') then Exit;

  for i := 0 to FListItems.Count-1 do
  begin
    oItem := FListItems.Items[i];

    if (oItem^.ClassID = FB_CLSID_BART) and WideSameText(oItem^.Name, ItemName) then
    begin
      Result := oItem;
      Break;
    end;
  end;//for
end;

{*****************************************************************}
procedure TOscarFB.UpdateBartItem(BartType: Word; BartFlags: Byte; ItemData: RawByteString);
var oItem: pOscarItem;
    aOper: pOscarOper;
begin
  oItem := GetBartItem(IntToStr(BartType));

  if (oItem <> nil) then
    aOper := AddOper(CMD_FEEDBAG_UPDATE_ITEMS, oItem, True, True)
  else
    aOper := AddOper(CMD_FEEDBAG_INSERT_ITEMS, IntToStr(BartType), 0, FB_CLSID_BART, True, True);

  aOper^.Item.UpdAtt(FB_ATT_BART_INFO, tb([BartFlags, Length(ItemData)]) + ItemData, True);

  ProcessOperQueue;
end;

{*****************************************************************}
procedure TOscarFB.DeleteBartItem(BartType: Word);
var oItem: pOscarItem;
begin
  oItem := GetBartItem(IntToStr(BartType));
  if (oItem = nil) then Exit;

  AddOper(CMD_FEEDBAG_DELETE_ITEMS, oItem, True, True);

  ProcessOperQueue;
end;



end.
