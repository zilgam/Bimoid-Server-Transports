// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_privacy_list;

interface

uses Windows, Classes, SysUtils, NativeXML, h_jabber;

// XEP-0016: Privacy Lists

const
  PRIV_ITEM_ANY          = 0;
  PRIV_ITEM_JID          = 1;
  PRIV_ITEM_GROUP        = 2;
  PRIV_ITEM_SUBSCRIPTION = 3;

  PRIV_ACTION_ALLOW      = 1;
  PRIV_ACTION_DENY       = 2;

  //special only for Bimoid transport
  PRIV_BIM_NONE         = 0;
  PRIV_BIM_VISIBLE      = 1;
  PRIV_BIM_INVISIBLE    = 2;
  PRIV_BIM_IGNORE       = 3;

type
  TPrivacyItem = record
    ItemType   : Byte;
    Value      : string;
    Allow      : Boolean;
    Order      : DWord;
    ForMessage : Boolean;
    ForPresIn  : Boolean;
    ForPresOut : Boolean;
    ForIQ      : Boolean;
  end;
  pPrivacyItem = ^TPrivacyItem;

  TPrivacyList = class
  private
    FListItems  : TList;
    FListName   : string;
  public
    IsActive    : Boolean;
    IsDefault   : Boolean;
    ListHash    : RawByteString;
    constructor Create(const ListName: string);
    destructor  Destroy; override;
    procedure ClearListItems;
    function  ListItemsCount: Integer;
    function  ListChanged: Boolean;
    function  ParseRcvdItems(Node: TXmlNode): Integer;
    function  GenListXml: string;
    function  EnumListItems(var iCounter: Integer; var aPrivItem: pPrivacyItem): Boolean;
    procedure DelPrivItem(aPrivItem: pPrivacyItem);
    function  AddPrivItem(ItemType: Byte; Value: string; Allow: Boolean; Order: DWord; ForMessage, ForPresIn, ForPresOut, ForIQ: Boolean; const FirstInList: Boolean = False): pPrivacyItem;
    function  GetPrivItem(const ItemType: Byte; const Value: string): pPrivacyItem;

    property  ListName: string read FListName;
  end;

implementation

{*****************************************************************}
function ActionToStr(const Allow: Boolean): string;
begin
  if Allow then
    Result := XVAL_ALLOW
  else
    Result := XVAL_DENY;
end;

{*****************************************************************}
function ItemTypeToInt(const S: string): Byte;
begin
  Result := PRIV_ITEM_ANY;
  if (Trim(S) = '') then Exit;

  if WideSameText(Trim(S), XVAL_JID) then
    Result := PRIV_ITEM_JID
  else
  if WideSameText(Trim(S), XVAL_GROUP) then
    Result := PRIV_ITEM_GROUP
  else
  if WideSameText(Trim(S), XVAL_SUBSCRIPTION) then
    Result := PRIV_ITEM_SUBSCRIPTION;
end;

{*****************************************************************}
function ItemTypeToStr(const I: Byte): string;
begin
  Result := '';

  case I of
    PRIV_ITEM_ANY          :;
    PRIV_ITEM_JID          : Result := XVAL_JID;
    PRIV_ITEM_GROUP        : Result := XVAL_GROUP;
    PRIV_ITEM_SUBSCRIPTION : Result := XVAL_SUBSCRIPTION;
  end;//case
end;

{ TPrivacyList }
{*****************************************************************}
constructor TPrivacyList.Create(const ListName: string);
begin
  FListItems := TList.Create;

  FListName := ListName;
  //update hash after adding list name
  ListHash := MD5Raw(UTF8Encode(GenListXml));
end;

{*****************************************************************}
destructor TPrivacyList.Destroy;
begin
  ClearListItems;
  FListItems.Free;
  inherited;
end;

{*****************************************************************}
procedure TPrivacyList.ClearListItems;
var aPrivItem: pPrivacyItem;
    i: Integer;
begin
  for i := FListItems.Count-1 downto 0 do
  begin
    aPrivItem := FListItems.Items[i];

    Dispose(aPrivItem);
    FListItems.Delete(i);
  end;//for

  ListHash := MD5Raw(UTF8Encode(GenListXml));
end;

{*****************************************************************}
function TPrivacyList.ListChanged: Boolean;
var sData: string;
    sMD5: RawByteString;
begin
  Result := False;

  sData := GenListXml;
  sMD5  := MD5Raw(UTF8Encode(sData));

  if (sMD5 <> ListHash) then
  begin
    ListHash := sMD5;
    Result   := True;
  end;
end;

{*****************************************************************}
function TPrivacyList.ListItemsCount: Integer;
begin
  Result := FListItems.Count;
end;

{*****************************************************************}
function TPrivacyList.ParseRcvdItems(Node: TXmlNode): Integer;
var aPrivItem: pPrivacyItem;
    i: integer;
    SubNode: TXmlNode;
begin
  Result := 0;

  Node := Node.NodeByName(UTF8Encode(XNS_LIST));
  if (Node = nil) or not WideSameText(Node.AttributeValueByNameWide[UTF8Encode(XATT_NAME)], FListName) then
    Exit;

  ClearListItems;

  for i := 0 to Node.ChildContainerCount-1 do
  begin
    SubNode := Node.ChildContainers[i];

    if WideSameText(UTF8ToUnicodeString(SubNode.Name), XNS_ITEM) then
    begin
      New(aPrivItem);
      ZeroMemory(aPrivItem, SizeOf(TPrivacyItem));

      try
        aPrivItem^.ItemType := ItemTypeToInt(SubNode.AttributeValueByNameWide[UTF8Encode(XATT_TYPE)]);
        aPrivItem^.Value    := SubNode.AttributeValueByNameWide[UTF8Encode(XATT_VALUE)];
        aPrivItem^.Allow    := WideSameText(SubNode.AttributeValueByNameWide[UTF8Encode(XATT_ACTION)], XVAL_ALLOW);
        aPrivItem^.Order    := StrToIntDef(SubNode.AttributeValueByNameWide[UTF8Encode(XATT_ORDER)], 0);

        if (SubNode.ChildContainerCount > 0) then
        begin
          aPrivItem^.ForMessage := SubNode.NodeByName(UTF8Encode(XNS_MESSAGE)) <> nil;
          aPrivItem^.ForPresIn  := SubNode.NodeByName(UTF8Encode(XNS_PRESENCE_IN)) <> nil;
          aPrivItem^.ForPresOut := SubNode.NodeByName(UTF8Encode(XNS_PRESENCE_OUT)) <> nil;
          aPrivItem^.ForIQ      := SubNode.NodeByName(UTF8Encode(XNS_IQ)) <> nil;
        end
        else
        begin
          aPrivItem^.ForMessage := True;
          aPrivItem^.ForPresIn  := True;
          aPrivItem^.ForPresOut := True;
          aPrivItem^.ForIQ      := True;
        end;
      except
        Dispose(aPrivItem);
        Continue;
      end;

      FListItems.Add(aPrivItem);
    end;
  end;//for

  Result := FListItems.Count;

  //gen hash
  ListHash := MD5Raw(UTF8Encode(GenListXml));
end;

{*****************************************************************}
function TPrivacyList.GenListXml: string;
var aItem: pPrivacyItem;
    i: integer;
    sTags, sAtts: string;
begin
  Result := '';

  i := 0;
  while EnumListItems(i, aItem) do
  begin
    sTags := '';
    if aItem^.ForMessage then sTags := sTags + XmlTag(XTAG_MESSAGE_EMPTY);
    if aItem^.ForPresIn  then sTags := sTags + XmlTag(XTAG_PRESENCE_IN);
    if aItem^.ForPresOut then sTags := sTags + XmlTag(XTAG_PRESENCE_OUT);
    if aItem^.ForIQ      then sTags := sTags + XmlTag(XTAG_IQ_NO_ATTS);

    sAtts := '';
    if (aItem^.ItemType <> PRIV_ITEM_ANY) then
      sAtts := sAtts + XmlAtt(XATT_TYPE, ItemTypeToStr(aItem^.ItemType)) + THE_SPC;

    if (aItem^.Value <> '') then
      sAtts := sAtts + XmlAtt(XATT_VALUE, aItem^.Value) + THE_SPC;

    sAtts := sAtts + XmlAtt(XATT_ACTION, ActionToStr(aItem^.Allow)) + THE_SPC;
    sAtts := sAtts + XmlAtt(XATT_ORDER, UIntToStr(aItem^.Order));

    if (sTags = '') then
      Result := Result + XmlTag(XTAG_ITEM_EMPTY, sAtts)
     else
      Result := Result + XmlTag(XTAG_ITEM, sAtts, sTags);
  end;//while

  if (Result = '') then
    Result := XmlTag(XTAG_LIST_EMPTY, FListName)
  else
    Result := XmlTag(XTAG_LIST, FListName, Result);
end;

{*****************************************************************}
function TPrivacyList.EnumListItems(var iCounter: Integer; var aPrivItem: pPrivacyItem): Boolean;
begin
  Result := False;
  aPrivItem := nil;
  if (FListItems.Count = 0) or (iCounter < 0) or (iCounter > Pred(FListItems.Count)) then Exit;

  aPrivItem := FListItems.Items[iCounter];
  Inc(iCounter);

  Result := True;
end;

{*****************************************************************}
procedure TPrivacyList.DelPrivItem(aPrivItem: pPrivacyItem);
var i: integer;
begin
  i := FListItems.IndexOf(aPrivItem);
  if (i > -1) then
  begin
    Dispose(aPrivItem);
    FListItems.Delete(i);
  end;
end;

{*****************************************************************}
function TPrivacyList.AddPrivItem(ItemType: Byte; Value: string; Allow: Boolean; Order: DWord; ForMessage, ForPresIn, ForPresOut, ForIQ: Boolean; const FirstInList: Boolean): pPrivacyItem;
begin
  New(Result);
  ZeroMemory(Result, SizeOf(TPrivacyItem));

  Result^.ItemType   := ItemType;
  Result^.Value      := Value;
  Result^.Allow      := Allow;
  Result^.Order      := Order;
  Result^.ForMessage := ForMessage;
  Result^.ForPresIn  := ForPresIn;
  Result^.ForPresOut := ForPresOut;
  Result^.ForIQ      := ForIQ;

  if FirstInList then
    FListItems.Insert(0, Result)
  else
    FListItems.Add(Result);
end;

{*****************************************************************}
function TPrivacyList.GetPrivItem(const ItemType: Byte; const Value: string): pPrivacyItem;
var aItem: pPrivacyItem;
    i: integer;
begin
  Result := nil;

  i := 0;
  while EnumListItems(i, aItem) do
  begin
    if (aItem^.ItemType = ItemType) and WideSameText(aItem^.Value, Value) then
    begin
      Result := aItem;
      Break;
    end;
  end;//while
end;



end.
