// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_privacy;

interface

uses Windows, Classes, SysUtils, NativeXML, u_xmppsock, h_jabber, u_xepclass, u_privacy_list;

// XEP-0016: Privacy Lists

type
  TXmppPrivacy = class(TXepClass)
  private
    FSocket      : TXmppSocket;
    FLists       : TList;
    function GetListActive: string;
    function GetListDefault: string;
  public
    BimIgnNilList: TStringList; //helping list to remember what jids added as ignored "not in list"
    constructor Create(XmppSocket: TXmppSocket);
    destructor  Destroy; override;
    procedure ResetData; override;
    procedure ClearLists;
    function  EnumLists(var iCounter: Integer; var aList: TPrivacyList): Boolean;
    function  GetList(const ListName: string): TPrivacyList;
    function  ListExists(const ListName: string): Boolean;
    function  AddList(const ListName: string): TPrivacyList;
    procedure DelList(const ListName: string);

    procedure Rcvd_Lists(Node: TXmlNode);
    procedure Send_SetActiveList(const ListName: string);
    procedure Send_RequestList(const ListName: string; SeqType: DWord);
    procedure Rcvd_ActiveListSet(const ListName: string);
    function  Rcvd_ItemsList(const ListName: string; Node: TXmlNode): Integer;

    //special bimoid transport funcs
    function  Bim_GetJidPrivacy(aPrivItem: pPrivacyItem): Byte;
    function  Bim_UpdJidPrivItem(const ListName: string; const Jid: string; const PrivBim: Byte): Boolean; //call Bim_SendUpdateList to apply new list
    function  Bim_SendUpdateList(const ListName: string; const StatusShow: string): Boolean;

    property  ListDefault : string  read GetListDefault;
    property  ListActive  : string  read GetListActive;
  end;


implementation

{ TXmppPrivacy }
{*****************************************************************}
constructor TXmppPrivacy.Create(XmppSocket: TXmppSocket);
begin
  FSocket := XmppSocket;

  FLists := TList.Create;

  BimIgnNilList := TStringList.Create;
  BimIgnNilList.Sorted := True;
  BimIgnNilList.Duplicates := dupIgnore;
end;

{*****************************************************************}
destructor TXmppPrivacy.Destroy;
begin
  BimIgnNilList.Free;

  ClearLists;
  FLists.Free;
  inherited;
end;

{*****************************************************************}
procedure TXmppPrivacy.ResetData;
begin
  inherited;

  ClearLists;
  BimIgnNilList.Clear;
end;

{*****************************************************************}
procedure TXmppPrivacy.ClearLists;
var aList: TPrivacyList;
    i: Integer;
begin
  for i := FLists.Count-1 downto 0 do
  begin
    aList := FLists.Items[i];
    aList.Free;

    FLists.Delete(i);
  end;//for
end;

{*****************************************************************}
procedure TXmppPrivacy.Rcvd_Lists(Node: TXmlNode);
var aList: TPrivacyList;
    sName, sValue, sActive, sDefault: string;
    i: integer;
begin
  ClearLists;

  sActive  := '';
  sDefault := '';

  for i := 0 to Node.ChildContainerCount-1 do
  begin
    sName  := UTF8ToUnicodeString(Node.ChildContainers[i].Name);
    sValue := Node.ChildContainers[i].AttributeValueByNameWide[UTF8Encode(XATT_NAME)];

    if WideSameText(sName, XNS_LIST) then
    begin
      //add new list
      if (sValue <> '') then
        AddList(sValue);
    end
    else
    if WideSameText(sName, XNS_DEFAULT) then
      sDefault := sValue
    else
    if WideSameText(sName, XNS_ACTIVE) then
      sActive  := sValue;
  end;//for

  if (sDefault <> '') then
  begin
    aList := GetList(sDefault);
    if (aList <> nil) then aList.IsDefault := True;
  end;

  if (sActive <> '') then
  begin
    aList := GetList(sActive);
    if (aList <> nil) then aList.IsActive := True;
  end;
end;

{*****************************************************************}
function TXmppPrivacy.EnumLists(var iCounter: Integer; var aList: TPrivacyList): Boolean;
begin
  Result := False;
  aList  := nil;
  if (FLists.Count = 0) or (iCounter < 0) or (iCounter > Pred(FLists.Count)) then Exit;

  aList := FLists.Items[iCounter];
  Inc(iCounter);

  Result := True;
end;

{*****************************************************************}
function TXmppPrivacy.GetList(const ListName: string): TPrivacyList;
var aList: TPrivacyList;
    i: Integer;
begin
  Result := nil;

  aList := nil;
  i     := 0;
  while EnumLists(i, aList) do
  begin
    if WideSameText(aList.ListName, ListName) then
    begin
      Result := aList;
      Break;
    end;
  end;//while
end;

{*****************************************************************}
procedure TXmppPrivacy.DelList(const ListName: string);
var aList: TPrivacyList;
    i: Integer;
begin
  for i := FLists.Count-1 downto 0 do
  begin
    aList := FLists.Items[i];

    if WideSameText(aList.ListName, ListName) then
    begin
      aList.Free;
      FLists.Delete(i);
    end;
  end;//for
end;

{*****************************************************************}
function TXmppPrivacy.AddList(const ListName: string): TPrivacyList;
begin
  Result := GetList(ListName);
  if (Result <> nil) then Exit;

  Result := TPrivacyList.Create(ListName);
  FLists.Add(Result);
end;

{*****************************************************************}
function TXmppPrivacy.GetListActive: string;
var aList: TPrivacyList;
    i: Integer;
begin
  Result := '';

  aList := nil;
  i     := 0;
  while EnumLists(i, aList) do
  begin
    if aList.IsActive then
    begin
      Result := aList.ListName;
      Break;
    end;
  end;//while
end;

{*****************************************************************}
function TXmppPrivacy.GetListDefault: string;
var aList: TPrivacyList;
    i: Integer;
begin
  Result := '';

  aList := nil;
  i     := 0;
  while EnumLists(i, aList) do
  begin
    if aList.IsDefault then
    begin
      Result := aList.ListName;
      Break;
    end;
  end;//while
end;

{*****************************************************************}
function TXmppPrivacy.ListExists(const ListName: string): Boolean;
begin
  Result := GetList(ListName) <> nil;
end;

{*****************************************************************}
procedure TXmppPrivacy.Send_RequestList(const ListName: string; SeqType: DWord);
begin
  FSocket.Send(XmlTagIqQuery(XVAL_GET, FSocket.NewSeqNum(SeqType, ListName), XNS_JABBER_IQ_PRIVACY, XmlTag(XTAG_LIST_EMPTY, ListName)));
end;

{*****************************************************************}
procedure TXmppPrivacy.Send_SetActiveList(const ListName: string);
begin
  if (GetList(ListName) <> nil) then
    FSocket.Send(XmlTagIqQuery(XVAL_SET, FSocket.NewSeqNum(SEQT_ACTIVE_LIST, ListName), XNS_JABBER_IQ_PRIVACY, XmlTag(XTAG_ACTIVE_EMPTY, ListName)));
end;

{*****************************************************************}
procedure TXmppPrivacy.Rcvd_ActiveListSet(const ListName: string);
var aList: TPrivacyList;
    i: Integer;
begin
  aList := nil;
  i     := 0;
  while EnumLists(i, aList) do
    aList.IsActive := WideSameText(aList.ListName, ListName);
end;

{*****************************************************************}
function TXmppPrivacy.Rcvd_ItemsList(const ListName: string; Node: TXmlNode): Integer;
var aList: TPrivacyList;
begin
  Result := 0;

  aList := GetList(ListName);
  if (aList = nil) then
    aList := AddList(ListName);

  if (aList = nil) then Exit;

  Result := aList.ParseRcvdItems(Node);
end;

{*****************************************************************}
function TXmppPrivacy.Bim_SendUpdateList(const ListName: string; const StatusShow: string): Boolean;
var aList: TPrivacyList;
    aItem: pPrivacyItem;
    i: integer;
    iOrder: DWord;
    ListBad: TList;
    ListJids: TStringList;
begin
  Result := False;

  //add list if not exist
  aList := GetList(ListName);
  if (aList = nil) then
    aList := AddList(ListName);

  if (aList = nil) then Exit;

  ListBad := TList.Create;

  ListJids        := TStringList.Create;
  ListJids.Sorted := True;
  try
  iOrder := 100;

  i := 0;
  while aList.EnumListItems(i, aItem) do
  begin
    //delete invisible concerned items and items added by any other way and duplicated jid items if any
    if (aItem^.ItemType = PRIV_ITEM_ANY) or (aItem^.ItemType = PRIV_ITEM_GROUP) or (aItem^.ItemType = PRIV_ITEM_SUBSCRIPTION) or
      ( (aItem^.ItemType = PRIV_ITEM_JID) and ( (aItem^.Value = '') or (Bim_GetJidPrivacy(aItem) = PRIV_BIM_NONE) or
                                                (ListJids.IndexOf(aItem^.Value) > -1) ) ) then
    begin
      ListBad.Add(aItem);
    end
    else
    begin
      //update order number of Jids
      aItem^.Order := iOrder;
      Inc(iOrder, 10);

      ListJids.Add(aItem^.Value);
    end;
  end;//while

  for i := 0 to ListBad.Count-1 do
    aList.DelPrivItem(ListBad.Items[i]);

  //add invisible concerned item
  if (StatusShow = PRES_SHOW_INVIS_SPC) then
    aList.AddPrivItem(PRIV_ITEM_ANY, '', False, DWord(-1), False, False, True, False)
  else
  if (StatusShow = PRES_SHOW_IFA_SPC) then
    aList.AddPrivItem(PRIV_ITEM_ANY, '', False, 0, False, False, True, False, True);

  //send list update if changed
  if aList.ListChanged then
  begin
    Result := True;
    FSocket.Send(XmlTagIqQuery(XVAL_SET, FSocket.NewSeqNum(SEQT_UPDCUR_LIST, aList.ListName), XNS_JABBER_IQ_PRIVACY, aList.GenListXml));
  end;

  if (aList.ListItemsCount = 0) then
    DelList(ListName);

  finally
    ListJids.Free;
    ListBad.Free;
  end;
end;

{*****************************************************************}
function TXmppPrivacy.Bim_GetJidPrivacy(aPrivItem: pPrivacyItem): Byte;
begin
  Result := PRIV_BIM_NONE;

  //=== visible
  if aPrivItem^.Allow and aPrivItem^.ForPresOut and
    not aPrivItem^.ForMessage and not aPrivItem^.ForPresIn and not aPrivItem^.ForIQ then
  begin
    Result := PRIV_BIM_VISIBLE;
    Exit;
  end;

  //=== invisible
  if not aPrivItem^.Allow and aPrivItem^.ForPresOut and
    not aPrivItem^.ForMessage and not aPrivItem^.ForPresIn and not aPrivItem^.ForIQ then
  begin
    Result := PRIV_BIM_INVISIBLE;
    Exit;
  end;

  //=== ignored
  if not aPrivItem^.Allow and aPrivItem^.ForPresOut and aPrivItem^.ForIQ and aPrivItem^.ForMessage and
    not aPrivItem^.ForPresIn then
  begin
    Result := PRIV_BIM_IGNORE;
    Exit;
  end;
end;

{*****************************************************************}
function TXmppPrivacy.Bim_UpdJidPrivItem(const ListName: string; const Jid: string; const PrivBim: Byte): Boolean;
var aList: TPrivacyList;
    aPrivItem: pPrivacyItem;
begin
  Result := True;

  aList := GetList(ListName);
  if (aList = nil) then
    aList := AddList(ListName);

  aPrivItem := aList.GetPrivItem(PRIV_ITEM_JID, Jid);

  case PrivBim of
   {==========================================================}
    PRIV_BIM_NONE       :
      begin
        if (aPrivItem <> nil) then
          aList.DelPrivItem(aPrivItem)
        else
          Result := False;
      end;
   {==========================================================}
    PRIV_BIM_VISIBLE    :
      begin
        if (aPrivItem = nil) then
          aList.AddPrivItem(PRIV_ITEM_JID, Jid, True, 1, False, False, True, False)
        else
        begin
          if (Bim_GetJidPrivacy(aPrivItem) = PRIV_BIM_VISIBLE) then
            Result := False
          else
          with aPrivItem^ do
          begin
            Allow      := True;
            ForMessage := False;
            ForPresIn  := False;
            ForPresOut := True;
            ForIQ      := False;
          end;//with
        end;
      end;
   {==========================================================}
    PRIV_BIM_INVISIBLE  :
      begin
        if (aPrivItem = nil) then
          aList.AddPrivItem(PRIV_ITEM_JID, Jid, False, 1, False, False, True, False)
        else
        begin
          if (Bim_GetJidPrivacy(aPrivItem) = PRIV_BIM_INVISIBLE) then
            Result := False
          else
          with aPrivItem^ do
          begin
            Allow      := False;
            ForMessage := False;
            ForPresIn  := False;
            ForPresOut := True;
            ForIQ      := False;
          end;//with
        end;
      end;
   {==========================================================}
    PRIV_BIM_IGNORE     :
      begin
        if (aPrivItem = nil) then
          aList.AddPrivItem(PRIV_ITEM_JID, Jid, False, 1, True, False, True, True)
        else
        begin
          if (Bim_GetJidPrivacy(aPrivItem) = PRIV_BIM_IGNORE) then
            Result := False
          else
          with aPrivItem^ do
          begin
            Allow      := False;
            ForMessage := True;
            ForPresIn  := False;
            ForPresOut := True;
            ForIQ      := True;
          end;//with
        end;
      end;
   {==========================================================}
  end;//case
end;


end.
