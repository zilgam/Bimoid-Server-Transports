// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_roster;

interface

uses Windows, Classes, SysUtils, u_xmppsock, h_jabber, NativeXML, {$IFDEF GTALK}u_gtalk,{$ENDIF} u_rosterdelim, u_roster_info;

type
  TOnRosterChanged = procedure (Sender: TObject; aItem: pRosterItem; RosterOper: Byte) of object;

  TXmppRoster = class
  private
    FSocket           : TXmppSocket;
    FListItems        : TList;
    FRequestedOnce    : Boolean;
    FOnRosterChanged  : TOnRosterChanged;
    procedure ClearRosterItems;
    {$IFDEF XMPP}
    function  GroupInArray(const Group: string; const Groups: TArrayGroups): Boolean;
    {$ENDIF}
    procedure DelRosterItem(aItem: pRosterItem);
    function  MakeItemCopy(aItem: pRosterItem): pRosterItem; overload;
    function  MakeNewItem(const Jid, Name, Group: string {$IFDEF GTALK}; const GTalk_T: string {$ENDIF}): pRosterItem;
    {$IFDEF GTALK}
    function  GTalk_Check_T_Param(aItem: pRosterItem; const New_T_Param: string; var Oper: Byte): Boolean;
    {$ENDIF}
  public
    GroupsDelim : TRosterDelimiter;
    constructor Create(XmppSocket: TXmppSocket);
    destructor  Destroy; override;
    procedure ResetData;
    procedure Send_RequestRoster;
    procedure Rcvd_Roster(Node: TXmlNode);
    function  EnumItems(var iCounter: Integer; var aRosterItem: pRosterItem): Boolean;
    function  IsAuthRequired(aRosterItem: pRosterItem): Boolean;
    function  GetRosterItem(const Jid: string): pRosterItem;
    procedure Send_DelRosterItem(const Jid: string);
    procedure Send_AddRosterItem(const Jid, Name, Group: string {$IFDEF GTALK}; const GTalk_T: string {$ENDIF});
    procedure Send_UpdRosterItem(const Jid, Name, Group: string {$IFDEF GTALK}; const GTalk_T: string {$ENDIF});
    procedure Rcvd_RosterItemOper(ItemNode: TXmlNode; const NewRosterRcvd: Boolean = False);
    procedure CancelRosterOper(const Jid: string; const RosterOper: Byte; aCopyItem: pRosterItem);
    function  GetJidCurResInfo(const Jid: string): pResInfo;

    property  RequestedOnce    : Boolean          read FRequestedOnce;
    property  OnRosterChanged  : TOnRosterChanged read FOnRosterChanged   write FOnRosterChanged;
  end;

implementation

{ TXmppRoster }
{*****************************************************************}
constructor TXmppRoster.Create(XmppSocket: TXmppSocket);
begin
  FSocket := XmppSocket;

  GroupsDelim := TRosterDelimiter.Create;

  FListItems := TList.Create;
end;

{*****************************************************************}
destructor TXmppRoster.Destroy;
begin
  ClearRosterItems;
  FListItems.Free;

  GroupsDelim.Free;
  inherited;
end;

{*****************************************************************}
procedure TXmppRoster.ClearRosterItems;
var aItem: pRosterItem;
    i: Integer;
begin
  for i := FListItems.Count-1 downto 0 do
  begin
    aItem := FListItems.Items[i];

    Dispose(aItem);
    FListItems.Delete(i);
  end;//for
end;

{*****************************************************************}
procedure TXmppRoster.DelRosterItem(aItem: pRosterItem);
var i: integer;
begin
  i := FListItems.IndexOf(aItem);
  if (i > -1) then
  begin
    Dispose(aItem);
    FListItems.Delete(i);
  end;
end;

{*****************************************************************}
procedure TXmppRoster.ResetData;
begin
  GroupsDelim.ResetData;

  FRequestedOnce := False;
end;

{*****************************************************************}
function TXmppRoster.MakeItemCopy(aItem: pRosterItem): pRosterItem;
begin
  Result := nil;
  if (aItem = nil) then Exit;

  New(Result);
  ZeroMemory(Result, SizeOf(TRosterItem));

  Result^ := aItem^;

  //copy arrays
  Result^.Groups   := Copy(aItem^.Groups);
  Result^.ResArray := Copy(aItem^.ResArray);
end;

{*****************************************************************}
function TXmppRoster.MakeNewItem(const Jid, Name, Group: string {$IFDEF GTALK}; const GTalk_T: string {$ENDIF}): pRosterItem;
begin
  New(Result);
  ZeroMemory(Result, SizeOf(TRosterItem));

  Result^.JID    := Jid;
  Result^.Name   := Name;
  Result^.Subscr := XVAL_NONE;

  SetLength(Result^.Groups, 0);

  {$IFDEF GTALK}
  Result^.GTalk_T := GTalk_T;
  {$ENDIF}

  if (Group <> '') then
  begin
    SetLength(Result^.Groups, Length(Result^.Groups)+1);
    Result^.Groups[Length(Result^.Groups)-1] := Group;
  end;
end;

{*****************************************************************}
procedure TXmppRoster.Send_RequestRoster;
begin
  FRequestedOnce := True;

  {$IFDEF XMPP}
  FSocket.Send(XmlTagIqQuery(XVAL_GET, FSocket.NewSeqNum(SEQT_ROSTER), XNS_JABBER_IQ_ROSTER));
  {$ENDIF}

  {$IFDEF GTALK}
  FSocket.Send(GoogleRosterIqQuery(XVAL_GET, FSocket.NewSeqNum(SEQT_ROSTER)));
  {$ENDIF}
end;
{*****************************************************************}
procedure TXmppRoster.Rcvd_Roster(Node: TXmlNode);
var i: Integer;
begin
  ClearRosterItems;

  for i := 0 to Node.ChildContainerCount-1 do
    Rcvd_RosterItemOper(Node.ChildContainers[i], True);
end;

{*****************************************************************}
function TXmppRoster.EnumItems(var iCounter: Integer; var aRosterItem: pRosterItem): Boolean;
begin
  Result := False;
  aRosterItem := nil;
  if (FListItems.Count = 0) or (iCounter < 0) or (iCounter > Pred(FListItems.Count)) then Exit;

  aRosterItem := FListItems.Items[iCounter];
  Inc(iCounter);

  Result := True;
end;

{*****************************************************************}
function TXmppRoster.IsAuthRequired(aRosterItem: pRosterItem): Boolean;
begin
  Result := True;
  if (aRosterItem = nil) then Exit;
  Result := WideSameText(aRosterItem^.Subscr, XVAL_NONE) or WideSameText(aRosterItem^.Subscr, XVAL_FROM);
end;

{*****************************************************************}
function TXmppRoster.GetRosterItem(const Jid: string): pRosterItem;
var aItem: pRosterItem;
    i: integer;
begin
  Result := nil;

  aItem := nil;
  i := 0;
  while EnumItems(i, aItem) do
  begin
    if WideSameText(aItem^.JID, Jid) then
    begin
      Result := aItem;
      Break;
    end;
  end;//while
end;

{*****************************************************************}
procedure TXmppRoster.Send_DelRosterItem(const Jid: string);
var aItem: pRosterItem;
    sAtts: string;
begin
  aItem := GetRosterItem(Jid);
  if (aItem = nil) then Exit;

  sAtts := XmlAtt(XATT_JID, aItem^.JID) + THE_SPC + XmlAtt(XATT_SUBSCRIPTION, XVAL_REMOVE);

  {$IFDEF GTALK}
  FSocket.Send(GoogleRosterIqQuery(XVAL_SET, FSocket.NewSeqNum(SEQT_ROSTER_OPER, aItem^.JID, ROSTER_OPER_DEL, MakeItemCopy(aItem)), XmlTag(XTAG_ITEM_EMPTY, sAtts)));
  {$ENDIF}

  {$IFDEF XMPP}
  FSocket.Send(XmlTagIqQuery(XVAL_SET, FSocket.NewSeqNum(SEQT_ROSTER_OPER, aItem^.JID, ROSTER_OPER_DEL, MakeItemCopy(aItem)), XNS_JABBER_IQ_ROSTER, XmlTag(XTAG_ITEM_EMPTY, sAtts)));
  {$ENDIF}
end;

{*****************************************************************}
procedure TXmppRoster.Send_AddRosterItem(const Jid, Name, Group: string {$IFDEF GTALK}; const GTalk_T: string {$ENDIF});
var sAtts, sTags: string;
begin
  if (GetRosterItem(Jid) <> nil) then Exit;

  sAtts := XmlAtt(XATT_JID, Jid);
  if (Name <> '') then
    sAtts := sAtts + THE_SPC + XmlAtt(XATT_NAME, Name);

  {$IFDEF GTALK}
  if (GTalk_T <> '') then
    sAtts := sAtts + THE_SPC + XmlAtt(XATT_GT_GR_T, GTalk_T);

  sTags := XmlTag(XTAG_ITEM_EMPTY, sAtts);

  FSocket.Send(GoogleRosterIqQuery(XVAL_SET, FSocket.NewSeqNum(SEQT_ROSTER_OPER, Jid, ROSTER_OPER_ADD, MakeNewItem(Jid, Name, Group, GTalk_T)), sTags));
  {$ENDIF}

  {$IFDEF XMPP}
  if (Group <> '') then
    sTags := XmlTag(XTAG_ITEM, sAtts, XmlTag(XTAG_GROUP, Group))
  else
    sTags := XmlTag(XTAG_ITEM_EMPTY, sAtts);

  FSocket.Send(XmlTagIqQuery(XVAL_SET, FSocket.NewSeqNum(SEQT_ROSTER_OPER, Jid, ROSTER_OPER_ADD, MakeNewItem(Jid, Name, Group)), XNS_JABBER_IQ_ROSTER, sTags));
  {$ENDIF}
end;

{*****************************************************************}
procedure TXmppRoster.Send_UpdRosterItem(const Jid, Name, Group: string {$IFDEF GTALK}; const GTalk_T: string {$ENDIF});
var aItem: pRosterItem;
    sAtts, sTags: string;
begin
  aItem := GetRosterItem(Jid);
  if (aItem = nil) then Exit;

  sAtts := XmlAtt(XATT_JID, aItem^.JID) + THE_SPC + XmlAtt(XATT_NAME, Name);

  //not used for updates in RFC
  //  sAtts := sAtts + THE_SPC + XmlAtt(XATT_SUBSCRIPTION, aItem^.Subscr);
  //if (aItem^.Ask <> '') then
  //  sAtts := sAtts + THE_SPC + XmlAtt(XATT_ASK, aItem^.Ask);

  {$IFDEF GTALK}
  if (GTalk_T <> '') then
    sAtts := sAtts + THE_SPC + XmlAtt(XATT_GT_GR_T, GTalk_T);

  sTags := XmlTag(XTAG_ITEM_EMPTY, sAtts);

  FSocket.Send(GoogleRosterIqQuery(XVAL_SET, FSocket.NewSeqNum(SEQT_ROSTER_OPER, aItem^.JID, ROSTER_OPER_UPD, MakeItemCopy(aItem)), sTags));
  {$ENDIF}

  {$IFDEF XMPP}
  if (Group <> '') then
    sTags := XmlTag(XTAG_ITEM, sAtts, XmlTag(XTAG_GROUP, Group))
  else
    sTags := XmlTag(XTAG_ITEM_EMPTY, sAtts);

  FSocket.Send(XmlTagIqQuery(XVAL_SET, FSocket.NewSeqNum(SEQT_ROSTER_OPER, aItem^.JID, ROSTER_OPER_UPD, MakeItemCopy(aItem)), XNS_JABBER_IQ_ROSTER, sTags));
  {$ENDIF}
end;

{$IFDEF XMPP}
{*****************************************************************}
function TXmppRoster.GroupInArray(const Group: string; const Groups: TArrayGroups): Boolean;
var i: Integer;
begin
  Result := False;
  if (Length(Groups) = 0) then Exit;

  for i := 0 to Length(Groups)-1 do
  begin
    if (Groups[i] = Group) then
    begin
      Result := True;
      Break;
    end;
  end;//for
end;
{$ENDIF}

{*****************************************************************}
procedure TXmppRoster.Rcvd_RosterItemOper(ItemNode: TXmlNode; const NewRosterRcvd: Boolean = False);
var aItem: pRosterItem;
    sJid, sName, sSubscr, sAsk: string;
    GroupNode: TXmlNode;
    Groups: TArrayGroups;
    i: Integer;
    iOper: Byte;
    {$IFDEF XMPP}
     sGroupCur, sGroupNew: string;
    {$ENDIF}
    {$IFDEF GTALK}
    sGTalk_T: string;
    {$ENDIF}
begin
  if not WideSameText(UTF8ToUnicodeString(ItemNode.Name), XNS_ITEM) then Exit;

  sJid := ItemNode.AttributeValueByNameWide[UTF8Encode(XATT_JID)];
  if (sJid = '') then Exit;

  sName   := ItemNode.AttributeValueByNameWide[UTF8Encode(XATT_NAME)];
  sSubscr := ItemNode.AttributeValueByNameWide[UTF8Encode(XATT_SUBSCRIPTION)];
  sAsk    := ItemNode.AttributeValueByNameWide[UTF8Encode(XATT_ASK)];
  if (sSubscr = '') then sSubscr := XVAL_NONE;

  {$IFDEF GTALK}
  sGTalk_T := ItemNode.AttributeValueByNameWide[UTF8Encode(XATT_GT_ROS_T)];
  {$ENDIF}

  for i := 0 to ItemNode.ChildContainerCount-1 do
  begin
    GroupNode := ItemNode.ChildContainers[i];
    if WideSameText(UTF8ToUnicodeString(GroupNode.Name), XNS_GROUP) then
    begin
      SetLength(Groups, Length(Groups)+1);
      Groups[Length(Groups)-1] := GroupNode.ValueUnicode;
    end;
  end;//for

  iOper := ROSTER_OPER_NON;

  aItem := nil;
  //if not roster rcvd operation then search existing item
  if not NewRosterRcvd then
    aItem := GetRosterItem(sJid);

  //if add new item
  if (aItem = nil) then
  begin
    if not WideSameText(sSubscr, XVAL_REMOVE) then
    begin
      aItem := MakeNewItem(sJid, sName, ''{$IFDEF GTALK}, sGTalk_T{$ENDIF});
      FListItems.Add(aItem);

      aItem^.Subscr  := sSubscr;
      aItem^.PrevAsk := sAsk;
      aItem^.Ask     := sAsk;

      for i := 0 to Length(Groups)-1 do
      begin
        SetLength(aItem^.Groups, Length(aItem^.Groups)+1);
        aItem^.Groups[Length(aItem^.Groups)-1] := Groups[i];
      end;//for

      {$IFDEF GTALK}
      //do not allow paasing hidden items oper to bimoid
      if WideSameText(sGTalk_T, GT_HIDDEN) then
        iOper := ROSTER_OPER_NON
      else
        iOper := ROSTER_OPER_ADD;
      {$ENDIF}

      {$IFDEF XMPP}
      iOper := ROSTER_OPER_ADD;
      {$ENDIF}
    end;
  end
  else
  begin
    //if del item
    if WideSameText(sSubscr, XVAL_REMOVE) then
    begin
      iOper := ROSTER_OPER_DEL;
    end
    else
    begin
      //if upd item
      if (aItem^.Name <> sName) then
      begin
        aItem^.Name := sName;
        iOper := ROSTER_OPER_UPD;
      end;

      if not WideSameText(aItem^.Subscr, sSubscr) then
      begin
        if (  ( WideSameText(aItem^.Subscr, XVAL_NONE) or WideSameText(aItem^.Subscr, XVAL_FROM) ) and
              ( WideSameText(sSubscr, XVAL_TO) or WideSameText(sSubscr, XVAL_BOTH) )  ) or
           (  ( WideSameText(aItem^.Subscr, XVAL_TO) or WideSameText(aItem^.Subscr, XVAL_BOTH) ) and
              ( WideSameText(sSubscr, XVAL_NONE) or WideSameText(sSubscr, XVAL_FROM) )  ) then
        begin
          //this condition is mostly for bimoid transport
          iOper := ROSTER_OPER_UPD;
        end;

        //update cur subscription state
        aItem^.Subscr := sSubscr;
      end;

      //save previous subscription state for detecting revoke or auth decline
      aItem^.PrevAsk := aItem^.Ask;

      aItem^.Ask := sAsk;

      {$IFDEF GTALK}
      if GTalk_Check_T_Param(aItem, sGTalk_T, iOper) then
        sJid := '-' //to pass into rosterchanged event
      else
      begin
        //do not allow paasing hidden items oper to bimoid
        if WideSameText(aItem^.GTalk_T, sGTalk_T) and WideSameText(sGTalk_T, GT_HIDDEN) then
          iOper := ROSTER_OPER_NON;
      end;

      aItem^.GTalk_T := sGTalk_T;
      {$ENDIF}

      {$IFDEF XMPP}
      //get current group
      sGroupCur := '';
      if (Length(aItem^.Groups) > 0) then sGroupCur := aItem^.Groups[0];

      //find existing cur group
      sGroupNew := '';
      if (Length(Groups) > 0) then
      begin
        if (sGroupCur <> '') and GroupInArray(sGroupCur, Groups) then
          sGroupNew := sGroupCur
        else
          sGroupNew := Groups[0];
      end;

      //set new cur group if changed
      if (sGroupCur <> sGroupNew) then
      begin
        SetLength(aItem^.Groups, 0);

        if (Length(Groups) > 0) then
          aItem^.Groups := Copy(Groups); //copy array

        iOper := ROSTER_OPER_UPD;
      end
      else
      begin
        //just copy other new groups if present
        if (Length(Groups) > 0) then
        begin
          for i := 0 to Length(Groups)-1 do
          begin
            if not GroupInArray(Groups[i], aItem^.Groups) then
            begin
              SetLength(aItem^.Groups, Length(aItem^.Groups)+1);
              aItem^.Groups[Length(aItem^.Groups)-1] := Groups[i];
            end;
          end;//for
        end;
      end;
      {$ENDIF}

    end;
  end;

  //notify
  if not NewRosterRcvd then
  begin
    //if important oper and not own oper push then notify
    if (iOper > ROSTER_OPER_NON) and not FSocket.IsSeqTypeExists(SEQT_ROSTER_OPER, sJid) then
    begin
      if Assigned(FOnRosterChanged) then FOnRosterChanged(Self, aItem, iOper);
    end;

    {$IFDEF XMPP} //GTALK must not delete item
    //del item if needed only after notification
    if (iOper = ROSTER_OPER_DEL) then
      DelRosterItem(aItem);
    {$ENDIF}
  end;
end;

{*****************************************************************}
procedure TXmppRoster.CancelRosterOper(const Jid: string; const RosterOper: Byte; aCopyItem: pRosterItem);
var aItem: pRosterItem;
    iOper: Byte;
begin
  if (aCopyItem = nil) then Exit;

  iOper := ROSTER_OPER_NON;
  aItem := GetRosterItem(Jid);

  case RosterOper of
   {=========================================}
    ROSTER_OPER_ADD:
      begin
        iOper := ROSTER_OPER_DEL;
        if (aItem <> nil) then DelRosterItem(aItem);
      end;
   {=========================================}
    ROSTER_OPER_UPD:
      begin
        iOper := ROSTER_OPER_UPD;
        if (aItem <> nil) then DelRosterItem(aItem);
        FListItems.Add(MakeItemCopy(aCopyItem));
      end;
   {=========================================}
    ROSTER_OPER_DEL:
      begin
        iOper := ROSTER_OPER_ADD;
        if (aItem = nil) then
          FListItems.Add(MakeItemCopy(aCopyItem));
      end;
   {=========================================}
  end;//case

  if (iOper > ROSTER_OPER_NON) then
  begin
    if Assigned(FOnRosterChanged) then FOnRosterChanged(Self, aCopyItem, iOper);
  end;
end;

{*****************************************************************}
function TXmppRoster.GetJidCurResInfo(const Jid: string): pResInfo;
var aItem: pRosterItem;
begin
  Result := nil;

  aItem := GetRosterItem(Jid);
  if (aItem = nil) then Exit;

  Result := aItem^.GetRes(aItem^.LastResource);
  if (Result = nil) then
    Result := aItem^.GetHighestPriorityRes;
end;

{$IFDEF GTALK}
{*****************************************************************}
function TXmppRoster.GTalk_Check_T_Param(aItem: pRosterItem; const New_T_Param: string; var Oper: Byte): Boolean;
var i: Byte;
begin
  Result := False;

  i := 0;
  if WideSameText(New_T_Param, GT_BLOCKED) then
    i := 1
  else
  if WideSameText(New_T_Param, GT_HIDDEN) then
    i := 2
  else
  if WideSameText(New_T_Param, GT_PINNED) then
    i := 3;

  case i of
   {======================================================}
    0, 3: //unknown/empty/pinned T param
    begin
      //if was blocked or hidden add it to CL, thats how gtalk behaves
      if WideSameText(aItem^.GTalk_T, GT_BLOCKED) or WideSameText(aItem^.GTalk_T, GT_HIDDEN) then
      begin
        Oper := ROSTER_OPER_ADD;
        Result := True;
      end;
    end;
   {======================================================}
    1: //BLOCKED
    begin
      if not WideSameText(aItem^.GTalk_T, GT_BLOCKED) and not WideSameText(aItem^.GTalk_T, GT_HIDDEN) then
      begin
        Oper := ROSTER_OPER_UPD;
        Result := True;
      end
      else
      if WideSameText(aItem^.GTalk_T, GT_HIDDEN) then
      begin
        Oper := ROSTER_OPER_ADD;
        Result := True;
      end;
    end;
   {======================================================}
    2: //HIDDEN
    begin
      if not WideSameText(aItem^.GTalk_T, GT_HIDDEN) then
      begin
        Oper := ROSTER_OPER_DEL;
        Result := True;
      end;
    end;
   {======================================================}
  end;//case
end;
{$ENDIF}



end.
