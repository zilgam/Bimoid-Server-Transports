// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_xmppsock;

interface

uses Windows, SysUtils, Classes, OverbyteIcsWSocket, u_socket, h_jabber, u_compress, u_roster_info;

type
  TOnXmlRcvd = procedure (Sender: TObject; const XmlTag: string) of object;
  TOnPktLog  = procedure (Sender: TObject; const Incoming: Boolean; const XmlTag: string) of object;

  TXmppSeq = record
    SeqNum  : DWord;
    SeqType : DWord;
    StrVal  : string;
    IntTag  : Integer;
    Data    : Pointer;
  end;
  pXmppSeq = ^TXmppSeq;

  TXmppSocket = class(TObimpSocket)
  private
    FCompress    : TXmppCompress;
    FPktBuf      : RawByteString;
    FSeqNum      : DWord;
    FSeqUID      : string;
    FCollectTags : Boolean;
    FSavedTags   : RawByteString;
    FListSeq     : TList;
    FCompressed  : Boolean;
    FOnXmlRcvd   : TOnXmlRcvd;
    {$IFDEF DEBUG}
    FOnPktLog    : TOnPktLog;
    {$ENDIF}
    procedure ParseData;
    function  IsNextXmlRcvd: Boolean;
    function  GetCorrectXml: string;
    procedure ClearUnwantedTags(var sBuf: string);
    procedure ClearListSeq;
    procedure FreeSeqData(ASeq: pXmppSeq);
    function  SendData(const Data: RawByteString): Boolean;
  protected
    procedure ResetConnectionParams; override;
    procedure DoDataAvailable(var Buf: RawByteString); override;
  public
    AliveTicks : DWord;
    constructor Create;
    destructor  Destroy; override;

    function  NewSeqNum(SeqType: DWord; StrVal: string = ''; IntTag: Integer = 0; Data: Pointer = nil): string;
    procedure DelSeqNum(SeqNum: DWord);
    function  GetSeqNum(SeqNum: DWord): pXmppSeq;
    function  IsSeqTypeExists(SeqType: DWord; StrVal: string = ''): Boolean;
    procedure StartCollect;
    function  EndCollect: Boolean;
    function  Send(const Pkt: string): Boolean;

    function  StreamDecompress(var Buf: RawByteString): RawByteString;

    property  SeqUID           : string         read FSeqUID     write FSeqUID;
    property  StreamCompressed : Boolean        read FCompressed write FCompressed;
    property  OnXmlRcvd        : TOnXmlRcvd     read FOnXmlRcvd  write FOnXmlRcvd;

    {$IFDEF DEBUG}
    property  OnPktLog: TOnPktLog read FOnPktLog write FOnPktLog;
    {$ENDIF}
  end;

implementation

{ TXmppSocket }
{***************************************************************}
constructor TXmppSocket.Create;
begin
  inherited;

  FListSeq  := TList.Create;
  FCompress := TXmppCompress.Create;
end;

{***************************************************************}
destructor TXmppSocket.Destroy;
begin
  if Assigned(FCompress) then FreeAndNil(FCompress);

  ClearListSeq;
  FListSeq.Free;
  inherited;
end;

{***************************************************************}
procedure TXmppSocket.FreeSeqData(ASeq: pXmppSeq);
begin
  case ASeq^.SeqType of
   {=====================================}
    SEQT_ROSTER_OPER:
      begin
        if (ASeq^.Data <> nil) then
          Dispose(pRosterItem(ASeq^.Data));
      end;
   {=====================================}
  end;//case

  ASeq^.Data := nil;
end;

{***************************************************************}
procedure TXmppSocket.ClearListSeq;
var aSeq: pXmppSeq;
    i: integer;
begin
  for i := FListSeq.Count-1 downto 0 do
  begin
    aSeq := FListSeq.Items[i];

    FreeSeqData(aSeq);

    Dispose(aSeq);
    FListSeq.Delete(i);
  end;//for
end;

{***************************************************************}
procedure TXmppSocket.DelSeqNum(SeqNum: DWord);
var aSeq: pXmppSeq;
    i: integer;
begin
  for i := FListSeq.Count-1 downto 0 do
  begin
    aSeq := FListSeq.Items[i];

    if (aSeq^.SeqNum = SeqNum) then
    begin
      FreeSeqData(aSeq);

      Dispose(aSeq);
      FListSeq.Delete(i);

      Break;
    end;
  end;//for
end;

{***************************************************************}
function TXmppSocket.GetSeqNum(SeqNum: DWord): pXmppSeq;
var aSeq: pXmppSeq;
    i: integer;
begin
  Result := nil;

  for i := 0 to FListSeq.Count-1 do
  begin
    aSeq := FListSeq.Items[i];

    if (aSeq^.SeqNum = SeqNum) then
    begin
      Result := aSeq;
      Break;
    end;
  end;//for
end;

{***************************************************************}
function TXmppSocket.IsSeqTypeExists(SeqType: DWord; StrVal: string = ''): Boolean;
var aSeq: pXmppSeq;
    i: integer;
begin
  Result := False;

  for i := 0 to FListSeq.Count-1 do
  begin
    aSeq := FListSeq.Items[i];

    if (aSeq^.SeqType = SeqType) and WideSameText(aSeq^.StrVal, StrVal) then
    begin
      Result := True;
      Break;
    end;
  end;//for
end;

{***************************************************************}
function TXmppSocket.NewSeqNum(SeqType: DWord; StrVal: string = ''; IntTag: Integer = 0; Data: Pointer = nil): string;
var aSeq: pXmppSeq;
begin
  Result := SEQ_ID_BIMOID + FSeqUID + SEQ_DIV + IntToStr(FSeqNum);

  New(aSeq);
  ZeroMemory(aSeq, SizeOf(TXmppSeq));

  aSeq^.SeqNum  := FSeqNum;
  aSeq^.SeqType := SeqType;
  aSeq^.StrVal  := StrVal;
  aSeq^.IntTag  := IntTag;
  aSeq^.Data    := Data;

  FListSeq.Add(aSeq);
  if (FListSeq.Count > SEQ_MAX_NUM) then
  begin
    aSeq := FListSeq.Items[0];
    DelSeqNum(aSeq^.SeqNum);
  end;

  Inc(FSeqNum);
end;

{***************************************************************}
procedure TXmppSocket.ResetConnectionParams;
begin
  inherited;

  FCompressed  := False;
  if Assigned(FCompress) then FCompress.ZlibReset(True);

  FPktBuf      := '';

  FSavedTags   := '';
  FCollectTags := False;
  FSeqNum      := Random(64);
  FSeqUID      := GetRndHexBytes(2);

  AliveTicks   := 0;

  ClearListSeq;
end;

{***************************************************************}
procedure TXmppSocket.StartCollect;
begin
  FCollectTags := True;
  //FSavedTags   := ''; should not do it if somewhere will be called twice
end;

{***************************************************************}
function TXmppSocket.EndCollect: Boolean;
begin
  Result := False;

  FCollectTags := False;

  if (FSavedTags <> '') then
  begin
    AliveTicks := 0;

    try
      Result := SendData(FSavedTags);
    finally
      FSavedTags := '';
    end;
  end;
end;

{***************************************************************}
function TXmppSocket.Send(const Pkt: string): Boolean;
var sRaw: RawByteString;
begin
  Result := False;
  if (Pkt = '') then Exit;

  if FCompressed then
    sRaw := FCompress.ZlibCompress(UTF8Encode(Pkt))
  else
    sRaw := UTF8Encode(Pkt);

  {$IFDEF DEBUG}
  if Assigned(FOnPktLog) then FOnPktLog(Self, False, Pkt);
  {$ENDIF}

  if FCollectTags then
  begin
    FSavedTags := FSavedTags + sRaw;
    Result := True;
    Exit;
  end;

  Result := SendData(sRaw);
end;

{***************************************************************}
function TXmppSocket.SendData(const Data: RawByteString): Boolean;
begin
  AliveTicks := 0;

  Result := False;
  try
    if (Socket.State <> wsClosed) then
      Result := SendNetPkt(Data) > -1;
  except end;
end;

{***************************************************************}
function TXmppSocket.StreamDecompress(var Buf: RawByteString): RawByteString;
var TotalIn: Integer;
begin
  Result := FCompress.ZlibDecompress(Buf, TotalIn);

  if (TotalIn > 0) then
    Delete(Buf, 1, TotalIn);
end;

{***************************************************************}
procedure TXmppSocket.DoDataAvailable(var Buf: RawByteString);
begin
  inherited;

  FPktBuf := FPktBuf + Buf;
  ParseData;
end;

{***************************************************************}
procedure TXmppSocket.ParseData;
begin
  if (Length(FPktBuf) = 0) then
    Exit;

  //buffer overflow check
  if (Length(FPktBuf) > High(Word)) then
  begin
    FPktBuf := '';
    Exit;
  end;

  //process next XML
  if not IsNextXmlRcvd then
    Exit;

  //if there is other tags remain in packet then handle them too
  if (Length(FPktBuf) > 0) then ParseData;
end;

{***************************************************************}
function TXmppSocket.IsNextXmlRcvd: Boolean;
var sData: string;
begin
  Result := False;

  sData := GetCorrectXml;
  if (sData = '') then Exit;

  Result := True;

  {$IFDEF DEBUG}
  if Assigned(FOnPktLog) then FOnPktLog(Self, True, sData);
  {$ENDIF}

  if Assigned(FOnXmlRcvd) then FOnXmlRcvd(Socket, sData);
end;

{***************************************************************}
function TXmppSocket.GetCorrectXml: string;
var s, sBuf: string;
    i, j, iStart, iEnd: integer;
begin
  //yes, own wheel development
  Result := '';
  if (Length(FPktBuf) = 0) then Exit;

  if FCompressed then
  begin
    sBuf := Trim( UTF8ToUnicodeString( StreamDecompress(FPktBuf) ) );
    if (sBuf = '') then Exit;
  end
  else
    sBuf := Trim( UTF8ToUnicodeString(FPktBuf) );

  //first of all cut unwanted tags like:
  //<?xml version='1.0'?>, <!-- comment -->, <!DOCTYPE .. []>, <![CDATA[any chars <>]]>
  ClearUnwantedTags(sBuf);

  if (Length(sBuf) = 0) then
  begin
    FPktBuf := '';
    Exit;
  end;

  //if bad xml inside buffer then clear it
  if (sBuf[1] <> '<') then
  begin
    FPktBuf := '';
    Exit;
  end;

  //end of xml is always '>', wait for the rest part if it's not last char
  if (sBuf[Length(sBuf)] <> '>') then
    Exit;

  //find tags
  while (sBuf <> '') do
  begin
    //if not found starting tag char then clean buffer, something went wrong way
    iStart := Pos('<', sBuf);
    if (iStart = 0) then
    begin
      sBuf := '';
      Break;
    end;

    //if tag is not starting from first char then delete data before tag char
    if (iStart > 1) then
      Delete(sBuf, 1, iStart-1);

    //if end not found then wait for other part of tag
    iEnd := Pos('>', sBuf);
    if (iEnd = 0) then Break;

    //========== get stream start
    //<stream:stream ...
    i := Pos('<' + XNS_STREAM_STREAM, sBuf);
    if (i > 0) then
    begin
      //making it parsed well by replacing '>' to '/>'
      s := Copy(sBuf, i, Length(sBuf));
      j := Pos('>', s);
      if (j > 0) then
      begin
        Delete(sBuf, i, i+j-1);
        s := Copy(s, 1, j);
        j := Pos('/>', s);
        if (j = 0) then Insert('/', s, Length(s));
        Insert(s, sBuf, i);
      end;
    end;

    //========== get stream end
    //</stream:stream>
    s := '</' + XNS_STREAM_STREAM + '>';
    i := Pos(s, sBuf);
    if (i > 0) then
    begin
      //replacing stream end tag with own, to make it parsed well
      Delete(sBuf, i, Length(s));
      Insert(XTAG_BYE, sBuf, i);
    end;

    //========== count other tags start/ends
    iStart := 0;
    iEnd   := 0;

    s := sBuf;

    //count ending tags 1
    i := Pos('/>', s);
    while (i > 0) do
    begin
      Inc(iEnd);
      s[i]   := '*';
      s[i+1] := '*';

      i := Pos('/>', s);
    end;//while

    //count ending tags 2
    i := Pos('</', s);
    while (i > 0) do
    begin
      Inc(iEnd);
      s[i]   := '*';
      s[i+1] := '*';

      i := Pos('</', s);
    end;//while

    //count starting tags, we counting starting after ending to prevent detecting '</' tags as starting
    i := Pos('<', s);
    while (i > 0) do
    begin
      Inc(iStart);

      s[i] := '*';

      i := Pos('<', s);
    end;//while

    //if starting and ending are equal then it seems we have got all data
    if (iStart = iEnd) then
    begin
      Result := sBuf;
      sBuf   := '';
    end
    else
    //clear buffer if start tags less than end tags
    if (iStart < iEnd) then
    begin
      Result := '';
      sBuf   := '';
    end
    else
    begin
      //wait for rest tag data
      Break;
    end;
  end;//while

  //update buffer
  if not FCompressed then
    FPktBuf := UTF8Encode(sBuf);
end;

{***************************************************************}
procedure TXmppSocket.ClearUnwantedTags(var sBuf: string);
var i, j, k: integer;
begin
  //====== <?xml version='1.0'?>
  i := Pos('<?', sBuf);
  while (i > 0) do
  begin
    j := Pos('?>', sBuf);

    if (i < j) then
      Delete(sBuf, i, j-i+2)
    else
      Break;

    i := Pos('<?', sBuf);
  end;//while

  //====== <!-- comment -->
  i := Pos('<!--', sBuf);
  while (i > 0) do
  begin
    j := Pos('-->', sBuf);

    if (i < j) then
      Delete(sBuf, i, j-i+3)
    else
      Break;

    i := Pos('<!--', sBuf);
  end;//while

  //====== <![CDATA[any chars <>]]>
  i := Pos('<![CDATA[', sBuf);
  while (i > 0) do
  begin
    j := Pos(']]>', sBuf);

    if (i < j) then
      Delete(sBuf, i, j-i+3)
    else
      Break;

    i := Pos('<![CDATA[', sBuf);
  end;//while

  //====== <!DOCTYPE [elements]>
  i := Pos('<!', sBuf);
  while (i > 0) do
  begin
    j := Pos('>', sBuf);
    k := Pos('[', sBuf);

    if (k > 0) and (k < j) then
    begin
      k := Pos(']>', sBuf);
      if (k > 0) then Inc(k);
    end
    else
      k := j;

    if (i < k) then
      Delete(sBuf, i, k-i+1)
    else
      Break;

    i := Pos('<!', sBuf);
  end;//while

  //===== prepare for parsing
  sBuf := Trim(sBuf);
end;



end.

