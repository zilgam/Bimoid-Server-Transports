// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_compress;

interface

uses Windows, SysUtils, Classes, h_jabber, Zlib;

type
  TXmppCompress = class
  private
    FStreamIn  : TZStreamRec;
    FStreamOut : TZStreamRec;
	  FZlibReady : Boolean;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure ZlibReset(Init: Boolean);
	  function  ZlibCompress(const Data: RawByteString): RawByteString;
	  function  ZlibDecompress(const Data: RawByteString; out TotalIn: Integer): RawByteString;
  end;


implementation

const
  Z_BUF_SIZE = 2048;

{ TXmppCompress }
{***************************************************************}
constructor TXmppCompress.Create;
begin
  ZlibReset(True);
end;

{***************************************************************}
destructor TXmppCompress.Destroy;
begin
  ZlibReset(False);
  inherited;
end;

{***************************************************************}
procedure TXmppCompress.ZlibReset(Init: Boolean);
begin
  deflateEnd(FStreamOut);
  inflateEnd(FStreamIn);

  FillChar(FStreamOut, SizeOf(TZStreamRec), 0);
  FillChar(FStreamIn,  SizeOf(TZStreamRec), 0);

  if Init then
  	FZlibReady := (deflateInit_(FStreamOut, Z_BEST_COMPRESSION, ZLIB_VERSION, SizeOf(TZStreamRec)) = Z_OK) and
                  (inflateInit_(FStreamIn, ZLIB_VERSION, SizeOf(TZStreamRec)) = Z_OK )
  else
    FZlibReady := False;
end;

{***************************************************************}
function TXmppCompress.ZlibCompress(const Data: RawByteString): RawByteString;
var buf: array[0..Z_BUF_SIZE-1] of AnsiChar;
    len: integer;
    sRaw: RawByteString;
begin
  Result := '';
  if not FZlibReady or (Data = '') then Exit;

	FStreamOut.avail_in := Length(Data);
	FStreamOut.next_in  := PAnsiChar(Data);

  while True do
  begin
    FStreamOut.avail_out := Z_BUF_SIZE;
    FStreamOut.next_out  := buf;

    if ( deflate(FStreamOut, Z_SYNC_FLUSH) <> Z_OK ) then
      Exit;

    len := Z_BUF_SIZE - FStreamOut.avail_out;
    if (len > 0) then
    begin
      SetLength(sRaw, len);
      Move(buf, sRaw[1], len);

      Result := Result + sRaw;
    end;
  end;//while
end;

{***************************************************************}
function TXmppCompress.ZlibDecompress(const Data: RawByteString; out TotalIn: Integer): RawByteString;
var buf: array[0..Z_BUF_SIZE-1] of AnsiChar;
    sRaw: RawByteString;
    len: integer;
begin
  Result  := '';
  TotalIn := 0;
  if not FZlibReady or (Data = '') then Exit;

	FStreamIn.avail_in := Length(Data);
	FStreamIn.next_in  := PAnsiChar(Data);
  FStreamIn.total_in := 0;

  while True do
  begin
  	FStreamIn.avail_out := Z_BUF_SIZE;
	  FStreamIn.next_out  := buf;

	  if ( inflate(FStreamIn, Z_NO_FLUSH) <> Z_OK ) then
      Break;

    len := Z_BUF_SIZE - FStreamIn.avail_out;
    if (len > 0) then
    begin
      SetLength(sRaw, len);
      Move(buf, sRaw[1], len);

      Result := Result + sRaw;
    end;
  end;//while

  TotalIn := FStreamIn.total_in;
end;



end.
