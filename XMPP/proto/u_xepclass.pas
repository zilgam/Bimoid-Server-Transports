// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_xepclass;

interface

type
  TXepClass = class
  private
    FSupported : Boolean;
  public
    constructor Create;
    procedure ResetData; virtual;
    property  Supported : Boolean read FSupported write FSupported;
  end;


implementation

{ TXepClass }
{*****************************************************************}
constructor TXepClass.Create;
begin
  inherited;
  FSupported := True;
end;

{*****************************************************************}
procedure TXepClass.ResetData;
begin
  FSupported := True;
end;

end.
