// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_rosterdelim;

interface

uses Windows, Classes, SysUtils, h_jabber, u_xepclass, Character;

// XEP-0083: Nested Roster Groups

type
  TRosterDelimiter = class(TXepClass)
  private
    FGroupsDelimiter : string;
    FNestingDisabled : Boolean;
    procedure SetGroupsDelimiter(const Value: string);
  public
    procedure ResetData; override;

    property GroupsDelimiter : string   read FGroupsDelimiter write SetGroupsDelimiter;
    property NestingDisabled : Boolean  read FNestingDisabled;
  end;

implementation

{ TRosterDelimiter }
{*****************************************************************}
procedure TRosterDelimiter.ResetData;
begin
  inherited;
  FGroupsDelimiter := '';
  FNestingDisabled := False;
end;

{*****************************************************************}
procedure TRosterDelimiter.SetGroupsDelimiter(const Value: string);
begin
  FGroupsDelimiter := Value;
  FNestingDisabled := (Length(Value) = 1) and TCharacter.IsLetterOrDigit(Value[1]);
end;

end.
