// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

library xmpp;

  //=========== reducing dll size
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}

uses
  Windows,
  u_proto in 'u_proto.pas',
  u_params in 'u_params.pas',
  u_obimp_const in '..\..\u_obimp_const.pas',
  u_ext_info in '..\..\u_ext_info.pas',
  u_obimp_codes in '..\..\u_obimp_codes.pas',
  u_timer in 'proto\u_timer.pas',
  h_jabber in 'proto\h_jabber.pas',
  u_jabber in 'proto\u_jabber.pas',
  u_socket in 'proto\u_socket.pas',
  u_auth in 'proto\u_auth.pas',
  u_compress in 'proto\u_compress.pas',
  u_privacy in 'proto\u_privacy.pas',
  u_roster in 'proto\u_roster.pas',
  u_rosterdelim in 'proto\u_rosterdelim.pas',
  u_xepclass in 'proto\u_xepclass.pas',
  u_xmppsock in 'proto\u_xmppsock.pas',
  u_convert in 'u_convert.pas',
  u_privacy_list in 'proto\u_privacy_list.pas',
  u_roster_info in 'proto\u_roster_info.pas',
  u_jabber_hlp in 'proto\u_jabber_hlp.pas';

{$R *.res}

{***************************************************************}
function GetExtensionInfo: pExtensionInfo; stdcall;
begin
  Result := @ExtInfo;
end;

{***************************************************************}
function CreateExtenInstanceTP(UniqID: DWord; EventsTP: IEventsTP): IExtensionTP; stdcall;
begin
  Result := TXmpProto.Create(UniqID, EventsTP);
end;

exports
  GetExtensionInfo name FUNC_NAME_EXTENSION_INFO,
  CreateExtenInstanceTP name FUNC_NAME_CREATE_INSTANCE_TP;

begin

end.
