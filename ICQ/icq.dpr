// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

library icq;

  //=========== reducing dll size
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}

uses
  Windows,
  u_proto in 'u_proto.pas',
  u_obimp_const in '..\..\u_obimp_const.pas',
  u_ext_info in '..\..\u_ext_info.pas',
  u_obimp_codes in '..\..\u_obimp_codes.pas',
  u_oscar_timer in 'oscar\u_oscar_timer.pas',
  h_oscar in 'oscar\h_oscar.pas',
  u_oscar_cli in 'oscar\u_oscar_cli.pas',
  u_oscar_fb in 'oscar\u_oscar_fb.pas',
  u_oscar_item in 'oscar\u_oscar_item.pas',
  u_convert in 'u_convert.pas',
  u_oscar_sock in 'oscar\u_oscar_sock.pas',
  u_oscar_caps in 'oscar\u_oscar_caps.pas',
  u_oscar_hlp in 'oscar\u_oscar_hlp.pas',
  u_conv_codes in 'u_conv_codes.pas',
  u_oscar_avatars in 'oscar\u_oscar_avatars.pas',
  u_params in 'u_params.pas';

{$R *.res}

{***************************************************************}
function GetExtensionInfo: pExtensionInfo; stdcall;
begin
  Result := @ExtInfo;
end;

{***************************************************************}
function CreateExtenInstanceTP(UniqID: DWord; EventsTP: IEventsTP): IExtensionTP; stdcall;
begin
  Result := TIcqProto.Create(UniqID, EventsTP);
end;

exports
  GetExtensionInfo name FUNC_NAME_EXTENSION_INFO,
  CreateExtenInstanceTP name FUNC_NAME_CREATE_INSTANCE_TP;

begin

end.
