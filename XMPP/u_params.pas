// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_params;

interface

uses u_ext_info, u_obimp_const;

{$message 'always check Transports version in params on release!'}

const
  //filling own user supported details fields
  ExtDetailsFields : TExtDetailsFields =
    (
    NickNameField       : True;
    FirstNameField      : True;
    LastNameField       : True;
    CountryCodeField    : False;
    RegionStateField    : True;
    CityField           : True;
    ZipCodeField        : True;
    AddressField        : True;
    LangCode1Field      : False;
    LangCode2Field      : False;
    GenderField         : False;
    BirthdayField       : True;
    HomePageField       : True;
    AboutField          : True;
    InterestsField      : False;
    Email1Field         : True;
    Email2Field         : True;
    HomePhoneField      : True;
    WorkPhoneField      : True;
    CellPhoneField      : True;
    FaxNumField         : True;
    CompanyField        : True;
    DivDeptField        : True;
    PositionField       : True;
    );

  //filling own user supported search fields
{  ExtSearchFields : TExtSearchFields =
    (
    AccountField        : True;
    EmailField          : True;
    NickNameField       : True;
    FirstNameField      : True;
    LastNameField       : True;
    GenderField         : True;
    AgeField            : True;
    CountryCodeField    : True;
    CityField           : True;
    LangCodeField       : True;
    InterestsField      : True;
    ZodiacCodeField     : False;
    OnlineOnlyField     : True;
    );
}
  //filling transport details
  {$IFDEF XMPP}
  ExtParams : TExtParamsTP =
    (
    TransportFullName   : 'Jabber';
    TransportShortName  : 'Jabber'; //without spaces!!!
    AccountIDsName      : 'JID:';
    DefServerHostIP     : '';
    DefServerPortNum    : 5222;
    PresStatusesArray   : (PRES_STATUS_INVISIBLE,
                           PRES_STATUS_INVISIBLE_FOR_ALL,
                           PRES_STATUS_AWAY,
                           PRES_STATUS_NOT_AVAILABLE,
                           PRES_STATUS_DO_NOT_DISTURB,
                           PRES_STATUS_FREE_FOR_CHAT,
                           0,
                           0,
                           0,
                           0);
    AdditStPicsSupport  : False;
    AdditStPicsCount    : 0;
    AddAccsSupport      : True;
    UpdAccsSupport      : True;
    DelAccsSupport      : True;
    VisListSupport      : True;
    InvisListSupport    : True;
    IgnoreListSupport   : True;
    MoveIgnoreSupport   : True;
    AuthRequestSupport  : True;
    AuthRevokeSupport   : True;
    MsgAcksSupport      : True;
    NotifMsgsSupport    : True;
    DetailsReqSupport   : True;
    UpdOwnDetsSupport   : True;
    UsersSearchSupport  : False;
    AvatarsSupport      : True;
    UpdOwnAvatarSupport : True;
    OffMsgSupport       : True;
    PresInfoReqSupport  : False;
    AuthReqMsgsSupport  : False;
    AuthRevMsgsSupport  : False;
    OwnMailUrlSupport   : False;
    DetsFieldsSupport   : @ExtDetailsFields; //can be nil if not supported
    SearchFieldsSupport : nil;//@ExtSearchFields;  //can be nil if not supported
    );

  ExtInfo: TExtensionInfo =
    (
    ExtType    : EXT_TYPE_TRANSPORT;
    ExtUUID    : ($2E,$14,$41,$75,$31,$94,$4A,$44,$85,$94,$F1,$E4,$50,$60,$BB,$E0); //MUST BE unique for every new extension/transport
    ExtName    : 'Jabber/XMPP transport';
    ExtVer     : '1.0.5';
    ExtAuthor  : 'Bimoid';
    ExtInfoURL : 'http://www.bimoid.com';
    ExtParams  : @ExtParams;
    ExtTimer   : True;
    );
  {$ENDIF}

  {$IFDEF GTALK}
  ExtParams : TExtParamsTP =
    (
    TransportFullName   : 'Google Talk';
    TransportShortName  : 'GTalk'; //without spaces!!!
    AccountIDsName      : 'JID:';
    DefServerHostIP     : '';
    DefServerPortNum    : 5222;
    PresStatusesArray   : (PRES_STATUS_INVISIBLE,
                           PRES_STATUS_AWAY,
                           PRES_STATUS_NOT_AVAILABLE,
                           PRES_STATUS_DO_NOT_DISTURB,
                           PRES_STATUS_FREE_FOR_CHAT,
                           0,
                           0,
                           0,
                           0,
                           0);
    AdditStPicsSupport  : False;
    AdditStPicsCount    : 0;
    AddAccsSupport      : True;
    UpdAccsSupport      : True;
    DelAccsSupport      : True;
    VisListSupport      : False;
    InvisListSupport    : False;
    IgnoreListSupport   : False;
    MoveIgnoreSupport   : True;
    AuthRequestSupport  : True;
    AuthRevokeSupport   : True;
    MsgAcksSupport      : True;
    NotifMsgsSupport    : True;
    DetailsReqSupport   : True;
    UpdOwnDetsSupport   : False;
    UsersSearchSupport  : False;
    AvatarsSupport      : True;
    UpdOwnAvatarSupport : True;
    OffMsgSupport       : True;
    PresInfoReqSupport  : False;
    AuthReqMsgsSupport  : False;
    AuthRevMsgsSupport  : False;
    OwnMailUrlSupport   : True;
    DetsFieldsSupport   : @ExtDetailsFields; //can be nil if not supported
    SearchFieldsSupport : nil;//@ExtSearchFields;  //can be nil if not supported
    );

  ExtInfo: TExtensionInfo =
    (
    ExtType    : EXT_TYPE_TRANSPORT;
    ExtUUID    : ($3E,$B0,$C4,$7C,$D9,$79,$49,$23,$8D,$7E,$F3,$A6,$12,$0E,$A2,$C0); //MUST BE unique for every new extension/transport
    ExtName    : 'Google Talk transport';
    ExtVer     : '1.0.5';
    ExtAuthor  : 'Bimoid';
    ExtInfoURL : 'http://www.bimoid.com';
    ExtParams  : @ExtParams;
    ExtTimer   : True;
    );
  {$ENDIF}

  //Options IDs
  OPT_ID_RESOURCE_NAME  = 1;
  OPT_ID_PRIORITY       = 2;

  {$IFDEF XMPP}
  //Default opts
  MAX_EXT_OPTS = 2;
  {$ENDIF}

  {$IFDEF GTALK}
  OPT_ID_SAVE_HIST      = 3;
  OPT_ID_MAIL_NOTIF     = 4;

  MAX_EXT_OPTS = 4;
  {$ENDIF}

type
  TOptionsArray = array[0..MAX_EXT_OPTS-1] of TExtOption;

const
  ExtDefOpts: TOptionsArray = (
    (OptID: OPT_ID_RESOURCE_NAME; OptType: TP_OT_UTF8; OptFlags: TP_OF_EDIT;  OptName: ''; OptValue: 'Bimoid'),
    (OptID: OPT_ID_PRIORITY;      OptType: TP_OT_BYTE; OptFlags: TP_OF_COMBO; OptName: ''; OptValue: '148'{<-index of 20})
    {$IFDEF GTALK},
    (OptID: OPT_ID_SAVE_HIST;     OptType: TP_OT_BOOL; OptFlags: TP_OF_CHECK; OptName: ''; OptValue: '1'),
    (OptID: OPT_ID_MAIL_NOTIF;    OptType: TP_OT_BOOL; OptFlags: TP_OF_CHECK; OptName: ''; OptValue: '1')
    {$ENDIF}
    );

implementation

end.
