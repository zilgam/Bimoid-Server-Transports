// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_params;

interface

uses u_ext_info, u_obimp_const, h_oscar;

{$message 'always check Transports version in params on release!'}

const
  //filling own user supported details fields
  ExtDetailsFields : TExtDetailsFields =
    (
    NickNameField       : True;
    FirstNameField      : True;
    LastNameField       : True;
    CountryCodeField    : True;
    RegionStateField    : True;
    CityField           : True;
    ZipCodeField        : True;
    AddressField        : True;
    LangCode1Field      : True;
    LangCode2Field      : True;
    GenderField         : True;
    BirthdayField       : True;
    HomePageField       : True;
    AboutField          : True;
    InterestsField      : True;
    Email1Field         : False;
    Email2Field         : False;
    HomePhoneField      : True;
    WorkPhoneField      : True;
    CellPhoneField      : True;
    FaxNumField         : True;
    CompanyField        : True;
    DivDeptField        : True;
    PositionField       : True;
    );

  //filling own user supported search fields
  ExtSearchFields : TExtSearchFields =
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

  //filling transport details
  ExtParams : TExtParamsTP =
    (
    TransportFullName   : 'ICQ';
    TransportShortName  : 'ICQ'; //without spaces!!!
    AccountIDsName      : 'ICQ#';
    DefServerHostIP     : 'login.icq.com';
    DefServerPortNum    : 5190;
    PresStatusesArray   : (PRES_STATUS_INVISIBLE,
                           PRES_STATUS_INVISIBLE_FOR_ALL,
                           PRES_STATUS_AWAY,
                           PRES_STATUS_NOT_AVAILABLE,
                           PRES_STATUS_OCCUPIED,
                           PRES_STATUS_DO_NOT_DISTURB,
                           PRES_STATUS_FREE_FOR_CHAT,
                           PRES_STATUS_AT_HOME,
                           PRES_STATUS_AT_WORK,
                           PRES_STATUS_LUNCH);
    AdditStPicsSupport  : True;
    AdditStPicsCount    : STPIC_MAX_COUNT;
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
    UsersSearchSupport  : True;
    AvatarsSupport      : True;
    UpdOwnAvatarSupport : True;
    OffMsgSupport       : True;
    PresInfoReqSupport  : True;
    AuthReqMsgsSupport  : True;
    AuthRevMsgsSupport  : True;
    OwnMailUrlSupport   : False;
    DetsFieldsSupport   : @ExtDetailsFields; //can be nil if not supported
    SearchFieldsSupport : @ExtSearchFields;  //can be nil if not supported
    );

  ExtInfo: TExtensionInfo =
    (
    ExtType    : EXT_TYPE_TRANSPORT;
    ExtUUID    : ($11,$34,$5C,$70,$6C,$5A,$4F,$24,$8C,$CA,$94,$E1,$C8,$07,$16,$E8); //MUST BE unique for every new extension/transport
    ExtName    : 'ICQ transport';
    ExtVer     : '1.0.11';
    ExtAuthor  : 'Bimoid';
    ExtInfoURL : 'http://www.bimoid.com';
    ExtParams  : @ExtParams;
    ExtTimer   : True;
    );


  //Options IDs
  OPT_ID_SHOW_ST_SEARCH = 1;
  OPT_ID_USERS_MUSTAUTH = 2;
  OPT_ID_PRIVACY_LEVEL  = 3;
  OPT_ID_DISABLE_MULTIS = 4;
  OPT_ID_ALLOW_UNS_UPDT = 5;

  //Default opts
  MAX_EXT_OPTS = 5;

type
  TOptionsArray = array[0..MAX_EXT_OPTS-1] of TExtOption;

const
  ExtDefOpts: TOptionsArray = (
    (OptID: OPT_ID_SHOW_ST_SEARCH; OptType: TP_OT_BOOL; OptFlags: TP_OF_CHECK; OptName: ''; OptValue: '0'),
    (OptID: OPT_ID_USERS_MUSTAUTH; OptType: TP_OT_BOOL; OptFlags: TP_OF_CHECK; OptName: ''; OptValue: '1'),
    (OptID: OPT_ID_PRIVACY_LEVEL;  OptType: TP_OT_BYTE; OptFlags: TP_OF_COMBO; OptName: ''; OptValue: '2'),
    (OptID: OPT_ID_DISABLE_MULTIS; OptType: TP_OT_BOOL; OptFlags: TP_OF_CHECK; OptName: ''; OptValue: '0'),
    (OptID: OPT_ID_ALLOW_UNS_UPDT; OptType: TP_OT_BOOL; OptFlags: TP_OF_CHECK; OptName: ''; OptValue: '0')
    );

implementation

end.
