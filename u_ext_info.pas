// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_ext_info;

interface

uses Windows, u_obimp_const;

const
  //============================================
  //dll exported functions names
  FUNC_NAME_EXTENSION_INFO     = 'GetExtensionInfo_v2';
  FUNC_NAME_CREATE_INSTANCE_TP = 'CreateExtenInstanceTP_v3';
  FUNC_NAME_CREATE_INSTANCE_PG = 'CreateExtenInstancePG_v3';

  //============================================
  //Available extension types:
  EXT_TYPE_TRANSPORT = $0001;
  EXT_TYPE_PLUGIN    = $0002;

  EXT_TYPE_MAX       = $0002;

  //============================================
  //Available extensions proxy types
  EXT_PROXY_TYPE_NONE    = 0;
  EXT_PROXY_TYPE_HTTP    = 1;
  EXT_PROXY_TYPE_SOCKS4  = 2;
  EXT_PROXY_TYPE_SOCKS4A = 3;
  EXT_PROXY_TYPE_SOCKS5  = 4;


type
  //common extension information
  TExtensionInfo = record
    ExtType    : Word;
    ExtUUID    : array[0..15] of Byte;
    ExtName    : WideString; //must not be empty
    ExtVer     : WideString;
    ExtAuthor  : WideString;
    ExtInfoURL : WideString;
    ExtParams  : Pointer;    //according ExtType
    ExtTimer   : Boolean;    //receive timer ticks for instances, every ~100 msecs will be called TimerTick function in the main thread
  end;
  pExtensionInfo = ^TExtensionInfo;

  ///////////////////////////////////////////////////////////////////////////////
  /// TRANSPORT extension
  ///////////////////////////////////////////////////////////////////////////////

  //transports extension parameters
  TExtParamsTP = record
    TransportFullName   : WideString; //must not be empty and must be unique name
    TransportShortName  : WideString; //must not be empty
    AccountIDsName      : WideString; //must not be empty
    DefServerHostIP     : WideString;
    DefServerPortNum    : DWord;
    PresStatusesArray   : array[0..PRES_STATUS_MAX-1] of DWord; //actual presense statuses excluding PRES_STATUS_ONLINE(supported by default)
    AdditStPicsSupport  : Boolean;
    AdditStPicsCount    : Byte;
    AddAccsSupport      : Boolean;
    UpdAccsSupport      : Boolean;
    DelAccsSupport      : Boolean;
    VisListSupport      : Boolean;
    InvisListSupport    : Boolean;
    IgnoreListSupport   : Boolean;
    MoveIgnoreSupport   : Boolean;
    AuthRequestSupport  : Boolean;
    AuthRevokeSupport   : Boolean;
    MsgAcksSupport      : Boolean;
    NotifMsgsSupport    : Boolean;
    DetailsReqSupport   : Boolean;
    UpdOwnDetsSupport   : Boolean;
    UsersSearchSupport  : Boolean;
    AvatarsSupport      : Boolean;
    UpdOwnAvatarSupport : Boolean;
    OffMsgSupport       : Boolean;
    PresInfoReqSupport  : Boolean;
    AuthReqMsgsSupport  : Boolean;
    AuthRevMsgsSupport  : Boolean;
    OwnMailUrlSupport   : Boolean;
    DetsFieldsSupport   : Pointer;
    SearchFieldsSupport : Pointer;
  end;
  pExtParamsTP = ^TExtParamsTP;


  TExtDetailsFields = record
    NickNameField       : Boolean;
    FirstNameField      : Boolean;
    LastNameField       : Boolean;
    CountryCodeField    : Boolean;
    RegionStateField    : Boolean;
    CityField           : Boolean;
    ZipCodeField        : Boolean;
    AddressField        : Boolean;
    LangCode1Field      : Boolean;
    LangCode2Field      : Boolean;
    GenderField         : Boolean;
    BirthdayField       : Boolean;
    HomePageField       : Boolean;
    AboutField          : Boolean;
    InterestsField      : Boolean;
    Email1Field         : Boolean;
    Email2Field         : Boolean;
    HomePhoneField      : Boolean;
    WorkPhoneField      : Boolean;
    CellPhoneField      : Boolean;
    FaxNumField         : Boolean;
    CompanyField        : Boolean;
    DivDeptField        : Boolean;
    PositionField       : Boolean;
  end;
  pExtDetailsFields = ^TExtDetailsFields;


  TExtSearchFields = record
    AccountField        : Boolean;
    EmailField          : Boolean;
    NickNameField       : Boolean;
    FirstNameField      : Boolean;
    LastNameField       : Boolean;
    GenderField         : Boolean;
    AgeField            : Boolean;
    CountryCodeField    : Boolean;
    CityField           : Boolean;
    LangCodeField       : Boolean;
    InterestsField      : Boolean;
    ZodiacCodeField     : Boolean;
    OnlineOnlyField     : Boolean;
  end;
  pExtSearchFields = ^TExtSearchFields;


  TExtOption = record
    OptID    : Word;
    OptType  : Byte;
    OptFlags : DWord;
    OptName  : WideString;
    OptValue : WideString;
  end;
  pExtOption = ^TExtOption;

  TExtOptionsArray = array[0..0] of TExtOption;
  pExtOptionsArray = ^TExtOptionsArray;

  TExtOptions = record
    SettingsFlags : Word;
    OptionsCount  : Word;
    OptionsArray  : pExtOptionsArray;
  end;
  pExtOptions = ^TExtOptions;


  TExtTranspSettings = record
    Account   : WideString;
    Password  : WideString;
    SrvHost   : WideString;
    SrvPort   : DWord;
    Options   : TExtOptions;
  end;
  pExtTranspSettings = ^TExtTranspSettings;


  TExtTranspManage = record
    ManCode   : Word;
    Status    : DWord;
    StPicID   : DWord;
    StPicDesc : WideString;
  end;
  pExtTranspManage = ^TExtTranspManage;


  TTranspContact = record
    AccountName : WideString;
    ContactName : WideString;
    GroupName   : WideString;
    PrivacyType : Byte;
    AuthFlag    : Boolean;
  end;
  pTranspContact = ^TTranspContact;

  TTranspContactArray = array[0..0] of TTranspContact;
  pTranspContactArray = ^TTranspContactArray;

  TExtTranspContacts = record
    ContactsCount : Word;
    ContactsArray : pTranspContactArray;
  end;
  pExtTranspContacts = ^TExtTranspContacts;


  TExtShowNotifInfo = record
    AutoClose   : Boolean;
    IsWarning   : Boolean;
    TitleText   : WideString;
    ContentText : WideString;
  end;
  pExtShowNotifInfo = ^TExtShowNotifInfo;


  TExtAuthMsg = record
    Account        : WideString;
    MsgText        : WideString;
    AuthReplyCode  : Word;
    IsOfflineMsg   : Boolean;
    OfflineMsgDT   : Int64; //unix date time in UTC!
  end;
  pExtAuthMsg = ^TExtAuthMsg;


  TExtContactStatus = record
    Account          : WideString;
    Status           : DWord;
    StatusName       : WideString;
    AdditStPicNum    : DWord;
    AdditStPicDesc   : WideString;
    ClientType       : Word;
    ClientName       : WideString;
    CliVerMajor      : Word;
    CliVerMinor      : Word;
    CliVerRelease    : Word;
    CliVerBuild      : Word;
    ClientIP         : WideString;
    ClientOS         : WideString;
    ClientDesc       : WideString;
    OnlineSince      : Int64; //unix date time in UTC!
    RegDate          : Int64; //unix date time in UTC!
    AvatarHashHex    : WideString; //avatar MD5 hash in HEX!
    CustStatusPicID  : Byte;
    ClientIdBlk      : WideString;
  end;
  pExtContactStatus = ^TExtContactStatus;


  TExtMsg = record
    Account      : WideString;
    UniqID       : DWord;
    MsgText      : WideString;
    AckRequired  : Boolean;
    IsOfflineMsg : Boolean;
    OfflineMsgDT : Int64; //unix date time in UTC!
  end;
  pExtMsg = ^TExtMsg;


  TExtMsgAck = record
    Account     : WideString;
    UniqID      : DWord;
  end;
  pExtMsgAck = ^TExtMsgAck;


  TExtNotif = record
    Account     : WideString;
    NotifType   : DWord;
    NotifValue  : DWord;
  end;
  pExtNotif = ^TExtNotif;


  TExtDetsReq = record
    Account     : WideString;
    ReqID       : DWord;
  end;
  pExtDetsReq = ^TExtDetsReq;


  TExtUserDets = record
    Account        : WideString;
    ReqID          : DWord;
    NickName       : WideString;
    FirstName      : WideString;
    LastName       : WideString;
    CountryCode    : Word;
    RegionState    : WideString;
    City           : WideString;
    ZipCode        : WideString;
    Address        : WideString;
    LangCode1      : Word;
    LangCode2      : Word;
    Gender         : Byte;
    Birthday       : Int64; //unix date time
    HomePage       : WideString;
    About          : WideString;
    Interests      : WideString;
    Email1         : WideString;
    Email2         : WideString;
    HomePhone      : WideString;
    WorkPhone      : WideString;
    CellPhone      : WideString;
    FaxNum         : WideString;
    Company        : WideString;
    DivDept        : WideString;
    Position       : WideString;
  end;
  pExtUserDets = ^TExtUserDets;

  TExtSrchReq = record
    ReqID         : DWord;
    Account       : WideString;
    Email         : WideString;
    NickName      : WideString;
    FirstName     : WideString;
    LastName      : WideString;
    CountryCode   : Word;
    City          : WideString;
    LanguageCode  : Word;
    Gender        : Byte;
    AgeMin        : Byte;
    AgeMax        : Byte;
    ZodiacCode    : Byte;
    Interests     : WideString;
    OnlineOnly    : Boolean;
  end;
  pExtSrchReq = ^TExtSrchReq;

  TExtSrchReply = record
    ReqID         : DWord;
    Online        : Boolean;
    Account       : WideString;
    NickName      : WideString;
    FirstName     : WideString;
    LastName      : WideString;
    Gender        : Byte;
    Age           : Byte;
    LastReply     : Boolean;
    TotalMatched  : DWord;
    StPicIndex    : DWord;
  end;
  pExtSrchReply = ^TExtSrchReply;

  TExtOwnDetsUpdReply = record
    UpdResultCode : Word;
    ReqID         : DWord;
  end;
  pExtOwnDetsUpdReply = ^TExtOwnDetsUpdReply;


  TExtAvatarRes = record
    Failed  : Boolean;
    HashHex : WideString; //avatar MD5 hash in HEX!
  end;


  TExtAvatarPic = record
    PicData     : Pointer;
    PicDataLen  : DWord;
    HashHex     : WideString;
    ReqID       : DWord;
  end;
  pExtAvatarPic = ^TExtAvatarPic;


  TExtOwnAvatarSetReply = record
    SetResultCode : Word;
    ReqID         : DWord;
  end;
  pExtOwnAvatarSetReply = ^TExtOwnAvatarSetReply;


  TExtReqOwnPresInfo = record
    ReqID         : DWord;
  end;
  pExtReqOwnPresInfo = ^TExtReqOwnPresInfo;


  TExtOwnPresenceInfo = record
    ReqID          : DWord;
    RegDate        : Int64; //unix date time in UTC!
    OnlineSince    : Int64; //unix date time in UTC!
    PrevOnline     : Int64; //unix date time in UTC!
    CurSessionIP   : WideString;
    PrevSessionIP  : WideString;
    CurInstCount   : Word;
    SrvAddedDescr  : WideString;
  end;
  pExtOwnPresenceInfo = ^TExtOwnPresenceInfo;


  TExtMoreInfo = record
    LangCode   : Word;
    ClientName : WideString;
    ClientVer  : Int64;
    ClientOS   : WideString;
  end;
  pExtMoreInfo = ^TExtMoreInfo;


  TExtProxyInfo = record
    ProxyType  : Byte;
    ProxyHost  : WideString;
    ProxyPort  : DWord;
    ProxyUser  : WideString;
    ProxyPass  : WideString;
  end;
  pExtProxyInfo = ^TExtProxyInfo;


  TExtMailNotif = record
    TotalUnread  : DWord;
    NewMailRcvd  : Boolean;
    RcvrEmail    : WideString;
    SenderName   : WideString;
    SenderEmail  : WideString;
    MailSubject  : WideString;
    MailText     : WideString;
    ClickUrl     : WideString;
  end;
  pExtMailNotif = ^TExtMailNotif;


  TExtReqOwnMailUrl = record
    ReqID         : DWord;
  end;
  pExtReqOwnMailUrl = ^TExtReqOwnMailUrl;


  TExtOwnMailUrlInfo = record
    ReqID         : DWord;
    MailUrl       : WideString;
  end;
  pExtOwnMailUrlInfo = ^TExtOwnMailUrlInfo;


  //transports extension interface
  IExtensionTP = interface
    procedure TimerTick; stdcall;
    procedure GetTransportOpts(var ExtOpts: TExtOptions); stdcall;
    function  TransportSettings(ExtTranspSet: pExtTranspSettings): LongBool; stdcall;
    procedure TransportManage(ExtTranspMan: pExtTranspManage); stdcall;
    procedure AddContact(TranspContact: pTranspContact); stdcall;
    procedure UpdContact(TranspContact: pTranspContact); stdcall;
    procedure DelContact(TranspContact: pTranspContact); stdcall;
    procedure SendAuthRequest(AuthMsg: pExtAuthMsg); stdcall;
    procedure SendAuthReply(AuthMsg: pExtAuthMsg); stdcall;
    procedure SendAuthRevoke(AuthMsg: pExtAuthMsg); stdcall;
    procedure SendInstantMsg(InstantMsg: pExtMsg); stdcall;
    procedure SendMsgAck(MsgAck: pExtMsgAck); stdcall;
    procedure SendNotif(ExtNotif: pExtNotif); stdcall;
    procedure DetailsRequest(ExtDetsReq: pExtDetsReq); stdcall;
    procedure SearchRequest(ExtSrchReq: pExtSrchReq); stdcall;
    procedure OwnDetailsUpdate(ExtUserDets: pExtUserDets); stdcall;
    procedure OwnAvatarSet(AvatarPic: pExtAvatarPic); stdcall;
    procedure OwnPresInfoRequest(ReqOwnPresInfo: pExtReqOwnPresInfo); stdcall;
    procedure OwnMailUrlRequest(ReqOwnMailUrl: pExtReqOwnMailUrl); stdcall;
  end;

  //transports events interface
  IEventsTP = interface
    procedure GetMoreInfo(UniqID: DWord; var MoreInfo: TExtMoreInfo); stdcall;
    procedure GetProxyInfo(UniqID: DWord; var ProxyInfo: TExtProxyInfo); stdcall;
    procedure TransportStateChanged(UniqID: DWord; NewState: Word); stdcall;
    procedure AddContacts(UniqID: DWord; DelOtherContacts: Boolean; TranspContacts: pExtTranspContacts); stdcall;
    procedure UpdContacts(UniqID: DWord; TranspContacts: pExtTranspContacts); stdcall;
    procedure DelContacts(UniqID: DWord; TranspContacts: pExtTranspContacts); stdcall;
    procedure ShowNotificaton(UniqID: DWord; ShowNotifInfo: pExtShowNotifInfo); stdcall;
    procedure AuthRequestRcvd(UniqID: DWord; AuthMsg: pExtAuthMsg); stdcall;
    procedure AuthReplyRcvd(UniqID: DWord; AuthMsg: pExtAuthMsg); stdcall;
    procedure AuthRevokeRcvd(UniqID: DWord; AuthMsg: pExtAuthMsg); stdcall;
    procedure ContactOnline(UniqID: DWord; Status: pExtContactStatus); stdcall;
    procedure ContactOffline(UniqID: DWord; Status: pExtContactStatus); stdcall;
    procedure InstantMsgRcvd(UniqID: DWord; InstantMsg: pExtMsg); stdcall;
    procedure MsgAckRcvd(UniqID: DWord; MsgAck: pExtMsgAck); stdcall;
    procedure NotifRcvd(UniqID: DWord; ExtNotif: pExtNotif); stdcall;
    procedure DetailsReply(UniqID: DWord; ExtUserDets: pExtUserDets; DetsResCode: Word); stdcall;
    procedure SearchReply(UniqID: DWord; ExtSrchReply: pExtSrchReply; SrchResCode: Word); stdcall;
    procedure OwnDetailsUpdReply(UniqID: DWord; OwnDetsUpdReply: pExtOwnDetsUpdReply); stdcall;
    procedure OwnAvatarSetReply(UniqID: DWord; OwnAvatarSetReply: pExtOwnAvatarSetReply); stdcall;
    function  AvatarExists(UniqID: DWord; AvatarHashHex: PWideChar): LongBool; stdcall;
    procedure AvatarRcvd(UniqID: DWord; AvatarPic: pExtAvatarPic; var AvatarRes: TExtAvatarRes); stdcall;
    procedure OwnAvatarHashNotify(UniqID: DWord; AvatarHashHex: PWideChar); stdcall;
    procedure OwnPresenceInfoRcvd(UniqID: DWord; OwnPresenceInfo: pExtOwnPresenceInfo); stdcall;
    procedure MailNotification(UniqID: DWord; ExtMailNotif: pExtMailNotif); stdcall;
    procedure OwnMailUrlRcvd(UniqID: DWord; OwnMailUrl: pExtOwnMailUrlInfo); stdcall;
  end;


  ///////////////////////////////////////////////////////////////////////////////
  /// PLUGIN extension
  ///////////////////////////////////////////////////////////////////////////////

  //plugin extension parameters
  TExtParamsPG = record
    ListenTextMsgs: Boolean;
    ListenFileMsgs: Boolean;
  end;
  pExtParamsPG = ^TExtParamsPG;


  TExtServerInfo = record
    VerMajor     : Word;
    VerMinor     : Word;
    VerRelease   : Word;
    VerBuild     : Word;
    PathUserDB   : WideString;
    PathLogFiles : WideString;
  end;
  pExtServerInfo = ^TExtServerInfo;


  TExtPlugTextMsg = record
    AccSender      : WideString;
    AccRcver       : WideString;
    MsgType        : DWord;
    MsgText        : WideString;
    AckRequired    : Boolean;    //delivery report from receiver required
    MsgEncrypted   : Boolean;    //if message is encrypted then it will be base64 encoded, but anyway it can't be decryted
    SystemMsg      : Boolean;    //system message flag
    SystemMsgPos   : Byte;       //system message popup position (0 - default, 1 - screen center)
    MultipleMsg    : Boolean;    //multiple message flag
    RcverOffline   : Boolean;    //receiver is offline at message sending moment
    TranspOwnerAcc : WideString; //transport owner account name if message was send to transport
    TranspUUIDHex  : WideString; //transport UUID in hex if message was sent to transport
  end;
  pExtPlugTextMsg = ^TExtPlugTextMsg;


  TExtPlugFileMsg = record
    AccSender      : WideString;
    AccRcver       : WideString;
    FilesCount     : DWord;
    FilesSize      : Int64;
    FileName       : WideString;
    SenderIP       : WideString;
  end;
  pExtPlugFileMsg = ^TExtPlugFileMsg;


  //plugin extension interface
  IExtensionPG = interface
    procedure TimerTick; stdcall;
    procedure NotifyTextMsg(const ExtPlugTextMsg: TExtPlugTextMsg); stdcall;
    procedure NotifyFileMsg(const ExtPlugFileMsg: TExtPlugFileMsg); stdcall;
  end;

  //plugin events interface
  IEventsPG = interface
    procedure GetServerInfo(var ServerInfo: TExtServerInfo); stdcall;
  end;

implementation

end.
