// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_oscar_caps;

interface

uses Windows, h_oscar, SysUtils;

type
  TCapUUID = array[0..15] of Byte;

  TCap = record
    UUID : TCapUUID;
    //Name : string;
  end;

  TCaps = array[0..0] of TCap;
  pCaps = ^TCaps;

const
  //==============================
  //OSCAR short caps
  OscCapsShort : array[0..35] of TCap = (
  (UUID: ($09,$46, $00,$00, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Short Caps
  (UUID: ($09,$46, $00,$01, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Secure IM
  (UUID: ($09,$46, $00,$02, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //XHtml IM
  (UUID: ($09,$46, $01,$00, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Video
  (UUID: ($09,$46, $01,$01, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Rtc Video
  (UUID: ($09,$46, $01,$02, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Has Camera
  (UUID: ($09,$46, $01,$03, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Has Microphone
  (UUID: ($09,$46, $01,$04, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Rtc Audio
  (UUID: ($09,$46, $01,$05, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Available For Call
  (UUID: ($09,$46, $01,$06, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Call Alert
  (UUID: ($09,$46, $01,$07, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Multi Audio
  (UUID: ($09,$46, $01,$08, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Multi Video
  (UUID: ($09,$46, $01,$09, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Call Alert 2
  (UUID: ($09,$46, $01,$0A, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Status Text Aware
  (UUID: ($09,$46, $01,$0B, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Rt IM
  (UUID: ($09,$46, $01,$0C, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //AMO
  (UUID: ($09,$46, $01,$0D, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Alerts
  (UUID: ($09,$46, $01,$FF, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Smart Caps
  (UUID: ($09,$46, $13,$41, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Talk
  (UUID: ($09,$46, $13,$42, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Direct Play
  (UUID: ($09,$46, $13,$43, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //File Transfer
  (UUID: ($09,$46, $13,$44, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //ICQ Direct Connect
  (UUID: ($09,$46, $13,$45, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Direct IM
  (UUID: ($09,$46, $13,$46, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Avatars
  (UUID: ($09,$46, $13,$47, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Games
  (UUID: ($09,$46, $13,$48, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //File Sharing
  (UUID: ($09,$46, $13,$49, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //ICQ Server Relay
  (UUID: ($09,$46, $13,$4A, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Chat Robots
  (UUID: ($09,$46, $13,$4B, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Share Buddies
  (UUID: ($09,$46, $13,$4C, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //ICQ Devils
  (UUID: ($09,$46, $13,$4D, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Unification
  (UUID: ($09,$46, $13,$4E, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //UTF-8 IM
  (UUID: ($09,$46, $13,$4F, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //No Incoming IMs
  (UUID: ($09,$46, $E0,$00, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Webbie
  (UUID: ($09,$46, $E0,$01, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Photo Sharing
  (UUID: ($09,$46, $E0,$02, $4C,$7F,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00))  //ICQ Unk_E002
  );

  //==============================
  //OSCAR full caps
  OscCapsFull : array[0..6] of TCap = (
  (UUID: ($74,$8F, $24,$20, $62,$87,$11,$D1,$82,$22,$44,$45,$53,$54,$00,$00)), //Chat
  (UUID: ($56,$3F, $C8,$09, $0B,$6F,$41,$BD,$9F,$79,$42,$26,$09,$DF,$A2,$F3)), //Client Events
  (UUID: ($97,$B1, $27,$51, $24,$3C,$43,$34,$AD,$22,$D6,$AB,$F7,$3F,$14,$92)), //RTF IM
  (UUID: ($1A,$09, $3C,$6C, $D7,$FD,$4E,$C5,$9D,$51,$A6,$47,$4E,$34,$F5,$A0)), //ICQ Xtraz
  (UUID: ($01,$38, $CA,$7B, $76,$9A,$49,$15,$88,$F2,$13,$FC,$00,$97,$9E,$A8)), //HTML IM
  (UUID: ($C8,$95, $3A,$9F, $21,$F1,$4F,$AA,$B0,$B2,$6D,$E6,$63,$AB,$F5,$B7)), //ICQ Unk_C895
  (UUID: ($B2,$EC, $8F,$16, $7C,$6F,$45,$1B,$BD,$79,$DC,$58,$49,$78,$88,$B9))  //ICQ Tzers
  );

  //==============================
  //Unofficial status caps
  OscCapsUnoff : array[0..6] of TCap = (
  (UUID: ($B7,$07, $43,$78, $F5,$0C,$77,$77,$97,$77,$57,$78,$50,$2D,$05,$70)), //StDepres
  (UUID: ($B7,$07, $43,$78, $F5,$0C,$77,$77,$97,$77,$57,$78,$50,$2D,$05,$75)), //StFFC
  (UUID: ($B7,$07, $43,$78, $F5,$0C,$77,$77,$97,$77,$57,$78,$50,$2D,$05,$76)), //StAtHome
  (UUID: ($B7,$07, $43,$78, $F5,$0C,$77,$77,$97,$77,$57,$78,$50,$2D,$05,$77)), //StAtWork
  (UUID: ($B7,$07, $43,$78, $F5,$0C,$77,$77,$97,$77,$57,$78,$50,$2D,$05,$78)), //StLunch
  (UUID: ($B7,$07, $43,$78, $F5,$0C,$77,$77,$97,$77,$57,$78,$50,$2D,$05,$79)), //StEvil
  (UUID: ($B7,$07, $43,$78, $F5,$0C,$77,$77,$97,$77,$57,$78,$50,$2D,$07,$77))  //Birthday
  );

  //==============================
  //Other caps
  OscCapsOther : array[0..1] of TCap = (
  (UUID: ($DD,$16, $F2,$02, $84,$E6,$11,$D4,$90,$DB,$00,$10,$4B,$9B,$4B,$7D)), //MacICQ
  (UUID: ($66,$4F, $C8,$09, $0B,$6F,$41,$BD,$9F,$79,$42,$26,$09,$DF,$A2,$F3))  //Alter Acks
  );

  //==============================
  //Moods caps
  OscCapsMoods : array[0..34] of TCap = (
  (UUID: ($63,$62, $73,$37, $A0,$3F,$49,$FF,$80,$E5,$F7,$09,$CD,$E0,$A4,$EE)), //XS(Shopping)
  (UUID: ($5A,$58, $1E,$A1, $E5,$80,$43,$0C,$A0,$6F,$61,$22,$98,$B7,$E4,$C7)), //XS(Duck)
  (UUID: ($83,$C9, $B7,$8E, $77,$E7,$43,$78,$B2,$C5,$FB,$6C,$FC,$C3,$5B,$EC)), //XS(Tired)
  (UUID: ($E6,$01, $E4,$1C, $33,$73,$4B,$D1,$BC,$06,$81,$1D,$6C,$32,$3D,$81)), //XS(Party)
  (UUID: ($8C,$50, $DB,$AE, $81,$ED,$47,$86,$AC,$CA,$16,$CC,$32,$13,$C7,$B7)), //XS(Beer)
  (UUID: ($3F,$B0, $BD,$36, $AF,$3B,$4A,$60,$9E,$EF,$CF,$19,$0F,$6A,$5A,$7F)), //XS(Thinking)
  (UUID: ($F8,$E8, $D7,$B2, $82,$C4,$41,$42,$90,$F8,$10,$C6,$CE,$0A,$89,$A6)), //XS(Eating)
  (UUID: ($80,$53, $7D,$E2, $A4,$67,$4A,$76,$B3,$54,$6D,$FD,$07,$5F,$5E,$C6)), //XS(TV)
  (UUID: ($F1,$8A, $B5,$2E, $DC,$57,$49,$1D,$99,$DC,$64,$44,$50,$24,$57,$AF)), //XS(Friends)
  (UUID: ($1B,$78, $AE,$31, $FA,$0B,$4D,$38,$93,$D1,$99,$7E,$EE,$AF,$B2,$18)), //XS(Coffee)
  (UUID: ($61,$BE, $E0,$DD, $8B,$DD,$47,$5D,$8D,$EE,$5F,$4B,$AA,$CF,$19,$A7)), //XS(Music)
  (UUID: ($48,$8E, $14,$89, $8A,$CA,$4A,$08,$82,$AA,$77,$CE,$7A,$16,$52,$08)), //XS(Business)
  (UUID: ($10,$7A, $9A,$18, $12,$32,$4D,$A4,$B6,$CD,$08,$79,$DB,$78,$0F,$09)), //XS(Camera)
  (UUID: ($6F,$49, $30,$98, $4F,$7C,$4A,$FF,$A2,$76,$34,$A0,$3B,$CE,$AE,$A7)), //XS(Funny)
  (UUID: ($12,$92, $E5,$50, $1B,$64,$4F,$66,$B2,$06,$B2,$9A,$F3,$78,$E4,$8D)), //XS(Phone)
  (UUID: ($D4,$A6, $11,$D0, $8F,$01,$4E,$C0,$92,$23,$C5,$B6,$BE,$C6,$CC,$F0)), //XS(Games)
  (UUID: ($60,$9D, $52,$F8, $A2,$9A,$49,$A6,$B2,$A0,$25,$24,$C5,$E9,$D2,$60)), //XS(College)
  (UUID: ($1F,$7A, $40,$71, $BF,$3B,$4E,$60,$BC,$32,$4C,$57,$87,$B0,$4C,$F1)), //XS(Sick)
  (UUID: ($78,$5E, $8C,$48, $40,$D3,$4C,$65,$88,$6F,$04,$CF,$3F,$3F,$43,$DF)), //XS(Sleeping)
  (UUID: ($A6,$ED, $55,$7E, $6B,$F7,$44,$D4,$A5,$D4,$D2,$E7,$D9,$5C,$E8,$1F)), //XS(Surfing)
  (UUID: ($12,$D0, $7E,$3E, $F8,$85,$48,$9E,$8E,$97,$A7,$2A,$65,$51,$E5,$8D)), //XS(Browsing)
  (UUID: ($BA,$74, $DB,$3E, $9E,$24,$43,$4B,$87,$B6,$2F,$6B,$8D,$FE,$E5,$0F)), //XS(Engineering)
  (UUID: ($63,$4F, $6B,$D8, $AD,$D2,$4A,$A1,$AA,$B9,$11,$5B,$C2,$6D,$05,$A1)), //XS(Typing)
  (UUID: ($01,$D8, $D7,$EE, $AC,$3B,$49,$2A,$A5,$8D,$D3,$D8,$77,$E6,$6B,$92)), //XS(Angry)
  (UUID: ($2C,$E0, $E4,$E5, $7C,$64,$43,$70,$9C,$3A,$7A,$1C,$E8,$78,$A7,$DC)), //XS(China1)
  (UUID: ($10,$11, $17,$C9, $A3,$B0,$40,$F9,$81,$AC,$49,$E1,$59,$FB,$D5,$D4)), //XS(China2)
  (UUID: ($16,$0C, $60,$BB, $DD,$44,$43,$F3,$91,$40,$05,$0F,$00,$E6,$C0,$09)), //XS(China3)
  (UUID: ($64,$43, $C6,$AF, $22,$60,$45,$17,$B5,$8C,$D7,$DF,$8E,$29,$03,$52)), //XS(China4)
  (UUID: ($16,$F5, $B7,$6F, $A9,$D2,$40,$35,$8C,$C5,$C0,$84,$70,$3C,$98,$FA)), //XS(China5)
  (UUID: ($63,$14, $36,$FF, $3F,$8A,$40,$D0,$A5,$CB,$7B,$66,$E0,$51,$B3,$64)), //XS(De1)
  (UUID: ($B7,$08, $67,$F5, $38,$25,$43,$27,$A1,$FF,$CF,$4C,$C1,$93,$97,$97)), //XS(De2)
  (UUID: ($DD,$CF, $0E,$A9, $71,$95,$40,$48,$A9,$C6,$41,$32,$06,$D6,$F2,$80)), //XS(De3)
  (UUID: ($D4,$E2, $B0,$BA, $33,$4E,$4F,$A5,$98,$D0,$11,$7D,$BF,$4D,$3C,$C8)), //XS(RuSearch)
  (UUID: ($CD,$56, $43,$A2, $C9,$4C,$47,$24,$B5,$2C,$DC,$01,$24,$A1,$D0,$CD)), //XS(RuLove)
  (UUID: ($00,$72, $D9,$08, $4A,$D1,$43,$DD,$91,$99,$6F,$02,$69,$66,$02,$6F))  //XS(RuJournal)
  );

  //==============================
  //Named caps indexes in short caps array
  CAP_SHORT_SHORT_CAPS                = 0;
  CAP_SHORT_SECURE_IM                 = 1;
  CAP_SHORT_XHTML_IM                  = 2;
  CAP_SHORT_VIDEO                     = 3;
  CAP_SHORT_RTC_VIDEO                 = 4;
  CAP_SHORT_HAS_CAMERA                = 5;
  CAP_SHORT_HAS_MICROPHONE            = 6;
  CAP_SHORT_RTC_AUDIO                 = 7;
  CAP_SHORT_AVAIL_FOR_CALL            = 8;
  CAP_SHORT_AOL_CALL_ALERT            = 9;
  CAP_SHORT_MULTI_AUDIO               = 10;
  CAP_SHORT_MULTI_VIDEO               = 11;
  CAP_SHORT_AOL_CALL_ALERT2           = 12;
  CAP_SHORT_STATUS_TXT_AWARE          = 13;
  CAP_SHORT_RT_IM                     = 14;
  CAP_SHORT_AMO                       = 15;
  CAP_SHORT_ALERTS                    = 16;
  CAP_SHORT_SMART_CAPS                = 17;
  CAP_SHORT_TALK                      = 18;
  CAP_SHORT_DIRECT_PLAY               = 29;
  CAP_SHORT_FILE_TRANSFER             = 20;
  CAP_SHORT_ICQ_DIRECT_CON            = 21;
  CAP_SHORT_DIRECT_IM                 = 22;
  CAP_SHORT_AVATARS                   = 23;
  CAP_SHORT_GAMES                     = 24;
  CAP_SHORT_FILE_SHARING              = 25;
  CAP_SHORT_ICQ_SRV_RELAY             = 26;
  CAP_SHORT_CHAT_ROBOTS               = 27;
  CAP_SHORT_SHARE_BUDDIES             = 28;
  CAP_SHORT_UNK_134C                  = 29;
  CAP_SHORT_UNIFICATION               = 30;
  CAP_SHORT_ICQ_UTF8                  = 31;
  CAP_SHORT_NO_INC_IM                 = 32;
  CAP_SHORT_ICQ_WEB                   = 33;
  CAP_SHORT_ICQ_PICS_SHARE            = 34;
  CAP_SHORT_ICQ_UNK_E002              = 35;

  //==============================
  //Named caps indexes in full caps array
  CAP_FULL_CHAT                       = 0;
  CAP_FULL_ICQ_CLI_EVENTS             = 1;
  CAP_FULL_ICQ_WIN_RTF                = 2;
  CAP_FULL_ICQ_XTRAZ                  = 3;
  CAP_FULL_ICQ_HTML_IM                = 4;
  CAP_FULL_ICQ_UNK_1                  = 5;
  CAP_FULL_ICQ_TZERS                  = 6;

  //==============================
  //Named caps indexes in unofficial caps array
  CAP_UNOFF_ICQ_ST_DEPRES             = 0;
  CAP_UNOFF_ICQ_ST_FFC                = 1;
  CAP_UNOFF_ICQ_ST_ATHOME             = 2;
  CAP_UNOFF_ICQ_ST_ATWORK             = 3;
  CAP_UNOFF_ICQ_ST_LUNCH              = 4;
  CAP_UNOFF_ICQ_ST_EVIL               = 5;
  CAP_UNOFF_ICQ_BIRTHDAY              = 6;

  //==============================
  //Named caps indexes in other caps array
  CAP_OTHER_MAC_ICQ                   = 0;
  CAP_OTHER_ALTER_ACKS                = 1;

  function HasOscarCap(const OscarCap: TCapUUID; CapsFull: RawByteString): Boolean;
  function FindXstCap(CapsFull: RawByteString): DWord;

implementation


{***************************************************************}
function SameCaps(const RawCap: RawByteString; CapUUID: TCapUUID): Boolean;
var sRaw: RawByteString;
begin
  Result := False;
  if (Length(RawCap) <> SizeOf(TCapUUID)) then Exit;

  SetLength(sRaw, SizeOf(TCapUUID));
  Move(CapUUID[0], sRaw[1], SizeOf(TCapUUID));

  Result := RawCap = sRaw;
end;

{***************************************************************}
function HasOscarCap(const OscarCap: TCapUUID; CapsFull: RawByteString): Boolean;
var sCap: RawByteString;
begin
  Result := False;

  while (CapsFull <> '') do
  begin
    sCap := ReadStr(CapsFull, 16, 16);
    if (Length(sCap) <> 16) then Break;

    if SameCaps(sCap, OscarCap) then
    begin
      Result := True;
      Break;
    end;
  end;//while
end;

{***************************************************************}
function IsCapInArray(const RawCap: RawByteString; CapsArray: pCaps; CapsCount: Integer; var CapIndex: DWord): Boolean;
var i: Integer;
begin
  Result := False;

  for i := 0 to CapsCount-1 do
  begin
    if SameCaps(RawCap, CapsArray^[i].UUID) then
    begin
      CapIndex := i;
      Result   := True;
      Break;
    end;
  end;//for
end;

{***************************************************************}
function FindXstCap(CapsFull: RawByteString): DWord;
var sCap: RawByteString;
begin
  Result := 0;

  while (CapsFull <> '') do
  begin
    sCap := ReadStr(CapsFull, 16, 16);
    if (Length(sCap) <> 16) then Break;

    if IsCapInArray(sCap, @OscCapsMoods[0], Length(OscCapsMoods), Result) then
    begin
      Inc(Result); //to return correct st pic num
      Break;
    end;
  end;//while
end;


end.


