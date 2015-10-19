// Bimoid Server/Messenger, by Ilgam Z. (ilgampub@gmail.com)
// www.bimoid.com

unit u_timer;

interface

uses Windows, Classes, Messages;

type
  TBestTimer = class
  private
    FInterval  : DWord;
    FWndHandle : THandle;
    FOnTimer   : TNotifyEvent;
    FEnabled   : Boolean;
    procedure CreateTimerWnd;
    procedure FreeTimerWnd;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure WndProc(var Msg: TMessage);
  protected
    procedure Timer;
  public
    constructor Create;
    destructor Destroy; override;
    property Enabled  : Boolean      read FEnabled  write SetEnabled;
    property Interval : DWord        read FInterval write SetInterval;
    property OnTimer  : TNotifyEvent read FOnTimer  write SetOnTimer;
  end;

implementation

{ TBestTimer }
{*****************************************************************}
constructor TBestTimer.Create;
begin
  FEnabled   := False;
  FInterval  := 1000;
end;

{*****************************************************************}
destructor TBestTimer.Destroy;
begin
  FEnabled := False;

  if (FWndHandle <> 0) then
    UpdateTimer;

  inherited;
end;

{*****************************************************************}
procedure TBestTimer.CreateTimerWnd;
begin
  if (FWndHandle <> 0) then Exit;
  FWndHandle := AllocateHWnd(WndProc);
end;

{*****************************************************************}
procedure TBestTimer.FreeTimerWnd;
begin
  if (FWndHandle = 0) then Exit;
  DeallocateHWnd(FWndHandle);
  FWndHandle := 0;
end;

{*****************************************************************}
procedure TBestTimer.SetEnabled(Value: Boolean);
begin
  if (Value <> FEnabled) then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

{*****************************************************************}
procedure TBestTimer.SetInterval(Value: Cardinal);
begin
  if (Value <> FInterval) then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

{*****************************************************************}
procedure TBestTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

{*****************************************************************}
procedure TBestTimer.Timer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

{*****************************************************************}
procedure TBestTimer.UpdateTimer;
begin
  if (FWndHandle <> 0) then
    KillTimer(FWndHandle, 1);

  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
  begin
    CreateTimerWnd;

    if (SetTimer(FWndHandle, 1, FInterval, nil) = 0) then
    begin
      //OutDebugStr('TBestTimer error: no timers available');
      FreeTimerWnd;
    end;
  end
  else
    FreeTimerWnd;
end;

{*****************************************************************}
procedure TBestTimer.WndProc(var Msg: TMessage);
begin
  with Msg do
    if (Msg = WM_TIMER) then
      try
        Timer;
      except
      end
    else
      Result := DefWindowProc(FWndHandle, Msg, wParam, lParam);
end;


end.
