unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  IniFiles, MMSystem,

  Settings;

type
  TForm1 = class(TForm)
    btnOK: TButton;
    btnBreak: TButton;
    btnSettings: TButton;
    btnMute: TButton;
    Label1: TLabel;
    TrayIcon1: TTrayIcon;
    WorkTimer: TTimer;
    BreakTimer: TTimer;

    // Event Handlers
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnBreakClick(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure WorkTimerTimer(Sender: TObject);
    procedure BreakTimerTimer(Sender: TObject);
    procedure btnMuteClick(Sender: TObject);

  private
    // Private Fields
    FWorkTime: Integer;
    FBreakTime: Integer;
    FCurrentTime: Integer;

    // Private Methods
    procedure LoadSettings;
    procedure SaveSettings(AWorkTime, ABreakTime: Integer);
    procedure ToTray;
    procedure RestoreFromTray;
    procedure AppToFront;
    procedure PlayNotification;
    procedure StopNotification;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  SETTINGS_FILE = './preferences.ini';

  // Event Handlers

procedure TForm1.FormCreate(Sender: TObject);
begin
  Color := TColor(RGB(24, 24, 24));
  ToTray;
  LoadSettings;
  if FWorkTime > 0 then
  begin
    WorkTimer.Enabled := True;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  btnOK.Left := Width - btnOK.Width - 8;
  btnBreak.Left := Width - btnBreak.Width - 8;
  btnBreak.Top := Height - btnBreak.Height - 8;
  btnSettings.Top := btnBreak.Top;
end;

procedure TForm1.btnSettingsClick(Sender: TObject);
var
  AWorkTime: Integer;
  ABreakTime: Integer;
begin
  AWorkTime := FWorkTime;
  ABreakTime := FBreakTime;
  if modShowSettings(AWorkTime, ABreakTime) then
  begin
    SaveSettings(AWorkTime, ABreakTime);
    FWorkTime := AWorkTime;
    FBreakTime := ABreakTime;
  end;
end;

procedure TForm1.btnOKClick(Sender: TObject);
begin
  if (Label1.Caption = '0') and (FWorkTime > 0) then
  begin
    StopNotification;
    Label1.Caption := IntToStr(FWorkTime - FCurrentTime);
    WorkTimer.Enabled := True;
  end;
  ToTray;
end;

procedure TForm1.btnBreakClick(Sender: TObject);
begin
  if (Label1.Caption = '0') and (FBreakTime > 0) then
  begin
    StopNotification;
    Label1.Caption := IntToStr(FBreakTime - FCurrentTime);
    BreakTimer.Enabled := True;
  end;
end;

procedure TForm1.btnMuteClick(Sender: TObject);
begin
  StopNotification;
end;

procedure TForm1.TrayIcon1Click(Sender: TObject);
begin
  RestoreFromTray;
end;

procedure TForm1.WorkTimerTimer(Sender: TObject);
begin
  Inc(FCurrentTime);
  Label1.Caption := IntToStr(FWorkTime - FCurrentTime);
  if FCurrentTime >= FWorkTime then
  begin
    // work ended
    PlayNotification;
    RestoreFromTray;
    WorkTimer.Enabled := False;
    FCurrentTime := 0;
  end;
end;

procedure TForm1.BreakTimerTimer(Sender: TObject);
begin
  Inc(FCurrentTime);
  Label1.Caption := IntToStr(FBreakTime - FCurrentTime);
  if FCurrentTime >= FBreakTime then
  begin
    // break ended
    PlayNotification;
    BreakTimer.Enabled := False;
    FCurrentTime := 0;
  end;
end;

// Private Methods

procedure TForm1.LoadSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(SETTINGS_FILE);
  try
    FWorkTime := IniFile.ReadInteger('Settings', 'WorkTime', 0);
    FBreakTime := IniFile.ReadInteger('Settings', 'BreakTime', 0);
  except
    on E: Exception do
      ShowMessage('Error reading settings: ' + E.Message);
  end;
  IniFile.Free;
end;

procedure TForm1.SaveSettings(AWorkTime, ABreakTime: Integer);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(SETTINGS_FILE);
  try
    IniFile.WriteInteger('Settings', 'WorkTime', AWorkTime);
    IniFile.WriteInteger('Settings', 'BreakTime', ABreakTime);
  except
    on E: Exception do
      ShowMessage('Error saving settings: ' + E.Message);
  end;
  IniFile.Free;
end;

procedure TForm1.ToTray;
begin
  Hide;
  WindowState := wsMinimized;
  TrayIcon1.Visible := True;
  Application.ShowMainForm := False;
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TForm1.RestoreFromTray;
begin
  Show;
  WindowState := wsMaximized;
  TrayIcon1.Visible := False;
  ShowWindow(Application.Handle, SW_SHOWNORMAL);
  AppToFront;
end;

procedure TForm1.AppToFront;
var
  Input: TInput;
begin
  ZeroMemory(@Input, SizeOf(Input));
  SendInput(1, Input, SizeOf(Input));
  SetForegroundWindow(Application.Handle);
end;

procedure TForm1.PlayNotification;
begin
  PlaySound('notification.wav', 0, SND_FILENAME or SND_NODEFAULT or SND_ASYNC or SND_LOOP);
end;

procedure TForm1.StopNotification;
begin
  PlaySound(nil, 0, 0);
end;

end.
