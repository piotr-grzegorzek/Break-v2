unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  IniFiles, Settings;

type
  TForm1 = class(TForm)
    btnOK: TButton;
    btnBreak: TButton;
    btnSettings: TButton;
    btnMute: TButton;
    Label1: TLabel;
    TrayIcon1: TTrayIcon;
    WorkTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure WorkTimerTimer(Sender: TObject);
  private
    FWorkTime: Integer;
    FBreakTime: Integer;
    FCurrentTime: Integer;
    procedure LoadSettings;
    procedure SaveSettings(AWorkTime, ABreakTime: Integer);
    procedure UpdateLabel;
    procedure ToTray;
    procedure RestoreFromTray;
    procedure AppToFront;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  SETTINGS_FILE = './preferences.ini';

procedure TForm1.AppToFront;
var
  Input: TInput;
begin
  ZeroMemory(@Input, SizeOf(Input));
  SendInput(1, Input, SizeOf(Input)); // don't send anything actually to another app..
  SetForegroundWindow(Application.Handle);
end;

procedure TForm1.btnOKClick(Sender: TObject);
begin
  if Label1.Caption = '0' then
  begin
    if FWorkTime > 0 then
    begin
      WorkTimer.Enabled := True;
    end;
  end;
  ToTray;
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

procedure TForm1.ToTray;
begin
  Hide;
  WindowState := wsMinimized;
  TrayIcon1.Visible := True;
  Application.ShowMainForm := False;
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TForm1.TrayIcon1Click(Sender: TObject);
begin
  RestoreFromTray;
end;

procedure TForm1.RestoreFromTray;
begin
  Show;
  WindowState := wsNormal;
  TrayIcon1.Visible := False;
  ShowWindow(Application.Handle, SW_SHOWNORMAL);
  AppToFront;
end;

procedure TForm1.WorkTimerTimer(Sender: TObject);
begin
  if FCurrentTime + 1 < FWorkTime then
  begin
    // work continues
    Inc(FCurrentTime);
    UpdateLabel;
  end
  else if FCurrentTime + 1 = FWorkTime then
  begin
    // work ended
    RestoreFromTray;
    Inc(FCurrentTime);
    UpdateLabel;
  end
  else
  begin
    WorkTimer.Enabled := False;
    FCurrentTime := 0;
  end;
end;

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

procedure TForm1.UpdateLabel;
begin
  Label1.Caption := IntToStr(FWorkTime - FCurrentTime);
end;

end.
