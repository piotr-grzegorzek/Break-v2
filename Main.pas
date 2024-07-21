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
    procedure ToTray;
    procedure btnOKClick(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure WorkTimerTimer(Sender: TObject);
  private
    workTime: Integer;
    breakTime: Integer;
    currentTime: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure AppToFront;
var
  Input: TInput;
begin
  ZeroMemory(@Input, SizeOf(Input));
  SendInput(1, Input, SizeOf(Input)); // don't send anyting actually to another app..
  SetForegroundWindow(Application.Handle);
end;

procedure TForm1.btnOKClick(Sender: TObject);
begin
  if Label1.Caption = '0' then
  begin
    if workTime > 0 then
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
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create('./preferences.ini');
  try
    AWorkTime := IniFile.ReadInteger('Settings', 'WorkTime', 0);
    ABreakTime := IniFile.ReadInteger('Settings', 'BreakTime', 0);
    if modShowSettings(AWorkTime, ABreakTime) then
    begin
      IniFile.WriteInteger('Settings', 'WorkTime', AWorkTime);
      IniFile.WriteInteger('Settings', 'BreakTime', ABreakTime);
      workTime := AWorkTime;
      breakTime := ABreakTime;
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AWorkTime: Integer;
  ABreakTime: Integer;
  IniFile: TIniFile;
begin
  Form1.Color := TColor(RGB(24, 24, 24));
  ToTray;

  IniFile := TIniFile.Create('./preferences.ini');
  try
    workTime := IniFile.ReadInteger('Settings', 'WorkTime', 0);
    breakTime := IniFile.ReadInteger('Settings', 'BreakTime', 0);
  finally
    IniFile.Free;
  end;

  if workTime > 0 then
  begin
    WorkTimer.Enabled := True;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  btnOK.Left := Form1.Width - btnOK.Width - 8;
  btnBreak.Left := Form1.Width - btnBreak.Width - 8;
  btnBreak.Top := Form1.Height - btnBreak.Height - 8;
  btnSettings.Top := btnBreak.Top;
end;

procedure TForm1.ToTray;
begin
  Self.Hide;
  WindowState := wsMinimized;
  TrayIcon1.Visible := True;
  Application.ShowMainForm := False;
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TForm1.TrayIcon1Click(Sender: TObject);
begin
  Self.Show;
  WindowState := wsMaximized;
  TrayIcon1.Visible := False;
  ShowWindow(Application.Handle, SW_SHOWNORMAL);
  AppToFront;
end;

procedure TForm1.WorkTimerTimer(Sender: TObject);
begin
  if currentTime + 1 < workTime then
  begin
    // work continues
    currentTime := currentTime + 1;
    Label1.Caption := IntToStr(workTime - currentTime);
  end
  else if currentTime + 1 = workTime then
  begin
    // work ended

    TrayIcon1Click(nil);
    currentTime := currentTime + 1;
    Label1.Caption := IntToStr(workTime - currentTime);
  end
  else
  begin
    WorkTimer.Enabled := False;
    currentTime := 0;
  end;
end;

end.
