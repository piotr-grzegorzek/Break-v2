unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls

    , IniFiles, Settings;

type
  TForm1 = class(TForm)
    btnOK: TButton;
    btnBreak: TButton;
    btnSettings: TButton;
    btnMute: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

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
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.Color := TColor(RGB(24, 24, 24));
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  btnOK.Left := Form1.Width - btnOK.Width - 8;
  btnBreak.Left := Form1.Width - btnBreak.Width - 8;
  btnBreak.Top := Form1.Height - btnBreak.Height - 8;
  btnSettings.Top := btnBreak.Top;
end;

end.
