unit Settings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmSettings = class(TForm)
    lblWorkTime: TLabel;
    lblBreakTime: TLabel;
    edtWorkTime: TEdit;
    edtBreakTime: TEdit;
    Panel1: TPanel;
    btnCancel: TButton;
    Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSettings: TfrmSettings;
  function modShowSettings(var AWorkTime, ABreakTime: Integer): Boolean;
implementation

{$R *.dfm}

function modShowSettings(var AWorkTime, ABreakTime: Integer): Boolean;
begin
  try
    frmSettings := TfrmSettings.Create(Application);
    with frmSettings do
    begin
      edtWorkTime.Text := IntToStr(AWorkTime);
      edtBreakTime.Text := IntToStr(ABreakTime);
      ShowModal;
      if ModalResult = mrOK then
      begin
        AWorkTime := StrToInt(edtWorkTime.Text);
        ABreakTime := StrToInt(edtBreakTime.Text);
        Result := True;
      end
      else
        Result := False;
    end;
  finally
    frmSettings.Free;
    frmSettings := nil;
  end;
end;

end.
