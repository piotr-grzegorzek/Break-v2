program Break;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Settings in 'Settings.pas' {frmSettings};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := true;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
