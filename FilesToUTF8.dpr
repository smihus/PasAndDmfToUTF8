program FilesToUTF8;

uses
  Vcl.Forms,
  main in 'src\main.pas' {fmPasAndDfmtoUTF8};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmPasAndDfmtoUTF8, fmPasAndDfmtoUTF8);
  Application.Run;
end.
