program TrsFl_Serveur;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  UnitServerTrsFl in 'UnitServerTrsFl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
