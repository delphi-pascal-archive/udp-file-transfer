unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, UnitClientTrsFl, UnitD, StdCtrls, ComCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    PanelCommande: TPanel;
    EditHost: TEdit;
    LabelPortTCP: TLabel;
    LabelPortUdp: TLabel;
    LabelHost: TLabel;
    EditPortTCP: TEdit;
    EditPortUDP: TEdit;
    ButtonEnvoyer: TButton;
    OpenDialog1: TOpenDialog;
    ProgressBar1: TProgressBar;
    ListBox1: TListBox;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonEnvoyerClick(Sender: TObject);
    procedure ClientTrsFl1Erreur(Sender: TObject; TransfertAnnuler: boolean; ErreurText: string);
    procedure ClientTrsFl1Evolution(Sender: TObject; Maximum,Evolution: integer);
    procedure ClientTrsFl1Fini(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;
  ClientTrsFl1: TClientTrsFl;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 ClientTrsFl1:=TClientTrsFl.Create(Self); // on creer le composant
 ClientTrsFl1.OnErreurEvent:=ClientTrsFl1Erreur; // on enregistre les evenements
 ClientTrsFl1.OnEvolutionEvent:=ClientTrsFl1Evolution;
 ClientTrsFl1.OnFiniEvent:=ClientTrsFl1Fini;

 OpenDialog1.InitialDir:=ExtractFilePath(ParamStr(0));
 Label1.Caption:='Transfer speed: '+IntToStr(TrackBar1.Max-TrackBar1.Position); 
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 ClientTrsFl1.Free; // a pas oublier pour ne pas avoir de pb de memoires
end;

procedure TForm1.ClientTrsFl1Erreur(Sender: TObject; TransfertAnnuler: boolean; ErreurText: string);
begin
 // lorsqu'une erreur survient, TransfertAnnuler assez parlant
 ListBox1.Items.Add(ErreurText);

 if TransfertAnnuler
 then
  begin
   EditHost.Enabled:=true;
   EditPortTCP.Enabled:=true;
   EditPortUDP.Enabled:=true;
   ButtonEnvoyer.Enabled:=true;
  end;
end;

procedure TForm1.ClientTrsFl1Evolution(Sender: TObject; Maximum,Evolution: integer);
begin
 // evolution (tout les IRep blocks que recoit le serveur)
 ProgressBar1.Min:=0;
 ProgressBar1.Max:=Maximum;
 ProgressBar1.Position:=Evolution; // au lieu de ProgressBar, Gauge peut etre plus joli (de la barre exmple)
end;

procedure TForm1.ClientTrsFl1Fini(Sender: TObject);
begin
 ListBox1.Items.Add('Transfer completed!');
 EditHost.Enabled:=true;
 EditPortTCP.Enabled:=true;
 EditPortUDP.Enabled:=true;
 ButtonEnvoyer.Enabled:=true;
end;

procedure TForm1.ButtonEnvoyerClick(Sender: TObject);
begin
 if OpenDialog1.Execute
 then
  begin
   EditHost.Enabled:=false;
   EditPortTCP.Enabled:=false;
   EditPortUDP.Enabled:=false;
   ButtonEnvoyer.Enabled:=false;
   ClientTrsFl1.Envoyer(EditHost.Text,StrToInt(EditPortTcp.Text),StrToInt(EditPortUDP.Text),OpenDialog1.FileName,TrackBar1.Position);
  end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
 Label1.Caption:='Transfer speed: '+IntToStr(TrackBar1.Max-TrackBar1.Position);
end;

end.
