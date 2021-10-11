unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UnitServerTrsFl, ExtCtrls, ExtDlgs, Gauges, ComCtrls,
  Spin;

type
  TForm1 = class(TForm)
    PanelConnect: TPanel;
    ButtonOpen: TButton;
    EditPortTCP: TEdit;
    EditPortUDP: TEdit;
    LabelPortTCP: TLabel;
    LabelPortUdp: TLabel;
    LabelDossier: TLabel;
    EditDossier: TEdit;
    ButtonChgDs: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ServerTrsFl1Connect(Sender: TObject);
    procedure ServerTrsFl1Disconnect(Sender: TObject);
    procedure ServerTrsFl1NouveauFichier(Sender: TObject; var accepter: boolean; Nom: string; Size: int64; NumConnection: integer);
    procedure ServerTrsFl1Evolution(Sender: TObject; Maximum,Evolution: integer; NumConnection: integer);
    procedure ServerTrsFl1Fini(Sender: TObject; Chemin: string; NumConnection: integer);
    procedure ServerTrsFl1Erreur(Sender: TObject; TransfertAnnuler: boolean; ErreurText: string; NumConnection: integer);
    procedure ButtonGrpBox(Sender: TObject);
    procedure ButtonChgDsClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;
  ServerTrsFl1: TServerTrsFl;
  ListComposants: TStringList;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 EditDossier.Text:=ExtractFilePath(Application.ExeName);
 ListComposants:=TStringList.Create;
 ServerTrsFl1:=TServerTrsFl.Create(Self);
 ServerTrsFl1.OnConnect:=ServerTrsFl1Connect;
 ServerTrsFl1.OnDisconnect:=ServerTrsFl1Disconnect;
 ServerTrsFl1.OnNouveauFichierEvent:=ServerTrsFl1NouveauFichier;
 ServerTrsFl1.OnEvolutionEvent:=ServerTrsFl1Evolution;
 ServerTrsFl1.OnFiniEvent:=ServerTrsFl1Fini;
 ServerTrsFl1.OnErreurEvent:=ServerTrsFl1Erreur;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
 i: integer;
begin
 ServerTrsFl1.Open();
 ServerTrsFl1.close;

 for i:=ListComposants.Count-1 downto 0 do
  begin
   if not(ListComposants.Objects[i]=nil)
   then ListComposants.Objects[i].Free;
  end;

 ServerTrsFl1.Free;
 ListComposants.Free;
end;

procedure TForm1.ServerTrsFl1Connect(Sender: TObject);
begin
 EditPortTCP.Enabled:=false;
 EditPortUDP.Enabled:=false;
 EditDossier.Enabled:=false;
 ButtonChgDs.Enabled:=false;
 ButtonOpen.Caption:='Stop Server';
end;

procedure TForm1.ServerTrsFl1Disconnect(Sender: TObject);
begin
 EditPortTCP.Enabled:=true;
 EditPortUDP.Enabled:=true;
 EditDossier.Enabled:=true;
 ButtonChgDs.Enabled:=true;
 ButtonOpen.Caption:='Start Server';
end;

procedure TForm1.ServerTrsFl1NouveauFichier(Sender: TObject; var accepter: boolean; Nom: string; Size: int64; NumConnection: integer);
var
 Gauge: TGauge;
 Button: TButton;
 LabelEv: TLabel;
 GroupBox: TGroupBox;
begin
 // on recoit un nouveau fichier (mettre accepter a true si on le veut, a false si on le veut pas)
 // ListComposants
 accepter:=true; // on accepte tout
 GroupBox:=TGroupBox.Create(Form1); // on creer tout les composant
 Button:=TButton.Create(Form1);
 Button.Parent:=GroupBox;
 Gauge:=TGauge.Create(Form1);
 Gauge.Parent:=GroupBox;
 LabelEv:=TLabel.Create(Form1);
 LabelEv.Parent:=GroupBox;
 GroupBox.Caption:=Nom+' - '+inttostr(Size) +' bytes';
 GroupBox.Left:=0;
 Button.Left:=8;
 Button.Top:=40;
 Button.Caption:='Cancel';
 LabelEv.Left:=88;
 LabelEv.Top:=48;
 LabelEv.Caption:='';
 LabelEv.Hint:='';
 Gauge.Height:=20;
 Gauge.MinValue:=0;
 Gauge.Progress:=0;
 GroupBox.Height:=72;
 GroupBox.Top:=999999999;
 GroupBox.Align:=alTop;
 Gauge.Align:=alTop;
 GroupBox.Parent := Form1;
 GroupBox.Tag:=NumConnection;
 Button.Tag:=NumConnection;
 Button.OnClick:=ButtonGrpBox;
 Gauge.Tag:=NumConnection;
 LabelEv.Tag:=NumConnection;

 //GroupBox.Align:=alTop;
 ListComposants.AddObject('GrpBox_'+inttostr(NumConnection),GroupBox);
 ListComposants.AddObject('Btn_'+inttostr(NumConnection),Button);
 ListComposants.AddObject('Gauge_'+inttostr(NumConnection),Gauge);
 ListComposants.AddObject('Label_'+inttostr(NumConnection),LabelEv);

 Application.ProcessMessages;
end;

procedure TForm1.ServerTrsFl1Evolution(Sender: TObject; Maximum,Evolution: integer; NumConnection: integer);
var
 i: integer;
begin
 // on a recu un bloc de plus (mettre a jour la progressbar)
 i:=ListComposants.IndexOf('Gauge_'+inttostr(NumConnection));
 if not (i=-1)
 then
  begin
   (ListComposants.Objects[i] as TGauge).MaxValue:=Maximum;
   (ListComposants.Objects[i] as TGauge).Progress:=Evolution;
  end;

 i:=ListComposants.IndexOf('Label_'+inttostr(NumConnection));

 if not (i=-1)
 then (ListComposants.Objects[i] as TLabel).Caption:=inttostr(Evolution)+'/'+inttostr(Maximum);
end;

procedure TForm1.ServerTrsFl1Fini(Sender: TObject; Chemin: string; NumConnection: integer);
var
 i: integer;
begin
 // Youpi le fichier a bien été recu
 i:=ListComposants.IndexOf('Btn_'+inttostr(NumConnection));
 if not(i=-1)
 then (ListComposants.Objects[i] as TButton).Caption:='Terminate';
 i:=ListComposants.IndexOf('Label_'+inttostr(NumConnection));
 if not(i=-1)
 then
  begin
   (ListComposants.Objects[i] as TLabel).Caption:='Finish: '+Chemin;
   (ListComposants.Objects[i] as TLabel).Hint:=ExtractFileName(Chemin);
  end;
end;

procedure TForm1.ServerTrsFl1Erreur(Sender: TObject; TransfertAnnuler: boolean; ErreurText: string; NumConnection: integer);
var
 i: integer;
begin
 // Oups on a eu une erreur si TransfertAnnuler=true cherche pas c fini et on a pas le fichier.
 if TransfertAnnuler
 then
  begin
   i:=ListComposants.IndexOf('Btn_'+inttostr(NumConnection));
   if not(i=-1)
   then (ListComposants.Objects[i] as TButton).Caption:='Cancel';
  end;

 i:=ListComposants.IndexOf('Label_'+inttostr(NumConnection));
 if not (i=-1)
 then (ListComposants.Objects[i] as TLabel).Caption:='Error: '+ErreurText;
end;

procedure TForm1.ButtonGrpBox(Sender: TObject);
var
 s: string;
 NumConnection,i: integer;
begin
 NumConnection:=(Sender as TButton).Tag;
 i:=ListComposants.IndexOf('Btn_'+inttostr(NumConnection));
 s:='';

 if not (i=-1)
 then s:=(ListComposants.Objects[i] as TButton).Caption;

 if s='Cancel'
 then ServerTrsFl1.AnnulerFichier(NumConnection)
 else
  if s='Close'
  then
   begin
    // rien (déjà annuler)
   end
  else
   if s='Terminate'
   then
    begin
     // rien (déjà annuler)
    end;

 (ListComposants.Objects[i] as TButton).Visible:=false; // on peut pas le free puisque a la fin de cette procedure on appelle quelque chose ds le TButton
 (ListComposants.Objects[i] as TButton).OnClick:=nil;
 (ListComposants.Objects[i] as TButton).Parent:=Form1;  // Sinon on a un pb qu'en on le free a l'evenement de fermeture de Form1

 // sera libéré lors de la fermeture donc de la fin de l'application
 i:=ListComposants.IndexOf('Label_'+inttostr(NumConnection));
 if not (i=-1)
 then
  begin
   ListComposants.Objects[i].Free;
   ListComposants.Delete(i);
  end;

 i:=ListComposants.IndexOf('Gauge_'+inttostr(NumConnection));
 if not(i=-1)
 then
  begin
   ListComposants.Objects[i].Free;
   ListComposants.Delete(i);
  end;

 i:=ListComposants.IndexOf('GrpBox_'+inttostr(NumConnection));
 if not(i=-1)
 then
  begin
   ListComposants.Objects[i].Free;
   ListComposants.Delete(i);
  end;
end;

procedure TForm1.ButtonOpenClick(Sender: TObject);
begin
 ServerTrsFl1.PortTCP:=StrToInt(EditPortTCP.text);
 ServerTrsFl1.PortUDP:=StrToInt(EditPortUDP.text);
 ServerTrsFl1.Dossier:=editDossier.Text;
 ServerTrsFl1.Active:=not ServerTrsFl1.Active;
end;

procedure TForm1.ButtonChgDsClick(Sender: TObject);
begin
 SaveDialog1.FileName:=EditDossier.Text+'ici'; // sa serai mieux avec un ShellTreeView
 if SaveDialog1.Execute
 then EditDossier.Text:=ExtractFilePath(SaveDialog1.FileName);
end;

procedure TForm1.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
 Resize:=true;
 if NewWidth<293
 then Resize:=false;
 if NewHeight<143
 then Resize:=false;
end;

end.
