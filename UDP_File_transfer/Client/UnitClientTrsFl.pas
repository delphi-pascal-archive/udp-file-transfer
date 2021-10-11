unit UnitClientTrsFl;

interface

uses Classes, SysUtils, ScktComp, IdBaseComponent, IdComponent, IdUDPBase, IdUDPClient,
     ComCtrls, UnitD, UnitTh;

type
  TClientTrsFlErreurEvent = procedure (Sender: TObject; TransfertAnnuler: boolean; ErreurText: string) of object;
  TClientTrsFlEvolutionEvent = procedure (Sender: TObject; Maximum, Evolution: integer) of object; // rappel min=0
  TClientTrsFlFiniEvent = procedure (Sender: TObject) of object;
  TClientTrsFl = class(TComponent)
  private
    ClientSocket: TClientSocket;
    IdUDPClient: TIdUDPClient;
    ThreadUdp: TClientTrsFlThread;
    FichierC: TFichierEnv;
    FHost: string;
    FSleepTime: integer;
    FPortTCP: integer;
    FPortUDP: integer;
    FFileName: string;
    FOnErreurEvent: TClientTrsFlErreurEvent;
    FOnEvolutionEvent: TClientTrsFlEvolutionEvent;
    FOnFiniEvent: TClientTrsFlFiniEvent;
    procedure SetHost(Value: string);
    procedure SetPortTCP(Value: integer);
    procedure SetPortUDP(Value: integer);
    procedure Annuler;
    procedure TCPConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure TCPRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure TCPError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure TCPDisconnect(Sender: TObject; Socket: TCustomWinSocket);
	published
    property Host: string read FHost write SetHost;            // adresse ip ou autre (ex localhost, votredomaine.com)
    property SleepTime: Integer read FSleepTime write FSleepTime;
    property PortTCP: Integer read FPortTCP write SetPortTCP;  // le port sur lequel se font les dialogue en TCP
    property PortUDP: Integer read FPortUDP write SetPortUDP;  // le port sur lequel se fait l'envoi du fichier
    property FileName: string read FFileName write FFileName;  // le fichier a envoyer
    property Utiliser: boolean read FichierC.Utiliser;         // false = on peut utilise le composant, true = composant deja utiliser pr envoyer un fichier
    property OnErreurEvent: TClientTrsFlErreurEvent read FOnErreurEvent write FOnErreurEvent; // execute si une erreur survient: TransfertAnnuler=true si on ne peut pas continuer donc composant libre (utiliser=false)
    property OnEvolutionEvent: TClientTrsFlEvolutionEvent read FOnEvolutionEvent write FOnEvolutionEvent; // evolution (pour mettre une progressbar par exemple (minimum=0)
    property OnFiniEvent: TClientTrsFlFiniEvent read FOnFiniEvent write FOnFiniEvent; // lorsque le transfert et terminer et que nous n'avons pas eu d'erreur fatal.
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Envoyer; overload;
    procedure Envoyer(AHost: string; APortTCP,APortUDP: integer; AFileName: string; STime: integer); overload;
  end;

implementation

/////////////////////// TClientTrsF ////////////////////////////////////////////

constructor TClientTrsFl.Create(AOwner: TComponent);
begin
 inherited;
 FichierC.Utiliser:=false;
 FichierC.FcAFree:=false;
 ClientSocket:=TClientSocket.Create(AOwner);  // on creer le socket pour les dialogue TCP
 ClientSocket.OnConnect:=TCPConnect;
 ClientSocket.OnRead:=TCPRead;
 ClientSocket.OnDisconnect:=TCPDisconnect;
 ClientSocket.OnError:=TCPError;
 IdUDPClient:=TIdUDPClient.Create(AOwner);    // on creer le composant indy (après l'essai de plusieurs autres composants que j'ai pas réussi a faire marcher je me suis résolu a contre coeur a utiliser les composants indy) pour l'envoi du fichier en UDP.
 ThreadUdp:=TClientTrsFlThread.Create(true);  // Thread qui envoi plien de packet avec des morceaux du fichier
 ThreadUdp.PFichierC:=@FichierC;
 ThreadUdp.OnSendBufferUdp:=IdUDPClient.SendBuffer;
end;

procedure TClientTrsFl.SetHost(Value: string);
begin
  FHost:=Value;
  ClientSocket.Host:=FHost;
  IdUDPClient.Host:=FHost;
end;

procedure TClientTrsFl.SetPortTCP(Value: integer);
begin
  FPortTCP:=Value;
  ClientSocket.Port:=FPortTCP;
end;

procedure TClientTrsFl.SetPortUDP(Value: integer);
begin
  FPortUDP:=Value;
  IdUDPClient.Port:=FPortUDP;
end;

procedure TClientTrsFl.Envoyer(AHost: string; APortTCP,APortUDP: integer; AFileName: string; STime: integer);
begin
  SetHost(AHost);
  SetPortTCP(APortTCP);
  SetPortUDP(APortUDP);
  SleepTime:=STime;
  FFileName:=AFileName;
  Envoyer;
end;

procedure TClientTrsFl.Envoyer;
begin
  if FichierC.Utiliser
  then
   begin
    if Assigned(FOnErreurEvent)
    then FOnErreurEvent(Self,true,'Current file is already transferring now');
    Exit;
   end;
  if ClientSocket.Active
  then
   begin
    if Assigned(FOnErreurEvent)
    then FOnErreurEvent(Self,true,'Error connecting by TCP protocol'); // bizarre si sa arrive (normalement arreter par utliser)
    Exit;
   end;
  if not(FileExists(FFileName))
  then
   begin
    if Assigned(FOnErreurEvent)
    then FOnErreurEvent(Self,true,'File "'+FFileName+'" not found');
    Exit;
   end;

  FichierC.Utiliser:=true;
  FichierC.NombreBlocI:=0;
  FichierC.NombreBlocJ:=-1;
  FichierC.SleepTime:=SleepTime;
  FichierC.Nom:=ExtractFileName(FFileName); // le nom du fichier pour informer le serveur
  if Length(FichierC.Nom)>254
  then FichierC.Nom:=copy(FichierC.Nom,1,254);

  try
    FichierC.Fc:=Tfilestream.create(FFileName,fmOpenRead);
    FichierC.FcAFree:=true;
  except
    FichierC.Utiliser:=false;
    if Assigned(FOnErreurEvent)
    then FOnErreurEvent(Self,true,'Error when reading the file!');
    exit;
  end;

  idUdpClient.Active:=true;    // on ouvre les connexion
  ClientSocket.Open;
end;

procedure TClientTrsFl.TCPConnect(Sender: TObject; Socket: TCustomWinSocket);
var BlocTcp: TBlocTcp;
begin                         // lorsque TCP est connecter
  BlocTcp.bOperation:=2;      // on envoi des infos au serveur: Nom,Taille.
  BlocTcp.bFileNameC:=FichierC.Nom;
  BlocTcp.bSize:=FichierC.Fc.Size;
  FichierC.NombreBlocM:=FichierC.Fc.Size div BlockSize;
  if Assigned(FOnEvolutionEvent) then FOnEvolutionEvent(Self,FichierC.NombreBlocM+1,FichierC.NombreBlocI);
  SetLength(FichierC.BlocOk,FichierC.NombreBlocM+1);  // BlockOK represente les differentes partie du fichier, en effet il est decoupe en bloc de 1000 octet.
  sleep(1);                                           // BlockOK[55]= true si le block 55 (de 55000 octet à 55999 octet) recu par le serveur, si false pas encore recu dc on continue de l'envoyer
  ClientSocket.Socket.SendBuf(BlocTcp,1+255+8); // envoi du packet
end;

procedure TClientTrsFl.TCPRead(Sender: TObject; Socket: TCustomWinSocket);
var Size,i: integer;
    j: int64;
    BlocTcp: TBlocTcp;
begin
  Size:=Socket.ReceiveLength;
  if Size<=SizeOf(BlocTcp) then
  begin
    Socket.ReceiveBuf(BlocTcp,Size);
    case BlocTcp.bOperation of
     1: begin
          case BlocTcp.bOk of
           1: begin  //fini le fichier a bien été envoyer en entier mais on ferme rien en TCP pr attendre que le serveur est recu la confirmation de cette reception
                IdUdpClient.Active:=false;
                ThreadUdp.Suspend;
                BlocTcp.bOperation:=1;
                BlocTcp.bOk:=2;
                Socket.SendBuf(BlocTcp,1+4);
                FichierC.NombreBlocI:=FichierC.NombreBlocM+1;
                if Assigned(FOnEvolutionEvent) then FOnEvolutionEvent(Self,FichierC.NombreBlocM+1,FichierC.NombreBlocI);
              end;
          end;
        end;
     3: begin // recu tout les IRep(definit ds UnitD) block recu par le serveur pour mettre a jour BlockOK
          For i:=0 to IRep-1 do // (on pourra peut etre transformer IRep en propriéte du composant attention si c'est fait ne pas oublier de tenir le server au courant de sa valeur... (lors du premier envoi par TCP))
          begin
            j:=BlocTcp.bFait[i];
            if not(j>FichierC.NombreBlocM+1) and not(j<0) then
              FichierC.BlocOk[j]:=true; // le block j a été recu par le serveur donc le thread ne l'enverra plus.
          end;
          inc(FichierC.NombreBlocI,IRep);
          if Assigned(FOnEvolutionEvent) then FOnEvolutionEvent(Self,FichierC.NombreBlocM+1,FichierC.NombreBlocI);
          BlocTcp.bOperation:=1;
          BlocTcp.bOk:=3;
          Socket.SendBuf(BlocTcp,1+4); // on confirme (certain se demande peut etre pourquoi toutes ces confirmation? c'est simple si elles ni sont pas on enverai peut etre ce packet sur tcp suivit du suivant pr la confirmation des block suivant avec une interval de tps très courte Donc probleme de recption et on fait buguer les sockets.
        end;                           // tant que le serveur n'a pas cette confirmation il ne fait pas attention a ce qu'il recoit en UDP
     4: begin //commencement de l'envoi des données (confirmation de la reception du packet au dessus (BlocTcp.bOperation:=2) lors de TCPConnect)
          For i:=0 to FichierC.NombreBlocM do FichierC.BlocOk[i]:=false; // initialisation de blockOK
          FichierC.NumConnection:=BlocTCP.bNumConnection;
          ThreadUdp.Resume;  // on lance le thread qui se chargera d'envoyer les packet par UDP
        end;
    end;
  end;
end;

procedure TClientTrsFl.TCPDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  if FichierC.NombreBlocI=FichierC.NombreBlocM+1 then
  begin // Deconnection normal ( la deconnection en TCP est tjr fait par le server dans notre programme ) => fin du transfert
   if Assigned(FOnFiniEvent)
   then FOnFiniEvent(Self);
  end
 else
  if Assigned(FOnErreurEvent)
  then FOnErreurEvent(Self,true,'TCP Socket: Disconnected');
  Annuler;
end;

procedure TClientTrsFl.TCPError(Sender: TObject; Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
var S: string;
begin   // erreur dans les socket TCP
  case ErrorEvent of
    eeGeneral: s:='General';
    eeSend: s:='Send';
    eeReceive: s:='Receive';
    eeConnect: s:='Connect';
    eeDisconnect: s:='Disconnect';
    eeAccept: s:='Accept';
    eeLookup: s:='Lookup';
  end;
  if not(ClientSocket.Active) then
  begin
    Annuler;
    if Assigned(FOnErreurEvent)
    then FOnErreurEvent(Self,true,'Port TCP: '+inttostr(ErrorCode)+' - '+s);
  end
 else
  if Assigned(FOnErreurEvent)
  then FOnErreurEvent(Self,false,'Port TCP: '+inttostr(ErrorCode)+' - '+s);
end;

procedure TClientTrsFl.Annuler;  // on reinitialise les variables
begin
  ThreadUdp.Free;                                       // Un peu bourin mais
  ThreadUdp:=TClientTrsFlThread.Create(true);           // je sais pas trop
  ThreadUdp.PFichierC:=@FichierC;                       // Comment faire pour que
  ThreadUdp.OnSendBufferUdp:=IdUDPClient.SendBuffer;    // lorque que le thread repartira il commence au debut de execute
  IdUdpClient.Active:=false;
  ClientSocket.Close;
  SetLength(FichierC.BlocOk,0);
  if FichierC.FcAFree then FichierC.Fc.Free;
  FichierC.FcAFree:=false;
  FichierC.Utiliser:=false; // on peut donc envoyer un autre fichier
end;

destructor TClientTrsFl.Destroy;
begin
  Annuler;
  ThreadUdp.Free;
  IdUDPClient.Free;
  ClientSocket.Free;
  inherited;
end;

end.
