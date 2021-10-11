unit UnitServerTrsFl;

interface

uses
  Classes, UnitD, UnitVerif, ScktComp, IdBaseComponent, IdComponent, IdUDPBase,
  IdUDPServer, IdSocketHandle, SysUtils;

type
  TServerTrsFlErreurEvent = procedure (Sender: TObject; TransfertAnnuler: boolean; ErreurText: string; NumConnection: integer) of object;
  TServerTrsFlNouveauFichierEvent = procedure (Sender: TObject; var accepter: boolean; Nom: string; Size: int64; NumConnection: integer) of object;
  TServerTrsFlEvolutionEvent = procedure (Sender: TObject; Maximum,Evolution: integer; NumConnection: integer) of object; //rappel min=0
  TServerTrsFlFiniEvent = procedure (Sender: TObject; Chemin: string; NumConnection: integer) of object;
  TServerTrsFlSimpleEvent = procedure (Sender: TObject) of object;
  TServerTrsFl = class(TComponent)
  private
    ServerSocket: TServerSocket;
    IdUDPServer: TIdUDPServer;
    FPortTCP: integer;
    FPortUDP: integer;
    FActive: boolean;
    FListFichierS : TStringList; // on y enregistre les fichier que l'on recoit permet de les retrouver a partir de leur NumConnection
    FConnectionI: integer;
    FDossier: string;
    FOnErreurEvent: TServerTrsFlErreurEvent;
    FOnNouveauFichierEvent: TServerTrsFlNouveauFichierEvent;
    FOnEvolutionEvent: TServerTrsFlEvolutionEvent;
    FOnFiniEvent: TServerTrsFlFiniEvent;
    FOnConnect,FOnDisconnect: TServerTrsFlSimpleEvent;
    procedure SetPortTCP(Value: integer);
    procedure SetPortUDP(Value: integer);
    procedure SetActive(Value: boolean);
    procedure ServerSocketClientConnection(Sender: TObject; Socket: TCustomWinSocket);
    procedure ServerSocketClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ServerSocketClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure ErrorEvent(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure UDPRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
    procedure Annuler;
  published
    property PortTCP: Integer read FPortTCP write SetPortTCP;  // le port sur lequel se font les dialogue en TCP
    property PortUDP: Integer read FPortUDP write SetPortUDP;  // le port sur lequel se fait l'envoi du fichier
    property Active: boolean read FActive write SetActive;     // si la connection est ouverte (TCP+UDP) = true sinon false
    property Dossier: string read FDossier write FDossier;     // Les recption se font ds ce dossier
    property OnErreurEvent: TServerTrsFlErreurEvent read FOnErreurEvent write FOnErreurEvent;
    property OnFiniEvent: TServerTrsFlFiniEvent read FOnFiniEvent write FOnFiniEvent;
    property OnEvolutionEvent: TServerTrsFlEvolutionEvent read FOnEvolutionEvent write FOnEvolutionEvent;
    property OnNouveauFichierEvent: TServerTrsFlNouveauFichierEvent read FOnNouveauFichierEvent write FOnNouveauFichierEvent;
    property OnConnect: TServerTrsFlSimpleEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TServerTrsFlSimpleEvent read FOnDisconnect write FOnDisconnect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open; overload;
    procedure Open(APortTCP,APortUDP: integer; ADossier: string); overload;
    procedure close;
    procedure AnnulerFichier(NumConnection: integer);    
  end;

implementation

//////////////////////////////// TServerTrsFl ////////////////////////////////////

constructor TServerTrsFl.Create(AOwner: TComponent);
begin
  inherited;
  Randomize;
  FConnectionI:=random(9999)-5000;
  FListFichierS:=TStringList.Create;
  ServerSocket:=TServerSocket.Create(AOwner);
  ServerSocket.OnClientConnect:=ServerSocketClientConnection;    // declancher quand un client se connecte on prepare alors son conteneur de fichier
  ServerSocket.OnClientDisconnect:=ServerSocketClientDisconnect; // le client se deconnecte donc erreur a par si le transfert est déjà fini
  ServerSocket.OnClientRead:=ServerSocketClientRead;             // dès que l'on recoit un packet par TCP
  ServerSocket.OnClientError:=ErrorEvent;                        // Erreur TCP
  IdUDPServer:=TIdUDPServer.Create(AOwner);
  idUDPServer.OnUDPRead:=UDPRead;                                // dès que l'on recoit un packet par UDP
  IdUDPServer.Bindings.Add;                                      // on a un bindings: bindings[0]
end;

procedure TServerTrsFl.SetPortTCP(Value: integer);
begin
  FPortTCP:=Value;
  ServerSocket.Port:=FPortTCP;
end;

procedure TServerTrsFl.SetPortUDP(Value: integer);
begin
  FPortUDP:=Value;
  idUdpServer.Bindings[0].Port:=FPortUDP;
end;

procedure TServerTrsFl.Open;
begin
  SetActive(true);
end;

procedure TServerTrsFl.Open(APortTCP,APortUDP: integer; ADossier: string);
begin
  FDossier:=ADossier;
  SetPortTCP(APortTCP);
  SetPortUDP(APortUDP);
  SetActive(true);
end;

procedure TServerTrsFl.Close;
begin
  SetActive(false);
end;

procedure TServerTrsFl.SetActive(Value: boolean);
begin
  if Value then // on veut ouvrir la connection
  begin
    if not(ServerSocket.Active) then
    begin
      ServerSocket.Open;        // on ouvre le serveur tcp
      idUdpServer.Active:=true; // on ouvre le serveur udp (indy je suis désolé j'ai rien trouver d'autre)
      FActive:=true;
      if Assigned(FOnConnect) then FOnConnect(Self);
    end;
  end else
  begin                        // on veut fermer la connection
    Annuler;                   // on annule tout les fichiers en cours
    idUdpServer.Active:=false; // on ferme le serveur udp
    ServerSocket.Close;        // on ferme le serveur tcp
    FActive:=false;
    if Assigned(FOnDisconnect) then FOnDisconnect(Self);
  end;
end;

procedure TServerTrsFl.ServerSocketClientConnection(Sender: TObject; Socket: TCustomWinSocket);
var FichierS: TFichierRecC;
begin
  inc(FConnectionI,1); // pour avoir un numeros unique pr repéré les packet udp et les associés a leur fichier
  FichierS:=TFichierRecC.Create;             //
  FichierS.NumConnection:=FConnectionI;      // <-//
  FichierS.Socket:=Pointer(Socket);
  Socket.Data:=Pointer(FichierS); // pour retrouver le fichier rapidement qd on recoit un packet tcp
  FichierS.EnAttente:=true;       // on est pret a recevoir un fichier (TCP bOperation=2)
  FichierS.FcAFree:=false;
  FichierS.SocketEnAnnulation:=false;
  FListFichierS.AddObject(inttostr(FConnectionI),FichierS);
end;

procedure TServerTrsFl.ServerSocketClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  if TFichierRecC(Socket.Data).NombreBlocI>TFichierRecC(Socket.Data).NombreBlocM then
  begin // vrai fin, pas d'erreur
    if Assigned(FOnFiniEvent) then FOnFiniEvent(Self,TFichierRecC(Socket.Data).Chemin,TFichierRecC(Socket.Data).NumConnection); // on appelle l'evenement pr prevenir que lon a fini
    if not(TFichierRecC(Socket.Data).SocketEnAnnulation) then
    begin
      TFichierRecC(Socket.Data).SocketEnAnnulation:=true;
      annulerFichier(TFichierRecC(Socket.Data).NumConnection);
    end;
  end else
  begin
    if Assigned(FOnFiniEvent) then FOnErreurEvent(Self,true,'TCP Socket deconnexion',TFichierRecC(Socket.Data).NumConnection);
    if not(TFichierRecC(Socket.Data).SocketEnAnnulation) then
      AnnulerFichier(TFichierRecC(Socket.Data).NumConnection);
  end;
end;

procedure TServerTrsFl.ServerSocketClientRead(Sender: TObject; Socket: TCustomWinSocket);
var Size,i: integer;
    BlocTcp: TBlocTcp;
    accepter: boolean;
begin
  Size:=Socket.ReceiveLength;
  if Size<=SizeOf(BlocTcp) then
  begin
    Socket.ReceiveBuf(BlocTcp,Size);
    case BlocTcp.bOperation of
     1: case BlocTcp.bOk of
         2: begin // confirmation de l'envoi du signal de fin de transfert au client (on peut donc fermer le socket)
              Socket.Close; // appele a l'evenement de fin ds l'evenement de deconexion du socket
            end;
         3: begin
              TFichierRecC(Socket.Data).AttenteRepbOK3:=false;
            end;
        end;
     2: begin
          if TFichierRecC(Socket.Data).EnAttente then
          begin
            TFichierRecC(Socket.Data).EnAttente:=false; // on ne transfert qu'un fichier donc une seul initialisation du transfert.
            TFichierRecC(Socket.Data).LibreEnEcr:=true; // on peut ecrire ds le fichier (permet de pas ecrire ds le fichier de deux facon sa pourrait faire des pb)
            TFichierRecC(Socket.Data).AttenteRepbOK3:=false; // pour pas saturer le tcp. on envoi pas de confirmation de reception de block (et meme on recois plus de bloc) tant que l'on a pas la reception de la confirmation de la reception des block recu par le serveur. (marrante cette phrase ^^)
            TFichierRecC(Socket.Data).NombreBlocI:=0; // incremente dès qu'un block est recu
            TFichierRecC(Socket.Data).Size:=BlocTcp.bSize; // taille du fichier
            TFichierRecC(Socket.Data).NombreBlocM:=TFichierRecC(Socket.Data).Size div BlockSize; // nombre de block total
            TFichierRecC(Socket.Data).Nom:=BlocTcp.bFileNameC; // son petit nom exemple toto.txt
            accepter:=true; // par defaut on accepte les fichier (bonne idée? pq pas...)
            if Assigned(FOnNouveauFichierEvent) then FOnNouveauFichierEvent(Self,accepter,TFichierRecC(Socket.Data).Nom,TFichierRecC(Socket.Data).Size,TFichierRecC(Socket.Data).NumConnection); //on appelle l'evenement pr prevenir de l'arriver d'un nouveau fichier. si en retour accepter = false alors on refuse le fichier, si accepter = true alors on continue
            if not(accepter) then
            begin  // pas accepter on libère tout les truc du fichier avec annulerFichier
              AnnulerFichier(TFichierRecC(Socket.Data).NumConnection);
              exit;
            end;
            SetLength(TFichierRecC(Socket.Data).BlocOk,TFichierRecC(Socket.Data).NombreBlocM+1); // on regle la taille du tableau dynamique
            if FileExists(FDossier+TFichierRecC(Socket.Data).Nom) then  // cette petite partie de code permet de trouver un chemin pr le fichier:
            begin                                                       // il sera ds le dossier "dossier" avec comme nom, son nom a part si il existe deja ds se cas la
              i:=0;                                                     // il s'appelera son nom_(numeros).ext avec le numeros le plus petit pr que le fichier n'existe pas.
              While FileExists(FDossier+ChangeFileExt(TFichierRecC(Socket.Data).Nom,'_('+inttostr(i)+')'+ExtractFileExt(TFichierRecC(Socket.Data).Nom))) do inc(i,1);
              TFichierRecC(Socket.Data).Chemin:=FDossier+ChangeFileExt(TFichierRecC(Socket.Data).Nom,'_('+inttostr(i)+')'+ExtractFileExt(TFichierRecC(Socket.Data).Nom));
            end else TFichierRecC(Socket.Data).Chemin:=FDossier+TFichierRecC(Socket.Data).Nom;
            try
              TFichierRecC(Socket.Data).Fc:=Tfilestream.create(TFichierRecC(Socket.Data).Chemin,fmCreate); // on creer le fichier
              TFichierRecC(Socket.Data).FcAFree:=true;   // donc il faudra le libèré si on annule le transfert par exemple
            except // si une erreur est survenu (creation du fichier imposible)
              if Assigned(FOnErreurEvent) then FOnErreurEvent(Self,true,'Erreur lors de la creation du fichier: '+TFichierRecC(Socket.Data).Chemin,TFichierRecC(Socket.Data).NumConnection);
              AnnulerFichier(TFichierRecC(Socket.Data).NumConnection);
              exit;
            end;
            BlocTcp.bOperation:=4;
            BlocTcp.bNumConnection:=TFichierRecC(Socket.Data).NumConnection; // on envoi le numdeconnection pour identifier le fichier correspondant au packet udp recu
            if Assigned(FOnEvolutionEvent) then FOnEvolutionEvent(Self,TFichierRecC(Socket.Data).NombreBlocM+1,TFichierRecC(Socket.Data).NombreBlocI,TFichierRecC(Socket.Data).NumConnection); // on appelle l'evenement d'evolution (pr une progressbar par exemple
            TCustomWinSocket(TFichierRecC(Socket.Data).Socket).SendBuf(BlocTcp,1+4); // au moins on est sur que socket et TFichierRecC(Socket.Data).Socket^ sont qu'un seul est meme socket ds le cas contraire on aura des pb.
          end;
        end;
    end;
  end;
end;

procedure TServerTrsFl.ErrorEvent(Sender: TObject; Socket: TCustomWinSocket;
 ErrorEvent: TErrorEvent; var ErrorCode: Integer);
var s: string;
begin
  case ErrorEvent of
    eeGeneral: s:='General';
    eeSend: s:='Send';
    eeReceive: s:='Receive';
    eeConnect: s:='Connect';
    eeDisconnect: s:='Disconnect';
    eeAccept: s:='Accept';
    eeLookup: s:='Lookup';
  end;
  if Assigned(FOnErreurEvent) then FOnErreurEvent(self,not(Socket.Connected),'Port TCP: '+inttostr(ErrorCode)+' - '+s,TFichierRecC(Socket.Data).NumConnection);
  if not(Socket.Connected) then
  begin // l'erreur a couper la connexion, on efface tout le fichier
    AnnulerFichier(TFichierRecC(Socket.Data).NumConnection);
  end;
end;

procedure TServerTrsFl.UDPRead(Sender: TObject; AData: TStream;
  ABinding: TIdSocketHandle);
var BlocUdp: TBlocUdp;
    BlocTcp: TBlocTcp;
    MD5Rec: TMD5Data;
    i: integer;
    FichierS: TFichierRecC; // j'espere que sa le recréer pas a chaque fois.
begin
  AData.Read(BlocUdp,AData.Size);
  if not(BlocUdp.bOperation=2) then exit; //seul truc que l'on recoit: boperation=2
  i:=FListFichierS.IndexOf(inttostr(BlocUdp.bNumConnection));
  if (i=-1) then exit; // pas de connection avec ce NumConneciton
  FichierS:=(FListFichierS.Objects[i] as TFichierRecC); // on retrouve le conteneur
  if not(FichierS.BlocOk[BlocUdp.bPosC]) then // si on a pas deja le bloc que l'on recoit
  if not(FichierS.AttenteRepbOK3) and FichierS.LibreEnEcr then   // a voir (peut etre interdire que lorque il reste plus que a envoyer bok2
  begin                                                         //et si on peut ecrire
    FichierS.LibreEnEcr:=false; // interdire d'autre ecriture
    FichierS.Fc.Position:=BlocUdp.bPosC*BlockSize;  // retrouver la position ds le fichier
    if BlocUdp.bPosC=FichierS.NombreBlocM then  // si c le dernier bloc, sa taille ne fait pas forcement 1000
    begin                                       // mais elle fait: FichierS.Size mod 1000
      MD5Rec:=MD5DataFromBuffer(BlocUdp.bBufferEnvC,FichierS.Size mod BlockSize);  // on fait sa somme md5
      if MD5MemEqual(BlocUdp.bSecur,MD5Rec) then     // on verifie l'integrité du packet en la comparant a celle fait a l'envoi
        FichierS.Fc.WriteBuffer(BlocUdp.bBufferEnvC,FichierS.Size mod BlockSize) // si tout va bien on ecrit le bloc dans le fichier
        else  // si non,
        begin // bloc pas bon, c'est pas le même a la reception que a l'envoi
          FichierS.LibreEnEcr:=true; // on libère tout
          exit;                      // sans ecrire notre bloc dans le fichier puisqu'il est corrompu (le bloc)
        end;
    end else
    begin  // si c'est pas le dernier bloc il fait une taille de 1000 octet
      MD5Rec:=MD5DataFromBuffer(BlocUdp.bBufferEnvC,BlockSize); // on fait la somme md5 du bloc
      if MD5MemEqual(BlocUdp.bSecur,MD5Rec) then           // on verifie l'integrité du packet en la comparant a celle fait a l'envoi
        FichierS.Fc.WriteBuffer(BlocUdp.bBufferEnvC,BlockSize)  // si tout va bien on ecrit le bloc dans le fichier
        else  // si non,
        begin // bloc pas bon, c'est pas le même a la reception que a l'envoi
          FichierS.LibreEnEcr:=true; // on libère tout
          exit;
        end;
    end;
    FichierS.BlocOk[BlocUdp.bPosC]:=true;  // on rajoute notre bloc a ceux des déjà recu.
    FichierS.Fait[FichierS.NombreBlocI mod IRep]:=BlocUdp.bPosC; // ceux qu'on enverra au client (numeros des bloc déjà recu) pr qu'il ne les envoi plus
    inc(FichierS.NombreBlocI,1);  // 1 bloc de plus recu
    if Assigned(FOnEvolutionEvent) then FOnEvolutionEvent(Self,FichierS.NombreBlocM+1,FichierS.NombreBlocI,FichierS.NumConnection); // il y a eu une evolution, un bloc a été recu dc evolution augmente(evolution=NombreBlocI)
    if (FichierS.NombreBlocI>FichierS.NombreBlocM) then
    begin // on a recu tout les bloc!
      FichierS.AttenteRepbOK3:=true; // au moin on refusera les bloc étant données qu'on les a tous  (variable pas prevu pr sa a la base mais marche très bien)
      BlocTcp.bOperation:=1;
      BlocTcp.bOk:=1;
      TCustomWinSocket(FichierS.Socket).SendBuf(BlocTcp,1+4);  // on previent le client
    end else if (FichierS.NombreBlocI mod IRep)=0 then // sinon , si on a recu IRep(c'est une constante definit ds UnitD) bloc on tient le client au courant
    begin // on fait la liste de ce que l'on a recu
      FichierS.AttenteRepbOK3:=true;
      BlocTcp.bOperation:=3;
      For i:=0 to IRep-1 do BlocTcp.bFait[i]:=FichierS.Fait[i];
      TCustomWinSocket(FichierS.Socket).SendBuf(BlocTcp,1+8*IRep); // on lui envoi pr pas qu'il nous renvoi les blocs que l'on a deja
    end;
    FichierS.LibreEnEcr:=true; // on peut ecrire ds le fichier maintenant
  end;
end;

procedure TServerTrsFl.Annuler;  // on annule tou
var i: integer;
begin
  IdUDPServer.Active:=false;  // comme sa on est sur de pas faire d'appelle a se que l'on supprime après
  ServerSocket.Close;
  for i:=0 to FListFichierS.Count-1 do
  begin
    if (FListFichierS.Objects[i] as TFichierRecC).FcAFree then (FListFichierS.Objects[i] as TFichierRecC).Fc.Free;   // on libère le fichier si il faut
    (FListFichierS.Objects[i] as TFichierRecC).FcAFree:=false;
    SetLength((FListFichierS.Objects[i] as TFichierRecC).BlocOk,0); // on vide les block (obligatoire c'est l'equivalent d'un free car les tableaux dynamique ne se libère pas tout seul cnotrairement au statique
    (FListFichierS.Objects[i] as TFichierRecC).Free; // on libère le conteneur.
  end;
  FListFichierS.Clear;
end;

procedure TServerTrsFl.AnnulerFichier(NumConnection: integer); // on annule un fichier
var i: integer;
begin
  i:=FListFichierS.IndexOf(inttostr(NumConnection));
  if i=-1 then exit; // fichier déjà annuler
  (FListFichierS.Objects[i] as TFichierRecC).AttenteRepbOK3:=true; // pas prevu pr sa a la base, permet de pas avoir de mauvaise surprise (ex: ajout de données ds un fichier libéré)
  if not((FListFichierS.Objects[i] as TFichierRecC).SocketEnAnnulation) then
  begin
    (FListFichierS.Objects[i] as TFichierRecC).SocketEnAnnulation:=true; // permet d'eviter la boucle j'annule donc je deconnecte donc j'annule donc je deconnecte...
    if (TCustomWinSocket((FListFichierS.Objects[i] as TFichierRecC).Socket).Connected) then
      TCustomWinSocket((FListFichierS.Objects[i] as TFichierRecC).Socket).Close;
  end;    
  if (FListFichierS.Objects[i] as TFichierRecC).FcAFree then (FListFichierS.Objects[i] as TFichierRecC).Fc.Free;   // on libère le fichier si il faut
  (FListFichierS.Objects[i] as TFichierRecC).FcAFree:=false;
  SetLength((FListFichierS.Objects[i] as TFichierRecC).BlocOk,0); // on vide les block (obligatoire c'est l'equivalent d'un free car les tableaux dynamique ne se libère pas tout seul cnotrairement au statique
  (FListFichierS.Objects[i] as TFichierRecC).Free; // on libère le conteneur.
  FListFichierS.Delete(i);      // on supprime la ligne ds la liste
end;

destructor TServerTrsFl.Destroy;
begin
  Annuler;
  IdUDPServer.Free;
  ServerSocket.Free;
  FListFichierS.Free;
  inherited;
end;

end.
