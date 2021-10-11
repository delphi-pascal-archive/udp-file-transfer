unit UnitD;

interface

uses Classes, ScktComp, UnitVerif;

const
  IRep = 18; // Nombre de Bloc avant de prevenir le client qu'on les a.
  BlockSize = 8000; // áîëüøå 8000 - ïåðåäà÷à ôàéëîâ çàâèñàåò

type
  TFichierEnv = record // chez le client
                  Utiliser,FcAFree: boolean; // FcAFree parlant ^^ veut dire que Fc doit etre free si on arrete
                  NumConnection: integer;    // numeros de connection il est donner par le serveur et on l'envoi a chaque bloc envoyer par udp pr que le server sache a quelle fichier appartient ce bloc
                  Fc: TStream;               // le fichier a envoyer
                  BlocOk: array of Boolean;  // les blocks (true si recu par le serveur, false sinon)
                  NombreBlocM,NombreBlocI,NombreBlocJ,SleepTime: int64; // M: nbre que l'on va envoyer, I nombre que l'on a envoyer + confirmation, J c le dernier qu'on a envoyer
                  Nom: string;               // nom du fichier (sans le chemin)
                end;

  TFichierRecC = class // chez le serveur (class au lieu de record pour pouvoir l'ajouter ds la list en tant que TObject (pas très reflechi...)) On en creer un par fichier reçu.
                  FcAFree,EnAttente, LibreEnEcr, AttenteRepbOK3, SocketEnAnnulation: boolean; // EnAttente= true le conteneur peut recevoir un fichier, AttenteRepOK3 pr ralentir les confirmation de reception de bloc en TCP, SocketEnAnnulation pour eviter les boucle infini lors de l'annulation et la fermeture du socket
                  NumConnection: integer; // Numeros de connection
                  Fc: TStream; //le fichier que l'on reçoit
                  BlocOk: array of Boolean; // les blocks (true si on l'a recu, false sinon)
                  NombreBlocM,NombreBlocI,Size: int64; // M: nbre que l'on va envoyer, I nombre que l'on a envoyer + confirmation, Size taille du fichier que l'on reçoit
                  Nom: string; // le Nom du fichier (ex toto.txt)
                  Chemin: string; // le chemin exact du fichier: (ex C:/toto.txt ou C:/toto_(0).txt si C:/toto.txt existe déjà)
                  Socket: Pointer; // pour pouvoir retrouver le socket.
                  Fait: array [0..IRep-1] of int64; // on enregistre les blocks recu avant d'envoyer la mise a jour au client
                end;

  TBlocTcp = packed record
              case bOperation: Byte of
                1: (bOk: integer); // pour les ok ...
                                   // 1 fichier recu en entier
                                   // 2 fini
                                   // 3 reception boperation=3 ok
                2: (bFileNameC: string[255]; bSize: int64); // Client envoi un fichier
                3: (bFait: array [0..IRep-1] of int64); // Pour dire au client ce qu'on a deja
                4: (bNumConnection: integer); //pret pr recevoir (envoyer par le serveur apres reception de 2) avec le NumConnection
             end;
             
  TBlocUdp = packed record
              case bOperation: Byte of
                1: (bOk: integer); // pour les ok ... (inutiliser) ok=confirmation=sure=pas UDP
                2: (bNumConnection: integer; bPosC: int64; bBufferEnvC: array [0..BlockSize-1] of byte; bSecur: TMD5Data); // envoi par udp de plien de morceau du fichier
             end;

implementation

end.
