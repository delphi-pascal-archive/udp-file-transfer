unit UnitTh;

interface

uses
  Classes, SysUtils, UnitD, UnitVerif;

type
  TOnSendBufferUdp =  procedure (var ABuffer; const AByteCount: integer) of object;
  TClientTrsFlThread = class(TThread)
  private
    { Déclarations privées }
  protected
    procedure Execute; override;
  public
    PFichierC: ^TFichierEnv; // tout les info sur le fichier et le fichier lui-même
    OnSendBufferUdp: TOnSendBufferUdp; // pour envoyer les packet udp.
  end;

implementation

{ TClientTrsFlThread }

procedure TClientTrsFlThread.Execute;
var
 J: int64;
 BlocUdp: TBlocUdp;
begin
  { Placez le code du thread ici }

  while not(Terminated) do
  begin
    J:=PFichierC^.NombreBlocJ; // le dernier bloc envoyer
    inc(J,1);                  // on prend le suivant
    if J>PFichierC^.NombreBlocM then J:=0;
    While PFichierC^.BlocOk[J] do // plus rapide avec until mais la fleme
    begin                         // on verifie s'il est deja recu. s'il est deja recu par le serveur alors on regarde pour le suivant etc..
      inc(J,1);
      if J>PFichierC^.NombreBlocM
      then J:=0;
    end;
    BlocUdp.bOperation:=2;  // on prepart le packet
    BlocUdp.bNumConnection:=PFichierC^.NumConnection; // on peut peut etre sortir c'est deux truc de la boucle, il ne change pas normalement.
    BlocUdp.bPosC:=J;
    PFichierC^.Fc.Position:=J*BlockSize; // la position réél du block ds le fichier
    if J=PFichierC^.NombreBlocM then
    begin
      PFichierC^.Fc.ReadBuffer(BlocUdp.bBufferEnvC,PFichierC^.Fc.Size mod BlockSize);  // on remplit le buffer
      BlocUdp.bSecur:=MD5DataFromBuffer(BlocUdp.bBufferEnvC,PFichierC^.Fc.Size mod BlockSize); // pour verifier que l'on recoit bien ce que l'on a envoyer
    end else
    begin
      PFichierC^.Fc.ReadBuffer(BlocUdp.bBufferEnvC,BlockSize); // on remplit le buffer
      BlocUdp.bSecur:=MD5DataFromBuffer(BlocUdp.bBufferEnvC,BlockSize); // pour verifier que l'on recoit bien ce que l'on a envoyer
    end;
    OnSendBufferUdp(BlocUdp,1+4+8+BlockSize+16); // envoi. size = 1029 normalement un seul packet
    PFichierC^.NombreBlocJ:=J; // on met a jour le dernier packet envoyer
    Sleep(PFichierC^.SleepTime); // pas trop vite coco sinon on pourait faire buguer la connection du client
  end;
end;

end.
