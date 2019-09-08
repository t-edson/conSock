{Part of the conSock library that implements a TCP/IP server, prepared to respond
requests in the Frame format defined in conSockFrames.
Requires the Synapse library.
The main class is TConSockServer, what is a thread, que hace la comunicación con el
exterior, usando eventos sincronizados.

                                              Creado por: Tito Hinostroza  05/2015.
                                              Modificado por: Tito Hinostroza 17/08/2019
}
unit conSockServer;
{$mode objfpc}{$H+}
interface
uses
  Classes, blcksock, synsock, lclproc, sysutils, conSockFrames;

const
  PER_WAIT_PRESENCE = 60;  //Period (in seconds times 10) to wait C_PRESENCE command or any response.
  MSG_CONN_DETECTED = 'Connection detected...';
  MSG_RESET_CONNECT = 'Reseting connection...';
  MSG_PROCESS_CONNEC = 'Processing connetion...';

type

  { TThreadSockServer }
  {Esta clase es la conexión que se usa para conectarse en modo Servidor. Es un hilo para
  manejar las conexiones de manera asíncrona. Está pensada para ser usada solo por
  TConSockServer.
  Se define que TThreadSockCabina solo maneje dos estados:
  cecCONNECTING -> cecCONNECTED }
  TThreadSockServer = class(TConBaseConnect)
  private
    cmdsStack: TCommStack;        //Pila de comandos
    procedure ProcConex(Hsock: TSocket);
  public
    frameState: integer;
    procedure SendFile(tipCom: byte; archivo: String);
    procedure Execute; override;
    //Manejo de Comandos
    procedure PutCommand(comm: byte; ParamX, ParamY: word; const data: string = '');
    function HaveCommand: boolean;
  public //Initializing
    port: string;   //port where listening.
    Constructor Create;
    Destructor Destroy; override;
  end;

  { TConSockServer }
  {Esta clase se usa como una envoltura para administrar la conexión con
   TThreadSockServer, ya que es un hilo, y el manejo directo se hace problemático, por
   las peculiaridades que tienen los hilos.
   }
  TConSockServer = class
  private
    Fport: string;
    Fstate: TConConnectState;
    thr : TThreadSockServer;
    procedure thrChangeState(nuevoEstado: TConConnectState);
    procedure thrRegMessage(NomPC: string; msj: string);
    procedure thrTerminate(Sender: TObject);
    procedure thrFrameReady(NomPC: string; tram: TConFrame);
  public   //Events
    OnChangeState: TEvChangeState;
    OnRegMessage  : TEvRegMessage;  //Indica que ha llegado un mensaje de la conexión
    OnFrameReady  : TEvFrameReady;  //Indica que hay una trama lista esperando
  public  //Properties
    function Connected: boolean;
    function StateStr: string;
    property State: TConConnectState read Fstate;  //De solo lectura
    function HaveCommad: boolean;
  public
    procedure Connect;
    procedure Disconnect;
    procedure PutCommand(command: byte; ParamX, ParamY: word; const data: string='');
    procedure SendFile(command: byte; archivo: String);
    constructor Create(port0: string = '80');
    destructor Destroy; override;
  end;


implementation
function StringFromFile(const FileName: string): string;
//Lee un archivo como una cadena.
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Result, (FileStream.Size div SizeOf(char)));
    FileStream.ReadBuffer(Pointer(Result)^, FileStream.Size);
  finally
    FreeAndNil(FileStream);
  end;
end;
{ TThreadSockServer }
procedure TThreadSockServer.SendFile(tipCom: byte; archivo: String);
//Envía una archivo como parte de la trama. Debe indicarse un comando
//que incluya un archivo como datos.
begin
  if ReceivingFrame then exit;  //Is this really needed?
  PutCommand(tipCom, 0, 0, StringFromFile(archivo));
End;
procedure TThreadSockServer.Execute;
var
  ClientSock:TSocket;
  nIntentos: Integer;
begin
  try
    sock.CreateSocket;
    sock.setLinger(true,10000);
    sock.bind('0.0.0.0', port);  //0.0.0.0 hace que se escuche en todas las interfaces
    sock.listen;
    nIntentos := 0;
    while not terminated do begin
      State := cecCONNECTING;
      if sock.canread(100) then begin
          RegMessage(MSG_CONN_DETECTED);  //Conexión detectada
          ClientSock := sock.accept;
          if sock.lastError=0 then ProcConex(ClientSock);
      end else begin
//         RegMensaje('Conexión fallida.');
         inc(nIntentos);
         if nIntentos > 50 then begin
           nIntentos := 0;
           RegMessage(MSG_RESET_CONNECT);  //Reiniciando coenxión
           //Intenta reabrir la conexión
           sock.CloseSocket;
           sock.CreateSocket;
           sock.setLinger(true,10000);
           sock.bind('0.0.0.0', port);  //0.0.0.0 hace que se escuche en todas las interfaces
           sock.listen;
         end;
      end;
    end;
  except

  end;
end;
procedure TThreadSockServer.ProcConex(Hsock:TSocket);
{Rutina de lazo del servidor, para procesar los paquetes recibidos.
 Se supone que si entra aquí, es porque ya se tiene conexión.}
var
  s: string;
  sock2: TTCPBlockSocket;
begin
  State := cecCONNECTED;
  //RegMessage(MSG_PROCESS_CONNEC);
  sock2:=TTCPBlockSocket.create;
  try
    sock2.socket:=Hsock;
    sock2.GetSins;
    while not terminated do begin
      s := sock2.RecvPacket(PER_WAIT_PRESENCE*100);   //Si no hay datos, espera hasta que llegen
      if sock2.lastError<>0 then break;
      //------------procesa los datos------------
      RegMessage(IntToStr(length(s)) +  ' bytes received.');
      ProcFrame.ProcessReceived(s, @ProcessFrame);
      //verifica si hay trama de respuesta
      if Not cmdsStack.HaveCommand then begin //no hay respuesta
        RegMessage('   Sending M_PRESENCE...');  {En realidad no es necesario, que se
                       envíe, un mensaje cada vez que se reciben datos, ya que en tramas
                       largas, que vienen en varios bloques, se está respondiendo con
                       múltiples mensajes.}
        sock2.SendString(CreateHeader(0, M_PRESENCE));  //envía solo presencia
      end else begin  //Envía el comando más antiguo
        RegMessage('   Sending ' + cmdsStack.FirstCommand.CommName);
        sock2.SendString(cmdsStack.FirstCommand.Header);
        sock2.SendString(cmdsStack.FirstCommand.data);
        cmdsStack.PopCommand;  //quita de la pila
      end;
      if sock2.lastError<>0 then break;
      //-----------------------------------------
    end;
    RegMessage('Socket error: ' + sock2.LastError.ToString);
  finally
    sock2.Free;
  end;
end;
//Manejo de Comandos
procedure TThreadSockServer.PutCommand(comm: byte; ParamX, ParamY: word; const data: string = '');
//Similar pero solo envía un comando, con datos
begin
  cmdsStack.PushCommand(comm, ParamX, ParamY, data);
end;
function TThreadSockServer.HaveCommand: boolean;
begin
  Result := cmdsStack.HaveCommand;
end;
//Constructor y destructor
constructor TThreadSockServer.Create;
begin
  FState := cecCONNECTING; {Estado inicial. Aún no está conectando, pero se asume que
                             está en proceso de conexión. Además, no existe estado
                             "cecDetenido" para TThreadSockCabina.
                             Notar que esta asignación de estado, no
              generará el evento de cambio de estado.}
  cmdsStack := TCommStack.Create;
  inherited Create(true);
end;
destructor TThreadSockServer.Destroy;
begin
  cmdsStack.Destroy;
  inherited Destroy;
end;

{ TConSockServer }
procedure TConSockServer.thrChangeState(nuevoEstado: TConConnectState);
begin
  if Fstate = nuevoEstado then exit;
  Fstate := nuevoEstado;
  if OnChangeState<>nil then OnChangeState(Fstate);
end;
procedure TConSockServer.thrRegMessage(NomPC: string; msj: string);
begin
  if OnRegMessage<>nil then OnRegMessage(NomPC, msj);
end;
procedure TConSockServer.thrTerminate(Sender: TObject);
begin
  { Se ha salido del Execute() y el hilo ya no procesa la conexión. El hilo pasa a un
  estado suspendido, pero aún existe el objeto en memoria, porque no se le define con
  auto-destrucción.}
 thrChangeState(cecSTOPPED);
end;
procedure TConSockServer.thrFrameReady(NomPC: string; tram: TConFrame);
begin
  if OnFrameReady<>nil then OnFrameReady(NomPC, tram);
end;
procedure TConSockServer.PutCommand(command: byte; ParamX, ParamY: word;
  const data: string);
begin
  thr.PutCommand(command, ParamX, ParamY, data);
end;
function TConSockServer.Connected: boolean;
begin
  Result := thr.Connected;
end;
function TConSockServer.StateStr: string;
begin
  Result := StateToString(Fstate);
end;
procedure TConSockServer.SendFile(command: byte; archivo: String);
begin
  thr.SendFile(command, archivo);
end;
function TConSockServer.HaveCommad: boolean;
begin
  Result := thr.HaveCommand;
end;
procedure TConSockServer.Connect;
{Inicia la conexión con el hilo.}
begin
  if Fstate in [cecCONNECTING, cecCONNECTED] then begin
    // El hilo ya existe, y esta conectado o en proceso de conexión.
    { TODO : Para ser más precisos se debería ver si se le ha dado la orden de terminar
    el hilo mirando hilo.Terminated. De ser así, la muerte del hilo es solo cuestion
    de tiempo, (milisegundos si está en estado cecCONNECTED o segundos si está en
    estado cecCONNECTING)
    }
    exit;
  end;
  if Fstate = cecSTOPPED then begin
    // El proceso fue terminado, tal vez porque dio error.
    thr.Destroy;   //libera referencia
    thr := nil;
    //Festado := cecMuerto;  //No es muy útil, fijar este estado, porque seguidamente se cambiará
  end;
  thr := TThreadSockServer.Create;
  thr.port := Fport;
  thr.OnChangeState := @thrChangeState; //Para detectar cambios de estado
  thr.OnChangeState(thr.State);         //Genera el primer evento de estado
  thr.OnTerminate    := @thrTerminate;    //Para detectar que ha muerto
  thr.OnRegMessage   := @thrRegMessage;   //Para recibir mensajes
  thr.OnFrameReady   := @thrFrameReady;
  // Inicia el hilo. Aquí empezará con el estado "Conectando"
  thr.Start;
end;
procedure TConSockServer.Disconnect;
begin
  if Fstate = cecDEAD then begin
    exit;  //Ya está muerto el proceso, o está a punto de morir
  end;
  // La única forma de matar al proceso es dándole la señal
  thr.Terminate;
  {puede tomar unos segundos hasta que el hilo pase a estado suspendido (milisegundos si está
  en estado cecCONNECTED o segundos si está en  estado cecCONNECTING)
  }
end;
constructor TConSockServer.Create(port0: string);
begin
  Fport := port0;       //Default port
  Fstate := cecDEAD;  //Este es el estado inicial, porque no se ha creado el hilo
  Connect;
end;
destructor TConSockServer.Destroy;
begin
  thr.OnFrameReady:=nil;  //para evitar eventos al morir
  thr.OnRegMessage:=nil;  //para evitar eventos al morir
  if Fstate<>cecDEAD then begin
    if thr = nil then begin
      {Este es un caso especial, cuando no se llegó a conectar nunca el hilo}
    end else begin
      //Caso normal en que se ha creado el hilo
      thr.Terminate;
      thr.WaitFor;
      thr.Destroy;
      //estado := cecMuerto;  //No es útil fijar el estado aquí, porque el objeto será destruido
    end;
  end;
  inherited Destroy;
end;

end.
