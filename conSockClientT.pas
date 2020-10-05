{Part of the conSock library that implements a TCP/IP client, prepared to generate
requests in the Frame format defined in conSockFrames.
Requires the Synapse library.
The main class is TConSockClientT, wich is a thread that make communication using
synchronized events.

                                              Created by: Tito Hinostroza  18/09/2019
}
unit conSockClientT;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, synamisc, conSockFrames;

const
  MAX_LIN_MSJ_CNX = 10;    //Max. number of lines saved in connectios messages.
  MSG_OPENIG_PORT  = 'Opening port ...';
  MSG_CONNECT_ERROR ='!Connection error.';
var
  perSendPresence: integer = 50;  //Period (in seconds times 10) to send C_PRESENCE command.
  perWaitPresence: integer = 100; //Period (in seconds times 10) to wait M_PRESENCE command or any response.

type

  { TThreadSockClient }
  {Esta clase es la conexión que se usa para conectarse en modo cliente. Es un hilo para
   manejar las conexiones de manera asíncrona. Está pensada para ser usada solo por
  TConSockClient.
   El ciclo de conexión normal de TThreadSockClient es:
   cecCONNECTING -> cecCONNECTED }
  TThreadSockClient = class(TConBaseConnect)
  private
    ip: string;   //IP del servidor al que se conectará
    port: string;
    procedure OpenConnection;
  protected
    procedure Execute; override;
  public
    reconnect: boolean;
    procedure sendCommand(comm: byte; ParamX, ParamY: word; data: string = '');
    constructor Create(ip0: string; port0: string);
    Destructor Destroy; override;
  end;

  { TConSockClient }
  {Se usa esta clase como una envoltura para administrar la conexión TThreadSockClient,
   ya que es un hilo, y el manejo directo se hace problemático, por las peculiaridades
   que tienen los hilos.
   El ciclo de conexión normal de TConSockClient es:
   cecDEAD -> cecCONNECTING -> cecCONNECTED -> cecDEAD
   Cuando se pierde la conexión puede ser:
   cecDEAD -> cecCONNECTING -> cecCONNECTED -> cecCONNECTING -> cecCONNECTED -> ...
   }
  TConSockClient = class
  private
    Fport: string;
    FIP: string;
    Freconnect: boolean;
    Fstate: TConConnectState;
    thr: TThreadSockClient;
    procedure thrChangeState(nuevoEstado: TConConnectState);
    procedure thrRegMessage(NomCab: string; msj: string);
    procedure thrTerminate(Sender: TObject);
    procedure thrFrameReady(NomCab: string; tram: TConFrame);
    function GetStateN: integer;
    procedure SetStateN(AValue: integer);
    procedure SetIP(AValue: string);
    procedure SetPort(AValue: string);
    procedure SetReconnect(AValue: boolean);
  public   //Events
    OnChangeState: TEvChangeState;
    OnRegMessage  : TEvRegMessage;  //Indica que ha llegado un mensaje de la conexión
    OnFrameReady  : TEvFrameReady;  //Indica que hay una trama lista esperando
  public  //Properties
    mac : string;   //Dirección Física
    property state: TConConnectState read Fstate
             write Fstate;   {"estado" es una propiedad de solo lectura, pero se habilita
                               la escritura, para cuando se usa CabConexión sin Red}
    property stateN: integer read GetStateN write SetStateN;
    function stateStr: string;
    property IP: string read FIP write SetIP;
    property port: string read Fport write SetPort;
    property reconnect: boolean read Freconnect write SetReconnect;
  public
    MsgError: string;       //Last error message string.
    MsgsConn: TstringList;  //Last messages from connection.
    CommMessages: boolean;   //Enable Communication messages.
    FrameMessages: boolean;  //Enable Frame processing messages.
    procedure Connect;
    procedure Disconnect;
    procedure SendWakeOnLan;
    procedure SendCommand(command: byte; ParamX, ParamY: word; data: string = '');
    constructor Create(ip0: string = '127.0.0.1'; port0: string='80');
    destructor Destroy; override;
  end;

implementation

{ TThreadSockClient }
procedure TThreadSockClient.OpenConnection;
begin
  RegMessage(MSG_OPENIG_PORT);
  repeat
    //Intenta abrir una conexión
    State := cecCONNECTING;
    sock.Connect(ip, port);  {Se bloquea unos segundo si no logra la conexión. No hay forma
                             directa de fijar un "Timeout", ya que depende de la implementación
                             del Sistema Operativo}
    if sock.LastError = 0 then begin
      //State := cecCONNECTED;
    end else begin
      RegMessage(MSG_CONNECT_ERROR);
      if not reconnect then Terminate;
      { Genera temporización por si "sock.Connect", sale inmediátamente. Esto suele suceder
       cuando hay un error de red. }
      sleep(1000);
    end;
  until (sock.LastError = 0) or Terminated;
  //sock.LastError = 0 no garantiza que haya logrado conectarse a un servidor.
end;
//Acciones sincronizadas
procedure TThreadSockClient.Execute;
var
  buffer: String = '';
  ticsNoSending: Integer;
  ticsNoReceiving: Integer;
begin
  OpenConnection;
  if terminated then exit;  //por si se canceló
  //Aquí ya logró AbrirConexion el socket con el destino, debería haber control
  ticsNoSending := 0;   //inicia cuenta
  ticsNoReceiving := 0;   //inicia cuenta
  RegMessage('Sending C_PRESENCE.');
  sock.SendString(CreateHeader(0, C_PRESENCE)); //tal vez debe verificarse primero si se puede enviar
  RegMessage('Waiting response ...');
  while not terminated do begin
    buffer := sock.RecvPacket(0);
    if buffer <> '' then begin
      // Hubo datos
      State := cecCONNECTED;  //Solo cuando hay respuesta, asumimos que hay conexión.
      RegMessage(IntToStr(length(buffer)) +  ' bytes received.');
      //ticsNoSending := 0;
      ticsNoReceiving := 0;
      ProcFrame.ProcessReceived(buffer, @ProcessFrame);
    end;
    Inc(ticsNoSending);
    Inc(ticsNoReceiving);
//    if tics mod 10 = 0 then RegMensaje('  tic=' + IntToStr(tics));
    if ticsNoSending>perSendPresence+1 then begin
      //No se ha enviado ningún comando en 5 segundos. Genera uno propio para mantner la conexión.
      RegMessage('Sending C_PRESENCE.');
      sock.SendString(CreateHeader(0, C_PRESENCE));
      ticsNoSending := 0;
    end;
    if ticsNoReceiving>perWaitPresence then begin
      //Probablemente se cortó la conexión
      RegMessage('Connection lost.');
      sock.CloseSocket;  //cierra conexión
      if reconnect then begin
        OpenConnection;
        ticsNoReceiving := 0;
      end else begin
        Terminate;
      end;
    end;
    sleep(100);  //periodo del lazo
  end;
end;
procedure TThreadSockClient.sendCommand(comm: byte; ParamX, ParamY: word;
  data: string='');
{Envía una trama sencilla de datos, al socket. }
var
  s: string;
begin
  if State <> cecCONNECTED then begin
    exit;
  end;
  ticsNoSending := 0;   //Clear flag
  //Se debe enviar una trama
  writestr(s, comm);
  RegMessage('  >>Sent: ' + s + ' ');
  //ENvía
  if data='' then begin
    //es una ProcTrama simple
    sock.SendString(CreateHeader(0, comm, ParamX, ParamY ));
  end else begin
    sock.SendString(CreateHeader(length(data), comm, ParamX, ParamY ));
    sock.SendString(data);
  end;
end;
constructor TThreadSockClient.Create(ip0: string; port0: string);
begin
  ip := ip0;
  port := port0;
  FState := cecCONNECTING; {Estado inicial. Aún no está conectando, pero se asume que
                             está en proceso de conexión. Además, no existe estado
                             "cecDetenido" para TThreadSockClient.
                             Notar que esta asignación de estado, no generará el evento de
                             cambio de estado, porque estamos en el constructor}
  inherited Create(true);  //crea suspendido
end;
destructor TThreadSockClient.Destroy;
begin
  inherited Destroy;
end;

{ TConSockClient }
procedure TConSockClient.thrChangeState(nuevoEstado: TConConnectState);
begin
  if Fstate = nuevoEstado then exit;
  Fstate := nuevoEstado;
  if OnChangeState<>nil then OnChangeState(Fstate);
end;
procedure TConSockClient.thrRegMessage(NomCab: string; msj: string);
begin
  //debugln(nombre + ': '+ msj);
  MsgsConn.Add(msj);  //Agrega mensaje
  //Mantiene tamaño, eliminando los más antiguos
  while MsgsConn.Count>MAX_LIN_MSJ_CNX do begin
    MsgsConn.Delete(0);
  end;
  if OnRegMessage<>nil then OnRegMessage('', msj);
end;
procedure TConSockClient.thrFrameReady(NomCab: string; tram: TConFrame);
begin
  //debugln(nombre + ': Trama recibida: '+ IntToStr(tram.tipTra));
  if OnFrameReady<>nil then OnFrameReady('', tram);
end;
procedure TConSockClient.thrTerminate(Sender: TObject);
begin
  { Se ha salido del Execute() y el hilo ya no procesa la conexión. El hilo pasa a un
  estado suspendido, pero aún existe el objeto en memoria, porque no se le define con
  auto-destrucción.}
 thrChangeState(cecSTOPPED);
end;
function TConSockClient.stateStr: string;
{Convierte TCibEstadoConex a cadena}
begin
 Result := StateToString(Fstate);
end;
procedure TConSockClient.SendWakeOnLan;
{Envía señal para encender PC remotamente.}
begin
  WakeOnLan(mac, '');
end;
procedure TConSockClient.SetIP(AValue: string);
begin
  //Only it can be changed when it's not connected.
  if state in [cecDEAD, cecSTOPPED] then begin
    FIP := AValue;
  end else begin
    self.MsgError := 'Cannot change IP when connection is established or in progress.';
  end;
end;
procedure TConSockClient.SetPort(AValue: string);
begin
  //Only it can be changed when it's not connected.
  if state in [cecDEAD, cecSTOPPED] then begin
    Fport := AValue;
  end else begin
    self.MsgError := 'Cannot change port when connection is established or in progress.';
  end;
end;
procedure TConSockClient.SetReconnect(AValue: boolean);
begin
  //Only it can be changed when it's not connected.
  if state in [cecDEAD, cecSTOPPED] then begin
    Freconnect := AValue;
  end else begin
    self.MsgError := 'Cannot change "reconnect" when connection is established or in progress.';
  end;
end;
function TConSockClient.GetStateN: integer;
begin
  Result := ord(Fstate);
end;
procedure TConSockClient.SetStateN(AValue: integer);
begin
 Fstate := TConConnectState(AValue);
end;
procedure TConSockClient.Connect;
{Crea el hilo con la IP actual e inicia la conexión}
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
    //Festado := cecDEAD;  //No es muy útil, fijar este estado, porque seguidamente se cambiará
  end;
  thr := TThreadSockClient.Create(FIP, Fport);
  thr.CommMessages  := CommMessages;   //Set property
  thr.FrameMessages := FrameMessages;  //Set property
  thr.reconnect     := Freconnect;     //Set property
  //Set events
  thr.OnChangeState := @thrChangeState; //Para detectar cambios de estado
  thr.OnChangeState(thr.State);         //Genera el primer evento de estado
  thr.OnTerminate   := @thrTerminate;   //Para detectar que ha muerto
  thr.OnRegMessage  := @thrRegMessage;  //Para recibir mensajes
  thr.OnFrameReady  := @thrFrameReady;
  // Inicia el hilo. Aquí empezará con el estado "Conectando"
  thr.Start;
end;
procedure TConSockClient.Disconnect;
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
procedure TConSockClient.SendCommand(command: byte; ParamX, ParamY: word;
  data: string = '');
begin
  if state<>cecCONNECTED then
    exit;
  thr.sendCommand(command, ParamX, ParamY, data);
end;
constructor TConSockClient.Create(ip0: string; port0: string);
begin
  MsgsConn:= TstringList.Create;
  FIP := ip0;
  Fport := port0;
  Freconnect := true;  //By default
  CommMessages := true;   //Default setting
  FrameMessages := false; //Default setting
  Fstate := cecDEAD;  //este es el estado inicial, porque no se ha creado el hilo
  //Connect;  //Start connection
end;
destructor TConSockClient.Destroy;
begin
  //Verifica si debe detener el hilo
  if Fstate<>cecDEAD then begin
    if thr = nil then begin
      {Este es un caso especial, cuando no se llegó a conectar nunca al hilo o cuando se
       usa a TConSockClient, sin red. Por los tanto no se crea nunca el hilo}
    end else begin
      //Caso normal en que se ha creado el hilo
      thr.Terminate;
      thr.WaitFor;  //Si no espera a que muera, puede dejarlo "zombie"
      thr.Destroy;
      //estado := cecDEAD;  //No es útil fijar el estado aquí, porque el objeto será destruido
    end;
  end;
  MsgsConn.Destroy;
  inherited Destroy;
end;

end.

