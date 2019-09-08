{
Unit used to define the Frame format to communicate using sockets over TCP/IP.
Here, is defined too, the base class (a thread) for deriving client and server part.
The Frame format, here defined, was taken from the program NILOTER-m (Frames with 9
bytes header), but it's implmented in this library, in a way it can be used in any
other TCP/IP communication.

                                        Creado por Tito Hinostroza
                                        Modificado por Tito Hinostroza 27/03/2018
                                        Modificado por Tito Hinostroza 17/08/2019
}
unit conSockFrames;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, LCLProc, blcksock;

Const
  HEADER_ID = 15;    //Identificador de mensaje
  HEADER_SIZE = 9;    //Tamaño de encabezado

  //Mensajes usados internamente para control de comunicaciones
//Sería más apropiado cambiarlos a $00 y $80, pero se mantienen en este valor por compatibilidad con el cliente NILOTER
  M_PRESENCE = $03;  //Mensaje de presencia. Sin datos
  C_PRESENCE = $83;  //Comando de solicitud de presencia

type
  { TConFrame }
  {Objeto que representa a una trama de comunicación del Servidor con las PC cliente.}
  TConFrame = class
  private
    function GetHeader: string;
    procedure SetHeader(AValue: string);
  public
    //Campos del encabezado
    id     : Byte;    //Header Id
    size   : longword; //Data size in 3 significativ bytes. DOn't consider the header size
    comm   : byte;    //Comando o Tipo de trama
    posX   : word;    //Posición X (en algunas tramas viene este dato)
    posY   : word;    //Posición Y (en algunas tramas viene este dato)
    //Campos de datos
    data   : String;   //Parte de los datos de la trama
    //Campos calculados
    property Header: string read GetHeader write SetHeader;  //encabezado como cadena
    function CommName: string; //Tipo de trama como cadena
    function CommHex: string; //Tipo de trama como código hexadecimal
  public
    procedure Assign(desde: TConFrame);
    procedure Init(comando: byte; ParamX, ParamY: word; cad: string='');
  end;
  TCPTrama_list = specialize TFPGObjectList<TConFrame>;

  TEvFrameReceived = procedure of object;  //evento de trama recibida
  TEvRegMessage = procedure(Source: string; msj: string) of object;

  { TConFrameProc }
  {Procesador de Tramas. Objeto que permite reconstruir las tramas a partir de los
   paquetes recibidos.}
  TConFrameProc = class
    BytesReceiv: Integer;
    BytesWait  : Integer;
    frame      : TConFrame; //trama recibida
    msjErr     : string;   //mensaje de error
    procedure ProcessReceived(var s: string; ProcesarTrama: TEvFrameReceived);
  public
    OnRegMensaje  : TEvRegMessage;   {Para generar un mensaje. Ya que no sabemos si esta
                                     trama, está dentro de un hilo o no.}
    procedure AcumulateFrame(dat: string; pos_ini: Longint=0; pos_fin: Longint=0);
    procedure InitFrame;
  public  //Constructor y Destructor
    constructor Create;
    Destructor Destroy; override;
  end;

  { TPilaCom }
  {Modela a una pila LIFO de comandos. Pensada para ser usada en el envío de comandos}
  TCommStack = class
  private
    items: TCPTrama_list;
  public
    procedure PushCommand(tram: TConFrame);
    procedure PushCommand(comando: byte; ParamX, ParamY: word; cad: string=''
      );
    procedure PopCommand;
    function HaveCommand: boolean;
    function FirstCommand: TConFrame;
  public  //Constructor y destructor
    constructor Create;
    Destructor Destroy; override;
  end;

type //Manejo de la conexión con hilos

  // Estados de una  conexión
  TConConnectState = (
    cecCONNECTING,  //Conectando.
    cecCONNECTED,   //Conectado con socket.
    cecSTOPPED,     //El proceso se detuvo (no hay control).
    cecDEAD         //Proceso detenido.
  );

  TEvChangeState = procedure(newState: TConConnectState) of object;

  TEvFrameReady = procedure(NomPC: string; tram: TConFrame) of object;

  { TConBaseConnect }
  {Clase base que se usa para implementar conexiones con sockets trabajando como hilos.
   Notar que esta clase define a un hilo y debe ser implementada como tal.
   Se puede usar tanto para el lado del cliente como para la del servidor. La diferencia
   estaría en cómo se implemente el Execute().}
  TConBaseConnect = class(TThread)
  private
    procedure EventFrameReady;
    procedure EventChangeState;
    procedure EventRegMessage;
  protected
    FState    : TConConnectState;
    Sock      : TTCPBlockSocket;
    ProcFrame : TConFrameProc;    //Para procesar tramas
    tmpEstCnx : TConConnectState; //Para pasar parámetro a EventoCambiaEstado()
    tmpRegMsje: string;           //Para pasar parámetro a EventoRegMensaje()
  public  //Eventos y Acciones sincronizadas
    OnRegMessage  : TEvRegMessage;  //Indica quue desea registrar un mensaje
    OnFrameReady  : TEvFrameReady;  //Indica que hay una trama lista esperando
    OnChangeState : TEvChangeState; //Cambio en estado de la conexión
    procedure SetState(AValue: TConConnectState);
    procedure RegMessage(msje: string);
    procedure ProcFrameRegMessage(Source: string; msj: string);
  public
    CommMessages: boolean;   //Enable Communication messages
    FrameMessages: boolean;  //Enable Frame processing messages
    property State: TConConnectState read FState write SetState;
    function Connected: boolean;
    function StateStr: string;
    function ReceivingFrame: boolean;
    procedure ProcessFrame;
  public
    constructor Create(CreateSuspended: Boolean;
                       const StackSize: SizeUInt = DefaultStackSize);
    Destructor Destroy; override;
  end;

  function CreateHeader(size: Longint; TipoDato: byte; ParamX: word = 0; ParamY: word = 0): string;
  function StateToString(state: TConConnectState): string;

implementation

function CreateHeader(size: Longint; TipoDato: byte; ParamX: word = 0; ParamY: word = 0): string;
//Genera la trama de mensaje de 9 bytes, envía dos parámetros Long
begin
  Result := '         ';  //valor inicial
  //Identificador de mensaje
  Result[1] := chr(HEADER_ID);
  //Envía tamaño de datos (sin considerar el encabezado)
  Result[2] := Chr((size shr 16) and 255); //mayor peso
  Result[3] := Chr((size shr 8) and 255);
  Result[4] := Chr(size and 255);          //menor peso
  //Tipo de dato
  Result[5] := Chr(TipoDato);
  //Envía parámetros adicionales
  Result[6] := Chr((ParamX shr 8) and 255);
  Result[7] := Chr((ParamX) and 255);
  Result[8] := Chr((ParamY shr 8) and 255);
  Result[9] := Chr((ParamY) and 255);
End;
function StateToString(state: TConConnectState): string;
begin
  case state of
  cecCONNECTING : exit('Connecting');
  cecCONNECTED  : exit('Connected');
  cecSTOPPED    : exit('Stopped');
  cecDEAD       : exit('Dead');
  else exit('???');
  end;
end;

{ TConBaseConnect }
//Acciones sincronizadas
procedure TConBaseConnect.SetState(AValue: TConConnectState);
begin
  if FState=AValue then Exit;
  FState:=AValue;
  tmpEstCnx := FState;
  Synchronize(@EventChangeState); //dispara evento sicnronizando
end;
procedure TConBaseConnect.RegMessage(msje: string);
{Procedimiento para generar un mensaje dentro del hilo.}
begin
  if not CommMessages then exit;
  tmpRegMsje := msje;
  Synchronize(@EventRegMessage);
end;
procedure TConBaseConnect.ProcFrameRegMessage(Source: string; msj: string);
begin
  if not FrameMessages then exit;
  //Genera mensajes detallados de la conexión
  tmpRegMsje := msj;
  Synchronize(@EventRegMessage);
end;
procedure TConBaseConnect.ProcessFrame;
//Llegó una trama y hay que hacer algo. La trama está en "Encab" y "traDat"
begin
  //Dispara evento sincronizando con proceso padre. Esto puede hacer que deba
  //esperar hasta que haya terminado algún procesamiento.
  Synchronize(@EventFrameReady);
End;
constructor TConBaseConnect.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  sock := TTCPBlockSocket.create;
  FreeOnTerminate := False;
  ProcFrame:= TConFrameProc.Create;
  ProcFrame.OnRegMensaje := @ProcFrameRegMessage;
  inherited Create(CreateSuspended, StackSize);
end;
destructor TConBaseConnect.Destroy;
begin
  ProcFrame.Destroy;
  sock.Destroy;
  RegMessage('Process finished.');
  //estado := cecMuerto;  //No es útil fijar el estado aquí, porque el objeto será destruido
  inherited Destroy;
end;
procedure TConBaseConnect.EventFrameReady;
begin
  if OnFrameReady <> nil then OnFrameReady('', ProcFrame.frame);
end;
procedure TConBaseConnect.EventChangeState;
begin
  if OnChangeState<>nil then OnChangeState(tmpEstCnx);
end;
procedure TConBaseConnect.EventRegMessage;
begin
  if OnRegMessage<>nil then OnRegMessage('', tmpRegMsje);
end;
function TConBaseConnect.Connected: boolean;
begin
  Result := FState = cecCONNECTED;
end;
function TConBaseConnect.StateStr: string;
{Convierte TCabEstadoConex a cadena}
begin
 Result := StateToString(FState);
end;
function TConBaseConnect.ReceivingFrame: boolean;
begin
  Result := ProcFrame.BytesWait <> 0;
end;

{ TCPTrama }
function TConFrame.GetHeader: string;
begin
  Result := CreateHeader(size, comm, posX, posY);
end;
procedure TConFrame.SetHeader(AValue: string);
{Lee las propiedades a partir de una cadena. Este método es usado cuando se recibe una
cadena por el socket.}
begin
  id:= ord(AValue[1]);
  size := ord(AValue[2]) * 65536 + ord(AValue[3]) * 256 + ord(AValue[4]);
  comm := ord(AValue[5]);
  //en caso que se indique coordenadas
  posX := ord(AValue[6]) * 256  + ord(AValue[7]);
  posY := ord(AValue[8]) * 256  + ord(AValue[9]);
end;
function TConFrame.CommName: string;
{Devuelve el tipo de trama como cadena}
begin
  try
    writestr(Result, comm);
  except
    Result := '<<Unknown>>'
  end;
end;
function TConFrame.CommHex: string;
{Devuelve el tipo de trama como número hexadecimal}
begin
  Result := IntToHex(ord(comm), 2);
end;
procedure TConFrame.Assign(desde: TConFrame);
{Copia el contenido, desde un objeto similar.}
begin
  id:= desde.id;
  size  := desde.size;
  comm  := desde.comm;
  posX    := desde.posX;
  posY    := desde.posY;
  data  := desde.data;
end;
procedure TConFrame.Init(comando: byte; ParamX, ParamY: word; cad: string);
{Inicializa la trama con parámetros}
begin
  id:= HEADER_ID;
  size  := length(cad);
  comm  := comando;
  posX    := ParamX;
  posY    := ParamY;
  data  := cad;
end;
{ TPilaCom }
procedure TCommStack.PushCommand(tram: TConFrame);
{Agrega una trama a la cola, que representa a un comando.}
var
  com: TConFrame;
begin
  com := TConFrame.Create;  //crea nueva trama
  com.Assign(tram);  //copia valores
  items.Add(com);
end;
procedure TCommStack.PushCommand(comando: byte; ParamX, ParamY: word; cad: string
  );
var
  com: TConFrame;
begin
  com := TConFrame.Create;  //crea nueva trama
  com.Init(comando, ParamX, ParamY, cad);
  items.Add(com);
end;
procedure TCommStack.PopCommand;
{Quita el comando más antiguo de la cola}
begin
  if not HaveCommand then exit;
  items.Delete(0);
end;
function TCommStack.HaveCommand: boolean;
begin
  Result := items.Count>0;
end;
function TCommStack.FirstCommand: TConFrame;
{Devuelve una referencia al primer comando de la pila (el más antiguo)}
begin
  if not HaveCommand then exit(nil);
  Result := items[0];
end;
//Constructor y destructor
constructor TCommStack.Create;
begin
  items:= TCPTrama_list.Create(true);
end;
destructor TCommStack.Destroy;
begin
  items.Destroy;
  inherited Destroy;
end;
{ TCibProcTrama }
procedure TConFrameProc.AcumulateFrame(dat: string; pos_ini: Longint = 0; pos_fin: Longint = 0);
{Agrega los bytes de dat a la "traDat()"
"pos_ini" es la posición inicial donde se encuentran los datos en "dat()"
Si no se especifica se asume que se debe copiar desde el principio.
"pos_fin" es la posición final hasta donde se encuentran los datos en "dat()"
Si no se especifica se asume que se debe copiar hasta el final.
"pos_ini" y "pos_fin" van desde 1 hasta length(dat)}
begin
  If pos_ini = 0 Then
    pos_ini := 1;           //valor por defecto
  If pos_fin = 0 Then
    pos_fin := length(dat); //valor por defecto
  frame.data += copy(dat, pos_ini, pos_fin - pos_ini + 1);
End;

procedure TConFrameProc.InitFrame;
//Inicializa estado para recibir un Frame
begin
  BytesReceiv := 0;          //Inicia contadores
  BytesWait := 0;
  frame.data := '';             //Inicia la matriz de datos
end;
procedure TConFrameProc.ProcessReceived(var s: string; ProcesarTrama: TEvFrameReceived);
{Procesa un fragmento de trama de datos que ha llegado por el puerto.
 Si se detecta error, en el procesamiento, se devuelve el mensaje en "msjErr".
 Cuando se ha terminado de procesar una trama completa, llama al evento ProcesarTrama().}
{ TODO : Ver si se puede quitar el VAR del parámetro  "s" }
  procedure EndFrame;
  //Se llama al finalizar la recepción de una trama
  begin
    if ProcesarTrama<>nil then ProcesarTrama;   //Hace algo con la trama que llegó
    InitFrame;
  end;
var
  bytesTotal: Integer;
begin
  msjErr := '';
  bytesTotal := length(s);  //bytes que llegan
  if bytesTotal = 0 then exit;  //No data received.
  if (BytesWait = 0) and (ord(s[1]) = HEADER_ID) then begin
    //The start of frame
    if bytesTotal < HEADER_SIZE then begin
      msjErr := '-Error: Frame too small.';
      exit;  //Actually, we don't expect to have a very small packet
    end;
    frame.Header := s;   //Read only the header size bytes
    //procesa los bytes restantes
    If bytesTotal = HEADER_SIZE Then begin
      //We have only the header in this frame.
      if frame.size = 0 then begin
          //No problem. We didn't expeted data.
          EndFrame;  //Ends the frame
      end else begin
       //Se esperan datos. Tal vez lleguen en los siguientes paquetes
       BytesWait := frame.size;   //Update waited bytes from header.
       BytesReceiv := 0;          //We only have processed the header.
      end;
    end else If bytesTotal > HEADER_SIZE then begin
      //We have more bytes to ptocess, Could be the data part or more packets in one.
      BytesWait := frame.size;   //Update waited bytes from header.
      BytesReceiv := 0;          //We only have processed the header.
      delete(s, 1, HEADER_SIZE);  //Extract header
      ProcessReceived(s, ProcesarTrama);  //Process next part
    end;
  end else if BytesWait = 0 then begin
    //We don't expect a packet and this is not the begin of another frame
    if OnRegMensaje<>nil then begin
      OnRegMensaje('', '-Unexpected packet ' + IntToStr(bytesTotal) + ' bytes (' +
                       IntToStr(BytesWait) + ':' + IntToStr(Ord(s[1])) +').');
    end;
    //We don't do anything with ths packet
  end else begin //BytesWait > 0
    //Otro paquete que contiene datos de una trama anterior o puede ser basura
    //Verificamos el tamaño del paquete
    BytesReceiv += bytesTotal;   //Actualiza contador
//          Log "Llegó paquete de " & bytesTotal & " bytes. Acumulados: " & BytesRecib
    if BytesReceiv = BytesWait then begin
      if OnRegMensaje<>nil then
        OnRegMensaje('', '-Frame completed with this packet');
      AcumulateFrame(s);
      EndFrame;
    end else If BytesReceiv < BytesWait then begin
      //Todavía hay más paquetes de la trama que deben llegar
      if OnRegMensaje<>nil then begin
        OnRegMensaje('', '-Acumulating packets of Frame: ' + IntToStr(BytesReceiv)+'/' + IntToStr(BytesWait) + ' bytes.');
//        OnRegMensaje('', '-Acumulating paquetes de trama: ' + IntToStr(BytesReceiv)+'/' + IntToStr(BytesWait) + ' bytes.');
      end;
      AcumulateFrame(s);
    end else begin //BytesReceiv > BytesWait
      //Está llegando lo que falta de la trama anterior y probablemente
      //el inicio de otra trama
      if OnRegMensaje<>nil then
        OnRegMensaje('', '-Frame completed but there is more data.');
      //Complete first frame
      AcumulateFrame(s, 0, bytesTotal - (BytesReceiv - BytesWait) );
      delete(s, 1, bytesTotal - (BytesReceiv - BytesWait));  //Extract data from first frame
      EndFrame;   //End frame
      //Llama recursivamente para procesar siguientes tramas
      ProcessReceived(s, ProcesarTrama);   //puede salir con error
    end;
  end;
end;
//Constructor y Destructor
constructor TConFrameProc.Create;
begin
  frame  := TConFrame.Create;
  InitFrame;  //Inicia recepción
end;
destructor TConFrameProc.Destroy;
begin
  frame.Destroy;
  inherited Destroy;
end;

end.

