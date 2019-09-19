unit Unit1;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  conSockFrames, conSockClient, conSockServer;

type

  { TForm1 }

  TForm1 = class(TForm)
    cliSend: TButton;
    edClient: TEdit;
    Label1: TLabel;
    lblClient: TLabel;
    Label3: TLabel;
    lblServer: TLabel;
    txtClient: TMemo;
    txtServer: TMemo;
    procedure cliSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    client: TConSockClient;
    Server: TConSockServer;
    procedure clientChangeState(newState: TConConnectState);
    procedure clientFrameReady(NomPC: string; tram: TConFrame);
    procedure ServerChangeState(newState: TConConnectState);
    procedure ServerFrameReady(NomPC: string; tram: TConFrame);
  public

  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}
{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Set client
  client := TConSockClient.Create('127.0.0.1');
  client.OnFrameReady := @clientFrameReady;
  client.OnChangeState := @clientChangeState;
  client.Connect;
  //Set server
  Server := TConSockServer.Create;
  Server.OnFrameReady := @ServerFrameReady;
  Server.OnChangeState := @ServerChangeState;
  Server.Connect;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Server.Destroy;
  client.Destroy;
end;

///////////////// Client //////////////////
procedure TForm1.clientFrameReady(NomPC: string; tram: TConFrame);
begin
  if tram.comm <> C_PRESENCE then begin
    txtClient.lines.Add('>' + tram.data);
  end;
end;

procedure TForm1.clientChangeState(newState: TConConnectState);
begin
  lblClient.Caption := StateToString(newState);
end;

procedure TForm1.cliSendClick(Sender: TObject);
begin
  client.SendCommand($FF, 0, 0, edClient.Text);
end;

///////////////// Server //////////////////
procedure TForm1.ServerFrameReady(NomPC: string; tram: TConFrame);
begin
  if tram.comm <> C_PRESENCE then begin
    txtServer.lines.Add(IntToStr(tram.comm)+ '>' + tram.data);
    Server.PutCommand($FF, 0, 0, 'Hi');
  end;
end;

procedure TForm1.ServerChangeState(newState: TConConnectState);
begin
  lblServer.Caption := StateToString(newState);
end;

end.

