unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  conSockFrames, conSockClient, conSockServer;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnClear: TButton;
    chkClient: TCheckBox;
    chkLog: TCheckBox;
    chkServer: TCheckBox;
    cliSend: TButton;
    edClient: TEdit;
    edServer: TEdit;
    txtClientLog: TMemo;
    txtServerLog: TMemo;
    srvSend: TButton;
    Label1: TLabel;
    lblClient: TLabel;
    Label3: TLabel;
    lblServer: TLabel;
    txtClient: TMemo;
    txtServer: TMemo;
    procedure btnClearClick(Sender: TObject);
    procedure chkClientChange(Sender: TObject);
    procedure chkServerChange(Sender: TObject);
    procedure cliSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure srvSendClick(Sender: TObject);
  private
    Client: TConSockClient;
    Server: TConSockServer;
    procedure clientChangeState(newState: TConConnectState);
    procedure clientFrameReady(NomPC: string; tram: TConFrame);
    procedure clientRegMessage(Source: string; msj: string);
    procedure ServerChangeState(newState: TConConnectState);
    procedure ServerFrameReady(NomPC: string; tram: TConFrame);
    procedure ServerRegMessage(Source: string; msj: string);
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
  Client := TConSockClient.Create('127.0.0.1', '80');
  Client.FrameMessages := true;  //Enable message from the frame processor
  Client.OnFrameReady  := @clientFrameReady;
  Client.OnChangeState := @clientChangeState;
  Client.OnRegMessage  := @clientRegMessage;
  lblClient.Caption    := StateToString(Client.state);
//  client.Connect;
  //Set server
  Server := TConSockServer.Create('80');
  Server.FrameMessages := true;  //Enable message from the frame processor
  Server.OnFrameReady  := @ServerFrameReady;
  Server.OnChangeState := @ServerChangeState;
  Server.OnRegMessage  := @ServerRegMessage;
  lblServer.Caption    := StateToString(Server.state);
//  Server.Connect;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Server.Destroy;
  Client.Destroy;
end;

///////////////// Client //////////////////
procedure TForm1.chkClientChange(Sender: TObject);
begin
  if chkClient.Checked then Client.Connect else Client.Disconnect;
end;
procedure TForm1.clientFrameReady(NomPC: string; tram: TConFrame);
begin
  if tram.comm = C_PRESENCE then begin
    txtClient.lines.Add('<Presence>');
  end else begin
    txtClient.lines.Add('>' + tram.data);
  end;
end;

procedure TForm1.clientRegMessage(Source: string; msj: string);
begin
  if chkLog.Checked then begin
     txtClientLog.Lines.Add(FormatDateTime('hh:nn:ss:zzz',time) + Source + ': ' + msj);
     txtServerLog.Lines.Add('');  //To maintain alignment
  end;
end;

procedure TForm1.clientChangeState(newState: TConConnectState);
begin
  lblClient.Caption := StateToString(newState);
end;

procedure TForm1.cliSendClick(Sender: TObject);
begin
  Client.SendCommand($FF, 0, 0, edClient.Text);
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  txtClient.Clear;
  txtClientLog.Clear;
  txtServer.Clear;
  txtServerLog.Clear;
end;

///////////////// Server //////////////////
procedure TForm1.chkServerChange(Sender: TObject);
begin
  if chkServer.Checked then server.Connect else server.Disconnect;
end;

procedure TForm1.ServerFrameReady(NomPC: string; tram: TConFrame);
begin
  if tram.comm = C_PRESENCE then begin
    txtServer.Lines.Add('<Presence>');
  end else begin
    txtServer.lines.Add(IntToStr(tram.comm)+ '>' + tram.data);
  end;
end;

procedure TForm1.ServerRegMessage(Source: string; msj: string);
begin
  if chkLog.Checked then begin
     txtServerLog.Lines.Add(FormatDateTime('hh:nn:ss:zzz',time) + Source + ': ' + msj);
     txtClientLog.Lines.Add('');  //To maintain alignment
  end;
end;

procedure TForm1.ServerChangeState(newState: TConConnectState);
begin
  lblServer.Caption := StateToString(newState);
end;

procedure TForm1.srvSendClick(Sender: TObject);
begin
  Server.PutCommand($FF, 0, 0, edServer.Text);
end;

end.

