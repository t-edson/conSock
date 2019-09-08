ConSock 0.4
===========

ConSock is a Lazarus library that implements TCP/IP connections, in Client-Server mode, using sockets.

The library is based on Synapse https://wiki.lazarus.freepascal.org/Synapse

Tested only in Windows.

# Operation

Using the library is easy because it presents objects that comunicates using events. At low level the objects are implemented using blocking sockets and threads.

The connection model is similar to a Client-Server architecture, with the following rules:

1. To implement a communication, there must be one Client and one Server part.
2. Client part must be set with the IP address of the Server part. Server don't need to be set.
3. Server part can connect only to one client. If there are more than one client connecting to the same server, only the first will accepted.
4. Once the connection is established, only client can start communication. The server can respond only to a request from the client.
5. All comnunication is implemented using the same Frame strcuture.

Default IP for Client is 127.0.0.1.
Default port for Client and Server is 80.

# Frame Structure

This library implements a Frame including:

* Header part -> 9 bytes size
* Data part -> Optional. Can be as big as 16777215 bytes

The header part, is small (9 bytes fixed) and have the following structure:

```
---------+---------------------+
1 byte   | Header ID           |
---------+---------------------+
3 bytes  | Data part size      | 
---------+---------------------+
1 byte   | Command             | 
---------+---------------------+
2 bytes  | X parameter         | 
---------+---------------------+
2 bytes  | Y parameter         | 
---------+---------------------+
```

The header ID is always the hexadecimal value $0F, and is a way to identify the start of a frame.

The user can send any command in the frame field "Command". But the values $03 and $83 are reserved to internal dialog of the protocol.

Parameters X and Y, can be used to send additional information for a command.

The "Data part size" indicates the size of the "Data part".

As the "Data part size" field is 3 bytes 

# Library

The library constains 3 units:

* conSockFrames.pas.- Defines the protocol and the routines to code/decode the Frame packet.
* conSockClient.pas.- Defines the base client object TConSockClient.
* conSockServer.pas.- Defines the base server object TConSockServer.


To implement a basic connection, it's needed to create a client and server objtec.

Client Part.- Modeled with the object TConSockClient.

Server Part.- Modeled with the object TConSockServer.

# Simple connection

To implements a simple connection, it's needed to create a client part and a server part:

```
  //Set client
  client := TConSockClient.Create('127.0.0.1');
  client.OnFrameReady := @clientFrameReady;
  client.Connect;
  //Set server
  Server := TConSockServer.Create;
  Server.OnFrameReady := @ServerFrameReady;
```

The state of the connection can be read in the field client.state and in server.State. This state can be:

* cecCONNECTING
* cecCONNECTED
* cecSTOPPED
* cecDEAD

To send data from the client, we can use the following isntruction:

```
  client.SendCommand($FF, 0, 0, sometext);
```
  
Where $FF represents some command, and "sometext" represents the data part of the command. We can use differents commands number, except the values $03 and $83 that are reserved for the library.
 
For a complete code, check the sample projects.