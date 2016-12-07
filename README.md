SEAM
====
SEAM stands for Secure Erlang Application Messaging.

The purpose of this project is to enable easy and secure messaging between remote Erlang nodes. It uses Public Key Encryption to protect Erlang messages.

Client Interface:
-----------------

Our client works by calling a single setup function that return three anonymous function.

The setup function takes no arguments and returns the following tuple of functions:
{Message, Upload, Get_Nodes}

The Message function takes 3 arguments:
  - Node: The global name of the node that the message should be sent to
  - Process: the registered name or PID of a process on the remote node the message is intended for
  - Term: the erlang term to be sent
  
The Upload function takes no arguments and simply uploads a key to the rsa key server

The Get_Nodes function takes no arguments and returns a list of Nodes that have keys in teh key server.

Server Interface:
-----------------

In order to start an RSA Server instance with a persistent database the erlang instance must be started with some additional argumets. It needs to be called like so:
unix> erl -mnesia dir '"/your/dir/here"'

This will make sure that the Mnesia database that holds the relevant information writes it to the specified directory

To start the key server call:
    rsa_server:start(). 
or 
    rsa_server:start(PubKeyPath, PrivKeyPath). 
Where the paths are strings with the path to each key. start/0 uses default
values: "./id_rsa.pub" for the public key and "id_rsa" for the private key.
