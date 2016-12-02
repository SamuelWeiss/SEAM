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
{Aubrey gets to decide this}
