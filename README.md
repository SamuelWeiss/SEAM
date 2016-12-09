SEAM
====
SEAM stands for Secure Erlang Application Messaging.

The purpose of this project is to enable easy and secure messaging between remote Erlang nodes. It uses Public Key Encryption to protect Erlang messages.

Client Interface:
-----------------

Our client works by calling a single setup function that returns a single anonymous `get_nodes` function.

The setup function takes the optional arguments of PubKey, PrivKey and defaults to using ssh keys in the current directory. It is expected to return:
`{ok, GetNodes, Message}`

Once the setup function is called Get\_Nodes, Message can be invoked.

The Message function takes 3 arguments:
  - Node: The global name of the node that the message should be sent to
  - Process: the registered name or PID of a process on the remote node the message is intended for
  - Term: the erlang term to be sent
  

The Get\_Nodes function takes no arguments and returns a list of Nodes with keys uploaded to the key server.

Server Interface:
-----------------
In order to start an RSA Server instance with a persistent database the erlang instance must be started with some additional argumets. It needs to be called like so:

    unix> erl -mnesia dir '"/your/dir/here"'

This will make sure that the Mnesia database that holds the relevant information writes it to the specified directory

To start the key server call:

    key_server:start().

or
 
    key_server:start(PubKeyPath, PrivKeyPath).

Where the paths are strings with the path to each key. start/0 uses default
values: "./id\_rsa.pub" for the public key and "id\_rsa" for the private key.

File Overview:
--------------

key\_server.erl

* Provides a gen\_server to store the public keys of nodes.
* Provides functions to easily interface with the server.
* Encryption is handled internally.

key\_client.erl

* Provides a function to send encrypted messages to other nodes also registered with the key server.
* Provides a callback function for encrypted messages received other nodes.

key\_crypto.erl

* Provides functions to encrypt and decrypt a message using a combination of RSA and AES encryption.

key\_db.erl

* Provides functions to store and retrieve keys from a mnesia database.
