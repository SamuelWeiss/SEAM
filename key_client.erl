%%% key_client.erl
%%% Russell Parker
%%% 12/9/16
%%% Description: client module for secure messaging between Erlang nodes.
%%%   Assumes that client is connected of the key_server in some way
%%%   Hint: use net_adm:ping(KEY_SERVER)

-module(key_client).
-export([setup/0, setup/2]).

% private functions
-export([message/4, listener/1]).


%% setup: takes in string paths for the client's public and private keys; does
%% one-time start for various encryption requirements and then registers the
%% node with the key_server. Returns a tuple {ok, List, Msg} where List is a
%% function that lists all nodes currently registered on the key server and
%% Msg is a secure messaging function.
setup(PubKeyPath, PrivKeyPath) ->
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    {ok, PubBin} = file:read_file(PubKeyPath),
    {ok, PrivBin} = file:read_file(PrivKeyPath),
    [{PubKey, _}] = public_key:ssh_decode(PubBin, public_key),
    [PrivKeyEntry] = public_key:pem_decode(PrivBin),
    PrivKey = public_key:pem_entry_decode(PrivKeyEntry),
    spawn(?MODULE, listener, [PrivKey]),
    key_server:register(PubKey),
    Get_Nodes = fun() -> key_server:get_nodes(PrivKey) end,
    Message = fun(Node, Process, Term) -> 
                      message(PrivKey, Node, Process, Term)
              end,
    {ok, Get_Nodes, Message}.

%% setup: same as setup/2 but uses default pathnames for the cient's
%% public and private keys.
setup() -> setup("id_rsa.pub", "id_rsa").

%% message: given a node, process, and term, securely send the term to the
%% specified process on the specified term. Non-blocking operation.
message(Priv, Node, Process, Term) -> 
    Bin = term_to_binary(Term),
    Foreign = cached_key_lookup(Node, Priv),
    Envelope = key_crypto:build_envelope(Foreign, Bin),
    {seam_listener, Node} ! {message, Process, Envelope},
    ok.

%% listener: spawns a caching key_db and loops to listen for encrypted messages,
%% unencrypts any received messages and passes them along to the original
%% client which ran the setup function.
listener(PrivKey) ->
    % register this spawned process
	register(seam_listener, self()),
    % initialize key caching key_db
	key_db:start_link(in_mem),
    % define anonymous function to return encrypted binary as an Erlang term
	Decrypt = fun(Msg) ->
        Bin = key_crypto:extract_message(Msg, PrivKey),
        binary_to_term(Bin)
        end,
    % enter loop to listen for encrypted messages
	listener_loop(Decrypt).

%% listener_loop: tail-recursive function to listen for encrypted messages,
%% decrypt them and forward the resulting Erlang terms to the main process.
%% Takes in a callback to decrypt  the received messages.
listener_loop(Decrypt) ->
	% recieve properly formatted message
	receive
		{message, Recipient, Contents} ->
			Recipient ! Decrypt(Contents);
		_ ->
			io:format("Bad message recieved!")
	end,
    % loop
	listener_loop(Decrypt).

%% cached_key_lookup: given a node, opaquely return the corresponding key, 
%% using caching when possible. If a key is not yet cached it is added
%% to the cache when retrieved from the key_server.
cached_key_lookup(Node, Priv) ->
    case key_db:get_key(Node) of 
        % key is not yet cached
        [] ->
	        Key = key_server:get_key(Node, Priv),
            key_db:insert({Node, Key}),
            Key;
        % key is already cached
        Key -> Key
    end.
