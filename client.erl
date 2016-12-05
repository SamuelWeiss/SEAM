%%% Assumption: client is aware of the rsa_server in some way. 

-module(client).
-import(public_key, [encrypt_private/2, encrypt_public/2,
                     decrypt_private/2, decrypt_public/2]).
-export([setup/0, setup/2, listener/1]).


%% setup: takes in string paths for the client's public and private keys; does
%% one-time start for various encryption requirements and then registers the
%% node with the rsa_server. Two anonymous functions are returned in a tuple: 
%% the first takes in a node, process and term and securely delivers the term
%% to the given process on the given node. The second lists the nodes that are
%% available to message.
setup(PubKeyPath, PrivKeyPath) ->
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    {ok, PubBin} = file:read_file(PubKeyPath),
    {ok, PrivBin} = file:read_file(PrivKeyPath),
    [PubKey] = public_key:ssh_decode(PubBin, public_key),
    [PrivKey] = public_key:pem_decode(PrivBin),
    spawn(?MODULE, listener, [PrivKey]),
    rsa_server:register(PubKey),
    Message = fun(Node, Process, Term) -> 
        Bin = term_to_binary(Term),
        Foreign = cached_key_lookup(Node),
        Envelope = broker_crypto:build_envelope(Foreign, Bin),
        {seam_listener, Node} ! {message, Process, Envelope} 
        end,
    Get_Nodes = fun() ->
        rsa_server:get_nodes()
        end,
    {Message, Get_Nodes}.

%% setup: same as setup/2 but uses default pathnames for the cient's
%% public and private keys.
setup()-> setup("id_rsa.pub", "id_rsa").


%% listener: spawns a caching db and loops to listen for encrypted messages,
%% unencrypts any received messages and passes them along to the original
%% client which ran the setup function.
listener(PrivKey) ->
    % register this spawned process
	register(seam_listener, self()),
    % initialize key caching db
	db:start_link(in_mem),
    % enter loop to listen for encrypted messages
	listener_loop(PrivKey).

%% listener_loop: tail-recursion loop to listen for encrypted messages, decrypt
%% them and forwards unencrypted Erlang terms to the main process. Takes in the
%% client's private key as the only argument to decrypt messages.
listener_loop(PrivKey) ->
	Decrypt = fun(Msg) ->
        Bin = broker_crypto:extract_message(Msg, PrivKey),
        binary_to_term(Bin)
        end,
	% recieve properly formatted message
	receive
		{message, Recipient, Contents} ->
			Recipient ! Decrypt(Contents);
		_ ->
			io:format("Bad message recieved!")
	end,
	listener_loop(PrivKey).

%% cached_key_lookup: given a node, opaquely return the corresponding key, 
%% using caching when possible. If a key is not yet cached it is added
%% to the cache when retrieved from the rsa_server.
cached_key_lookup(Node) ->
    case db:get_key(Node) of 
        % key is not yet cached
        [] ->
	        Key = rsa_server:get_key(Node),
            db:insert({Node, Key}),
            Key;
        % key is already cached
        Key -> Key
    end.
