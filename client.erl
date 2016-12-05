%%% client.erl: client module for secure messaging between Erlang nodes.
%%% Assumes that client is aware of the rsa_server in some way. 

-module(client).
-import(public_key, [encrypt_private/2, encrypt_public/2,
                     decrypt_private/2, decrypt_public/2]).
-export([setup/0, setup/2, message/3, get_nodes/0, listener/1]).


%% setup: takes in string paths for the client's public and private keys; does
%% one-time start for various encryption requirements and then registers the
%% node with the rsa_server. Should return the atom ok if everything is
%% successfully setup.
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
    ok.

%% setup: same as setup/2 but uses default pathnames for the cient's
%% public and private keys.
setup()-> setup("id_rsa.pub", "id_rsa").

%% message: given a node, process, and term, securely send the term to the
%% specified process on the specified term. Non-blocking operation.
message(Node, Process, Term) -> 
    Bin = term_to_binary(Term),
    Foreign = cached_key_lookup(Node),
    Envelope = broker_crypto:build_envelope(Foreign, Bin),
    {seam_listener, Node} ! {message, Process, Envelope},
    ok.

%% get_nodes: returns a list of nodes available to securely message.
get_nodes() ->
    rsa_server:get_nodes().

%% listener: spawns a caching db and loops to listen for encrypted messages,
%% unencrypts any received messages and passes them along to the original
%% client which ran the setup function.
listener(PrivKey) ->
    % register this spawned process
	register(seam_listener, self()),
    % initialize key caching db
	db:start_link(in_mem),
    % define anonymous function to return encrypted binary as an Erlang term
	Decrypt = fun(Msg) ->
        Bin = broker_crypto:extract_message(Msg, PrivKey),
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
