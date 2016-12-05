-module(client).
-import(public_key, [encrypt_private/2, encrypt_public/2,
                     decrypt_private/2, decrypt_public/2]).
-export([setup/0, setup/2, listener/1]).

setup()-> setup("id_rsa.pub", "id_rsa").

setup(PubKeyPath, PrivKeyPath) ->
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    {ok, PubBin} = file:read_file(PubKeyPath),
    {ok, PrivBin} = file:read_file(PrivKeyPath),
    [PubKey] = public_key:ssh_decode(PubBin, public_key),
    [PrivKey] = public_key:pem_decode(PrivBin),
    ServerKey = rsa_server:get_server_key(),
    spawn(?MODULE, listener, [PrivKey]),
    % TODO: ping a node connected to rsa_server
    rsa_server:register(PubKey),
    Message   = fun(Node, Process, Term) -> 
        Bin = term_to_binary(Term),
        Foreign = cached_key_lookup(Node),
        Envelope = broker_crypto:build_envelope(Foreign, Bin),
        {seam_listener, Node} ! {message, Process, Envelope} 
        end,
    Get_Nodes = fun() ->
        rsa_server:get_nodes()
        end,
    {Message, Get_Nodes}.


listener(PrivKey) ->
	register(seam_listener, self()),
	db:start_link(in_mem),
	listener_loop(PrivKey).

listener_loop(PrivKey) ->
	Decrypt = fun(Msg) ->
        Bin = broker_crypto:extract_message(Msg, PrivKey),
        binary_to_term(Bin)
        end,
	% recieve a message
	receive
		{message, Recipient, Contents} ->
			Recipient ! Decrypt(Contents);
		_ ->
			io:format("Bad message recieved!")
	end,
	listener_loop(PrivKey).

cached_key_lookup(Node) ->
    case db:get_key(Node) of 
        [] ->
	        Key = rsa_server:get_key(Node),
            db:insert({Node, Key}),
            Key;
        Key -> Key
    end.
