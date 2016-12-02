%%% Date: 12/2/16

-module(client).
-import(public_key, [encrypt_private/2, encrypt_public/2,
                     decrypt_private/2, decrypt_public/2]).
-export([setup/0, listener/1]).

setup() ->
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    {ok, PubBin} = file:read_file("id_rsa.pub"),
    {ok, PrivBin} = file:read_file("id_rsa"),
    [PubKey] = public_key:ssh_decode(PubBin, public_key),
    [PrivKey] = public_key:pem_decode(PrivBin),
    ServerKey = rsa_server:get_server_key(),
    spawn(?MODULE, listener, [PrivKey]),
    Message   = fun(Node, Process, Term) -> 
        Bin = term_to_binary(Term),
        OtherPub = rsa_server:get_key(Node), % cache that
        Msg = encrypt_public(encrypt_private(Bin, PrivKey), OtherPub),
        {seam_listener, Node} ! {message, node(), Process, Msg} 
        end,
    Upload = fun() -> 
        Bin = term_to_binary(PubKey),
        rsa_server:register(encrypt_public(Bin, ServerKey))
        end,
    Get_Nodes = fun() ->
        rsa_server:get_nodes()
        end,
    {Message, Upload, Get_Nodes}.


listener(PrivKey) ->
	register(seam_listener, self()),
	db:start_link(in_mem),
	listener_loop(PrivKey).

listener_loop(PrivKey) ->
	Decrypt = fun(Bin, Foreign) ->
        B = decrypt_private(decrypt_public(Bin, Foreign), PrivKey),
        binary_to_term(B)
        end,
	% recieve a message
	receive
		{message, Origin, Recipient, Contents} ->
			Recipient ! Decrypt(Contents, db:get_key(Origin));
		_ ->
			io:format("Bad message recieved!")
	end,
	listener_loop(PrivKey).
