%%% Date: 12/2/16

-module(client).
-import(public_key, [encrypt_private/2, encrypt_public/2]).
-export([setup/0]).

setup() ->
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    {ok, PubBin} = file:read_file("id_rsa.pub"),
    {ok, PrivBin} = file:read_file("id_rsa"),
    [PubKey] = public_key:ssh_decode(PubBin, public_key),
    [PrivKey] = public_key:pem_decode(PrivBin),
    ServerKey = rsa_server:get_server_key(),
    Message   = fun(Node, Term) -> 
        Bin = term_to_binary(Term),
        OtherPub = rsa_server:get_key(Node),
        Msg = encrypt_public(encrypt_private(Bin, PrivKey), OtherPub),
        Node ! Msg
        end,
    Upload = fun() -> 
        Bin = term_to_binary(PubKey),
        rsa_server:register(encrypt_public(Bin, ServerKey))
        end,
    Get_Nodes = fun() ->
        rsa_server:get_nodes()
        end,
    {Message, Upload, Get_Nodes}.
