-module(rsa_server).
-import(public_key, [encrypt_private/2, encrypt_public/2, decrypt_private/2,
                     decrypt_public/2, generate_key/1]).
-export([init/0, handle_call/3, handle_cast/2, apply_encrypt_send/4, code_change/3]).
-behaviour(gen_server).

init() -> 
    ok = application:load(asn1),
    ok = application:start(asn1),
    ok = application:load(public_key),
    ok = application:start(public_key),
    {ok, rsa_db:new()}.

handle_cast({register, Node, Data}, {{PubKey, PrivKey}, DB}) -> 
    Key = decrypt_private(Data, PrivKey),
    {noreply, {{PubKey, PrivKey}, rsa_db:insert(DB, Key, Node)}}.

handle_call({get, Node}, From, ) ->
    {{_, PrivKey}, DB} = State,
    Args = [rsa_db, get, [DB, Node], From, PrivKey, DB],
    spawn(rsa_server, apply_encrypt_send, Args),
    {noreply, State};

handle_call({nodes}, From, State) ->
    {{_, PrivKey}, DB} = State,
    Args = [rsa_db, get_nodes, [DB], From, PrivKey, DB],
    spawn(rsa_server, apply_encrypt_send, Args),
    {noreply, State}.
handle_call(get_key, From, State)

apply_encrypt_send(Module, Fun, Args, To, PrivKey, DB) -> 
    Message = term_to_binary(apply(Module, Fun, Args)),
    ClientKey = rsa_db:get(DB, node(To)),
    CText = encrypt_private(encrypt_public(Message, ClientKey), PrivKey),
    gen_server:reply(To, CText).

code_change(_OldVsn, State, _Extra) -> {ok, State}.
