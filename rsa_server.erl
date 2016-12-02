-module(rsa_server).
-import(public_key, [encrypt_private/2, encrypt_public/2, decrypt_private/2,
                     decrypt_public/2, generate_key/1]).
-export([init/1, handle_call/3, handle_cast/2, get_encrypt_send/5,
         code_change/3, terminate/2, handle_info/2]).
-export([register/1, get_key/1, get_nodes/0, get_server_key/0]).
-export([start/0, stop/0]).

-behaviour(gen_server).

%% ~~~~~~~~~~~~~~~~~~~~~~~~
%% API Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~

start() ->
    gen_server:start({global, ?MODULE}, ?MODULE, [], []).

stop() -> 
    application:stop(asn1),
    application:stop(crypto),
    application:stop(public_key),
    gen_server:stop({global, ?MODULE}).

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% External Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

register(Data) ->
    gen_server:cast({global, ?MODULE}, {register, node(), Data}).

get_key(Node) ->
    gen_server:call({global, ?MODULE}, {get_key, Node}).

get_server_key() ->
    gen_server:call({global, ?MODULE}, get_server_key).

get_nodes() ->
    gen_server:call({global, ?MODULE}, get_nodes).

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% gen_server Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init(_) -> 
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    {ok, PubBin} = file:read_file("id_rsa.pub"),
    {ok, PrivBin} = file:read_file("id_rsa"),
    [{PubKey, _}] = public_key:ssh_decode(PubBin, public_key),
    [PrivKeyEntry] = public_key:pem_decode(PrivBin),
    PrivKey = public_key:pem_entry_decode(PrivKeyEntry),
    {ok, DB} = db:start_link(),
    {ok, {{PubKey, PrivKey}, DB}}.

handle_cast({register, Node, Data}, {{PubKey, PrivKey}, DB}) -> 
    KeyBin = decrypt_private(Data, PrivKey),
    Key = binary_to_term(KeyBin),
    db:insert(DB, {Node, Key}),
    {noreply, {{PubKey, PrivKey}, DB}}.

handle_call({get_key, Node}, From, State) ->
    {{_, PrivKey}, DB} = State,
    Args = [get_key, Node, From, PrivKey, DB],
    spawn(rsa_server, get_encrypt_send, Args),
    {noreply, State};

handle_call(get_server_key, _, State) ->
    {{PubKey, _}, _} = State,
    {reply, PubKey, State};

handle_call(get_nodes, From, State) ->
    {{_, PrivKey}, DB} = State,
    Args = [get_nodes, {}, From, PrivKey, DB],
    spawn(rsa_server, get_encrypt_send, Args),
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, {_, DB}) ->
    gen_server:stop(DB).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Internal Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_encrypt_send(Call, Name, To, PrivKey, DB) -> 
    Result = case Call of
        get_key -> db:get_key(DB, Name);
        get_nodes -> db:get_nodes(DB)
    end,
    Message = term_to_binary(gen_server:call(Result)),
    ClientKey = db:get_key(DB, node(To)),
    CText = encrypt_private(encrypt_public(Message, ClientKey), PrivKey),
    gen_server:reply(To, CText).
