-module(rsa_server).
-export([init/1, handle_call/3, handle_cast/2, get_encrypt_send/3,
         code_change/3, terminate/2, handle_info/2]).
-export([register/1, get_key/2, get_nodes/1, get_server_key/0]).
-export([start/0, stop/0]).

-define(SERVER, ?MODULE).

-behaviour(gen_server).

%% ~~~~~~~~~~~~~~~~~~~~~~~~
%% API Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~

start() ->
    gen_server:start({global, ?SERVER}, ?MODULE, [], []).

stop() -> 
    application:stop(asn1),
    application:stop(crypto),
    application:stop(public_key),
    gen_server:cast({global, ?SERVER}, stop).

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% External Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

register(PubKey) ->
    PubBin = term_to_binary(PubKey),
    Envelope = broker_crypto:build_envelope(get_server_key(), PubBin),
    gen_server:cast({global, ?SERVER}, {register, node(), Envelope}).

get_key(Node, PrivKey) ->
    Envelope = gen_server:call({global, ?SERVER}, {get_key, Node}),
    binary_to_term(broker_crypto:extract_message(Envelope, PrivKey)).

get_server_key() ->
    gen_server:call({global, ?SERVER}, get_server_key).

get_nodes(PrivKey) ->
    Envelope = gen_server:call({global, ?SERVER}, get_nodes),
    binary_to_term(broker_crypto:extract_message(Envelope, PrivKey)).

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
    {ok, _} = db:start_link(),
    {ok, {PubKey, PrivKey}}.

handle_cast({register, Node, Envelope}, {PubKey, PrivKey}) -> 
    KeyBin = broker_crypto:extract_message(Envelope, PrivKey),
    Key = binary_to_term(KeyBin),
    db:insert({Node, Key}),
    {noreply, {PubKey, PrivKey}};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({get_key, Node}, From, State) ->
    Args = [get_key, Node, From],
    spawn(rsa_server, get_encrypt_send, Args),
    {noreply, State};

handle_call(get_server_key, _, State) ->
    {PubKey, _} = State,
    {reply, PubKey, State};

handle_call(get_nodes, From, State) ->
    Args = [get_nodes, {}, From],
    spawn(rsa_server, get_encrypt_send, Args),
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_, _) ->
    db:stop().

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Internal Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_encrypt_send(Call, Name, To) -> 
    Result = case Call of
        get_key -> db:get_key(Name);
        get_nodes -> db:get_nodes()
    end,
    Message = term_to_binary(Result),
    {ClientPid, _} = To,
    ClientKey = db:get_key(node(ClientPid)),
    Envelope = broker_crypto:build_envelope(ClientKey, Message),
    gen_server:reply(To, Envelope).

