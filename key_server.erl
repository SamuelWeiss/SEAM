% rsa_server.erl
% Aubrey Anderson
% 5 December 2016
%
% This module provides a public key distribution gen_server.
-module(rsa_server).

% Public interface functions
-export([start/0, start/2, stop/0, register/1, get_key/2, 
         get_nodes/1, get_server_key/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
         code_change/3, terminate/2, handle_info/2]).

% Private functions
-export([get_encrypt_send/3]).

-define(SERVER, ?MODULE).


%% ~~~~~~~~~~~~~~~~~~~~~~~~
%% API Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~

% Start the key server
% Either pass in paths to the keys or reads in ssh-style keys from the current 
% directory named "id_rsa.pub" and "id_rsa" by default
start(PubKeyPath, PrivKeyPath) ->
    gen_server:start({global, ?SERVER}, ?MODULE, [PubKeyPath, PrivKeyPath], []).
start() ->
    gen_server:start({global, ?SERVER}, ?MODULE, ["id_rsa.pub", "id_rsa"], []).

% Stop the key server
stop() -> 
    gen_server:cast({global, ?SERVER}, stop).

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% External Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% Register the provided public key with the key server
register(PubKey) ->
    PubBin = term_to_binary(PubKey),
    Envelope = key_crypto:build_envelope(get_server_key(), PubBin),
    gen_server:cast({global, ?SERVER}, {register, node(), Envelope}).

% Return the key associated with the provided node
% PrivKey is necessary to decrypt the key from the server
% Prerequisite: node on which this is called is registered with the key server
get_key(Node, PrivKey) ->
    Envelope = gen_server:call({global, ?SERVER}, {get_key, Node}),
    binary_to_term(key_crypto:extract_message(Envelope, PrivKey)).

% Return the server's public key
get_server_key() ->
    gen_server:call({global, ?SERVER}, get_server_key).

% Return all list of all registered nodes
get_nodes(PrivKey) ->
    Envelope = gen_server:call({global, ?SERVER}, get_nodes),
    binary_to_term(key_crypto:extract_message(Envelope, PrivKey)).

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% gen_server Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% Initialize the key server
init([PubKeyPath, PrivKeyPath]) -> 
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    {ok, PubBin} = file:read_file(PubKeyPath),
    {ok, PrivBin} = file:read_file(PrivKeyPath),
    [{PubKey, _}] = public_key:ssh_decode(PubBin, public_key),
    [PrivKeyEntry] = public_key:pem_decode(PrivBin),
    PrivKey = public_key:pem_entry_decode(PrivKeyEntry),
    {ok, _} = key_db:start_link(),
    {ok, {PubKey, PrivKey}}. % State is a tuple of the public and private keys

% Register the key stored in the Envelope with the provided node
handle_cast({register, Node, Envelope}, {PubKey, PrivKey}) -> 
    KeyBin = key_crypto:extract_message(Envelope, PrivKey),
    Key = binary_to_term(KeyBin),
    key_db:insert({Node, Key}),
    {noreply, {PubKey, PrivKey}};

% Stop the key server
handle_cast(stop, State) ->
    application:stop(asn1),
    application:stop(crypto),
    application:stop(public_key),
    {stop, normal, State}.

% Respond with an encrpyted envelope containing the requested key
handle_call({get_key, Node}, From, State) ->
    Args = [get_key, Node, From],
    % The reply is handled in get_encrypt_send
    spawn(rsa_server, get_encrypt_send, Args),
    {noreply, State};

% Respond with the key server's public key
handle_call(get_server_key, _, State) ->
    {PubKey, _} = State,
    {reply, PubKey, State};

% Respond with an encrpyted envelope containing a list of registered nodes
handle_call(get_nodes, From, State) ->
    Args = [get_nodes, {}, From],
    % The reply is handled in get_encrypt_send
    spawn(rsa_server, get_encrypt_send, Args),
    {noreply, State}.

% This server does not handle info
handle_info(_Info, State) -> {noreply, State}.

terminate(_, _) ->
    key_db:stop().

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Internal Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% Reply to To with the result of the requested call
% Name is the Name of the requested node for get_key
get_encrypt_send(Call, Name, To) -> 
    % DB call
    Result = case Call of
        get_key -> key_db:get_key(Name);
        get_nodes -> key_db:get_nodes()
    end,
    % Encrypt result
    Message = term_to_binary(Result),
    {ClientPid, _} = To,
    ClientKey = key_db:get_key(node(ClientPid)),
    Envelope = key_crypto:build_envelope(ClientKey, Message),
    % Reply
    gen_server:reply(To, Envelope).
