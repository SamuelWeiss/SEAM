% key_db.erl
% Sam Weiss
% 7 December 2016
%
% This module implements a simplified interface for node/key storage
-module(key_db).

% public interface functions 
-export([start_link/0, start_link/1, stop/0,
         insert/1, get_key/1, get_nodes/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2,
         code_change/3, terminate/2, handle_info/2]).

% This file contains the schema that will be used for the Mnesia database
-include("key_db_schema.hrl").

%% ~~~~~~~~~~~~~~~~~~~~~~~~
%% API Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~

% Start the server
% optionally specify whether the database should use mnesia or a list based
% in memory implementation (when mnesia isn't present)
% the default database backend is mnesia as it comes with most erlang installs
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, mnesia, []).

start_link(in_mem) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, in_mem, []);

start_link(mnesia) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, mnesia, []).

% stop the database, for in memory databses this resets all data
stop() -> 
	gen_server:cast({global, ?MODULE}, stop).

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% External Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% get the names of all the nodes current in the database
% if no nodes are registered this returns an empty list
get_nodes() ->
        gen_server:call(?MODULE, get_nodes).

% get the key associated with a given node
% if no node by that name is registered this returns an empty list
get_key(Name) ->
    gen_server:call(?MODULE, {get_key, Name}).

% associate a node with the provided key
% if that node already exists in the database then its value is replaced
insert({Name, Key}) ->
    gen_server:cast(?MODULE, {insert_key, Name, Key}).

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% gen_server Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% Initialize the in memory database by creating an empty list
init(in_mem) ->
    {ok, []};

% Initialize the mnesia database by calling a setup function
% that is defined below
init(mnesia) ->
    setup_mnesia_db(),
    {ok, mnesia}.

%
% These gen_server function calls largely call functions defined below
% For that reason the functions they call are documented.
%

handle_call(get_nodes, _From, mnesia) ->
    {reply, mnesia_get_nodes(), mnesia};

handle_call(get_nodes, _From, State) ->
    {reply, list_nodes_internal(State), State};

handle_call({get_key, Node}, _From, mnesia) ->
    {reply, mnesia_get_key(Node), mnesia};

handle_call({get_key, Node}, _From, State) ->
    {reply, get_key_internal(Node, State), State}.

handle_cast({insert_key, Node, Key}, mnesia) ->
    {noreply, mnesia_db_insert({Node, Key})};

handle_cast({insert_key, Node, Key}, State) ->
    {noreply, insert_key_internal(Node, Key,State)}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Internal Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% search the in memory database for all of its nodes
list_nodes_internal(DB) ->
    lists:map(fun({Name, _}) -> Name end, DB).

% Search for the key in the in memory database
% this also selects the first occurance of a node name
get_key_internal(Node, [ {Cur_node, Cur_key} | _Remainder_DB ])
    when Node == Cur_node -> Cur_key;

get_key_internal(Node, [ {_Cur_node, _Cur_key} | Remainder_DB ]) ->
    get_key_internal(Node, Remainder_DB);

get_key_internal(_Node, []) ->
    [].

% put the new node and key pair on to the front of the database
insert_key_internal(Node, Key, DB) -> 
    [{Node, Key}|DB].

% Make sure that the mnesia database is up and running
% also make sure that a table with the correct schema exists
setup_mnesia_db() ->
    mnesia:start(),
    mnesia:create_table(keypair,
        [{attributes, record_info(fields, keypair)}]).

% compose the provided node and key into a pair that conforms to the 
% database schema. Then create a lambda function to insert the record
% into the database. Finally, we call that lambda function in a transaction
% so that it happens atomically.
mnesia_db_insert({Node, Key}) ->
    Pair = #keypair{node = Node, key = Key},
    Fun = fun() ->
        mnesia:write(Pair) end,
    mnesia:transaction(Fun),
    mnesia.

% compose a function that 
mnesia_get_key(Node) ->
    F = fun() ->
        Key = #keypair{node = Node, key = '$1'},
        mnesia:select(keypair, [{Key, [], ['$1']}])
    end,
    {atomic, Data} = mnesia:transaction(F),
    Data.

mnesia_get_nodes() ->
    F = fun() ->
        Node = #keypair{node = '$1', _ = '_'},
        mnesia:select(keypair, [{Node, [], ['$1']}])
    end,
    {atomic, Data} = mnesia:transaction(F),
    Data.
