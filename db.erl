-module(db).
-compile(export_all).
-include("db_schema.hrl").

%% ~~~~~~~~~~~~~~~~~~~~~~~~
%% API Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, mnesia, []).

start_link(in_mem) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, in_mem, []);

start_link(mnesia) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, mnesia, []).


stop() -> ok.

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% gen_server Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init(in_mem) ->
    {ok, []};

init(mnesia) ->
    setup_mnesia_db(),
    {ok, mnesia}.

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

list_nodes_internal(DB) ->
    lists:map(fun({Name, _}) -> Name end, DB).

get_key_internal(Node, [ {Cur_node, Cur_key} | _Remainder_DB ])
    when Node == Cur_node -> Cur_key;

get_key_internal(Node, [ {_Cur_node, _Cur_key} | Remainder_DB ]) ->
    get_key_internal(Node, Remainder_DB);

get_key_internal(_Node, []) ->
    [].

insert_key_internal(Node, Key, DB) -> 
    [{Node, Key}|DB].

setup_mnesia_db() ->
    mnesia:start(),
    mnesia:create_table(keypair,
        [{attributes, record_info(fields, keypair)}]).

mnesia_db_insert({Node, Key}) ->
    Pair = #keypair{node = Node, key = Key},
    Fun = fun() ->
        mnesia:write(Pair) end,
    mnesia:transaction(Fun),
    mnesia.

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


%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% External Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_nodes() ->
        gen_server:call(?MODULE, get_nodes).

get_key(Name) ->
    gen_server:call(?MODULE, {get_key, Name}).

insert({Name, Key}) ->
    gen_server:cast(?MODULE, {insert_key, Name, Key}).

