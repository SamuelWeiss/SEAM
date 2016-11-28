-module(db).
-compile(export_all).

%% ~~~~~~~~~~~~~~~~~~~~~~~~
%% API Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~

% server should spin up a process for every connection for every user
% and have a db that maps messages to processes



start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() -> ok.




%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% gen_server Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init(Args) ->
	register(?MODULE, self()),
    {ok, Args}.

handle_call(get_nodes, _From, State) ->
	{reply, list_nodes_internal(State), State};

handle_call({get_key, Node}, _From, State) ->
	{reply, get_key_internal(Node, State), State}.

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
	not_found.

insert_key_internal(Node, Key, DB) -> 
	[{Node, Key}|DB].

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% External Function Definitions
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_nodes(DB) ->
              % DB here is going to store the Server_name (from chat.erl)
        gen_server:call(
                {?MODULE, DB},
                {get_nodes}).

get_key(DB, Name) ->
	gen_server:call(
		{?MODULE, DB},
		{get_key, Name}).

insert(DB, {Name, Key}) ->
	gen_server:cast(
		{?MODULE, DB},
		{insert, Name, Key}).



