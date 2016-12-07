-module(key_db_eval).
-compile(export_all).


eval() ->
	io:format("Testing DB module\n"),
	key_db:start_link(),
	key_db:insert({a,b}),
	[a] = key_db:get_nodes(),
	key_db:insert({c,d}),
	[c,a] = key_db:get_nodes(),
	key_db:insert({e,f}),
	[e,c,a] = key_db:get_nodes(),
	[b] = key_db:get_key(a),
	key_db:insert({a,b2}),
	[b2] = key_db:get_key(a),
	key_db:insert({a,b3}),
	[b3] = key_db:get_key(a),
	key_db:insert({a,b4}),
	[b4] = key_db:get_key(a),
	key_db:insert({a,b5}),
	[b5] = key_db:get_key(a),
	key_db:insert({a,b6}),
	[b6] = key_db:get_key(a),
	key_db:insert({a,b7}),
	[b7] = key_db:get_key(a),
	key_db:insert({a,b8}),
	[b8] = key_db:get_key(a),
	[e,c,a] = key_db:get_nodes(),
	[f] = key_db:get_key(e),
	io:format("Test Success!\n").

	
