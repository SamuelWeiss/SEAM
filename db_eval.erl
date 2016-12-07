-module(db_eval).
-compile(export_all).


eval() ->
	io:format("Testing DB module\n"),
	db:start_link(),
	db:insert({a,b}),
	[a] = db:get_nodes(),
	db:insert({c,d}),
	[c,a] = db:get_nodes(),
	db:insert({e,f}),
	[e,c,a] = db:get_nodes(),
	[b] = db:get_key(a),
	db:insert({a,b2}),
	[b2] = db:get_key(a),
	db:insert({a,b3}),
	[b3] = db:get_key(a),
	db:insert({a,b4}),
	[b4] = db:get_key(a),
	db:insert({a,b5}),
	[b5] = db:get_key(a),
	db:insert({a,b6}),
	[b6] = db:get_key(a),
	db:insert({a,b7}),
	[b7] = db:get_key(a),
	db:insert({a,b8}),
	[b8] = db:get_key(a),
	[e,c,a] = db:get_nodes(),
	[f] = db:get_key(e),
	io:format("Test Success!\n").

	
