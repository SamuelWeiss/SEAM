-module(temp_client_code).
-compile(export_all).
-include("db.erl").
-import(public_key, [encrypt_private/2, encrypt_public/2, decrypt_private/2,
                     decrypt_public/2, generate_key/1]).

listener() ->
	register(seam_listener, self()),
	db:start_link(in_mem),
	listener_loop().

listener_loop() ->
	% recieve a message
	recieve
		{message, Origin, Recipient, Contents} ->
			Recipient ! decrypt(Contents, db:get_key(Origin));
		_ ->
			io:format("Bad message recieved!")
	end.
	listener_loop()

decrypt(Contents, Key) ->
	ok.

% encrypt(Message, Key) ->
% 	ok.

% sender(Destination_node, Destination_process, Message) ->
% 	{seam_listener, Destination_node} ! {message, node(), Destination_process, encrypt(Message)}
