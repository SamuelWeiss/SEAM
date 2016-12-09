% server_eval.erl
% Aubrey Anderson
% 9 December 2016
%
% This module contains a function to stress test the server
-module(server_eval).

-export([test_server/2]).

% Call GetNodes N times. GetNodes should be the function returned by the
% key_client:setup function.
test_server(GetNodes, 0) -> test(GetNodes);
test_server(GetNodes, N) -> test(GetNodes), test_server(GetNodes, N-1).

test(GetNodes) -> spawn(fun() -> io:fwrite('~p~n', [GetNodes()]) end).
