%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

%% @doc zerolog startup code

-module(zerolog_client).
-author('Bip Thelin <bip.thelin@evolope.se>').

-export([run/1]).

-define(DEF_ADDR, "tcp://evodisco01.evolope.com:2121").

run(N) ->
	{ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, push),
    erlzmq:connect(Socket, ?DEF_ADDR),
	send_msg(Socket, N),
	erlzmq:close(Socket),
    erlzmq:term(Context),
    ok.

send_msg(_Socket, 0) ->
	ok;

send_msg(Socket, N) ->
	Msg = integer_to_list(N) ++ ": Lorem ipsum dolor sit amet, consectetur"
				" adipiscing elit. Nulla auctor blandit varius. Phasellus"
				" consequat ornare massa, ac fringilla augue suscipit sit amet",
	Message = erlang:iolist_to_binary([
				    protobuffs:encode(1, list_to_binary(Msg), string)
				]),
	erlzmq:send(Socket, Message),
	send_msg(Socket, N-1).
