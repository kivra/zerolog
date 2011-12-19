%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

-module(zerolog_receiver).
-author('Bip Thelin <bip.thelin@evolope.se>').

-export([init/1, start/1, stop/1]).

-include_lib("zerolog.hrl").

-define(DEF_SERVER, zerolog_server).

-record(state, {context, socket}).

init(Config) ->
	Addr = zerolog_config:get_conf(Config, addr, undefined),
	erlang:display(Addr),
	{ok, Context} = erlzmq:context(),
 	{ok, Socket} = erlzmq:socket(Context, pull),
	ok = erlzmq:bind(Socket, Addr),
	{ok, #state{context=Context, socket=Socket}}.

start(#state{context = _Context, socket = Socket} = State) ->
	spawn(fun() -> receive_loop(Socket) end),
	{ok, State}.

stop(#state{context = Context, socket = Socket} = _State) ->
	erlzmq:close(Socket),
    erlzmq:term(Context).

%% Private
receive_loop(Socket) ->
	{ok, Msg} = erlzmq:recv(Socket),
	{Prio, Message} = binary_to_term(Msg),
	gen_server:call(?DEF_SERVER, {receive_log, #message{prio=Prio, payload=Message}}),
	receive_loop(Socket).
