%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

%% @doc zerolog startup code

-module(zerolog).
-author('Bip Thelin <bip.thelin@evolope.se>').

-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    zerolog_sup:start_link().

%% @spec start() -> ok
%% @doc Start the zerolog server.
start() ->
	ok = application:start(sasl),
	ok = application:start(inets),
    ok = application:start(zerolog).

%% @spec stop() -> ok
%% @doc Stop the zerologserver.
stop() ->
    ok = application:stop(zerolog),
    ok = application:stop(inets),
    ok = application:stop(sasl).
