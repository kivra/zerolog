%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

%% @doc zerolog startup code

-module(zerolog).
-author('Bip Thelin <bip.thelin@evolope.se>').

-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
	start_common(),
    zerolog_sup:start_link().

%% @spec start() -> ok
%% @doc Start the zerolog server.
start() ->
	start_common(),
    application:start(zerolog),
    gen_server:call(zerolog_server, start_backends).

%% @spec stop() -> ok
%% @doc Stop the zerologserver.
stop() ->
    Res = application:stop(zerolog),
    application:stop(mnesia),
    Res.

%% @private
start_common() ->
	ok.
