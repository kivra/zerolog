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
    ok = start_apps([sasl, crypto, inets, webmachine,
                     luke, erlang_js, mochiweb, os_mon,
                     riak_sysmon, riak_core, riak_pipe, riak_kv]),
    ok = application:start(zerolog).

%% @spec stop() -> ok
%% @doc Stop the zerologserver.
stop() ->
    ok = application:stop(zerolog),
    ok = application:stop(riak_kv),
    ok = application:stop(riak_core),
    ok = application:stop(inets),
    ok = application:stop(crypto),
    ok = application:stop(sasl).

start_apps([]) ->
    ok;
start_apps([H|T]) ->
    ok = application:load(H),
    ok = application:start(H),
    start_apps(T).