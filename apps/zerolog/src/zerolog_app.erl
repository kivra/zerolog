%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

%% @doc zerolog application code

-module(zerolog_app).
-author('Bip Thelin <bip.thelin@evolope.se>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	error_logger:info_msg("Starting Zerolog~n"),
	setup_common(),
    Ret = zerolog_sup:start_link(),
    gen_server:call(zerolog_server, start_backends),
    gen_server:call(zerolog_server, start_receiver),
	Ret.


stop(_State) ->
    ok.

setup_common() ->
	Root = filename:absname(""),
	DbDir = filename:join(Root, "db"),
	case filelib:is_file(DbDir) of
		true -> application:set_env(mnesia, dir, DbDir);
		false -> ignore
	end.
