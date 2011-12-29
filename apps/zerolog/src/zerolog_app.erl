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
    zerolog_sup:start_link().

stop(_State) ->
    ok.
