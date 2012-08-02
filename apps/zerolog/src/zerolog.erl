%% ----------------------------------------------------------------------------
%%
%% zerolog: Log transport that just works with zero effort.
%%
%% Copyright 2012 (c) KIVRA.  All Rights Reserved.
%% http://developer.kivra.com dev@kivra.com
%%
%% This file is provided to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
%% License for the specific language governing permissions and limitations
%% under the License.
%%
%% ----------------------------------------------------------------------------

-module(zerolog).

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
