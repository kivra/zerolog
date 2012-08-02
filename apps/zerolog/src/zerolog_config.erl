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

-module(zerolog_config).

-export([get_conf/1,
         get_conf/2,
         get_conf/3,
         get_log_dir/0,
         get_db_dir/0]).

-define(APP, zerolog).
-define(LOGDIR, "log").
-define(DBDIR, "data").

get_conf(Prop) ->
    get_conf(Prop, undefined).

get_conf(Prop, DefVal) ->
    get_conf(application:get_all_env(), Prop, DefVal).

get_conf(Config, Prop, DefVal) ->
    case proplists:get_value(Prop, Config) of
        undefined -> DefVal;
        Value -> Value
    end.

get_log_dir() ->
	?LOGDIR.

get_db_dir() ->
    ?DBDIR.
