%% ----------------------------------------------------------------------------
%%
%% zerolog: Log transport that just works with zero effort.
%%
%% Copyright (c) 2012 KIVRA
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
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
