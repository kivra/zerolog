%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

%% @doc zerolog server code

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
