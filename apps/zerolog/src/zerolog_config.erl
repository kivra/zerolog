%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

%% @doc zerolog server code

-module(zerolog_config).

-export([get_conf/1,
         get_conf/2,
         get_conf/3,
         get_db_dir/0]).

-define(APP, zerolog).

get_conf(Prop) ->
    get_conf(Prop, undefined).

get_conf(Prop, DefVal) ->
    get_conf(application:get_all_env(), Prop, DefVal).

get_conf(Config, Prop, DefVal) ->
    case proplists:get_value(Prop, Config) of
        undefined -> DefVal;
        Value -> Value
    end.

get_db_dir() ->
    Root = filename:absname(""),
    DbDir = filename:join(Root, "db"),
    case filelib:is_file(DbDir) of
        true -> DbDir;
        false -> code:priv_dir(?APP)
    end.
