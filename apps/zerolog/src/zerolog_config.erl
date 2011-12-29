%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

%% @doc zerolog server code

-module(zerolog_config).

-export([get_conf/1,
         get_conf/2,
         get_conf/3]).

-define(app, zerolog).

get_conf(Prop) ->
	get_conf(Prop, undefined).

get_conf(Prop, DefVal) ->
	get_conf(application:get_all_env(), Prop, DefVal).

get_conf(Config, Prop, DefVal) ->
	case proplists:get_value(Prop, Config) of
		undefined -> DefVal;
		Value -> Value
	end.
