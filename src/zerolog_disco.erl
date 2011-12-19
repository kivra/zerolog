%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

-module(zerolog_disco).
-author('Bip Thelin <bip.thelin@evolope.se>').

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-include_lib("zerolog.hrl").

-record(zerolog, {id, prio, message}).

-define(TABLE_NAME, zerolog).

init(Config) ->
	ensure_schema(),
    mnesia:start(),
    ensure_table(),
    {ok, []}.

start_link() ->
	start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

handle_call({handle_log, #message{prio=Prio, payload=Payload}},
				_From, State) ->
	persist_transactional(Prio, Payload),
	{reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private
ensure_schema() ->
	case mnesia:create_schema([node()]) of
    	ok -> ok;
    	{error, {Node, {already_exists, Node}}} -> ok;
    	Error -> Error
  	end.

%% Private
ensure_table() ->
	case mnesia:create_table(zerolog,
								[{disc_copies, [node()]},
    								{attributes, record_info(fields, zerolog)}]) of
    	ok -> ok;
    	{aborted,{already_exists,zerolog}} -> ok;
    	Error -> Error
  	end.
    
%% Private
binary_to_hex(B) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(B)]).

%% Private
persist_transactional(Prio, Message) ->
	T = fun() ->
		Id = generate_id(Prio, Message),
		Data = #zerolog {
			id=Id,
			prio=Prio,
			message=Message
		},
		mnesia:write(Data)
	end,
	mnesia:transaction(T).

%% Private
generate_id(Prio, Message) ->
	S = string:concat(Prio, Message),
	binary_to_hex(erlang:md5(S)).
