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
-record(state, {zerolog_master, threshold, prefix, tag}).

-define(MB, 1024 * 1024).
-define(SECOND, 1000).
-define(MINUTE, 60 * ?SECOND).
-define(HOUR, 60 * ?MINUTE).
-define(DAY, 24 * ?HOUR).
-define(PUT_WAIT_TIMEOUT, 1 * ?MINUTE).
-define(TABLE_NAME, zerolog).
-define(TAG, "data:zerolog").
-define(ZEROLOG_MASTER, "http://localhost:8989").
-define(THRESHOLD, 64*?MB).
-define(PREFIX, "0b").

init(Config) ->
	Threshold = zerolog_config:get_conf(Config, threshold, ?THRESHOLD),
	ZerologMaster = zerolog_config:get_conf(Config, master, ?THRESHOLD),
	Prefix =  zerolog_config:get_conf(Config, prefix, ?PREFIX),
	Tag =  zerolog_config:get_conf(Config, tag, ?TAG),
	ensure_schema(),
    mnesia:start(),
    ensure_table(),
    {ok, #state{zerolog_master=ZerologMaster, prefix=Prefix, threshold=Threshold, tag=Tag}}.

start_link() ->
	start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

handle_call({handle_log, #message{prio=Prio, payload=Payload}},
				_From, #state{zerolog_master=ZerologMaster,
								prefix=Prefix,
								threshold=Threshold,
								tag=Tag} = State) ->
	persist_transactional(Prio, Payload),
	push_to_ddfs(ZerologMaster, Prefix, Threshold, Tag),
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
push_to_ddfs(ZerologMaster, Prefix, Threshold, Tag) ->
	case db_size(Threshold) of
		time_to_dump ->
			db_to_ddfs(ZerologMaster, Prefix, Tag);
		_ ->
			ignore
	end,
	ok.

db_to_ddfs(ZerologMaster, Prefix, Tag) ->
	Filename = write_to_file(),
	Url = get_put_path(ZerologMaster, Prefix),
	case ddfs_http:http_put(Filename, Url, ?PUT_WAIT_TIMEOUT) of
		{ok, DiscoUrl} ->
			ddfs_put_tag(ZerologMaster, DiscoUrl, Tag),
			file:delete(Filename);
		_ -> ok
	end.

ddfs_put_tag(ZerologMaster, Url, Tag) ->
	Addr = ZerologMaster ++ "/ddfs/tag/" ++ Tag,
	{ok, {{_Version, 200, _Reason}, _Headers, _Body}} =
					httpc:request(post, {Addr, [], "application/json",
									"[["++ Url ++"]]"}, [], []),
	ok.

write_to_file() ->
% Dump Mnesia to filename and return filename
	ok.

get_put_path(ZerologMaster, Prefix) ->
	Addr = ZerologMaster ++ "/ddfs/new_blob/" ++ Prefix,
	{ok, {{_Version, 200, _Reason}, _Headers, Body}} =
					httpc:request(Addr),
	{ok,[_|[{string, _, Url}|_]],_} = erl_scan:string(Body),
	Url.

db_size(Threshold) ->
	F = fun() -> mnesia:table_info(?TABLE_NAME, memory) end,
	case mnesia:activity(transaction, F ,mnesia_frag) of
		S when S >= Threshold -> time_to_dump;
		_ -> ok
	end.

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
