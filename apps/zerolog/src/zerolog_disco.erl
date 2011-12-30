%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

-module(zerolog_disco).
-author('Bip Thelin <bip.thelin@evolope.se>').

-behaviour(gen_leader).

%% API
-export([start_link/1, start_link/2]).

%% gen_leader callbacks
-export([init/1,
         handle_cast/3,
         handle_call/4,
         handle_info/2,
         handle_leader_call/4,
         handle_leader_cast/3,
         handle_DOWN/3,
         elected/3,
         surrendered/3,
         from_leader/3,
         code_change/4,
         terminate/2]).

-include_lib("zerolog.hrl").

-record(zerolog, {id, message}).
-record(state, {zerolog_master, threshold, prefix, tag}).

-define(SERVER, ?MODULE).
-define(MB, 1024 * 1024).
-define(SECOND, 1000).
-define(MINUTE, 60 * ?SECOND).
-define(HOUR, 60 * ?MINUTE).
-define(DAY, 24 * ?HOUR).
-define(PUT_WAIT_TIMEOUT, 1 * ?MINUTE).
-define(TABLE_NAME, zerolog).
-define(TAG, "data:zerolog").
-define(NODES, [node()|nodes()]).
-define(ZEROLOG_MASTER, "http://localhost:8989").
-define(THRESHOLD, 64*?MB).
-define(PREFIX, "0b").
-define(INFO(Message, Args), error_logger:info_msg(lists:flatten(io_lib:format(Message, Args))++"~n")).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Config) ->
    start_link(Config, []).

start_link(Config, Seed) when is_atom(Seed) ->
    start_link(Config, {seed, Seed});

start_link(Config, Opts) ->
	Nodes =  zerolog_config:get_conf(Config, nodes, ?NODES),
    gen_leader:start_link(?SERVER, Nodes, [Opts], ?MODULE, Config, []).

%%%===================================================================
%%% gen_leader callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Config) ->
	Threshold = zerolog_config:get_conf(Config, threshold, ?THRESHOLD),
	ZerologMaster = zerolog_config:get_conf(Config, master, ?THRESHOLD),
	Prefix =  zerolog_config:get_conf(Config, prefix, ?PREFIX),
	Tag =  zerolog_config:get_conf(Config, tag, ?TAG),
	ok = ensure_schema(),
    ok = mnesia:start(),
    ensure_table(),
    {ok, #state{zerolog_master=ZerologMaster, prefix=Prefix, threshold=Threshold, tag=Tag}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called only in the leader process when it is elected. The Synch
%% term will be broadcasted to all the nodes in the cluster.
%%
%% @spec elected(State, Election, undefined) -> {ok, Synch, State}
%% @end
%%--------------------------------------------------------------------
elected(State, _Election, undefined) ->
    ?INFO("~p elected",[node()]),
    {ok, [], State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called only in the leader process when a new candidate joins the
%% cluster. The Synch term will be sent to Node.
%%
%% @spec elected(State, Election, Node) -> {ok, Synch, State}
%% @end
%%--------------------------------------------------------------------
elected(State, _Election, _Node) ->
    ?INFO("~p elected",[node()]),
    {reply, [], State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called in all members of the cluster except the leader. Synch is a
%% term returned by the leader in the elected/3 callback.
%%
%% @spec surrendered(State, Synch, Election) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
surrendered(State, _Synch, Election) ->
	?INFO("~p surrendered to ~p.",[node(), gen_leader:leader_node(Election)]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages. Called in the leader.
%%
%% @spec handle_leader_call(Request, From, State, Election) ->
%%                                            {reply, Reply, Broadcast, State} |
%%                                            {reply, Reply, State} |
%%                                            {noreply, State} |
%%                                            {stop, Reason, Reply, State} |
%%                                            {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_leader_call(_Request, _From, State, _Election) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages. Called in the leader.
%%
%% @spec handle_leader_cast(Request, State, Election) ->
%%                                            {ok, Broadcast, State} |
%%                                            {noreply, State} |
%%                                            {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_leader_cast(push_to_ddfs, #state{zerolog_master=ZerologMaster,
										prefix=Prefix,
										tag=Tag} = State,
										_Election) ->
	db_to_ddfs(ZerologMaster, Prefix, Tag),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling messages from leader.
%%
%% @spec from_leader(Request, State, Election) ->
%%                                    {ok, State} |
%%                                    {noreply, State} |
%%                                    {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
from_leader(_Synch, State, _Election) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling nodes going down. Called in the leader only.
%%
%% @spec handle_DOWN(Node, State, Election) ->
%%                                  {ok, State} |
%%                                  {ok, Broadcast, State} |
%% @end
%%--------------------------------------------------------------------
handle_DOWN(_Node, State, _Election) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State, Election) ->
%%                                   {reply, Reply, State} |
%%                                   {noreply, State} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({handle_log, #message{payload=Payload}},
							_From, #state{threshold=Threshold} = State,
							_Election) ->
	{atomic,_} = persist_transactional(Payload),
	push_to_ddfs(Threshold),
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State, Election) ->
%%                                  {noreply, State} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State, _Election) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_leader when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_leader terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Election, Extra) ->
%%                                          {ok, NewState} |
%%                                          {ok, NewState, NewElection}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Election, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
push_to_ddfs(Threshold) ->
	case db_size(Threshold) of
		time_to_dump ->
			gen_leader:leader_cast(?MODULE, push_to_ddfs);
		_ ->
			ignore
	end,
	ok.

db_to_ddfs(ZerologMaster, Prefix, Tag) ->
	Filename = write_to_file(),
	Url = get_put_path(ZerologMaster, Prefix),
	case ddfs_http:http_put(Filename, Url, ?PUT_WAIT_TIMEOUT) of
		{ok, DiscoUrl} ->
			ok = ddfs_put_tag(ZerologMaster, DiscoUrl, Tag),
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
	{{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
	ID = lists:flatten(io_lib:format("~4..0w~2..0w~2..0w-~2..0w~2..0w~2..0w",
									[Year, Month, Day, Hour, Minute, Second])),
	DbDir = zerolog_config:get_db_dir(),
	File = filename:join([DbDir, ID++".0dmp"]),
	F = fun() ->
		{ok, FD} = file:open(File, [write]),
		write_table_to_file(FD, ?TABLE_NAME),
	    file:close(FD)
	end,
	{atomic, _} = mnesia:transaction(F),
	File.

write_table_to_file(FD, TableName)->
    I =  fun(#zerolog{message=Message},_)->
		file:write(FD, io_lib:format("~s~n",[Message]))
		% Delete Mnesia record
    end,
	case mnesia:is_transaction() of
        true ->
        	mnesia:foldl(I, [], TableName);
        false -> 
            F = fun({Fun, Tab}) -> mnesia:foldl(Fun, [], Tab) end,
            mnesia:activity(transaction, F, [{I, TableName}], mnesia_frag)
    end.

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
    	{error, {_, {already_exists, _}}} -> ok;
    	Error -> Error
  	end.

ensure_table() ->
	case mnesia:create_table(?TABLE_NAME,
								[{disc_copies, [node()]},
    								{attributes, record_info(fields, zerolog)}]) of
    	ok -> ok;
    	{aborted,{already_exists,?TABLE_NAME}} -> ok;
    	Error -> Error
  	end.

persist_transactional(Message) ->
	T = fun() ->
		Id = generate_id(Message),
		Data = #zerolog {
			id=Id,
			message=Message
		},
		mnesia:write(Data)
	end,
	mnesia:transaction(T).

generate_id(Message) ->
	<<I:160/integer>> = crypto:sha(term_to_binary({make_ref(), now()})), 
    erlang:integer_to_list(I, 16).
