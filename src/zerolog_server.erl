%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

%% @doc zerolog server code

-module(zerolog_server).
-author('Bip Thelin <bip.thelin@evolope.se>').

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(DEF_ENABLED_BACKENDS, [zerolog_tty]).
-define(DEF_RECEIVER, [{addr, "tcp://*:2121"}]).

-type backend() :: atom().

-record(state, {receiver :: term(),
                backends :: list(backend())}).

%% API
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
	Backends = zerolog_config:get_conf(enabled_backends, ?DEF_ENABLED_BACKENDS),
	OrdBackends = ordsets:from_list(Backends),
	ReceiverConfig =  zerolog_config:get_conf(zerolog_receiver, ?DEF_RECEIVER),
	{ok, State} = zerolog_receiver:init(ReceiverConfig),
	{ok, NewState} = zerolog_receiver:start(State),
	process_flag(trap_exit, true),
	{ok, #state{receiver=NewState, backends=OrdBackends}}.

%% @private
handle_call(start_backends, _From, #state{backends = Backends} = State) ->
	start_backends(Backends),
	{reply, ok, State};

handle_call({receive_log, Payload}, _From,
			#state{backends = Backends} = State) ->
	receive_log(Payload, Backends),
	{reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{receiver = ReceiverState} = _State) ->
	zerolog_receiver:stop(ReceiverState),
	ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private
start_backends(Backends) ->
	[begin
		BackendConfig = zerolog_config:get_conf(Backend, []),
		ChildSpec = {Backend,
                 		{Backend, start_link, [BackendConfig]},
                 		permanent,
		                2000,
		                worker,
		                [Backend]},
		supervisor:start_child(zerolog_sup, ChildSpec)
	end || Backend <- Backends],
    ok.

%% Private
receive_log(_Payload, []) ->
	ok;

%% Private
receive_log(Payload, [H|T]) ->
	gen_server:call(H, {handle_log, Payload}),
	receive_log(Payload, T).
