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
-define(DEF_ENABLED_RECEIVERS, [zerolog_rest]).

-type backend() :: atom().
-type receiver() :: atom().

-record(state, {backends :: list(backend()),
                receivers :: list(receiver())}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    Backends = zerolog_config:get_conf(enabled_backends, ?DEF_ENABLED_BACKENDS),
    OrdBackends = ordsets:from_list(Backends),
    Receivers = zerolog_config:get_conf(enabled_receivers, ?DEF_ENABLED_RECEIVERS),
    OrdReceivers = ordsets:from_list(Receivers),
    spawn(fun() -> attach_childs(Backends) end),
    spawn(fun() -> attach_childs(Receivers) end),
    {ok, #state{backends=OrdBackends, receivers=OrdReceivers}}.

%% @private
handle_call({receive_log, Payload}, _From,
            #state{backends = Backends} = State) ->
    receive_log(Payload, Backends),
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
attach_childs(Inputs) ->
    [begin
        InputConfig = zerolog_config:get_conf(Input, []),
        ChildSpec = {Input,
                 		{Input, start_link, [InputConfig]},
                 		permanent,
		                2000,
		                worker,
		                [Input]},
        supervisor:start_child(zerolog_sup, ChildSpec)
    end || Input <- Inputs],
    ok.

%% Private
receive_log(_Payload, []) ->
    ok;

%% Private
receive_log(Payload, [H|T]) ->
    gen_server:call(H, {handle_log, Payload}),
    receive_log(Payload, T).
