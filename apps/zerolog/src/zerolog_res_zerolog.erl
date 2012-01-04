%% @author Bip Thelin <bip.thelin@evolope.se>
%% @copyright 2010-2011 Evolope.

-module(zerolog_res_zerolog).
-author('Bip Thelin <bip.thelin@evolope.se>').

-export([init/1, allowed_methods/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("zerolog.hrl").

-define(DEF_SERVER, zerolog_server).

init([]) ->
    {ok, undefined}.

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

process_post(Req, State) ->
    Msg = wrq:req_body(Req),
    gen_server:call(?DEF_SERVER, {receive_log, #message{payload=Msg}}),
    {true, wrq:set_resp_body("ok", Req), State}.
