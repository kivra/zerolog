%% ----------------------------------------------------------------------------
%%
%% zerolog: Log transport that just works with zero effort.
%%
%% Copyright 2012 (c) KIVRA.  All Rights Reserved.
%% http://developer.kivra.com dev@kivra.com
%%
%% This file is provided to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
%% License for the specific language governing permissions and limitations
%% under the License.
%%
%% ----------------------------------------------------------------------------

-module(zerolog_res_zerolog).

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
