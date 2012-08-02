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

-module(zerolog_client).

-export([run/1]).

-define(DEF_ADDR, "tcp://localhost:2121").

run(N) ->
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, push),
    erlzmq:connect(Socket, ?DEF_ADDR),
    send_msg(Socket, N),
    erlzmq:close(Socket),
    erlzmq:term(Context),
    ok.

send_msg(_Socket, 0) ->
    ok;

send_msg(Socket, N) ->
    Msg = integer_to_list(N) ++ ": Lorem ipsum dolor sit amet, consectetur"
                " adipiscing elit. Nulla auctor blandit varius. Phasellus"
                " consequat ornare massa, ac fringilla augue suscipit sit amet",
    Message = erlang:iolist_to_binary([
                              protobuffs:encode(1, list_to_binary(Msg), string)
                          ]),
    erlzmq:send(Socket, Message),
    send_msg(Socket, N-1).
