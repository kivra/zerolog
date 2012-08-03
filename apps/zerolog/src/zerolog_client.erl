%% ----------------------------------------------------------------------------
%%
%% zerolog: Log transport that just works with zero effort.
%%
%% Copyright (c) 2012 KIVRA
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
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
