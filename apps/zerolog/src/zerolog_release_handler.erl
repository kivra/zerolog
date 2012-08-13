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

-module(zerolog_release_handler).

-export([upgrade_latest/1, make_permanent/1]).

-define(NAME, "zerolog_").

-spec upgrade_latest([]) -> ok | {error, tuple()}.
upgrade_latest([]) ->
    io:format("Starting upgrade... ~n"),
    Latest = get_latest_release(),
    release_handler:unpack_release(?NAME ++ Latest),
    case release_handler:install_release(Latest) of
        {ok, Oldrelease, _} ->
            release_handler:make_permanent(Latest),
            io:format("Old release: ~p~nNew release: ~p~n", [Oldrelease, Latest]);
        {error, Reason} ->
            io:format("~p~n", [Reason])
    end.

-spec make_permanent(Latest :: string()) -> ok | {error, atom()}.
make_permanent(Latest) ->
    release_handler:make_permanent(Latest).

%% Internal functions
-spec get_latest_release() -> Release :: string() | {error, atom()}.
get_latest_release() ->
    {ok, Dir} = file:list_dir("releases/"),
    case [string:sub_string(X, 1,B-1) || X <- Dir, (B = string:str(X, ".tar")) > 0] of
        [] ->
            {error, no_tar};
        [?NAME ++ Release] ->
            Release
    end.
