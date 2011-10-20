%% Copyright (c), 2011 Drimmi (http://www.drimmi.com)
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without modification,
%% are permitted provided that the following conditions are met:
%%
%% Redistributions of source code must retain the above copyright notice,
%% this list of conditions and the following disclaimer.
%% Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(social_net_api_utils).

-export
([
    merge/2,
    sort/1,
    delete/2,
    find/2,
    find/3,
    concat/2,
    concat/3,
    http_request/1,
    get_network_module/1,
    timestamp/0,
    md5_hex/1,
    md5_hex/2,
    call_functor/2,
    test/0
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge(List1, List2) ->
    lists:keymerge(1, sort(List1), sort(List2)).

sort(List) ->
    lists:keysort(1, List).

delete(Key, List) ->
    lists:keydelete(Key, 1, List).

find(Key, List) ->
    proplists:get_value(Key, List, nil).

find(Key, List, integer) ->
    case find(Key, List) of
        nil -> nil;
        Res -> list_to_integer(Res)
    end.

concat(Args, Separator) ->
    lists:flatten(lists:reverse(concat_vals(Args, Separator, []))).

concat(Args, RowSeparator, ColSeparator) ->
    lists:flatten(lists:reverse(concat_pairs(Args, RowSeparator, ColSeparator, []))).

concat_vals([], _, Result) ->
    Result;
concat_vals([Value], _, Result) ->
    [to_list(Value)|Result];
concat_vals([Value|Tail], Separator, Result) ->
    S = [to_list(Value), Separator],
    concat_vals(Tail, Separator, [S|Result]).

concat_pairs([], _, _, Result) ->
    Result;
concat_pairs([{Key, Value}], RowSeparator, _, Result) ->
    S = [to_list(Key), RowSeparator, to_list(Value)],
    [S|Result];
concat_pairs([{Key, Value}|Tail], RowSeparator, ColSeparator, Result) ->
    S = [to_list(Key), RowSeparator, to_list(Value), ColSeparator],
    concat_pairs(Tail, RowSeparator, ColSeparator, [S|Result]).

http_request(Request) ->
    httpc:request(Request).

get_network_module(Network) when is_atom(Network) ->
    list_to_atom( "social_net_api_network_" ++ atom_to_list(Network) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_list(A) when is_list(A)      -> A;
to_list(A) when is_atom(A)      -> atom_to_list(A);
to_list(A) when is_integer(A)   -> integer_to_list(A);
to_list(A) when is_float(A)     -> float_to_list(A);
to_list(A) when is_binary(A)    -> binary_to_list(A).

timestamp() ->
    now_to_seconds(erlang:now()).

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.

md5_hex(Data) ->
    md5_hex(Data, bin).

md5_hex(Data, bin) ->
    bin_to_hex(erlang:md5(Data));

md5_hex(Data, list) ->
    list_to_hex(binary_to_list(erlang:md5(Data))).

bin_to_hex(B) when is_binary(B) ->
    list_to_binary(list_to_hex(binary_to_list(B))).

list_to_hex(L) when is_list(L) ->
    lists:flatten(lists:map(fun(X) -> int_to_hex(X) end, L)).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 -> $0+N;
hex(N) when N >= 10, N < 16 -> $a + (N-10).

call_functor({M, F, A}, Args) ->
    erlang:apply(M, F, Args ++ A);
call_functor({M, F}, Args) ->
    erlang:apply(M, F, Args);
call_functor(Functor, Args) ->
    erlang:apply(Functor, Args).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

test() ->
    ?assertEqual(social_net_api_utils:concat([], $-), []),
    ?assertEqual(social_net_api_utils:concat([a], $-), "a"),
    ?assertEqual(social_net_api_utils:concat([a, b], $-), "a-b"),
    ?assertEqual(social_net_api_utils:concat([a, b, c], $-), "a-b-c"),
    ?assertEqual(social_net_api_utils:concat([{a, x}, {b,y},{c,z}], $=, $;), "a=x;b=y;c=z"),
    ok.
