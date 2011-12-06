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

-module(social_net_api_network_vkontakte).

-export
([
    init_client/0,
    init_server/0,
    validate_auth/1,
    invoke_method/3,
    send_message/3,
    get_currency_multiplier/0
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_client() -> [].
init_server() -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_currency_multiplier() -> 100.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_auth(AuthData) ->
    AppID         = social_net_api_settings:app_id(),
    SecretKey     = social_net_api_settings:secret_key(),
	validate_auth(AppID, SecretKey, AuthData).

validate_auth(AppID, SecretKey, {UserID, _, Signature}) ->
    Data = social_net_api_utils:concat([AppID, UserID, SecretKey], $_),
    case social_net_api_utils:md5_hex(Data) of
        Signature -> ok;
        _         -> {error, invalid_signature}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_method({secure, Function}, Args, State) ->

    AppID         = social_net_api_settings:app_id(),
    SecretKey     = social_net_api_settings:secret_key(),
    Host          = social_net_api_settings:client_host("api.vkontakte.ru"),

    Method        = social_net_api_utils:concat([secure, Function], $.),
    Required      = [{api_id, AppID}, {format, json}, {method, Method}, {v, "3.0"},
                     {random, random:uniform(10000000)}, {timestamp, social_net_api_utils:timestamp()}],

    Arguments     = social_net_api_utils:merge(Args, Required),
    UnsignedQuery = social_net_api_utils:concat(Arguments, $=, []) ++ SecretKey,
    SignedQuery   = mochiweb_util:urlencode(social_net_api_utils:merge(Arguments, [{sig, social_net_api_utils:md5_hex(UnsignedQuery)}])),

    Request = "http://" ++ Host ++ "/api.php?" ++ SignedQuery,

    NewState = wait_if_needed(State),
    case catch(httpc:request(Request)) of
        {ok, {{_HttpVer, 200, _Msg}, _Headers, Body}} ->
            {mochijson2:decode(Body), NewState};
        {error, Reason} ->
            {{error, Reason}, NewState};
        Unexpected ->
            {{error, {unexpected_response, Unexpected}}, NewState}
    end;

invoke_method({Group, Function}, Args, State) ->

    AppID         = social_net_api_settings:app_id(),
    SecretKey     = social_net_api_settings:secret_key(),
    Host          = social_net_api_settings:client_host("api.vkontakte.ru"),

    Method        = social_net_api_utils:concat([Group, Function], $.),
    Required      = [{api_id, AppID}, {format, json}, {method, Method}, {v, "3.0"}],

    ViewerID      = social_net_api_utils:to_list(proplists:get_value(viewer_id, Args, "")),
    Arguments     = social_net_api_utils:merge(social_net_api_utils:delete(viewer_id, Args), Required),
    UnsignedQuery = ViewerID ++ social_net_api_utils:concat(Arguments, $=, []) ++ SecretKey,
    SignedQuery   = social_net_api_utils:concat(social_net_api_utils:merge(Arguments, [{sig, social_net_api_utils:md5_hex(UnsignedQuery)}]), $=, $&),

    Request = "http://" ++ Host ++ "/api.php?" ++ SignedQuery,

    NewState = wait_if_needed(State),
    case catch(httpc:request(Request)) of
        {ok, {{_HttpVer, 200, _Msg}, _Headers, Body}} ->
            {mochijson2:decode(Body), NewState};
        {error, Reason} ->
            {{error, Reason}, NewState};
        Unexpected ->
            {{error, {unexpected_response, Unexpected}}, NewState}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Do not send more than 5 requests per second:
wait_if_needed(History) ->
    T1 = erlang:now(),
    case length(History) =:= 5 of
        true  ->
            T2 = lists:nth(5, History),
            case timer:now_diff(T1, T2) div 1000 of
                Delta when Delta >= 1000 -> ok;
                Delta -> timer:sleep(1000 - Delta)
            end,
            [erlang:now()];
        false ->
            [erlang:now()|History]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_message(Message, Users, State) ->
    Fun =
    fun(UserList, {Acc, TmpState}) ->
        {Reply, NewState} = do_send(Message, UserList, TmpState),
        {[Reply|Acc], NewState}
    end,
    {Result, NewState} = lists:foldl(Fun, {[], State}, social_net_api_utils:split(100, Users)),
    {lists:concat(lists:reverse(Result)), NewState}.

do_send(Message, Users, State) ->
    Method = {secure, sendNotification},
    Args   = [{uids, social_net_api_utils:concat(Users, $,)}, {message, Message}],
    {Result, NewState} = invoke_method(Method, Args, State),
    {parse_response(Users, Result), NewState}.

parse_response(Users, {struct, [{<<"error">>, {struct, ErrorInfo}}]}) ->
    Code = proplists:get_value(<<"error_code">>, ErrorInfo),
    Message = proplists:get_value(<<"error_msg">>, ErrorInfo),
    lists:zip(Users, lists:duplicate(length(Users), {error, {Code, Message}}));

parse_response(Users, {struct,[{<<"response">>,Result}]}) ->
    {Delivered, Undelivered} = social_net_api_utils:split_delivered(Users, Result),
    lists:zip(Delivered, lists:duplicate(length(Delivered), ok)) ++
    lists:zip(Undelivered, lists:duplicate(length(Undelivered), {error, undelivered})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
