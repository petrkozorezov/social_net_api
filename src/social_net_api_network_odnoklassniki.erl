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

-module(social_net_api_network_odnoklassniki).

-export
([
    init_client/0,
    init_server/0,
    validate_auth/1,
    validate_auth/3,
    invoke_method/3,
    send_message/3,
    process_payment/2,
    get_currency_multiplier/0
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_client() -> undefined.
init_server() -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_currency_multiplier() -> 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_auth(AuthData) ->
    SecretKey = social_net_api_settings:secret_key(),
	validate_auth(fake_app_id, SecretKey, AuthData).

validate_auth(_,  SecretKey, {UserID, UserData, Signature}) ->
    Data = social_net_api_utils:concat([UserID, UserData, SecretKey]),
    case social_net_api_utils:md5_hex(Data) of
        Signature -> ok;
        _         -> {error, invalid_signature}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_method({Group, Function}, Args, State) ->

    AppID         = social_net_api_settings:app_id(),
    SecretKey     = social_net_api_settings:secret_key(),
    Host          = social_net_api_settings:client_host("api.odnoklassniki.ru"),

    Method        = social_net_api_utils:concat([Group, Function], $/),
    Required      = [{format, "JSON"}, {application_key, AppID}],
    Arguments     = social_net_api_utils:merge(Args, Required),
    UnsignedQuery = social_net_api_utils:concat(Arguments, $=, []) ++ SecretKey,
    SignedQuery   = mochiweb_util:urlencode(social_net_api_utils:merge(Arguments, [{sig, social_net_api_utils:md5_hex(UnsignedQuery)}])),

    Request = "http://" ++ Host ++ "/api/" ++ Method ++ "?" ++ SignedQuery,

    case catch(httpc:request(Request)) of
        {ok, {{_HttpVer, 200, _Msg}, _Headers, Body}} ->
            {mochijson2:decode(Body), State};
        {error, Reason} ->
            {{error, Reason}, State};
        Unexpected ->
            {{error, {unexpected_response, Unexpected}}, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_message(Message, Users, State) ->
    Fun =
    fun(UserID, {Acc, TmpState}) ->
        {Reply, NewState} = do_send(Message, UserID, TmpState),
        {[Reply|Acc], NewState}
    end,
    {Result, NewState} = lists:foldl(Fun, {[], State}, Users),
    {lists:concat(lists:reverse(Result)), NewState}.

do_send(Message, UserID, State) ->
    Method = {notifications, sendSimple},
    Args = [{uid, social_net_api_utils:to_list(UserID)}, {text, Message}],
    {Result, NewState} = invoke_method(Method, Args, State),
    {parse_response(UserID, Result), NewState}.

parse_response(UserID, true) -> [{social_net_api_utils:to_integer(UserID), ok}];
parse_response(UserID, {struct,ErrorInfo}) ->
    Code = proplists:get_value(<<"error_code">>, ErrorInfo),
    ErrMsg = proplists:get_value(<<"error_msg">>, ErrorInfo),
    [{social_net_api_utils:to_integer(UserID), {error, {Code, ErrMsg}}}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_payment({Args,_}, State) ->
    AppID       = social_net_api_settings:app_id(),
    SecretKey   = social_net_api_settings:secret_key(),
    Callback    = social_net_api_settings:payment_callback(),
    Mode        = social_net_api_settings:server_mode(parsed),
    Response =
    case validate_keys(AppID, SecretKey, Args) of
        ok ->
            case invoke_callback(Mode, Callback, Args) of
                ok                              -> response(ok);
                {error, Err} when is_atom(Err)  -> response({error, Err});
                _                               -> response({error, invalid_response})
            end;
        {error, Err} when is_atom(Err)          -> response({error, Err});
        _                                       -> response({error, invalid_response})
    end,
    {Response, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_keys(AppID, SecretKey, Args) ->
    case social_net_api_utils:find("application_key", Args) of
        AppID ->
            Args2 = social_net_api_utils:sort(Args),
            Args3 = social_net_api_utils:delete("sig", Args2),
            UnsignedQuery = social_net_api_utils:concat(Args3, $=, []) ++ SecretKey,
            Signature     = social_net_api_utils:md5_hex(UnsignedQuery, list),
            case social_net_api_utils:find("sig", Args) of
                Signature -> ok;
                _         -> {error, invalid_signature}
            end;
        _ -> {error, invalid_app_id}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_callback(raw, Callback, Args) ->
    catch social_net_api_utils:call_functor(Callback, [Args]);

invoke_callback(parsed, Callback, Args) ->
    TransactionID   = social_net_api_utils:find("transaction_id", Args),
    UID             = social_net_api_utils:find("uid",            Args),
    ProductCode     = social_net_api_utils:find("product_code",   Args),
    ProductOption   = social_net_api_utils:find("product_option", Args),
    Amount          = social_net_api_utils:find("amount",         Args, integer),
    Profit          = nil,
    invoke_callback(raw, Callback, {{TransactionID, UID}, {ProductCode, ProductOption}, {Amount, Profit}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

response(ok) ->
    Response = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n"
               "<callbacks_payment_response xmlns=\"http://api.forticom.com/1.0/\">\r\n"
               "true\r\n"
               "</callbacks_payment_response>",
    {"application/xml", Response};

response({Code, Msg}) when is_integer(Code), is_list(Msg) ->
    CodeString = integer_to_list(Code),
    Response = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n"
               "<ns2:error_response xmlns:ns2='http://api.forticom.com/1.0/'>\r\n"
               "    <error_code>" ++ CodeString ++ "</error_code>\r\n"
               "    <error_msg>" ++ Msg ++ "</error_msg>\r\n"
               "</ns2:error_response>",
    {"application/xml", [{"invocation-error", CodeString}], Response};

response({error, invalid_app_id}) ->
    response({1001, "Payment is invalid and can not be processed"});

response({error, invalid_signature}) ->
    response({104, "Invalid signature"});

response({error, _}) ->
    response({1, "Unknown error"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
