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
    parse_client_options/1,
    parse_server_options/1,
    validate_auth/2,
    invoke_method/3,
    process_payment/2
]).

-record(client_options, {app_id, secret_key, host}).
-record(server_options, {app_id, secret_key, callback, mode}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_client_options(Options) ->
    {ok, #client_options{app_id     = proplists:get_value(app_id,     Options),
                         secret_key = proplists:get_value(secret_key, Options),
                         host       = proplists:get_value(host,       Options)}}.

parse_server_options(Options) ->
    {ok, #server_options{app_id     = proplists:get_value(app_id,     Options),
                         secret_key = proplists:get_value(secret_key, Options),
                         callback   = proplists:get_value(callback,   Options),
                         mode       = proplists:get_value(mode,       Options)}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_auth({UserID, UserData, Signature}, #client_options{secret_key=SecretKey}) ->
    Data = UserID ++ UserData ++ SecretKey,
    case social_net_api_utils:md5_hex(Data) of
        Signature -> ok;
        _         -> {error, invalid_signature}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_method({Group, Function}, Args, #client_options{app_id=AppID, secret_key=SecretKey, host=Host}) ->
    Method        = social_net_api_utils:concat([{atom_to_list(Group), atom_to_list(Function)}], $/, []),
    Required      = [{format, "JSON"}, {application_key, AppID}],
    Arguments     = social_net_api_utils:merge(Args, Required),
    UnsignedQuery = social_net_api_utils:concat(Arguments, $=, []) ++ SecretKey,
    SignedQuery   = social_net_api_utils:concat(social_net_api_utils:merge(Arguments, [{sig, social_net_api_utils:md5_hex(UnsignedQuery)}]), $=, $&),

    Request = "http://" ++ Host ++ "/api/" ++ Method ++ "?" ++ SignedQuery,

    case catch(social_net_api_utils:http_request(Request)) of
        {ok, {{_HttpVer, 200, _Msg}, _Headers, Body}} ->
            mochijson2:decode(Body);
        {error, Reason} ->
            {error, Reason};
        Unexpected ->
            {error, unexpected_response, Unexpected}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_payment(Request, #server_options{app_id=AppID, secret_key=SecretKey, callback=Callback, mode=Mode}) ->
    Args = Request:parse_qs(),
    case validate_keys(AppID, SecretKey, Args) of
        ok ->
            case invoke_callback(Mode, Callback, Args) of
                ok                              -> send_response(Request, ok);
                {error, Err} when is_atom(Err)  -> send_response(Request, {error, Err});
                _                               -> send_response(Request, {error, invalid_response})
            end;
        {error, Err} when is_atom(Err)          -> send_response(Request, {error, Err});
        _                                       -> send_response(Request, {error, invalid_response})
    end.

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
    social_net_api_utils:call_functor(Callback, [Args]);

invoke_callback(parsed, Callback, Args) ->
    TransactionID   = social_net_api_utils:find("transaction_id", Args),
    UID             = social_net_api_utils:find("uid",            Args),
    ProductCode     = social_net_api_utils:find("product_code",   Args),
    ProductOption   = social_net_api_utils:find("product_option", Args),
    Amount          = social_net_api_utils:find("amount",         Args, integer),
    Profit          = nil,
    invoke_callback(raw, Callback, {{TransactionID, UID}, {ProductCode, ProductOption}, {Amount, Profit}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_response(Request, ok) ->
    Response = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n"
               "<callbacks_payment_response xmlns=\"http://api.forticom.com/1.0/\">\r\n"
               "true\r\n"
               "</callbacks_payment_response>",
    Request:ok({"application/xml", Response});

send_response(Request, {Code, Msg}) when is_integer(Code), is_list(Msg) ->
    CodeString = integer_to_list(Code),
    Response = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n"
               "<ns2:error_response xmlns:ns2='http://api.forticom.com/1.0/'>\r\n"
               "    <error_code>" ++ CodeString ++ "</error_code>\r\n"
               "    <error_msg>" ++ Msg ++ "</error_msg>\r\n"
               "</ns2:error_response>",
    Request:ok({"application/xml", [{"invocation-error", CodeString}], Response});

send_response(Request, {error, invalid_app_id}) ->
    send_response(Request, {1001, "Payment is invalid and can not be processed"});

send_response(Request, {error, invalid_signature}) ->
    send_response(Request, {104, "Invalid signature"});

send_response(Request, {error, _}) ->
    send_response(Request, {1, "Unknown error"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

