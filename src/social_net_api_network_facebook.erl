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

-module(social_net_api_network_facebook).
-compile(export_all).

-export
([
    init_client/0,
    init_server/0,
    validate_auth/1,
    validate_auth/3,
    generate_auth/1,
    process_payment/2,
    get_currency_multiplier/0
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_client() -> undefined.
init_server() -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_currency_multiplier() -> 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_auth(AuthData) ->
    AppID         = social_net_api_settings:app_id(),
    SecretKey     = social_net_api_settings:secret_key(),
    Data = social_net_api_utils:concat([AppID, AuthData, SecretKey]),
    social_net_api_utils:md5_hex(Data).

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

process_payment({_,Args}, State) ->
    try
        Method   = proplists:get_value("method", Args),
        Signed   = proplists:get_value("signed_request", Args),
        Parsed   = parse_signed_request(Signed),
        {Request} = proplists:get_value(<<"credits">>, Parsed),
        Response = handle_request(Method, Request),
        {{"application/json", Response}, State}
    catch
        _:_ ->
            {{"text/plain", "error"}, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_request("payments_get_items", Request) ->
    ID = proplists:get_value(<<"order_info">>, Request),
    Callback = social_net_api_settings:info_callback(),
    {ok, Info} = invoke_callback(raw, Callback, ID),
    info(ID, Info);

handle_request("payments_status_update", Request) ->
    Status  = proplists:get_value(<<"status">>, Request),
    OrderID = proplists:get_value(<<"order_id">>, Request),
    case Status of
        <<"placed">> ->
            Callback = social_net_api_settings:payment_callback(),
            Mode     = social_net_api_settings:server_mode(parsed),
            case invoke_callback(Mode, Callback, Request) of
                ok -> result(OrderID, <<"settled">>);
                _  -> result(OrderID, <<"canceled">>)
            end;
        _  -> result(OrderID, <<"canceled">>)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_callback(raw, Callback, Request) ->
    catch social_net_api_utils:call_functor(Callback, [Request]);

invoke_callback(parsed, Callback, Request) ->
    {ok, {Details}} = json:decode(proplists:get_value(<<"order_details">>, Request)),
    [{Item}] = proplists:get_value(<<"items">>, Details),
    UID             = drimmi_ecore_utils:to_list(social_net_api_utils:find(<<"buyer">>, Details)),
    TransactionID   = drimmi_ecore_utils:to_list(social_net_api_utils:find(<<"order_id">>, Details)),
    ProductCode     = drimmi_ecore_utils:to_list(social_net_api_utils:find(<<"item_id">>, Item)),
    Amount          = drimmi_ecore_utils:to_list(social_net_api_utils:find(<<"amount">>, Details)),
    ProductOption   = nil,
    Profit          = nil,
    invoke_callback(raw, Callback, {{TransactionID, UID}, {ProductCode, ProductOption}, {Amount, Profit}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info(ID, {Title, Description, Price, ImageURL}) ->
    Obj = {[
        {method, <<"payments_get_items">>},
        {content, [{[
            {item_id,     ID},
            {price,       Price},
            {title,       social_net_api_utils:to_binary(Title)},
            {description, social_net_api_utils:to_binary(Description)},
            {image_url,   social_net_api_utils:to_binary(ImageURL)},
            {product_url, social_net_api_utils:to_binary(ImageURL)}
        ]}]}
    ]},
    {ok, Body} = json:encode(Obj), Body.


result(OrderID, Status) ->
    Obj = {[
        {method, <<"payments_status_update">>},
        {content, {[
            {status, Status},
            {order_id, OrderID}
        ]}}
    ]},
    {ok, Body} = json:encode(Obj), Body.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

base64url_decode(Data0) ->
    Data1 = re:replace(Data0, "-", "+", [global, {return, binary}]),
    Data2 = re:replace(Data1, "_", "/", [global, {return, binary}]),
    case size(Data2) rem 4 of
        2 -> base64:decode(<<Data2/binary, "==">>);
        3 -> base64:decode(<<Data2/binary,  "=">>);
        _ -> base64:decode(Data2)
    end.

parse_signed_request(Request) ->
    [SHA, Data] = re:split(Request, "\\."),
    Payload = base64url_decode(Data),
    {ok, {JSON}} = json:decode(Payload),
    case proplists:get_value(<<"algorithm">>, JSON) of
        <<"HMAC-SHA256">> ->
            SecretKey = social_net_api_settings:secret_key(),
            ProvidedSHA = base64url_decode(SHA),
            ExpectedSHA = hmac:hmac256(SecretKey, Data),
            case ExpectedSHA =:= ProvidedSHA of
                true -> JSON;
                false -> exit(invalid_signature)
            end;
        _ -> exit(invalid_algorithm)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
