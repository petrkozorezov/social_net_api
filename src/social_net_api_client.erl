%% Copyright (c) 2011, Drimmi (http://www.drimmi.com)
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

-module(social_net_api_client).

-include_lib("logger.hrl").

-export
([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-export
([
    start_link/1,
    start_link/2,
    stop/1,
    stop/2,
    validate_auth/2,
    invoke_method/3,
    get_currency_multiplier/1,
    test/0
]).

-record(state, {module, data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Options)        -> gen_server:start_link( ?MODULE, Options, [] ).
start_link(Name, Options)  -> gen_server:start_link( Name, ?MODULE, Options, [] ).
stop(Pid)                  -> stop(Pid, shutdown).
stop(Pid, Reason)          -> gen_server:call(Pid, {shutdown, Reason}, infinity).

validate_auth(Pid, AuthData)        -> gen_server:call(Pid, {validate_auth, AuthData}).
invoke_method(Pid, Method, Args)    -> gen_server:call(Pid, {invoke_method, Method, Args}).
get_currency_multiplier(Pid)        -> gen_server:call(Pid, get_currency_multiplier).

init(Options) ->
    Network = proplists:get_value(network, Options),
    Module  = social_net_api_utils:get_network_module(Network),
    {ok, Data} = Module:parse_client_options(Options),
    {ok, #state{module=Module, data=Data}}.

handle_call({invoke_method, Method, Args}, From, State=#state{module=Module, data=Data}) ->
    spawn( fun() -> gen_server:reply(From, Module:invoke_method(Method, Args, Data)) end ),
    {noreply, State};

handle_call({validate_auth, AuthData}, From, State=#state{module=Module, data=Data}) ->
    spawn( fun() -> gen_server:reply(From, Module:validate_auth(AuthData, Data)) end ),
    {noreply, State};

handle_call(get_currency_multiplier, _, State=#state{module=Module}) ->
    {reply, Module:get_currency_multiplier(), State};

handle_call({shutdown, Reason}, _From, State) ->
    {stop, Reason, ok, State};

handle_call(Msg, _From, State) ->
    ?LOG_ERROR(": unexpected call received: ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?LOG_ERROR(": unexpected cast received: ~p", [Msg]),
    {noreply, State}.

handle_info( {'EXIT', _Pid, _Msg}, State ) ->
    ?LOG_INFO(": exit signal received from ~p: ~p", [_Pid, _Msg]),
    {noreply, State};

handle_info( Msg, State ) ->
    ?LOG_ERROR(": unexpected info received: ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, _State) ->
    ?LOG_DEBUG(": terminated with reason ~p", [Reason]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

test() ->
    VkOptions = [
        {network,           vkontakte},
        {app_id,            "1879348"},
        {secret_key,        "5wm8DAKamkvR5YtHksKw"},
        {host,              "api.vkontakte.ru"} ],

    OkOptions = [
        {network,           odnoklassniki},
        {app_id,            "CBADIJABABABABABA"},
        {secret_key,        "A7A3B3474A76A8E153A9CF16"},
        {host,              "api-sandbox.odnoklassniki.ru:8088"} ],

    MmOptions = [
        {network,           mymail},
        {app_id,            "607036"},
        {secret_key,        "c846df264489ad174e06811af9146d6f"} ],

    test(VkOptions),
    test(OkOptions),
    test(MmOptions).


test(Options) ->
    OldVal = process_flag(trap_exit, true),
    R = case ?MODULE:start_link(Options) of
        {ok, Pid} ->

            {network, Network} = proplists:lookup(network, Options),
            ok = test_auth(Network, Pid),
            ok = test_operations(Network, Pid),

            ?assertEqual(ok,   ?MODULE:stop(Pid)),
            receive {'EXIT', Pid, shutdown} -> ok;
                    {'EXIT', Pid, _}        -> {error, invalid_shutdown_reason}
            after 5000 ->
                {error, shutdown_timeout}
            end;
        Err ->
            Err
    end,
    process_flag(trap_exit, OldVal),
    ?LOG_INFO(": testing ~p : ~p", [?MODULE, R]), R.


test_auth(vkontakte, Pid) ->
    ?LOG_DEBUG(": Testing vkontakte auth...", []),
    UserID      = <<"8307297">>,
    UserData    = <<"0">>,
    InvalidHash = <<"be0ed52012aed87c353583c386fab7e8">>,
    ValidHash   = <<"be0ed52012aed87c353583c386fab7e7">>,
    ok          = social_net_api_client:validate_auth(Pid, {UserID, UserData, ValidHash}),
    {error, _}  = social_net_api_client:validate_auth(Pid, {UserID, UserData, InvalidHash}),
    ok;

test_auth(odnoklassniki, Pid) ->
    ?LOG_DEBUG(": Testing odnoklassniki auth...", []),
    UserID      = <<"3602688835892500856">>,
    UserData    = <<"FDGFLHNJNQJSHPGEJHKKKHGGINFLIHJOGGECDCHMFCIGJOKHGDS">>,
    InvalidHash = <<"98e130941824f259e553b5404b6b06a8">>,
    ValidHash   = <<"98e130941824f259e553b5404b6b06a9">>,
    ok          = social_net_api_client:validate_auth(Pid, {UserID, UserData, ValidHash}),
    {error, _}  = social_net_api_client:validate_auth(Pid, {UserID, UserData, InvalidHash}),
    ok;

test_auth(mymail, Pid) ->
    ?LOG_DEBUG(": Testing mymail auth...", []),
    UserID      = <<"3072581181014944200">>,
    UserData      = <<"app_id=607036authentication_key=be874fffe4ef483590ef7454a32f3d14"
                    "ext_perm=notificationsis_app_user=1oid=3072581181014944200session_expire=1304604106"
                    "session_key=d7f25e25d6d505012160b511f1c9debfvid=3072581181014944200"
                    "window_id=CometName_b36b5e6e57f44e1c6e34e2e3d0af45b4">>,
    InvalidHash = <<"72fa69734f9315e8d45695d2efa16dff">>,
    ValidHash   = <<"72fa69734f9315e8d45695d2efa16dfe">>,
    ok          = social_net_api_client:validate_auth(Pid, {UserID, UserData, ValidHash}),
    {error, _}  = social_net_api_client:validate_auth(Pid, {UserID, UserData, InvalidHash}),
    ok.

test_operation(Pid, Method, Args) ->
    ?LOG_INFO(": invoking ~p with args ~p...", [Method, Args]),
    Result = ?MODULE:invoke_method(Pid, Method, Args),
    ?LOG_INFO(": invoking ~p: result: ~p", [Method, Result]),
    ok.

test_operations(vkontakte, Pid) ->
    ok = test_operation(Pid, {secure, getBalance}, [{uid,1170703}]);

test_operations(odnoklassniki, Pid) ->
    Uids = [ get_uid(Pid, N) || N <- lists:seq(1, 19) ],
    ?LOG_DEBUG(": Users: ~p", [Uids]);

test_operations(mymail, Pid) ->
    Response = test_operation(Pid, {friends, get}, [{uid,"3072581181014944200"}]),
    ?LOG_DEBUG(": my.mail.ru response: ~p", [Response]).

get_uid(Pid, N) ->
    Login = "dev_drimmi_" ++ integer_to_list(N),
    Passwd = Login ++ "_pwd",
    case ?MODULE:invoke_method(Pid, {auth, login}, [{user_name, Login}, {password, Passwd}]) of
        {struct, [{<<"uid">>, UID}, _, _, _, _, _]} -> binary_to_list(UID);
        Another -> {error, response_parsing_failed, Another}
    end.

