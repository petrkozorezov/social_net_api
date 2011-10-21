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

-module(social_net_api_core).

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
    get_currency_multiplier/1,
    start_link/1,
    start_link/2,
    stop/1,
    stop/2,
    validate_auth/2,
    invoke_method/3,
    test/0
]).

-record(state, {client_pid, server_pid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_module({_, nil}) -> {ok, nil};
start_module({Module, Args}) -> Module:start_link(Args).

stop_module({_, nil}) -> ok;
stop_module({Module, Pid}) -> Module:stop(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Options)                 -> gen_server:start_link( ?MODULE, Options, [] ).
start_link(Name, Options)           -> gen_server:start_link( Name, ?MODULE, Options, [] ).
stop(Pid)                           -> stop(Pid, shutdown).
stop(Pid, Reason)                   -> gen_server:call(Pid, {shutdown, Reason}, infinity).

validate_auth(Pid, AuthData)        -> gen_server:call(Pid, {validate_auth, AuthData}).
invoke_method(Pid, Method, Args)    -> gen_server:call(Pid, {invoke_method, Method, Args}).
get_currency_multiplier(Pid)        -> gen_server:call(Pid, get_currency_multiplier).

init(Options) ->
    process_flag(trap_exit, true),

    Network   = proplists:get_value(network,    Options),
    AppID     = proplists:get_value(app_id,     Options),
    SecretKey = proplists:get_value(secret_key, Options),

    BaseOptions   = [{network, Network}, {app_id, AppID}, {secret_key, SecretKey}],

    ClientOptions = case proplists:get_value(client_options, Options) of
                        undefined  -> nil;
                        ClientOpts -> lists:append(BaseOptions, ClientOpts)
                    end,
    ServerOptions = case proplists:get_value(server_options, Options) of
                        undefined  -> nil;
                        ServerOpts -> lists:append(BaseOptions, ServerOpts)
                    end,

    {ok, ClientPid} = start_module({social_net_api_client, ClientOptions}),
    {ok, ServerPid} = start_module({social_net_api_server, ServerOptions}),

    {ok, #state{client_pid=ClientPid, server_pid=ServerPid}}.

handle_call({invoke_method, Method, Args}, From, State=#state{client_pid=ClientPid}) ->
    spawn( fun() -> gen_server:reply(From, social_net_api_client:invoke_method(ClientPid, Method, Args)) end ),
    {noreply, State};

handle_call({validate_auth, AuthData}, From, State=#state{client_pid=ClientPid}) ->
    spawn( fun() -> gen_server:reply(From, social_net_api_client:validate_auth(ClientPid, AuthData)) end ),
    {noreply, State};

handle_call(get_currency_multiplier, _, State=#state{client_pid=ClientPid}) ->
    {reply, social_net_api_client:get_currency_multiplier(ClientPid), State};

handle_call({shutdown, Reason}, _From, State=#state{client_pid=ClientPid, server_pid=ServerPid}) ->
    ok = stop_module({social_net_api_client, ClientPid}),
    ok = stop_module({social_net_api_server, ServerPid}),
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
    PaymentCallback = fun(Req) -> ?LOG_DEBUG(": Payment request received: ~p", [Req]), ok end,

    VkOptions = [
        {network,               vkontakte},
        {app_id,                "1879348"},
        {secret_key,            "5wm8DAKamkvR5YtHksKw"},
        {client_options, [{host, "api.vkontakte.ru"}]}
    ],

    OkOptions = [
        {network,               odnoklassniki},
        {app_id,                "CBAEGGABABABABABA"},
        {secret_key,            "30761790E444BAF1A6544E3C"},
        {client_options, [{host, "api-sandbox.odnoklassniki.ru:8088"}]},
        {server_options, [{ip, "0.0.0.0"}, {port, 54160}, {callback, PaymentCallback}, {mode, raw}]}
    ],

    test(VkOptions),
    test(OkOptions).


test(Options) ->
    OldVal = process_flag(trap_exit, true),
    R = case ?MODULE:start_link(Options) of
        {ok, Pid} ->

            {network, Network} = proplists:lookup(network, Options),
            ok = test_operations(Network, Pid),

            receive after 5000 -> noop end,

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

test_operation(Pid, Method, Args) ->
    ?LOG_INFO(": invoking ~p with args ~p...", [Method, Args]),
    Result = ?MODULE:invoke_method(Pid, Method, Args),
    ?LOG_INFO(": invoking ~p: result: ~p", [Method, Result]),
    ok.

test_operations(vkontakte, Pid) ->
    ok = test_operation(Pid, {secure, getBalance}, [{uid,1170703}]);

test_operations(odnoklassniki, Pid) ->
    Uids = [ get_uid(Pid, N) || N <- lists:seq(1, 19) ],
    ?LOG_DEBUG(": Users: ~p", [Uids]).

get_uid(Pid, N) ->
    Login = "dev_drimmi_" ++ integer_to_list(N),
    Passwd = Login ++ "_pwd",
    case ?MODULE:invoke_method(Pid, {auth, login}, [{user_name, Login}, {password, Passwd}]) of
        {struct, [{<<"uid">>, UID}, _, _, _, _, _]} -> binary_to_list(UID);
        Another -> {error, response_parsing_failed, Another}
    end.

