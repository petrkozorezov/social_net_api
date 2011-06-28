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

-module(social_net_api_server).

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
    test/0
]).

-record(state, {pid, module, data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Options)        -> gen_server:start_link( ?MODULE, Options, [] ).
start_link(Name, Options)  -> gen_server:start_link( Name, ?MODULE, Options, [] ).
stop(Pid)                  -> stop(Pid, shutdown).
stop(Pid, Reason)          -> gen_server:call(Pid, {shutdown, Reason}, infinity).

init(Options) ->
    process_flag(trap_exit, true),

    Network = proplists:get_value(network, Options),
    IP      = proplists:get_value(ip,      Options),
    Port    = proplists:get_value(port,    Options),

    Module = social_net_api_utils:get_network_module(Network),

    Self = self(),
    Loop = fun(Request) -> gen_server:call(Self, {payment, Request}) end,

    ?LOG_INFO(": starting social server at ~p:~p", [IP, Port]),
    {ok, Pid} = mochiweb_http:start([{ip, IP}, {port, Port}, {loop, Loop}, {acceptor_pool_size, 1}]),

    {ok, Data} = Module:parse_server_options(Options),

    {ok, #state{pid=Pid, module=Module, data=Data}}.

handle_call({payment, Request}, From, State=#state{module=Module, data=Data}) ->
    spawn( fun() -> gen_server:reply(From, Module:process_payment(Request, Data)) end ),
    {noreply, State};

handle_call({shutdown, Reason}, _From, State=#state{pid=Pid}) ->
    ?LOG_DEBUG(": stopping social server...", []),
    ok = mochiweb_http:stop(Pid),
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

    Options = [ {network,           mymail},
                {ip,                "0.0.0.0"},
                {port,              54160},
                {app_id,            "607061"},
                {secret_key,        "3485e5c90e1687b7e1eeceedf1303830"},
                {callback,          PaymentCallback},
                {mode,              parsed} ],

    OldVal = process_flag(trap_exit, true),
    R = case ?MODULE:start_link(Options) of
        {ok, Pid} ->

            receive after 60000 -> noop end,

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
