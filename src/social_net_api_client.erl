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

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([validate_auth/1, invoke_method/2, send_message/2, get_currency_multiplier/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

validate_auth(AuthData) ->
    Module = social_net_api_settings:network_mod(),
    Module:validate_auth(AuthData).

get_currency_multiplier() ->
    Module = social_net_api_settings:network_mod(),
    Module:get_currency_multiplier().

invoke_method(Method, Args) ->
    gen_server:call(?MODULE, {invoke_method, Method, Args}).

send_message(Message, Users) ->
    UnicodeMessage = unicode:characters_to_binary(Message),
    gen_server:call(?MODULE, {send_message, UnicodeMessage, Users}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    Module = social_net_api_settings:network_mod(),
    {ok, Module:init_client()}.

handle_call({invoke_method, Method, Args}, _, State) ->
    Module = social_net_api_settings:network_mod(),
    {Reply, NewState} = Module:invoke_method(Method, Args, State),
    {reply, Reply, NewState};

handle_call({send_message, Message, Users}, _, State) ->
    Module = social_net_api_settings:network_mod(),
    {Reply, NewState} = Module:send_message(Message, Users, State),
    {reply, Reply, NewState};

handle_call(get_currency_multiplier, _, State) ->
    {reply, Module:get_currency_multiplier(), State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
