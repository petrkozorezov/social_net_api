Social_net_api
==============

Social net api provides interface for various social networks api.
Currently is supported only a few russian networks: vkontakte, mymail, odnoklassniki.
Typical social net api consist of two parts: client and server.
Server part is a http callback, which recieve payment transactions requests,
and client part, which works with usual social api requests.

Compilation
-----------

To compile, run:

    rebar get-deps
    rebar compile

Usage as application
--------------------

First you need to write applications configuration.

Exapmple config:

    {network, vkontakte},                                        %% network type
    {app_id, "2268542"},                                         %% application id
    {secret_key, "sdfasdfasdfasd"},                              %% applications secret key
    {client_options, [                                           %% client part options
        {host, "api.vkontakte.ru"}                                %% host, which recieve requests
    ]}
    % for odnoklassniki or mymail
    %{server_options, [                                          %% server part options
    %   {ip, "0.0.0.0"},                                         %% http callback bind host
    %   {port, 31337},                                           %% http callback bind port
    %   {callback, {my_callback_module, my_callback_fun}},       %% callback function, may be "{module_name, function_name}" or "{module_name, function_name, ArgumentsList}"
    %   {mode, parsed}                                           %% parse or not callback request, may be "parsed" or "raw", by default is parsed
    %]}

Then start application social_net_api:

    applications:start(social_net_api).

and then you can use api methods, for example:

    social_net_api:invoke_method({secure, getBalance}, [{uid, 1025185}]).

Usage as library
----------------

In other way, you can use social_net_api as library without starting application.
This may be required when you want to use more than one application id same time.

    Options = [{network, vkontakte}, {app_id, "2268542"}, {secret_key, "sdfasdfasdfasd"}, {client_options, [{host, "api.vkontakte.ru"}]}],
    {ok, Pid} = social_net_api_core:start_link(Options),
    social_net_api_core:invoke_method(Pid, {secure, getBalance}, [{uid, 1025185}]).

Add another social network interface
------------------------------------
See

    social_net_api_network_*

