%%%-------------------------------------------------------------------
%% @doc simple_chat public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_chat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, WebConfig} = application:get_env(simple_chat, web_config),
    Host = maps:get(host, WebConfig),
    simple_chat_sup:start_link(Host).

stop(_State) ->
    ok.

%% internal functions
