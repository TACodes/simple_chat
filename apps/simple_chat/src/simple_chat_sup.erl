%%%-------------------------------------------------------------------
%% @doc simple_chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(simple_chat_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Host) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Host]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([Host]) ->
    {ok, {#{strategy => rest_for_one,
            intensity => 5,
            period => 2000},
          [backend_server(), web(Host)]}}.

%% internal functions

backend_server() ->
    #{id => backend_server,
      start => {backend_server, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [backend_server]}.

web(Host) ->
    #{id => web,
      start => {web, start_link, [Host]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [web]}.
