-module(egetter_app).
-author("marcelog@gmail.com").

-behaviour(application).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_phase/3, start/2, stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_phase(read_user_agents, _Type, _PhaseArgs) ->
  File = egetter:cfg_get(user_agents),
  lager:debug("Reading user agents from ~p", [File]),
  ok = egetter:load_user_agents(File);

start_phase(read_proxies, _Type, _PhaseArgs) ->
  File = egetter:cfg_get(proxies),
  lager:debug("Reading proxies from ~p", [File]),
  ok = egetter:load_proxies(File).

start(_StartType, _StartArgs) ->
  egetter_sup:start_link().

stop(_State) ->
  ok.
