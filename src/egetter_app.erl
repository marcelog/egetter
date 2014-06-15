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
-spec start_phase(
  atom(), normal|{takeover, node()}|{failover, node()}, term()
) -> ok.
start_phase(read_user_agents, _Type, _PhaseArgs) ->
  ok = load(user_agents);

start_phase(read_proxies, _Type, _PhaseArgs) ->
  ok = load(proxies).

-spec start(atom(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  egetter_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec load(user_agents|proxies) -> ok.
load(Type) ->
  Ets = new_ets(),
  case egetter:cfg_get(Type) of
    undefined -> lager:debug("NOT READING ~p", [Type]);
    File ->
      lager:debug("Reading ~p from ~p", [Type, File]),
      ok = egetter_file:load(Ets, File)
  end,
  OptName = list_to_atom(lists:concat([Type, "_ets"])),
  _ = egetter:cfg_set(OptName, Ets),
  ok.

-spec new_ets() -> ets:tid().
new_ets() ->
  ets:new(ets_table, [
    public, ordered_set,
    {write_concurrency, false}, {read_concurrency, true},
    compressed
  ]).
