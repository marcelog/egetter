-module(egetter_sup).
-author("marcelog@gmail.com").

-behaviour(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SUP(M), {
  M, {M, start_link, []},
  permanent, 5000, supervisor, [supervisor]
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% supervisor behavior.
init([]) ->
  egetter = ets:new(egetter:internal_ets(), [
    named_table, public, {write_concurrency, false}, {read_concurrency, true}
  ]),
  ok = load(user_agents),
  ok = load(proxies),
  Children = [
    ?SUP(ibrowse_sup),
    ?SUP(ssl_sup)
  ],
  {ok, {{one_for_one, 5, 10}, Children}}.

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