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
  Children = [
    ?SUP(ibrowse_sup),
    ?SUP(ssl_sup)
  ],
  {ok, {{one_for_one, 5, 10}, Children}}.

