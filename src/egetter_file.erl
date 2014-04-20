-module(egetter_file).
-author("marcelog@gmail.com").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([load/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates an ETS file and loads a file into it.
-spec load(string()) -> {ets:tid(), pos_integer()}.
load(Filename) ->
  EtsName = ets:new(ets_table, [
    public, ordered_set,
    {write_concurrency, false}, {read_concurrency, true},
    compressed
  ]),
  lager:debug("Loading ~p into ETS ~p", [Filename, EtsName]),
  {ok, IoDevice} = file:open(Filename, [read, binary]),
  Lines = load(IoDevice, 0, EtsName),
  lager:debug("Read ~p lines from ~p into ETS ~p", [Lines, Filename, EtsName]),
  {EtsName, Lines}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load(IoDevice, LineNum, EtsName) ->
  case io:get_line(IoDevice, "") of
    {error, Error} -> {error, Error};
    eof ->
      ok = file:close(IoDevice),
      LineNum;
    Line ->
      Trimmed = binary:part(Line, 0, size(Line) - 1),
      true = ets:insert(EtsName, {LineNum, Trimmed}),
      load(IoDevice, LineNum + 1, EtsName)
  end.
