-module(egetter_file).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([load/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Loads a file into an ets.
-spec load(ets:tid(), string()) -> ok.
load(Ets, File) ->
  lager:debug("Loading ~p into ETS ~p", [File, Ets]),
  {ok, IoDevice} = file:open(File, [read, binary]),
  Lines = load(IoDevice, 0, Ets),
  lager:debug("Read ~p lines from ~p into ETS ~p", [Lines, File, Ets]),
  ok.

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
