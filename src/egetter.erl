-module(egetter).
-author("marcelog@gmail.com").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([cfg_set/2, cfg_get/1]).
-export([load_user_agents/1, load_proxies/1]).
-export([internal_ets/0]).
-export([start/0]).
-export([req/1]).

-define(APPS, [
  compiler,
  syntax_tools,
  lager,
  crypto,
  asn1,
  public_key,
  egetter
]).

-type option()::
  {url, string()}
  | {timeout, pos_integer()}
  | {headers, [{string()|atom(), string()}]}
  | {body, binary()}
  | {method, get | post | put | delete | head | options}
  | {follow_redirect, true | false}
  | {ibrowse_options, [{atom(), term()}]}
  | {use_proxy, true|false}
  | {save_to, string()}.

-export_type([option/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Useful to start the application via the -s command line argument.
-spec start() -> ok.
start() ->
  _ = [application:start(A) || A <- ?APPS],
  ok.

%% @doc Returns the name of the internal ets config table.
-spec internal_ets() -> atom().
internal_ets() ->
  ?MODULE.

%% @doc Sets a key to the given value in the internal ets config table.
-spec cfg_set(atom(), term()) -> ok.
cfg_set(Key, Value) ->
  lager:debug("Setting: ~p to ~p", [Key, Value]),
  true = ets:insert(internal_ets(), {Key, Value}),
  ok.

%% @doc Retrieves a configuration key from the application environment or
%% the internal ets config table.
-spec cfg_get(atom()) -> term().
cfg_get(Key) ->
  case application:get_env(egetter, Key) of
    undefined -> internal_table_cfg_get(Key, undefined);
    {ok, Val} -> Val
  end.

%% @doc Loads a list of user agents.
-spec load_user_agents(string()) -> ok.
load_user_agents(Filename) ->
  File = egetter:cfg_get(user_agents),
  lager:debug("Reading user agents from ~p", [File]),
  {EtsName, Lines} = egetter_file:load(Filename),
  _ = egetter:cfg_set(user_agents_ets, {EtsName, Lines}),
  ok.

%% @doc Loads a list of proxies.
-spec load_proxies(string()) -> ok.
load_proxies(Filename) ->
  File = egetter:cfg_get(proxies),
  lager:debug("Reading proxies from ~p", [File]),
  {EtsName, Lines} = egetter_file:load(Filename),
  _ = egetter:cfg_set(proxies_ets, {EtsName, Lines}),
  ok.

%% @doc Does a request.
-spec req([option()]) ->
  {ok, proplists:proplist()}
  | {error, proplists:proplist()}
  | {ibrowse_error, term()}.
req(Options) ->
  Get = fun(K, Default) -> proplists:get_value(K, Options, Default) end,
  Agent = random_user_agent(),
  Url = Get(url, missing_url),
  Body = Get(body, <<>>),
  Timeout = Get(timeout, 5000),
  Method = Get(method, get),
  FollowRedirect = Get(follow_redirect, false),
  Headers = [{"User-Agent", Agent} | Get(headers, [])],
  IOptions = [{response_format, binary}|setup_ibrowse_options(Options)],
  case ibrowse:send_req(Url, Headers, Method, Body, IOptions, Timeout) of
    {error, Error} -> {ibrowse_error, Error};
    {ok, ResponseStatus, ResponseHeaders, ResponseBody} ->
      IsRedirect = $3 =:= hd(ResponseStatus),
      IsSuccess = $2 =:= hd(ResponseStatus),
      Result = [
        {status, ResponseStatus},
        {headers, ResponseHeaders},
        {body, ResponseBody}
      ],
      if
        IsRedirect andalso FollowRedirect ->
          NewUrl = proplists:get_value("Location", ResponseHeaders),
          lager:debug("Redirecting: ~p: ~p", [ResponseStatus, ResponseHeaders]),
          req(lists:keystore(url, 1, Options, {url, NewUrl}));
        IsSuccess -> {ok, Result};
        true ->
          lager:error("Request failed: ~p", [Result]),
          {error, Result}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Retrieves an option from the internal ets config table or returns the
%% default value.
-spec internal_table_cfg_get(atom(), term()) -> term().
internal_table_cfg_get(Key, Default) ->
  case ets:lookup(internal_ets(), Key) of
    [] -> Default;
    [{Key, Value}] -> Value
  end.

%% @doc Returns a proxy chosen at random.
-spec random_proxy() -> binary()|undefined.
random_proxy() ->
  random_element(proxies_ets).

%% @doc Returns a user agent chosen at random.
-spec random_user_agent() -> binary()|undefined.
random_user_agent() ->
  random_element(user_agents_ets).

%% @doc Selects a random element from the given ets type.
-spec random_element(proxies_ets|user_agents_ets) -> term().
random_element(EtsType) ->
  random_element(EtsType, 0, 3).

random_element(EtsType, MaxAttempt, MaxAttempt) ->
  lager:error("Giving up on getting an element from ~p", [EtsType]),
  undefined;

random_element(EtsType, Attempt, MaxAttempt) ->
  {EtsName, Elements} = cfg_get(EtsType),
  Index = random(Elements) - 1,
  try
    lager:debug(
      "Attempt ~p/~p: Getting index ~p/~p of ~p (~p)",
      [Attempt, MaxAttempt, Index, Elements, EtsType, EtsName]
    ),
    case ets:lookup(EtsName, Index) of
      [{_Index, Value}] -> Value;
      [] -> throw(not_found)
    end
  catch
    _:Error ->
      lager:error("Error getting item from ets: ~p", [Error]),
      timer:sleep(10),
      random_element(EtsType, Attempt + 1, MaxAttempt)
  end.

%% @doc Chooses a random number after initializing the seed.
-spec random(pos_integer()) -> pos_integer().
random(N) ->
  {A, B, C} = os:timestamp(),
  random:seed(A, B, C),
  random:uniform(N).

%% @doc Setups different ibrowse options based on the request options.
-spec setup_ibrowse_options([option()]) -> [{atom(), term()}].
setup_ibrowse_options(ReqOptions) ->
  setup_ibrowse_options(ReqOptions, []).

setup_ibrowse_options([], Acc) ->
  Acc;

setup_ibrowse_options([Option|Rest], Acc) ->
  setup_ibrowse_options(Rest, Acc ++ ibrowse_option(Option)).

%% @doc Setups different ibrowse options based on the request options.
-spec ibrowse_option(option()) -> [{atom(), term()}].
ibrowse_option({use_proxy, true}) ->
  {Host, Port} = case binary:split(random_proxy(), <<":">>) of
    [H] -> {H, <<"80">>};
    [H, P] -> {H, P}
  end,
  lager:debug("Requesting through ~p", [{Host, Port}]),
  [
    {proxy_host, binary_to_list(Host)},
    {proxy_port, binary_to_integer(Port)}
  ];
ibrowse_option({use_proxy, false}) ->
  [];
ibrowse_option({ibrowse_options, Options}) ->
  Options;
ibrowse_option(_) ->
  [].
