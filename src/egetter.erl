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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Useful to start the application via the -s command line argument.
-spec start() -> ok.
start() ->
  _ = [ok = application:start(A) || A <- ?APPS],
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

%% @doc Does a GET request.
-spec req(proplists:proplist()) -> proplists:proplist().
req(Options) ->
  Agent = random_user_agent(),
  Url = proplists:get_value(url, Options),
  Body = proplists:get_value(body, Options, <<>>),
  Timeout = proplists:get_value(timeout, Options, 5000),
  Method = proplists:get_value(method, Options, get),
  Headers = [
    {"User-Agent", Agent}
    | proplists:get_value(headers, Options, [])
  ],
  {Host, Port} = case binary:split(random_proxy(), <<":">>) of
    [H] -> {H, <<"80">>};
    [H, P] -> {H, P}
  end,
  IOptions = [
    {response_format, binary},
    {proxy_host, binary_to_list(Host)},
    {proxy_port, binary_to_integer(Port)}
    |proplists:get_value(ibrowse_options, Options, [])
  ],
  lager:debug("Requesting: ~p through ~p", [Options, {Host, Port}]),
  {ok, Status, ResponseHeaders, ResponseBody} = ibrowse:send_req(
    Url, Headers, Method, Body, IOptions, Timeout
  ),
  case Status of
    [$3, _, _] ->
      NewUrl = proplists:get_value("Location", ResponseHeaders),
      lager:debug("Following redirect: ~p: ~p", [Status, ResponseHeaders]),
      req(lists:keystore(url, 1, Options, {url, NewUrl}));
    [$2, _, _] -> [{headers, ResponseHeaders}, {body, ResponseBody}];
    _ ->
      lager:error(
        "Request failed: ~p: Headers: ~p / Body: ~p",
        [Status, ResponseHeaders, ResponseBody]
      )
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
