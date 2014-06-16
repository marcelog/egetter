-module(egetter).
-author("marcelog@gmail.com").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([cfg_set/2, cfg_get/1]).
-export([internal_ets/0]).
-export([start/0]).
-export([req/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type qs_field():: string().
-type qs_value():: string().
-type query_string_option():: {qs_field(), qs_value()}.
-type query_string():: [query_string_option()].

-type option()::
  {url, string()}
  | {timeout, pos_integer()}
  | {headers, [{string()|atom(), string()}]}
  | {body, binary()}
  | {method, get | post | put | delete | head | options}
  | {follow_redirect, true | false}
  | {ibrowse_options, [{atom(), term()}]}
  | {use_proxy, true|false}
  | {query_string, query_string()}
  | {host, string()}
  | {port, pos_integer()}
  | {scheme, string()}
  | {path_components, [string()]}.

-type result_field()::
  {body, binary()}
  | {status, pos_integer()}
  | {headers, [{string(), string()}]}.

-type result()::
  {ok, [result_field()]}
  | {error, [result_field()]}
  | {ibrowse_error, term()}.

-export_type([option/0]).
-export_type([result/0]).

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

%% @doc Does a request.
-spec req([option()]) -> result().
req(Options) ->
  Get = fun(K, Default) -> proplists:get_value(K, Options, Default) end,
  Agent = random_user_agent(),
  Url = form_url(Options),
  Body = Get(body, <<>>),
  Timeout = Get(timeout, 5000),
  Method = Get(method, get),
  Headers = [{"User-Agent", Agent} | Get(headers, [])],
  IOptions = [{response_format, binary}|setup_ibrowse_options(Options)],
  lager:debug(
    "Sending ~p request to ~p, Headers: ~p, Body: ~p, Ibrowse Options: ~p",
    [Method, Url, Headers, Body, IOptions]
  ),
  form_result(
    Options, ibrowse:send_req(Url, Headers, Method, Body, IOptions, Timeout)
  ).

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
  case random_element(user_agents_ets) of
    undefined -> <<"Egetter - http://github.com/marcelog/egetter">>;
    UA -> UA
  end.

%% @doc Selects a random element from the given ets type.
-spec random_element(proxies_ets|user_agents_ets) -> term().
random_element(EtsType) ->
  random_element(EtsType, 0, 3).

-spec random_element(
  proxies_ets|user_agents_ets, non_neg_integer(), pos_integer()
) -> undefined|term().
random_element(EtsType, MaxAttempt, MaxAttempt) ->
  lager:error("Giving up on getting an element from ~p", [EtsType]),
  undefined;

random_element(EtsType, Attempt, MaxAttempt) ->
  EtsName = cfg_get(EtsType),
  Elements = ets:info(EtsName, size),
  random_element(EtsType, EtsName, Attempt, MaxAttempt, Elements).

-spec random_element(
  proxies_ets|user_agents_ets, ets:tid(), non_neg_integer(),
  pos_integer(), non_neg_integer()
) -> undefined|term().
random_element(_EtsType, _EtsName, _Attempt, _MaxAttempt, 0) ->
  undefined;

random_element(EtsType, EtsName, Attempt, MaxAttempt, Elements) ->
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

-spec form_url([option()]) -> string().
form_url(Options) ->
  Get = fun(K, Default) -> proplists:get_value(K, Options, Default) end,
  PreUrl = case Get(url, missing_url) of
    missing_url ->
      Host = Get(host, missing_host),
      Port = Get(port, missing_port),
      Scheme = Get(scheme, missing_scheme),
      PathComponents = [ibrowse_lib:url_encode(P) || P <- Get(path_components, [])],
      lists:concat([
        Scheme, "://", Host, ":", Port, "/", string:join(PathComponents, "/")
      ]);
    PreUrl_ -> PreUrl_
  end,
  QueryString = form_query_string(Get(query_string, [])),
  string:join([PreUrl, QueryString], "?").

-spec form_query_string(query_string()) -> string().
form_query_string(QueryString) ->
  form_query_string(QueryString, []).

-spec form_query_string(query_string(), [string()]) -> string().
form_query_string([], Acc) ->
  string:join(Acc, "&");

form_query_string([{Key, Value}|Rest], Acc) ->
  Option = string:join(
    [ibrowse_lib:url_encode(Key), ibrowse_lib:url_encode(Value)],
    "="
  ),
  form_query_string(Rest, [Option|Acc]).

-spec form_result(
  [option()],
  {error, term()} | {ok, string(), [{string(), string()}], binary()}
) -> result().
form_result(_Options, {error, Error}) ->
  {ibrowse_error, Error};

form_result(Options, {ok, ResponseStatus, ResponseHeaders, ResponseBody}) ->
  FollowRedirect = proplists:get_value(follow_redirect, Options, false),
  IntStatus = list_to_integer(ResponseStatus),
  IsRedirect = IntStatus >= 300 andalso IntStatus < 400,
  IsSuccess = IntStatus >= 200 andalso IntStatus < 400,
  Result = [
    {status, IntStatus},
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
  end.