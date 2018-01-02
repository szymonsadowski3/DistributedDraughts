-module(dra).
-import(io_lib, [format/2]).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([cross_domains/3]).
-export([get/3]).
-export([terminate/4]).

%% Cross Domain Origin
%% It accepts any host for cross-domain requests
cross_domains(_Route, _Req, State) ->
   {['_'], State}.

init(_Route, _Req, State) ->
    {ok, State}.

get("/", _Req, State) ->
    {<<"Hello, leptus!">>, State};
get("/board", _Req, State) ->
    Board = [
    empty, black, empty, black, empty, black, empty, black, empty, black,
    black, empty, black, empty, black, empty, black, empty, black, empty,
    empty, black, empty, black, empty, black, empty, black, empty, black,
    black, empty, black, empty, black, empty, black, empty, black, empty,
    empty, empty, empty, empty, empty, empty, empty, empty, empty, empty,
    empty, empty, empty, empty, empty, empty, empty, empty, empty, empty,
    empty, white, empty, white, empty, white, empty, white, empty, white,
    white, empty, white, empty, white, empty, white, empty, white, empty,
    empty, white, empty, white, empty, white, empty, white, empty, white,
    white, empty, white, empty, white, empty, white, empty, white, empty
    ],
    IOList = io_lib:format("~w", [Board]),
    {IOList, State};
get("/hi/:name", Req, State) ->
    Status = ok,
    Name = leptus_req:param(Req, name),
    Body = [{<<"say">>, <<"Hi">>}, {<<"to">>, Name}],
    {Name, State}.

terminate(_Reason, _Route, _Req, _State) ->
    ok.


%% develop

start() ->
  Board = [
  empty, black, empty, black, empty, black, empty, black, empty, black,
  black, empty, black, empty, black, empty, black, empty, black, empty,
  empty, black, empty, black, empty, black, empty, black, empty, black,
  black, empty, black, empty, black, empty, black, empty, black, empty,
  empty, empty, empty, empty, empty, empty, empty, empty, empty, empty,
  empty, empty, empty, empty, empty, empty, empty, empty, empty, empty,
  empty, white, empty, white, empty, white, empty, white, empty, white,
  white, empty, white, empty, white, empty, white, empty, white, empty,
  empty, white, empty, white, empty, white, empty, white, empty, white,
  white, empty, white, empty, white, empty, white, empty, white, empty
  ].
