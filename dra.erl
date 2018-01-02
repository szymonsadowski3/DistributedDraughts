-module(dra).
-import(io_lib, [format/2]).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([cross_domains/3]).
-export([get/3]).
-export([terminate/4]).
-export([server_start/0]).

%% Cross Domain Origin
%% It accepts any host for cross-domain requests
cross_domains(_Route, _Req, State) ->
   {['_'], State}.

init(_Route, _Req, State) ->
    {ok, State}.

get("/", _Req, State) ->
    {<<"Hello, leptus!">>, State};
get("/board", _Req, State) ->
    Board = start(),
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

server_start()->
  leptus:start_listener(http, [{'_', [{dra, undefined_state}]}]).

start() ->
  Board = [
  0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
  1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
  0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
  1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
  2, 0, 2, 0, 2, 0, 2, 0, 2, 0,
  0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
  2, 0, 2, 0, 2, 0, 2, 0, 2, 0
  ].
