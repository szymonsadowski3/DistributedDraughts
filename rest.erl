-module(rest).
-import(io_lib, [format/2]).
-compile({parse_transform, leptus_pt}).
-compile(export_all).

%% DICTIONARY

%% 0: empty
%% 1: white
%% 2: black

%% Cross Domain Origin
%% It accepts any host for cross-domain requests
cross_domains(_Route, _Req, State) ->
  {['_'], State}.

%% API

init(_Route, _Req, State) ->
  {ok, State}.

get("/", _Req, State) ->
  Board = dra:getStartingBoard(),
  IOList = dra:getOutputBoard(Board),
  {IOList, State};
get("/board", _Req, State) ->
  Board = dra:getStartingBoard(),
  IOList = dra:getOutputBoard(Board),
  {IOList, State};
get("/move/white", _Req, State) ->
  %UpdatedBoard = getMove(Board, White),
  %IOList = getOutputBoard(Board),
  Board = dra:getStartingBoard(),
  IOList = dra:getOutputBoard(Board),
  {IOList, State};
get("/move/black", _Req, State) ->
  %UpdatedBoard = getMove(Board, Black),
  %IOList = getOutputBoard(Board),
  Board = dra:getStartingBoard(),
  IOList = dra:getOutputBoard(Board),
  {IOList, State}.

terminate(_Reason, _Route, _Req, _State) ->
  ok.

server_start() ->
  leptus:start_listener(http, [{'_', [{rest, undefined_state}]}]).
