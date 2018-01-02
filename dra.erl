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
    Board = getStartingBoard(),
    IOList = getOutputBoard(Board),
    {IOList, State};
get("/move/white", _Req, State) ->
    %UpdatedBoard = getMove(Board, White),
    %IOList = getOutputBoard(Board),
    Board = getStartingBoard(),
    IOList = getOutputBoard(Board),
    {IOList, State};
get("/move/black", _Req, State) ->
    %UpdatedBoard = getMove(Board, Black),
    %IOList = getOutputBoard(Board),
    Board = getStartingBoard(),
    IOList = getOutputBoard(Board),
    {IOList, State};
get("/hi/:name", Req, State) ->
    Status = ok,
    Name = leptus_req:param(Req, name),
    Body = [{<<"say">>, <<"Hi">>}, {<<"to">>, Name}],
    {Name, State}.

terminate(_Reason, _Route, _Req, _State) ->
    ok.

server_start()->
    leptus:start_listener(http, [{'_', [{dra, undefined_state}]}]).

%% develop

getOutputBoard(Board) ->
  BoardList = [maps:get(Index, Board) || Index <- lists:seq(1,100)],
  % BoardList = maps:values(Board),
  IOList = io_lib:format("~w", [BoardList]).

getStartingBoard() ->
 Board = #{1 => 0, 2 => 1, 3 => 0, 4 => 1, 5 => 0, 6 => 1, 7 => 0, 8 => 1, 9 => 0, 10 => 1,
            11 => 1, 12 => 0, 13 => 1, 14 => 0, 15 => 1, 16 => 0, 17 => 1, 18 => 0, 19 => 1, 20 => 0,
            21 => 0, 22 => 1, 23 => 0, 24 => 1, 25 => 0, 26 => 1, 27 => 0, 28 => 1, 29 => 0, 30 => 1,
            31 => 1, 32 => 0, 33 => 1, 34 => 0, 35 => 1, 36 => 0, 37 => 1, 38 => 0, 39 => 1, 40 => 0,
            41 => 0, 42 => 0, 43 => 0, 44 => 0, 45 => 0, 46 => 0, 47 => 0, 48 => 0, 49 => 0, 50 => 0,
            51 => 0, 52 => 0, 53 => 0, 54 => 0, 55 => 0, 56 => 0, 57 => 0, 58 => 0, 59 => 0, 60 => 0,
            61 => 0, 62 => 2, 63 => 0, 64 => 2, 65 => 0, 66 => 2, 67 => 0, 68 => 2, 69 => 0, 70 => 2,
            71 => 2, 72 => 0, 73 => 2, 74 => 0, 75 => 2, 76 => 0, 77 => 2, 78 => 0, 79 => 2, 80 => 0,
            81 => 0, 82 => 2, 83 => 0, 84 => 2, 85 => 0, 86 => 2, 87 => 0, 88 => 2, 89 => 0, 90 => 2,
            91 => 2, 92 => 0, 93 => 2, 94 => 0, 95 => 2, 96 => 0, 97 => 2, 98 => 0, 99 => 2, 100 => 0
            },
  Board.

getMove(Board, Black) ->
  Board;

getMove(Board, White) ->
  Board.
