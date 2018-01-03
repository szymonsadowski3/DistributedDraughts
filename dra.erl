-module(dra).
-import(io_lib, [format/2]).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([cross_domains/3]).
-export([get/3]).
-export([terminate/4]).
-export([server_start/0]).
-export([testEvaluate/0, reverseMapBoardIndex/1, getTestBoard/0, findLegalJumpMove/3, getElementFromBoard/2]).

%% DICTIONARY

%% 0: empty
%% 1: white
%% 2: black

%% Cross Domain Origin
%% It accepts any host for cross-domain requests
cross_domains(_Route, _Req, State) ->
  {['_'], State}.

%% Util
reverseMapBoardIndex(Index) -> {trunc(Index / 10), Index rem 10}.
mapPositionToIndex(Row, Col) -> Row * 10 + Col.
mapPositionToIndex(RowCol) -> element(1, RowCol) * 10 + element(2, RowCol).
getElementFromBoard(Board, RowCol) -> maps:get(mapPositionToIndex(RowCol), Board).
getElementFromBoard(Board, Row, Col) -> maps:get(mapPositionToIndex(Row, Col), Board).

isEmpty(Board, RowCol) -> getElementFromBoard(Board, RowCol) == 0.

%% Board Evaluation

evaluateFor(Who, Board) -> % Who: Integer
  BoardToList = maps:to_list(Board),
  FilteredBoard = [element(2, Tup) || Tup <- BoardToList, element(2, Tup) == Who],
  length(FilteredBoard).

isWithinBounds(ToRowCol) ->
  {ToRow, ToCol} = ToRowCol,
  (ToRow >= 0) and (ToRow =< 10) and (ToCol >= 0) and (ToCol =< 10).

findSimpleMoves(Board, FromRowCol) ->
  {Row, Col} = FromRowCol,
  ListOfPosLeftAndRight = [{Row + 1, Col + 1}, {Row + 1, Col - 1}],
  [Pos || Pos <- ListOfPosLeftAndRight, isWithinBounds(Pos) and isEmpty(Board, Pos)].

findLegalJumpMove(Board, FromRowCol, ForWho) ->
  Sign = (if ForWho == 1 -> 1; true -> -1 end), % 1 for white, -1 for black
  io:format("The Sign is: ~p\n", [Sign]),

  {Row, Col} = FromRowCol,
  ThroughLeft = {Row + Sign*1, Col - 1},
  ToLeft = {Row + Sign*2, Col - 2},

  IsLeftJumpWithinBounds = isWithinBounds(ToLeft),
  IsThroughLeftOpposite = getElementFromBoard(Board, FromRowCol) + getElementFromBoard(Board, ThroughLeft) == 3,
  IsFinalLeftFieldEmpty = isEmpty(Board, ToLeft),
  IsLeftLegit = IsLeftJumpWithinBounds and IsThroughLeftOpposite and IsFinalLeftFieldEmpty,

  io:format("IsLeftJumpWithinBounds: ~p\n", [IsLeftJumpWithinBounds]),
  io:format("IsThroughLeftOpposite: ~p\n", [IsThroughLeftOpposite]),
  io:format("IsFinalLeftFieldEmpty: ~p\n", [IsFinalLeftFieldEmpty]),
  io:format("IsLeftLegit: ~p\n", [IsLeftLegit]),

  ThroughRight = {Row + Sign*1, Col + 1},
  ToRight = {Row + Sign*2, Col + 2},

  io:format("ElFrom: ~p\n", [getElementFromBoard(Board, FromRowCol)]),
  io:format("ElThrough: ~p\n", [getElementFromBoard(Board, ThroughRight)]),
  io:format("ThroughLeft: ~p\n", [ThroughRight]),

  IsRightJumpWithinBounds = isWithinBounds(ToRight),
  IsThroughRightOpposite = getElementFromBoard(Board, FromRowCol) + getElementFromBoard(Board, ThroughRight) == 3,
  IsFinalRightFieldEmpty = isEmpty(Board, ToRight),
  IsRightLegit = IsRightJumpWithinBounds and IsThroughRightOpposite and IsFinalRightFieldEmpty,

  if
    IsLeftLegit -> [FromRowCol, ToLeft] ++ findLegalJumpMove(Board, ToLeft, ForWho);
    IsRightLegit -> [FromRowCol, ToRight] ++ findLegalJumpMove(Board, ToRight, ForWho);
    true -> []
  end.

%% API

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
  {IOList, State}.

terminate(_Reason, _Route, _Req, _State) ->
  ok.

server_start() ->
  leptus:start_listener(http, [{'_', [{dra, undefined_state}]}]).

%% develop

getOutputBoard(Board) ->
  BoardList = [maps:get(Index, Board) || Index <- lists:seq(1, 100)],
  io_lib:format("~w", [BoardList]).

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
% dra:findLegalJumpMove(dra:getStartingBoard(), {6, 3}, 1).
getTestBoard() ->
  Board = #{1 => 0, 2 => 1, 3 => 0, 4 => 1, 5 => 0, 6 => 1, 7 => 0, 8 => 1, 9 => 0, 10 => 1,
    11 => 1, 12 => 0, 13 => 1, 14 => 0, 15 => 1, 16 => 0, 17 => 1, 18 => 0, 19 => 1, 20 => 0,
    21 => 0, 22 => 1, 23 => 0, 24 => 1, 25 => 0, 26 => 1, 27 => 0, 28 => 1, 29 => 0, 30 => 1,
    31 => 1, 32 => 0, 33 => 1, 34 => 0, 35 => 1, 36 => 0, 37 => 1, 38 => 0, 39 => 1, 40 => 0,
    41 => 0, 42 => 0, 43 => 0, 44 => 0, 45 => 0, 46 => 0, 47 => 0, 48 => 0, 49 => 0, 50 => 0,
    51 => 0, 52 => 0, 53 => 1, 54 => 0, 55 => 0, 56 => 0, 57 => 0, 58 => 0, 59 => 0, 60 => 0,
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

%% output

testEvaluate() ->
  evaluateFor(1, getStartingBoard()).
