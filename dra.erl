-module(dra).
-import(io_lib, [format/2]).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([cross_domains/3]).
-export([get/3]).
-export([terminate/4]).
-export([server_start/0]).
-export([reverseMapBoardIndex/1, getTestBoard/0, findLegalJumpMove/3, getElementFromBoard/2]).
-export([testEvaluate/0, testRandom/0, testPrintList/0]).
%% DICTIONARY

%% 0: empty
%% 1: white
%% 2: black

%% Cross Domain Origin
%% It accepts any host for cross-domain requests
cross_domains(_Route, _Req, State) ->
  {['_'], State}.

%% Util
reverseMapBoardIndex(Index) -> {Index div 10, Index rem 10}.

mapPositionToIndex(Row, Col) -> Row * 10 + Col.
mapPositionToIndex(RowCol) -> element(1, RowCol) * 10 + element(2, RowCol).
getElementFromBoard(Board, RowCol) -> maps:get(mapPositionToIndex(RowCol), Board).
getElementFromBoard(Board, Row, Col) -> maps:get(mapPositionToIndex(Row, Col), Board).

isEmpty(Board, RowCol) -> getElementFromBoard(Board, RowCol) == 0.

printAList([])-> io:format("~n");
printAList([H|T]) when H rem 2 /= 0 ->
    printAList(T);
printAList([H|T]) ->
    io:format("~p ", [H]),
    [H|printAList(T)].

%% Board Evaluation

evaluateFor(Who, Board) -> % Who: Integer
  BoardToList = maps:to_list(Board),
  FilteredBoard = [element(2, Tup) || Tup <- BoardToList, element(2, Tup) == Who],
  length(FilteredBoard).

isWithinBounds(ToRowCol) ->
  {ToRow, ToCol} = ToRowCol,
  (ToRow >= 0) and (ToRow < 10) and (ToCol >= 0) and (ToCol < 10).

findSimpleMoves(Board, FromRowCol) ->
  {Row, Col} = FromRowCol,
  ListOfPosLeftAndRight = [{Row + 1, Col + 1}, {Row + 1, Col - 1}],
  [Pos || Pos <- ListOfPosLeftAndRight, isWithinBounds(Pos) and isEmpty(Board, Pos)].

findLegalJumpMove(Board, FromRowCol, ForWho) ->
  io:format("FromRowCol:~p\n", [FromRowCol]),
  Sign = (if ForWho == 1 -> 1; true -> -1 end), % 1 for white, -1 for black

  {Row, Col} = FromRowCol,
  ThroughLeft = {Row + Sign*1, Col - 1},
  ToLeft = {Row + Sign*2, Col - 2},

  IsLeftJumpWithinBounds = isWithinBounds(ToLeft),
  IsThroughLeftOpposite = getElementFromBoard(Board, FromRowCol) + getElementFromBoard(Board, ThroughLeft) == 3,
  IsFinalLeftFieldEmpty = isEmpty(Board, ToLeft),
  IsLeftLegit = IsLeftJumpWithinBounds and IsThroughLeftOpposite and IsFinalLeftFieldEmpty,

  ThroughRight = {Row + Sign*1, Col + 1},
  ToRight = {Row + Sign*2, Col + 2},

  IsRightJumpWithinBounds = isWithinBounds(ToRight),
  IsThroughRightOpposite = getElementFromBoard(Board, FromRowCol) + getElementFromBoard(Board, ThroughRight) == 3,
  IsFinalRightFieldEmpty = isEmpty(Board, ToRight),
  IsRightLegit = IsRightJumpWithinBounds and IsThroughRightOpposite and IsFinalRightFieldEmpty,

  % Podmianka boarda

  if
    IsLeftLegit -> [FromRowCol, ToLeft] ++ findLegalJumpMove(maps:update(mapPositionToIndex(ToLeft), ForWho, Board), ToLeft, ForWho);
    IsRightLegit -> [FromRowCol, ToRight] ++ findLegalJumpMove(maps:update(mapPositionToIndex(ToRight), ForWho, Board), ToRight, ForWho);
    true -> []
  end.

getAllPlayerPiecesLocationsOnBoard(Board, Who) ->
  BoardToList = maps:to_list(Board),
  ListOfPiecesIndices = [element(1, Tup) || Tup <- BoardToList, element(2, Tup) == Who],
  ListOfPiecesLocations = lists:map(fun reverseMapBoardIndex/1, ListOfPiecesIndices).

getRandomMove(Board, 1) ->
  ListOfPieces = getAllPlayerPiecesLocationsOnBoard(Board, 1),
  ListOfMoves = [findSimpleMoves(Board, FromRowCol) || FromRowCol <- ListOfPieces],
  Index = random:uniform(length(ListOfMoves)),
  lists:nth(Index, ListOfMoves);

getRandomMove(Board, 2) ->
  ListOfPieces = getAllPlayerPiecesLocationsOnBoard(Board, 2),
  ListOfMoves = [findSimpleMoves(Board, FromRowCol) || FromRowCol <- ListOfPieces],
  Index = random:uniform(length(ListOfMoves)),
  lists:nth(Index, ListOfMoves).

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
  Board = #{0 => 0, 1 => 1, 2 => 0, 3 => 1, 4 => 0, 5 => 1, 6 => 0, 7 => 1, 8 => 0, 9 => 1,
    10 => 1, 11 => 0, 12 => 1, 13 => 0, 14 => 1, 15 => 0, 16 => 1, 17 => 0, 18 => 1, 19 => 0,
    20 => 0, 21 => 1, 22 => 0, 23 => 1, 24 => 0, 25 => 1, 26 => 0, 27 => 1, 28 => 0, 29 => 1,
    30 => 1, 31 => 0, 32 => 1, 33 => 0, 34 => 1, 35 => 0, 36 => 1, 37 => 0, 38 => 1, 39 => 0,
    40 => 0, 41 => 0, 42 => 0, 43 => 0, 44 => 0, 45 => 0, 46 => 0, 47 => 0, 48 => 0, 49 => 0,
    50 => 0, 51 => 0, 52 => 0, 53 => 0, 54 => 0, 55 => 0, 56 => 0, 57 => 0, 58 => 0, 59 => 0,
    60 => 0, 61 => 2, 62 => 0, 63 => 2, 64 => 0, 65 => 2, 66 => 0, 67 => 2, 68 => 0, 69 => 2,
    70 => 2, 71 => 0, 72 => 2, 73 => 0, 74 => 2, 75 => 0, 76 => 2, 77 => 0, 78 => 2, 79 => 0,
    80 => 0, 81 => 2, 82 => 0, 83 => 2, 84 => 0, 85 => 2, 86 => 0, 87 => 2, 88 => 0, 89 => 2,
    90 => 2, 91 => 0, 92 => 2, 93 => 0, 94 => 2, 95 => 0, 96 => 2, 97 => 0, 98 => 2, 99 => 0
  },

  Board.
% dra:findLegalJumpMove(dra:getStartingBoard(), {6, 3}, 1).
getTestBoard() ->
  Board = #{1 => 0, 2 => 1, 3 => 0, 4 => 1, 5 => 0, 6 => 1, 7 => 0, 8 => 1, 9 => 0, 10 => 1,
    11 => 1, 12 => 0, 13 => 1, 14 => 0, 15 => 1, 16 => 0, 17 => 1, 18 => 0, 19 => 1, 20 => 0,
    21 => 0, 22 => 1, 23 => 0, 24 => 1, 25 => 0, 26 => 0, 27 => 0, 28 => 1, 29 => 0, 30 => 1,
    31 => 1, 32 => 0, 33 => 1, 34 => 0, 35 => 1, 36 => 0, 37 => 1, 38 => 0, 39 => 1, 40 => 0,
    41 => 0, 42 => 0, 43 => 0, 44 => 0, 45 => 0, 46 => 0, 47 => 0, 48 => 0, 49 => 0, 50 => 0,
    51 => 0, 52 => 0, 53 => 1, 54 => 0, 55 => 0, 56 => 0, 57 => 0, 58 => 0, 59 => 0, 60 => 0,
    61 => 0, 62 => 2, 63 => 0, 64 => 2, 65 => 0, 66 => 2, 67 => 0, 68 => 2, 69 => 0, 70 => 2,
    71 => 2, 72 => 0, 73 => 2, 74 => 0, 75 => 2, 76 => 0, 77 => 2, 78 => 0, 79 => 2, 80 => 0,
    81 => 0, 82 => 2, 83 => 0, 84 => 2, 85 => 0, 86 => 2, 87 => 0, 88 => 2, 89 => 0, 90 => 2,
    91 => 2, 92 => 0, 93 => 2, 94 => 0, 95 => 2, 96 => 0, 97 => 2, 98 => 0, 99 => 2, 100 => 0
  },
  Board.

getMove(Board, 1) ->
  Board;

getMove(Board, 2) ->
  Board.

%% output

testEvaluate() ->
  evaluateFor(1, getStartingBoard()).

testRandom() ->
  printAList(getAllPlayerPiecesLocationsOnBoard(getStartingBoard(), 2)),
  ok.

testPrintList() ->
  printAList(maps:to_list(getStartingBoard())),
  ok.
