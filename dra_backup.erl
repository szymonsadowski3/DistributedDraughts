-module(dra).
-import(io_lib, [format/2]).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([cross_domains/3]).
-export([get/3]).
-export([terminate/4]).
-export([server_start/0]).
-export([testEvaluate/0, reverseMapBoardIndex/1]).

%% DICTIONARY

%% 0: empty
%% 1: white
%% 2: black

%% Cross Domain Origin
%% It accepts any host for cross-domain requests
cross_domains(_Route, _Req, State) ->
   {['_'], State}.

 %% Util
reverseMapBoardIndex(Index) -> {trunc(Index/10), Index rem 10}.
mapPositionToIndex(Row, Col) -> Row*10 + Col.
mapPositionToIndex(RowCol) -> element(1, RowCol)*10 + element(2, RowCol).
getElementFromBoard(Board, RowCol) -> maps:get(mapPositionToIndex(RowCol), Board).
getElementFromBoard(Board, Row, Col) -> maps:get(mapPositionToIndex(Row, Col), Board).

 %% Board Evaluation

evaluateFor(Who, Board) -> % Who: Integer
  BoardToList = maps:to_list(Board),
  FilteredBoard = [element(2, Tup) || Tup <- BoardToList, element(2, Tup) == Who],
  length(FilteredBoard).

 isWithinBounds(ToRowCol) ->
  {ToRow, ToCol} = ToRowCol,
  (ToRow >= 0) and (ToRow =< 10) and (ToCol >= 0) and (ToCol =< 10).

getPotentialJumpMove(FromRowCol, ThroughRowCol) ->
  {FromRow, FromCol} = FromRowCol,
  {ThroughRow, ThroughCol} = ThroughRowCol,
  DirectionRow = ThroughRow - FromRow,
  DirectionCol = ThroughCol - FromCol,
  {FromRow + 2*DirectionRow, FromCol + 2*DirectionCol}.

jumpThrough(Board, FromRowCol, ThroughRowCol, Move) ->
   NewFrom = getPotentialJumpMove(FromRowCol, ThroughRowCol),
   CanJumpThrough = isWithinBounds(NewFrom) and checkIfCanJumpThrough(Board, FromRowCol, ThroughRowCol),

   if CanJumpThrough -> jumpThrough(Board, getPotentialJumpMove(FromRowCol, ThroughRowCol))
   .

jumpThrough(Board, FromRowCol, ThroughRowCol) -> jumpThrough(Board, FromRowCol, ThroughRowCol, []).



 generateAvailableMovesForWhitePiece(Index, Board) -> % Index: Integer
   {Row, Col} = reverseMapBoardIndex(Index),
   PosLeft = {Row+1, Col-1},
   PosRight = {Row+1, Col+1},
   SimpleMovesAndLegality = [
{[{Row, Col}, PosLeft], isWithinBounds(PosLeft) and isMoveToEmpty(Board, {Row, Col}, PosLeft)},
{[{Row, Col}, PosRight], isWithinBounds(PosRight) and isMoveToEmpty(Board, {Row, Col}, PosRight)}
    ],
    GeneratedJumpThroughs = [jumpThrough(Board, {Row, Col}, PosLeft), jumpThrough(Board, {Row, Col}, PosRight)]
   .

isMoveToEmpty(Board, FromRowCol, ToRowCol) -> (getElementFromBoard(Board, ToRowCol) == 0).

checkIfCanJumpThrough(Board, FromRowCol, ThroughRowCol) ->
  ToRowCol = getPotentialJumpMove(FromRowCol, ThroughRowCol),
  From = getElementFromBoard(Board, FromRowCol),
  Through = getElementFromBoard(Board, ThroughRowCol),
  To = getElementFromBoard(Board, ToRowCol),
  (From + Through == 3) and (To == 0).
%% TODO na razie jest sprawdzany pojedynczy skok (trzeba zrobic obsluge chainingu)

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

server_start()->
    leptus:start_listener(http, [{'_', [{dra, undefined_state}]}]).

%% develop

getOutputBoard(Board) ->
  BoardList = [maps:get(Index, Board) || Index <- lists:seq(1,100)],
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

getMove(Board, Black) ->
  Board;

getMove(Board, White) ->
  Board.

%% output

testEvaluate() ->
  evaluateFor(1, getStartingBoard()).
