%% --- DEFINITIONS ---

%% 0: empty
%% 1: white
%% 2: black

-module(dra).

-import(io, [format/2]).
-import(lists, [keysort/2]).

-compile(export_all).
-compile([debug_info]).

%% ------ INIT VALUE PROVIDERS ------

getOutputBoard(BoardMap) ->
  BoardMapToList = maps:to_list(BoardMap),
  BoardMapToListSortedByIndex = lists:keysort(1, BoardMapToList),
  BoardMapToListSortedByIndexSecElements = [element(2, X) || X <- BoardMapToListSortedByIndex],
  io_lib:format("~w", [BoardMapToListSortedByIndexSecElements]).

getOutputBoards(Boards) ->
  ListOfOutputBoards = [getOutputBoard(Board) || Board <- Boards],
  lists:join("|", ListOfOutputBoards).

getStartingBoard() -> #{0 => 0, 1 => 1, 2 => 0, 3 => 1, 4 => 0, 5 => 1, 6 => 0, 7 => 1, 8 => 0, 9 => 1,
  10 => 1, 11 => 0, 12 => 1, 13 => 0, 14 => 1, 15 => 0, 16 => 1, 17 => 0, 18 => 1, 19 => 0,
  20 => 0, 21 => 1, 22 => 0, 23 => 1, 24 => 0, 25 => 1, 26 => 0, 27 => 1, 28 => 0, 29 => 1,
  30 => 1, 31 => 0, 32 => 1, 33 => 0, 34 => 1, 35 => 0, 36 => 1, 37 => 0, 38 => 1, 39 => 0,
  40 => 0, 41 => 0, 42 => 0, 43 => 0, 44 => 0, 45 => 0, 46 => 0, 47 => 0, 48 => 0, 49 => 0,
  50 => 0, 51 => 0, 52 => 0, 53 => 0, 54 => 0, 55 => 0, 56 => 0, 57 => 0, 58 => 0, 59 => 0,
  60 => 0, 61 => 2, 62 => 0, 63 => 2, 64 => 0, 65 => 2, 66 => 0, 67 => 2, 68 => 0, 69 => 2,
  70 => 2, 71 => 0, 72 => 2, 73 => 0, 74 => 2, 75 => 0, 76 => 2, 77 => 0, 78 => 2, 79 => 0,
  80 => 0, 81 => 2, 82 => 0, 83 => 2, 84 => 0, 85 => 2, 86 => 0, 87 => 2, 88 => 0, 89 => 2,
  90 => 2, 91 => 0, 92 => 2, 93 => 0, 94 => 2, 95 => 0, 96 => 2, 97 => 0, 98 => 2, 99 => 0
}.

getTestBoard() -> #{0 => 0, 1 => 3, 2 => 2, 3 => 1, 4 => 0, 5 => 0, 6 => 0, 7 => 1, 8 => 0, 9 => 1,
  10 => 1, 11 => 0, 12 => 2, 13 => 0, 14 => 1, 15 => 0, 16 => 1, 17 => 0, 18 => 1, 19 => 0,
  20 => 0, 21 => 0, 22 => 0, 23 => 0, 24 => 0, 25 => 1, 26 => 0, 27 => 1, 28 => 0, 29 => 1,
  30 => 0, 31 => 0, 32 => 1, 33 => 0, 34 => 1, 35 => 2, 36 => 1, 37 => 0, 38 => 1, 39 => 0,
  40 => 0, 41 => 0, 42 => 0, 43 => 0, 44 => 3, 45 => 0, 46 => 0, 47 => 0, 48 => 0, 49 => 0,
  50 => 0, 51 => 0, 52 => 0, 53 => 0, 54 => 0, 55 => 0, 56 => 0, 57 => 0, 58 => 0, 59 => 0,
  60 => 0, 61 => 4, 62 => 0, 63 => 0, 64 => 0, 65 => 2, 66 => 0, 67 => 2, 68 => 0, 69 => 2,
  70 => 0, 71 => 0, 72 => 2, 73 => 0, 74 => 0, 75 => 0, 76 => 2, 77 => 0, 78 => 2, 79 => 0,
  80 => 0, 81 => 0, 82 => 0, 83 => 2, 84 => 0, 85 => 2, 86 => 0, 87 => 2, 88 => 0, 89 => 2,
  90 => 2, 91 => 0, 92 => 0, 93 => 0, 94 => 2, 95 => 1, 96 => 0, 97 => 0, 98 => 2, 99 => 0
}.

%% ------ END OF INIT VALUE PROVIDERS ------

%% ------ DEBUGGING & PRINTING ------

printAList([]) -> io:format("~n");
printAList([H | T]) when H rem 2 /= 0 ->
  printAList(T);
printAList([H | T]) ->
  io:format("~p ", [H]),
  [H | printAList(T)].

optionalNewlineFun(Index) -> if
                               (Index rem 10) == 9 -> io:format("~n");
                               true -> ok
                             end.

printBoard(BoardMap) ->
  BoardMapToList = maps:to_list(BoardMap),
  BoardMapToListSortedByIndex = lists:keysort(1, BoardMapToList),
  [{io:format("~p | ", [element(2, IndexValTuple)]),
    optionalNewlineFun(element(1, IndexValTuple))} || IndexValTuple <- BoardMapToListSortedByIndex],
  io:format("~n", []).

printBoards(BoardMaps) ->
  [printBoard(BoardMap) || BoardMap <- BoardMaps],
  ok.

%% ------ END OF DEBUGGING & PRINTING ------

%% ------ UTILS ------

remove_dups([]) -> [];
remove_dups([H | T]) -> [H | [X || X <- remove_dups(T), X /= H]].
reverseMapBoardIndex(Index) -> {Index div 10, Index rem 10}.
mapPositionToIndex(Row, Col) -> Row * 10 + Col.
mapPositionToIndex(RowCol) -> element(1, RowCol) * 10 + element(2, RowCol).
isWithinBounds(ToRowCol) ->
  {ToRow, ToCol} = ToRowCol,
  (ToRow >= 0) and (ToRow < 10) and (ToCol >= 0) and (ToCol < 10).

invertWhoseMove(WhoseMove) -> case WhoseMove of
                                1 -> 2;
                                2 -> 1
                              end.

maxByFirstKey([H|T]) -> maxByFirstKey(T, H).
maxByFirstKey([H|T], Max) when element(1, H) > element(1, Max) -> maxByFirstKey(T, H);
maxByFirstKey([_|T], Max)              -> maxByFirstKey(T, Max);
maxByFirstKey([],    Max)              -> Max.

minByFirstKey([H|T]) -> minByFirstKey(T, H).
minByFirstKey([H|T], Max) when element(1, H) < element(1, Max) -> minByFirstKey(T, H);
minByFirstKey([_|T], Max)              -> minByFirstKey(T, Max);
minByFirstKey([],    Max)              -> Max.


%% ------ END OF UTILS ------

%% ------ BOARD FUNCTIONS ------

getElementFromBoard(Board, RowCol) -> maps:get(mapPositionToIndex(RowCol), Board).
getElementFromBoard(Board, Row, Col) -> maps:get(mapPositionToIndex(Row, Col), Board).

isEmpty(Board, RowCol) ->
  Element = getElementFromBoard(Board, RowCol),
  Element == 0.

numberOfPieces(Board, WhoseMove) ->
  BoardToList = maps:to_list(Board),
  FilteredBoard = [element(2, Tup) || Tup <- BoardToList, element(2, Tup) == WhoseMove],
  length(FilteredBoard).

getAllPlayerPiecesLocationsOnBoard(Board, WhoseMove) ->
  BoardToList = maps:to_list(Board),
  ListOfPiecesIndices = [element(1, Tup) || Tup <- BoardToList, element(2, Tup) == WhoseMove],
  lists:map(fun reverseMapBoardIndex/1, ListOfPiecesIndices).

clearAllPositionsButLast(Board, _, []) ->
  Board;

clearAllPositionsButLast(Board, WhoseMove, [Position | ListOfPositions]) ->
  case ListOfPositions of
    [] ->
      fillPositionOnBoard(Board, Position, WhoseMove);
    _ ->
      NewBoard = emptyPositionOnBoard(Board, Position),
      clearAllPositionsButLast(NewBoard, WhoseMove, ListOfPositions)
  end.

clearAllPositionsButLastKingMove(Board, _, []) ->
  Board;

clearAllPositionsButLastKingMove(Board, WhoseMove, [Position, NextPosition | ListOfPositions]) ->
  case ListOfPositions of
    [] ->
      MiddlePos = checkIfDoubleMove(Position, NextPosition),
      NewBoard = emptyPositionOnBoard(Board, Position),
      NewBoard2 = emptyPositionOnBoard(NewBoard, MiddlePos),
      fillPositionOnBoard(NewBoard2, NextPosition, WhoseMove);
    _ ->
      MiddlePos = checkIfDoubleMove(Position, NextPosition),
      NewBoard = emptyPositionOnBoard(Board, MiddlePos),
      NewBoard2 = emptyPositionOnBoard(NewBoard, Position),
      clearAllPositionsButLast(NewBoard2, WhoseMove, [NextPosition | ListOfPositions])
  end.

promoteToKing(Board) ->
  ListOfBlackLocations = getAllPlayerPiecesLocationsOnBoard(Board, 2),
  ListOfWhiteLocations = getAllPlayerPiecesLocationsOnBoard(Board, 1),

  WhiteKing = lists:keyfind(9,1,ListOfWhiteLocations),
  BlackKing = lists:keyfind(0,1,ListOfBlackLocations),
  if
    WhiteKing /= false ->
      NewBoard = fillPositionOnBoard(Board, WhiteKing, 3),
      NewBoard;
    BlackKing /= false ->
      NewBoard = fillPositionOnBoard(Board, BlackKing, 4),
      NewBoard;
    true ->
      Board
  end.

getBoardAfterMove(Board, WhoseMove, [MoveType | ListOfPositions]) ->
  if
    MoveType == king ->
      UpdatedBoard = clearAllPositionsButLastKingMove(Board, WhoseMove+2, ListOfPositions);
    true ->
      UpdatedBoard = clearAllPositionsButLast(Board, WhoseMove, ListOfPositions)
  end,
  UpdatedBoard.

checkIfDoubleMove({R1,C1},{R2,C2}) ->
  if
    (R1+R2) rem 2 == 0 andalso (C1+C2) rem 2 == 0 ->
      {(R1+R2) div 2,(C1+C2) div 2};
    true ->
      {-1, -1}
  end.

emptyPositionOnBoard(Board, {-1,-1}) ->
  Board;

emptyPositionOnBoard(Board, From) ->
  Index = mapPositionToIndex(From),
  NewBoard = Board#{Index := 0},
  NewBoard.

getMove(Board, 1) ->
  Board;

getMove(Board, 2) ->
  Board.

getRandomMove(Board, WhoseMove) ->
  ListOfNonEmptyMoves = findAllAvailableMoves(Board, WhoseMove),
  Index = rand:uniform(length(ListOfNonEmptyMoves)),
  lists:nth(Index, ListOfNonEmptyMoves).

fillPositionOnBoard(Board, To, WhoseMove) ->
  Index = mapPositionToIndex(To),
  NewBoard = Board#{Index := WhoseMove},
  NewBoard.

%% ------ END OF BOARD FUNCTIONS ------

%% ------ FINDING MOVES ------

findSimpleMovesForOnePiece(Board, FromRowCol, WhoseMove) ->
  Sign = (if WhoseMove == 1 -> 1; true -> -1 end), % 1 for white, -1 for black
  {Row, Col} = FromRowCol,
  ListOfPosLeftAndRight = [{Row + Sign * 1, Col + 1}, {Row + Sign * 1, Col - 1}],
  Result = [[FromRowCol, Pos] || Pos <- ListOfPosLeftAndRight, isWithinBounds(Pos) andalso isEmpty(Board, Pos)],
  Result.

getAllTopLeftMoves(Board, FromRowCol, WhoseMove) ->
  Sign = (if WhoseMove == 3 -> 1; true -> -1 end),
  {Row, Col} = FromRowCol,

  Through = {Row - Sign * 1, Col - 1},
  To = {Row - Sign * 2, Col - 2},


      FromValue = getElementFromBoard(Board, FromRowCol),

      IsLegit = isWithinBounds(Through) andalso
        FromValue-2 /= getElementFromBoard(Board, Through),
      IsToLegit = isWithinBounds(To) andalso
        FromValue-2 /= getElementFromBoard(Board, To),

                if
                  IsLegit ->
                    ThroughValue = getElementFromBoard(Board, Through),

                    if
                      IsToLegit ->
                        ToValue = getElementFromBoard(Board, To),

                          if
                            ThroughValue == 0 andalso ToValue == 0 ->
                              NewBoard = maps:update(mapPositionToIndex(To), WhoseMove, Board),
                              remove_dups([FromRowCol, Through, To] ++ getAllTopLeftMoves(NewBoard, To, WhoseMove));
                            ToValue == 0 ->
                              ClearBoard = emptyPositionOnBoard(Board, Through),
                              NewBoard = maps:update(mapPositionToIndex(To), WhoseMove, ClearBoard),
                              remove_dups([FromRowCol, To] ++ getAllTopLeftMoves(NewBoard, To, WhoseMove));
                            ToValue /= 0 andalso ThroughValue == 0 ->
                              NewBoard = maps:update(mapPositionToIndex(Through), WhoseMove, Board),
                              remove_dups([FromRowCol, Through] ++ getAllTopLeftMoves(NewBoard, Through, WhoseMove));
                            true ->
                              []
                          end;
                      ThroughValue /= 0 ->
                        [];
                      true ->
                        [FromRowCol, Through]
                    end;
                  true ->
                    []
                end.

getAllBottomLeftMoves(Board, FromRowCol, WhoseMove) ->
  Sign = (if WhoseMove == 3 -> 1; true -> -1 end),
  {Row, Col} = FromRowCol,

  Through = {Row + Sign * 1, Col - 1},
  To = {Row + Sign * 2, Col - 2},


      FromValue = getElementFromBoard(Board, FromRowCol),

      IsLegit = isWithinBounds(Through) andalso
        FromValue-2 /= getElementFromBoard(Board, Through),
      IsToLegit = isWithinBounds(To) andalso
        FromValue-2 /= getElementFromBoard(Board, To),

                if
                  IsLegit ->
                    ThroughValue = getElementFromBoard(Board, Through),

                    if
                      IsToLegit ->
                        ToValue = getElementFromBoard(Board, To),

                          if
                            ThroughValue == 0 andalso ToValue == 0 ->
                              NewBoard = maps:update(mapPositionToIndex(To), WhoseMove, Board),
                              remove_dups([FromRowCol, Through, To] ++ getAllTopLeftMoves(NewBoard, To, WhoseMove));
                            ToValue == 0 ->
                              ClearBoard = emptyPositionOnBoard(Board, Through),
                              NewBoard = maps:update(mapPositionToIndex(To), WhoseMove, ClearBoard),
                              remove_dups([FromRowCol, To] ++ getAllBottomLeftMoves(NewBoard, To, WhoseMove));
                            ToValue /= 0 andalso ThroughValue == 0 ->
                              NewBoard = maps:update(mapPositionToIndex(Through), WhoseMove, Board),
                              remove_dups([FromRowCol, Through] ++ getAllBottomLeftMoves(NewBoard, Through, WhoseMove));
                            true ->
                              []
                          end;
                      ThroughValue /= 0 ->
                        [];
                      true ->
                        [FromRowCol, Through]
                    end;
                  true ->
                    []
                end.

getAllBottomRightMoves(Board, FromRowCol, WhoseMove) ->
  Sign = (if WhoseMove == 3 -> 1; true -> -1 end),
  {Row, Col} = FromRowCol,

  Through =  {Row + Sign * 1, Col + 1},
  To = {Row + Sign * 2, Col + 2},


      FromValue = getElementFromBoard(Board, FromRowCol),

      IsLegit = isWithinBounds(Through) andalso
        FromValue-2 /= getElementFromBoard(Board, Through),
      IsToLegit = isWithinBounds(To) andalso
        FromValue-2 /= getElementFromBoard(Board, To),

                if
                  IsLegit ->
                    ThroughValue = getElementFromBoard(Board, Through),

                    if
                      IsToLegit ->
                        ToValue = getElementFromBoard(Board, To),

                          if
                            ThroughValue == 0 andalso ToValue == 0 ->
                              NewBoard = maps:update(mapPositionToIndex(To), WhoseMove, Board),
                              remove_dups([FromRowCol, Through, To] ++ getAllTopLeftMoves(NewBoard, To, WhoseMove));
                            ToValue == 0 ->
                              ClearBoard = emptyPositionOnBoard(Board, Through),
                              NewBoard = maps:update(mapPositionToIndex(To), WhoseMove, ClearBoard),
                              remove_dups([FromRowCol, To] ++ getAllBottomRightMoves(NewBoard, To, WhoseMove));
                            ToValue /= 0 andalso ThroughValue == 0 ->
                              NewBoard = maps:update(mapPositionToIndex(Through), WhoseMove, Board),
                              remove_dups([FromRowCol, Through] ++ getAllBottomRightMoves(NewBoard, Through, WhoseMove));
                            true ->
                              []
                          end;
                      ThroughValue /= 0 ->
                        [];
                      true ->
                        [FromRowCol, Through]
                    end;
                  true ->
                    []
                end.

getAllTopRightMoves(Board, FromRowCol, WhoseMove) ->
  Sign = (if WhoseMove == 3 -> 1; true -> -1 end),
  {Row, Col} = FromRowCol,

  Through =  {Row - Sign * 1, Col + 1},
  To = {Row - Sign * 2, Col + 2},


    FromValue = getElementFromBoard(Board, FromRowCol),

    IsLegit = isWithinBounds(Through) andalso
      FromValue-2 /= getElementFromBoard(Board, Through),
    IsToLegit = isWithinBounds(To) andalso
      FromValue-2 /= getElementFromBoard(Board, To),

              if
                IsLegit ->
                  ThroughValue = getElementFromBoard(Board, Through),

                  if
                    IsToLegit ->
                      ToValue = getElementFromBoard(Board, To),

                        if
                          ThroughValue == 0 andalso ToValue == 0 ->
                            NewBoard = maps:update(mapPositionToIndex(To), WhoseMove, Board),
                            remove_dups([FromRowCol, Through, To] ++ getAllTopLeftMoves(NewBoard, To, WhoseMove));
                          ToValue == 0 ->
                            ClearBoard = emptyPositionOnBoard(Board, Through),
                            NewBoard = maps:update(mapPositionToIndex(To), WhoseMove, ClearBoard),
                            remove_dups([FromRowCol, To] ++ getAllTopRightMoves(NewBoard, To, WhoseMove));
                          ToValue /= 0 andalso ThroughValue == 0 ->
                            NewBoard = maps:update(mapPositionToIndex(Through), WhoseMove, Board),
                            remove_dups([FromRowCol, Through] ++ getAllTopRightMoves(NewBoard, Through, WhoseMove));
                          true ->
                            []
                        end;
                    ThroughValue /= 0 ->
                      [];
                    true ->
                      [FromRowCol, Through]
                  end;
                true ->
                  []
              end.

getAllMovesWithin(MovesList) ->
  Len = length(MovesList),
  if
    Len == 0 ->
      Moves = [];
    true ->
      Moves = [lists:sublist(MovesList, Index) || Index <- lists:seq(2, Len)]
  end,
  Moves.

findLegalKingMovesForOnePiece(Board, FromRowCol, WhoseMove) ->
  getAllMovesWithin(getAllTopLeftMoves(Board, FromRowCol, WhoseMove)) ++
  getAllMovesWithin(getAllBottomLeftMoves(Board,FromRowCol, WhoseMove)) ++
  getAllMovesWithin(getAllTopRightMoves(Board,FromRowCol, WhoseMove)) ++
  getAllMovesWithin(getAllBottomRightMoves(Board,FromRowCol, WhoseMove)).

findLegalJumpMovesForOnePieceDuplicates(Board, FromRowCol, WhoseMove, ParentPID) ->
  Sign = (if WhoseMove == 1 -> 1; true -> -1 end), % 1 for white, -1 for black

  {Row, Col} = FromRowCol,
  ThroughLeft = {Row + Sign * 1, Col - 1},
  ToLeft = {Row + Sign * 2, Col - 2},

  FromValue = getElementFromBoard(Board, FromRowCol),

  IsLeftLegit = isWithinBounds(ToLeft) andalso isEmpty(Board, ToLeft) andalso
    (FromValue + getElementFromBoard(Board, ThroughLeft) == 3), % is "ThroughLeft" element of an opposite value
  % e.g. from = white and through = black
  ThroughRight = {Row + Sign * 1, Col + 1},
  ToRight = {Row + Sign * 2, Col + 2},

  IsRightLegit = isWithinBounds(ToRight) andalso isEmpty(Board, ToRight) andalso
    (FromValue + getElementFromBoard(Board, ThroughRight) == 3),  % is "Through" element of an opposite
if
    IsLeftLegit ->
      LeftBoard = maps:update(mapPositionToIndex(ToLeft), WhoseMove, Board),
      LeftJumps = [FromRowCol, ThroughLeft, ToLeft]
      ++ findLegalJumpMovesForOnePiece(LeftBoard, ToLeft, WhoseMove);
    true ->
      LeftJumps = []
end,
if
    IsRightLegit ->
      RightBoard = maps:update(mapPositionToIndex(ToRight), WhoseMove, Board),
      RightJumps = [FromRowCol, ThroughRight, ToRight] ++ findLegalJumpMovesForOnePiece(RightBoard, ToRight, WhoseMove);
    true ->
      RightJumps = []
end,
List = LeftJumps ++ RightJumps,
ParentPID ! {self(), List}.

findLegalJumpMovesForOnePiece(Board, FromRowCol, WhoseMove) ->
  PID = spawn(dra, findLegalJumpMovesForOnePieceDuplicates, [Board, FromRowCol, WhoseMove, self()]),
  receive
    {PID, List} ->
      List
  end.

findAllSimpleMoves(Board, WhoseMove, SendPID) ->
  ListOfPieces = getAllPlayerPiecesLocationsOnBoard(Board, WhoseMove),
  ListOfMoves = [findSimpleMovesForOnePiece(Board, FromRowCol, WhoseMove) || FromRowCol <- ListOfPieces],
  ListOfNonEmptyMoves = [Move || Move <- ListOfMoves, Move /= []],
  ListOfNonEmptyMovesFlat = [lists:nth(1, Elem) || Elem <- ListOfNonEmptyMoves],
  SendPID ! {simple, ListOfNonEmptyMovesFlat}.

findAllJumpMoves(Board, WhoseMove, SendPID) ->
  ListOfPieces = getAllPlayerPiecesLocationsOnBoard(Board, WhoseMove),
  ListOfMoves = [findLegalJumpMovesForOnePiece(Board, FromRowCol, WhoseMove) || FromRowCol <- ListOfPieces],
  ListOfNonEmptyMoves = [Move || Move <- ListOfMoves, Move /= []],
  SendPID ! {jump, ListOfNonEmptyMoves}.

findAllKingMoves(Board, WhoseMove, SendPID) ->
  ListOfPieces = getAllPlayerPiecesLocationsOnBoard(Board, WhoseMove),
  if
    ListOfPieces == [] ->
      KingMoves = [];
    true ->
      ListOfMoves = [findLegalKingMovesForOnePiece(Board, FromRowCol, WhoseMove) || FromRowCol <- ListOfPieces],
      ListOfMovesFlat = lists:nth(1, ListOfMoves),
      KingMoves = [Move || Move <- ListOfMovesFlat, Move /= []]
  end,
  SendPID ! {king, KingMoves}.

findAllAvailableMoves(Board, WhoseMove) ->
       spawn(dra, findAllKingMoves, [Board, WhoseMove+2, self()]),
       spawn(dra, findAllSimpleMoves, [Board, WhoseMove, self()]),
       spawn(dra, findAllJumpMoves, [Board, WhoseMove, self()]),

  ListOfMoves = [],
  receive
    {simple, SimpleList} ->
      ListOfSimpleMoves = ListOfMoves ++ [[simple] ++ X || X <- SimpleList]
  end,
 receive
    {jump, JumpList} ->
      ListOfJumpAndSimpleMoves = if
                         length(JumpList) > 0 -> [[jump] ++ X || X <- JumpList];
                         true -> ListOfSimpleMoves ++ [[jump] ++ X || X <- JumpList]
                       end
 end,
 receive
   {king, KingList} ->
      ListOfAllMoves = ListOfJumpAndSimpleMoves ++ [[king] ++ X || X <- KingList]
 end,
 ListOfAllMoves.

%% ------ END OF FINDING MOVES ------

game() ->
  Board = getStartingBoard(),
  printBoard(Board),
  WhoseMove = 1,
  doGameLoop(Board, WhoseMove).

doGameLoop(Board, Player) ->
  NewBoard = doRandomTurn(Board, Player),
  printBoard(NewBoard),
  NewPlayer = if
                Player == 1 -> 2;
                true -> 1
              end,

  NumberOfPieces = numberOfPieces(NewBoard, NewPlayer),
  if
    NumberOfPieces > 0 ->
      doGameLoop(NewBoard, NewPlayer);
    true ->
      io:format("Player ~p won!", [Player])
  end.

%% ------- AI FUNCTIONS ------

evaluateFor(Who, Board) -> % Who: Integer
  BoardToList = maps:to_list(Board),
  FilteredBoard = [element(2, Tup) || Tup <- BoardToList, element(2, Tup) == Who],
  length(FilteredBoard).

doRandomTurn(Board, WhoseMove) ->
  Move = getRandomMove(Board, WhoseMove),
  getBoardAfterMove(Board, WhoseMove, Move).

getAllPossibleBoards(Board, WhoseMove) ->
  AvailableMoves = findAllAvailableMoves(Board, WhoseMove),
  PossibleBoards = [getBoardAfterMove(Board, WhoseMove, Move) || Move <- AvailableMoves],
  PossibleBoards.

doAiTurn(Board, WhoseMove) ->
  Boards = getAllPossibleBoards(Board, WhoseMove),
  Boards.

%% ------- END OF AI FUNCTIONS ------


%% ------------------------------- TESTING FUNCTION -------------------------

testEvaluate() ->
  evaluateFor(1, getStartingBoard()).

testRandom() ->
  printAList(getAllPlayerPiecesLocationsOnBoard(getStartingBoard(), 2)),
  ok.

testPrintList() ->
  printAList(maps:to_list(getStartingBoard())),
  ok.

testGetBoardAfterSimpleMove() ->
  Board = getTestBoard(),
  getBoardAfterMove(Board, 2, [simple, {6, 3}, {5, 4}]).

testGetBoardAfterJumpMove() ->
  Board = getTestBoard(),
  getBoardAfterMove(Board, 2, [jump, {6, 3}, {5, 2}, {4, 1}, {3, 2}, {2, 3}, {1, 4}, {0, 5}]).

%% ------ GAME TREE ------

-record(node, {
  children = [],
  value = 0
}).

isLeaf(#node{children = Children, value = _}) -> if
                                                   Children == [] -> true;
                                                   true -> false
                                                 end.

addChild(#node{children = Children, value = Val}, Child) -> #node{children = Children ++ [Child], value = Val}.

initTree(Board) -> #node{children = [], value = Board}.

evaluateGlobal(Board) -> % Who: Integer
  BoardToList = maps:to_list(Board),
  FilteredBoardWhites = [element(2, Tup) || Tup <- BoardToList, element(2, Tup) == 1],
  WhitesLength = length(FilteredBoardWhites),
  FilteredBoardBlacks = [element(2, Tup) || Tup <- BoardToList, element(2, Tup) == 2],
  BlacksLength = length(FilteredBoardBlacks),
  BlacksLength - WhitesLength.

generateGameTree(Board, _, 0) -> #node{children = [], value = Board};
generateGameTree(Board, WhoseMove, Depth) ->
  PossibleBoards = getAllPossibleBoards(Board, WhoseMove),
  DeeperTrees = [generateGameTree(PossibleBoard, invertWhoseMove(WhoseMove), Depth-1) || PossibleBoard <- PossibleBoards],
  #node{children = DeeperTrees, value = Board}.


minimax(#node{children = [], value = Board}, _, _) -> evaluateGlobal(Board);
minimax(#node{children = _, value = Board}, 0, _) -> evaluateGlobal(Board);
minimax(Node, Depth, true) ->
%%  BestValue = -1000, %-Inf
  ChildValues = [minimax(Child, Depth - 1, false) || Child <- Node#node.children],
  BestValue = lists:max(ChildValues),
  BestValue;

minimax(Node, Depth, false) ->
%%  BestValue = -1000, %-Inf
  ChildValues = [minimax(Child, Depth - 1, true) || Child <- Node#node.children],
  BestValue = lists:min(ChildValues),
  BestValue.

minimaxOuter(Node, Depth, true) ->
  ChildrenMinimax = [{minimax(Child, Depth - 1, false), Child} || Child <- Node#node.children],
  BestValue = maxByFirstKey(ChildrenMinimax),
  BestValue;

minimaxOuter(Node, Depth, false) ->
  ChildrenMinimax = [{minimax(Child, Depth - 1, true), Child} || Child <- Node#node.children],
  BestValue = minByFirstKey(ChildrenMinimax),
  BestValue.



isMaximizingPlayer(WhoseMove) -> if WhoseMove == 2 -> true; WhoseMove == 1 -> false end.

%%getBestNextBoard(Board, WhoseMove) ->
%%  GameTree = generateGameTree(Board, WhoseMove, 3),
%%  BestValue = minimax(GameTree, 3, isMaximizingPlayer(WhoseMove)),
%%  FilteredBoard = [element(2, Node#node.value) || Node <- GameTree#node.children, element(1, Node#node.value) == BestValue],
%%  lists:nth(1, FilteredBoard).

getBestNextBoard(Board, WhoseMove) ->
  GameTree = generateGameTree(Board, WhoseMove, 3), % Starting depth
  #node{children = _, value = BestValue} = element(2, minimaxOuter(GameTree, 3, isMaximizingPlayer(WhoseMove))),
  FinalBoard = promoteToKing(BestValue),
  FinalBoard.



%% ------- MAINER ------
mainer() ->
%%  Result = getAllPossibleBoards(getTestBoard(), 2),
%%  Result = generateGameTree(getTestBoard(), 2, 3),
%%  Res = minimax(Result, 3, true),
  gameScenario().
%%  Res = getBestNextBoard(getTestBoard(), 2),
%%  printBoards(Res).

gameScenario() -> gameScenario(40, getStartingBoard(), 2).

gameScenario(0, _, _) -> ok;
gameScenario(NumOfMoves, Board, WhoseMove) ->
  printBoard(Board),
  io:format("~n", []),
  NextBoard = getBestNextBoard(Board, WhoseMove),
  gameScenario(NumOfMoves - 1, NextBoard, invertWhoseMove(WhoseMove)).

gameScenarioBoards() -> gameScenarioBoards(70, getStartingBoard(), 2).
gameScenarioBoards(0, _, _) -> [];
gameScenarioBoards(NumOfMoves, Board, WhoseMove) ->
  NextBoard = getBestNextBoard(Board, WhoseMove),
  [NextBoard] ++ gameScenarioBoards(NumOfMoves - 1, NextBoard, invertWhoseMove(WhoseMove)).

listFromSpaceSeparatedString(Str) -> [begin {Int,_}=string:to_integer(Token), Int end|| Token<-string:tokens(Str," ")].
boardMapFromList(Lista) -> maps:from_list(lists:zip(lists:seq(0, length(Lista) - 1), Lista)).
