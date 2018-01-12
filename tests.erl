-module(tests).
-include_lib("eunit/include/eunit.hrl").

finding_moves_for_2_starting_test() -> ?assert(dra:findAllAvailableMoves(dra:getStartingBoard(), 2) == [[simple,{6,3},{5,4}],
  [simple,{6,5},{5,6}],
  [simple,{6,7},{5,8}],
  [simple,{6,9},{5,8}],
  [simple,{6,1},{5,2}]]
).

finding_moves_for_1_starting_test() -> ?assert(dra:findAllAvailableMoves(dra:getStartingBoard(), 1) == [[simple,{3,4},{4,5}],
  [simple,{3,6},{4,7}],
  [simple,{3,2},{4,3}],
  [simple,{3,0},{4,1}],
  [simple,{3,8},{4,9}]]).

finding_moves_for_2_custom_test() -> ?assert(dra:findAllAvailableMoves(dra:getTestBoard(), 2) == [[simple,{8,3},{7,4}],
  [simple,{6,5},{5,6}],
  [simple,{6,7},{5,8}],
  [simple,{8,5},{7,4}],
  [simple,{6,9},{5,8}],
  [simple,{3,5},{2,6}],
  [simple,{7,2},{6,3}],
  [simple,{9,0},{8,1}],
  [king,{6,1},{7,0}],
  [king,{6,1},{5,0}],
  [king,{6,1},{5,2}],
  [king,{6,1},{5,2},{4,3}],
  [king,{6,1},{5,2},{4,3},{7,0}]]
).

finding_moves_for_1_custom_test() -> ?assert(dra:findAllAvailableMoves(dra:getTestBoard(), 1) == [[jump,{0,3},{1,2},{2,1}],
  [king,{4,4},{3,3}],
  [king,{4,4},{3,3},{2,2}],
  [king,{4,4},{3,3},{2,2},{1,1}],
  [king,{4,4},{3,3},{2,2},{1,1},{0,0}],
  [king,{4,4},{5,3}],
  [king,{4,4},{5,3},{6,2}],
  [king,{4,4},{5,3},{6,2},{5,1}],
  [king,{4,4},{5,3},{6,2},{5,1},{4,0}],
  [king,{4,4},{2,6}],
  [king,{4,4},{2,6},{1,7}],
  [king,{4,4},{2,6},{1,7},{0,8}],
  [king,{4,4},{5,5}],
  [king,{4,4},{5,5},{6,6}],
  [king,{4,4},{5,5},{6,6},{3,3}],
  [king,{4,4},{5,5},{6,6},{3,3},{2,2}],
  [king,{4,4},{5,5},{6,6},{3,3},{2,2},{1,1}],
  [king,{4,4},{5,5},{6,6},{3,3},{2,2},{1,1},{0,0}]]).




