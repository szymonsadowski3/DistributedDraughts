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
  {<<"Hello, leptus!">>, State};
get("/board", _Req, State) ->
  Board = dra:getStartingBoard(),
  Body = dra:getOutputBoard(Board),
  {Body, State}.
post("/move/:who", _Req, State) ->
  %UpdatedBoard = getMove(Board, White),
  %IOList = getOutputBoard(Board),
  Who = list_to_integer(binary_to_list(leptus_req:param(_Req, who))),
  Body = leptus_req:body_raw(_Req),
  ListRaw = binary_to_list(Body),
  ParsedList = dra:listFromSpaceSeparatedString(ListRaw),
  ParsedBoard = dra:boardMapFromList(ParsedList),
  NextBestBoard = dra:getBestNextBoard(ParsedBoard, Who),
  Output = list_to_binary(dra:getOutputBoard(NextBestBoard)),
  %NewBoard = dra:getRandomMove(Board, Who),
  {Output, State}.

terminate(_Reason, _Route, _Req, _State) ->
  ok.

server_start() ->
  leptus:start_listener(http, [{'_', [{rest, undefined_state}]}]).
