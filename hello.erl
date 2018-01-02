-module(hello).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([get/3]).
-export([terminate/4]).

init(_Route, _Req, State) ->
    {ok, State}.

get("/", _Req, State) ->
    {<<"Hello, leptus!">>, State};
get("/hi/:name", Req, State) ->
    Status = ok,
    Name = leptus_req:param(Req, name),
    Body = [{<<"say">>, <<"Hi">>}, {<<"to">>, Name}],
    {Status, {json, Body}, State}.

terminate(_Reason, _Route, _Req, _State) ->
    ok.
