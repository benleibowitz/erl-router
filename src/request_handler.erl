-module(request_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req=#{path := <<Path/binary>>}, State) ->
  {ok, _, Req1} = cowboy_req:read_body(Req, #{length => 1000000, period => 1000}),
  {ok,{{_, AppResponseStatus, _}, _, AppResponseBody}} = httpc:request(["http://localhost:4567"|Path]),
  CliResponse = cowboy_req:reply(AppResponseStatus, #{<<"content-type">> => <<"text/plain">>}, AppResponseBody, Req1),
  {ok, CliResponse, State}.