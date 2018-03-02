-module(request_handler).
-behavior(cowboy_handler).

-import(routes, [get_ip_and_port/1]).

-export([init/2]).

init(Req=#{path := <<Path/binary>>}, State) ->
  {ok, _, Req1} = cowboy_req:read_body(Req, #{length => 1000000, period => 1000}),
  {AppRouteRes, AppIp, AppPort} = get_ip_and_port(Path),
  case AppRouteRes of
    ok ->
      {ok,{{_, AppResponseStatus, _}, _, AppResponseBody}} = httpc:request(["http://"|[AppIp|[":"|[AppPort|Path]]]]),
      CliResponse = cowboy_req:reply(AppResponseStatus, #{<<"content-type">> => <<"text/plain">>}, AppResponseBody, Req1),
      {ok, CliResponse, State};
    _ ->
      CliResponse = cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, "404 - Not Found", Req1),
      {ok, CliResponse, State}
  end.