-module(request_handler).
-behavior(cowboy_handler).

-import(routes, [get_dest/1]).

-export([init/2]).

init(Req=#{path := <<Path/binary>>, method := <<Method/binary>>, headers := Headers}, State) ->
  {ok, _, Req1} = cowboy_req:read_body(Req, #{length => 1000000, period => 1000}),
  {Resp, DestIp, DestPort} = get_dest(Path),
  case Resp of
    ok ->
      Dest = build_host(DestIp, DestPort, Path),
      HeaderStrMap = parse_headers(maps:to_list(Headers)),
      {ok,{{_, AppResponseStatus, _}, _, AppResponseBody}} = httpc:request(http_method_to_atom(Method), {Dest, HeaderStrMap}, [], []),
      CliResponse = cowboy_req:reply(AppResponseStatus, #{<<"content-type">> => <<"text/plain">>}, AppResponseBody, Req1),
      {ok, CliResponse, State};
    _ ->
      CliResponse = cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, "404 - router could not find destination route", Req1),
      {ok, CliResponse, State}
  end.

build_host(Ip, Port, Path) -> ["http://"|[Ip|[":"|[Port|Path]]]].

parse_headers([]) -> [];
parse_headers([{K,V}|T]) ->
  [{binary_to_list(K), binary_to_list(V)}|parse_headers(T)].


http_method_to_atom(Method) ->
  maps:get(Method, #{<<"GET">> => get, <<"POST">> => post, <<"PUT">> => put, <<"DELETE">> => delete,
    <<"PATCH">> => patch, <<"HEAD">> => head, <<"OPTIONS">> => options, <<"TRACE">> => trace}).
