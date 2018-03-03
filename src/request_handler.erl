-module(request_handler).
-behavior(cowboy_handler).

-import(routes, [get_dest/1]).

-export([init/2]).

%% TODO - variable names
init(Req=#{path := <<Path/binary>>, method := <<Method/binary>>, headers := Headers}, State) ->
  {ok, _, Req1} = cowboy_req:read_body(Req, #{length => 1000000, period => 100}),
  {Resp, DestIp, DestPort} = get_dest(Path),
  case Resp of
    ok ->
      Dest = build_host(DestIp, DestPort, Path),
      HeaderStrMap = parse_headers(maps:to_list(Headers)),
      {AppRespStatus, AppResp} = httpc:request(http_method_to_atom(Method), {Dest, HeaderStrMap}, [{timeout, timer:seconds(1)}], []),
      case AppRespStatus of
        ok ->
          {{_, AppRespStatusCode, _}, _, AppRespBody} = AppResp,
          CliResponse = cowboy_req:reply(AppRespStatusCode, #{<<"content-type">> => <<"text/plain">>}, AppRespBody, Req1),
          {ok, CliResponse, State};
        error ->
          io:format("Failed to connect: ~p~n", [AppResp]),
          handle_err(AppResp, Req1, State)
      end;
    _ ->
      CliResponse = cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, "404 - router could not find destination route", Req1),
      {ok, CliResponse, State}
  end.

handle_err(AppResp, Req1, State) ->
  {ErrMsg, _} = AppResp,
  case ErrMsg of
    failed_connect ->
      CliResponse = cowboy_req:reply(502, #{<<"content-type">> => <<"text/plain">>}, "Unable to connect to requested service", Req1),
      {ok, CliResponse, State}
  end.

%% Return hostname URL (http://myhost:port/path?k=v)
build_host(Ip, Port, Path) -> ["http://"|[Ip|[":"|[Port|Path]]]].

%% Convert binary header map to string header map
parse_headers([]) -> [];
parse_headers([{K,V}|T]) ->
  [{binary_to_list(K), binary_to_list(V)}|parse_headers(T)].


http_method_to_atom(Method) ->
  maps:get(Method, #{<<"GET">> => get, <<"POST">> => post, <<"PUT">> => put, <<"DELETE">> => delete,
    <<"PATCH">> => patch, <<"HEAD">> => head, <<"OPTIONS">> => options, <<"TRACE">> => trace}).
