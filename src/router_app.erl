-module(router_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  application:start(inets),
  Dispatch = cowboy_router:compile([{'_', [{'_', request_handler, []}]}]),
  {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
  router_sup:start_link().

stop(_State) ->
  ok.
