%%%-------------------------------------------------------------------
%%% @author ben
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Mar 2018 11:31 PM
%%%-------------------------------------------------------------------
-module(routes).
-author("ben").

-export([get_ip_and_port/1]).

%% Define routes here
get_ip_and_port(<<"/foo">>) ->
  {ok, "127.0.0.1", "4567"};

get_ip_and_port(_) ->
  {error, null, null}.