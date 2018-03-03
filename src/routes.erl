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

-export([get_dest/1]).

%% Define routes here
get_dest(<<"/a">>) ->
  {ok, "127.0.0.1", "35000"};
get_dest(<<"/b">>) ->
  {ok, "127.0.0.1", "35010"};
get_dest(<<"/c">>) ->
  {ok, "127.0.0.1", "35020"};
get_dest(<<"/d">>) ->
  {ok, "127.0.0.1", "35030"};

get_dest(_) ->
  {error, null, null}.