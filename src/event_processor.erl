%%%-------------------------------------------------------------------
%%% @author sanr
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Янв. 2016 12:00
%%%-------------------------------------------------------------------
-module(event_processor).
-author("sanr").

%% API
-export([handle_event/1]).

handle_event(_Event)->
  io:format("Handle event~n"),

  ok.