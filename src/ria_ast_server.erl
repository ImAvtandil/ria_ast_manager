%%%-------------------------------------------------------------------
%%% @author sanr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. авг 2014 16:42
%%%-------------------------------------------------------------------
-module(ria_ast_server).
-author("sanr").

%% API
-export([start_link/4]).

start_link(Host, Port, Name, Secret) ->
  Result = ria_ast_manager:start_link(Host, Port),
  case Result of
    {ok, _} ->
      ria_ast_manager:login(Name, Secret);
    _       -> ok
  end,
  Result.
%%       gen_server:cast(ria_ast_manager, {set_function,fun(Message)->event_processor:handle_event(Message) end});
