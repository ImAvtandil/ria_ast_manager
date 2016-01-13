-module(ria_ast_manager_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	ria_ast_manager_sup:start_link().

stop(_State) ->
	ok.
