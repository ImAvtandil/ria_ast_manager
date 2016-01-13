-module(ria_ast_manager_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, AmiPort} = application:get_env(ria_ast_manager,ami_port),
	{ok, AmiHost} = application:get_env(ria_ast_manager,ami_host),
	{ok, AmiName} = application:get_env(ria_ast_manager,ami_name),
	{ok, AmiPasswd} = application:get_env(ria_ast_manager,ami_secret),
	Procs = [
		{ria_ast_server,
			{ria_ast_server, start_link, [AmiHost, AmiPort, AmiName, AmiPasswd]},
			permanent,
			infinity,
			worker,
			[ria_ast_server]
		}
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.
