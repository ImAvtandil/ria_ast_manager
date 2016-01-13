{application, ria_ast_manager, [
	{description, "New project"},
	{vsn, "0.0.1"},
	{modules, ['ast_parser','event_processor','ria_ast_manager','ria_ast_manager_app','ria_ast_manager_sup','ria_ast_server']},
	{registered, [ria_ast_manager_sup]},
	{applications, [kernel,stdlib]},
	{mod, {ria_ast_manager_app, []}}
]}.