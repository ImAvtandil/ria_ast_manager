{application, ria_ast_manager, [
	{description, "New project"},
	{vsn, "0.0.1"},
	{modules, ['ria_ast_manager_app','ria_ast_manager_sup']},
	{registered, [ria_ast_manager_sup]},
	{applications, [kernel,stdlib]},
	{mod, {ria_ast_manager_app, []}}
]}.