return {
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		main = "nvim-treesitter.configs",
		opts = function(_plugin, opts)
			local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
			parser_config.gleam = {
				install_info = {
					url = "https://github.com/gleam-lang/tree-sitter-gleam",
					revision = "main",
					files = { "src/parser.c", "src/scanner.c" },
				},
				filetype = "gleam",
			}

			return vim.tbl_deep_extend("force", opts, {
				auto_install = true,
				highlight = { enable = true },
				indent = { enable = false },
				incremental_selection = {
					enable = true,
					keymaps = {
						init_selection = "<A-o>",
						node_incremental = "<A-o>",
						scope_incremental = "<A-O>",
						node_decremental = "<A-i>",
					},
				},
			})
		end,
	},
}
