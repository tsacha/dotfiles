require("blink.cmp").setup({
	keymap = { preset = "super-tab" },
	signature = { enabled = true },
	sources = {
		default = { "lsp", "path", "snippets" },
	},
	completion = {
		documentation = { auto_show = true, auto_show_delay_ms = 500 },
		menu = {
			auto_show = true,
			draw = {
				treesitter = { "lsp" },
				columns = { { "kind_icon", "label", "label_description", gap = 1 }, { "kind" } },
			},
		},
	},
	fuzzy = {
		implementation = "lua",
	},
})
vim.lsp.config("*", {
	capabilities = require("blink.cmp").get_lsp_capabilities({}),
	root_markers = { ".git" },
})
