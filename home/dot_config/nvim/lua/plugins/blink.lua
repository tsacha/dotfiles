return {
	"saghen/blink.cmp",
	version = "*",
	lazy = false,
	opts = {
		keymap = { preset = "super-tab" },
		signature = { enabled = true },
		sources = {
			default = { "lsp", "path", "snippets" },
		},
	},
}
-- vim: ts=2 sts=2 sw=2 et
