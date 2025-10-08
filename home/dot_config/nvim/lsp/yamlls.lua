return {
	cmd = { "yaml-language-server", "--stdio" },
	filetypes = { "yml", "yaml" },
	settings = {
		validate = true,
		format = { enable = true },
		completion = true,
		hover = true,
	},
}
