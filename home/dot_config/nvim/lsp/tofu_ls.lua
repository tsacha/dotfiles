return {
	cmd = { "tofu-ls", "serve" },
	-- Base filetypes
	filetypes = { "terraform", "terraform-vars" },
	root_markers = { ".terraform", ".git" },
	settings = {
		validate = false,
		format = { enable = true },
		completion = true,
		hover = true,
	},
}
