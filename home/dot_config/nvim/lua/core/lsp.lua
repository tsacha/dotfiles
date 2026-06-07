vim.lsp.enable({
	"basedpyright",
	"gopls",
	"helm_ls",
	"lua_ls",
	"rust_analyzer",
	"tofu_ls",
	"yamlls",
	"zls",
})

vim.lsp.config("rust_analyzer", {
	cmd = { "rust-analyzer" },
	filetypes = { "rust" },
	root_markers = { "Cargo.toml", "rust-project.json", ".git" },
	settings = {
		["rust-analyzer"] = {
			cargo = {
				allFeatures = true,
			},
			check = {
				command = "clippy",
			},
		},
	},
})

vim.lsp.config("zls", {
	cmd = { "zls" },
	filetypes = { "zig", "zon" },
	root_markers = { "build.zig", ".git" },
	settings = {
		zls = {
			enable_build_on_save = true,
		},
	},
})

vim.api.nvim_create_autocmd("LspAttach", {
	desc = "LSP actions",
	callback = function(event)
		local opts = { buffer = event.buf }
		local telescope = require("telescope.builtin")

		vim.keymap.set("n", "K", "<cmd>lua vim.lsp.buf.hover()<cr>", opts)

		vim.keymap.set("n", "<leader>ll", telescope.lsp_document_symbols, {})
		vim.keymap.set("n", "<leader>ld", telescope.lsp_definitions, {})
		vim.keymap.set("n", "<leader>lr", telescope.lsp_references, {})
		vim.keymap.set("n", "<leader>li", telescope.lsp_incoming_calls, {})
		vim.keymap.set("n", "<leader>lo", telescope.lsp_outgoing_calls, {})
		vim.keymap.set("n", "<leader>lm", telescope.lsp_implementations, {})

		vim.keymap.set("n", "<F2>", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
		vim.keymap.set({ "n", "x" }, "<F3>", "<cmd>lua vim.lsp.buf.format({async = true})<cr>", opts)
		vim.keymap.set("n", "<F4>", "<cmd>lua vim.lsp.buf.code_action()<cr>", opts)
	end,
})

vim.diagnostic.config({
	virtual_text = false,
	virtual_lines = false,
	signs = true,
	float = { source = true },
})

vim.lsp.config("tofu_ls", {
	cmd = { "tofu-ls", "serve" },
	filetypes = { "terraform", "terraform-vars" },
	root_markers = { ".terraform", ".git" },
	settings = {
		validate = false,
		format = { enable = true },
		completion = true,
		hover = true,
	},
})
