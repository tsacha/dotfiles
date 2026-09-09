vim.lsp.enable({
	"basedpyright",
	"gopls",
	"helm_ls",
	"lua_ls",
	"rust_analyzer",
	"tinymist",
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
	desc = "LSP shortcuts",
	callback = function(event)
		local telescope = require("telescope.builtin")

		local function map(key, rhs, desc, mode)
			vim.keymap.set(mode or "n", "<leader>l" .. key, rhs, { buffer = event.buf, desc = "LSP: " .. desc })
		end

		-- Keep native key mnemonics under <leader>l; use Telescope for lists.
		map("r", telescope.lsp_references, "References (Telescope)")
		map("i", telescope.lsp_implementations, "Implementations (Telescope)")
		map("t", telescope.lsp_type_definitions, "Type definitions (Telescope)")
		map("O", telescope.lsp_document_symbols, "Document symbols (Telescope)")
		map("d", telescope.lsp_definitions, "Definitions (Telescope)")
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, {
			buffer = event.buf,
			desc = "LSP: Go to definition",
		})
		map("n", vim.lsp.buf.rename, "Rename")
		map("a", vim.lsp.buf.code_action, "Code action", { "n", "x" })

		map("c", telescope.lsp_incoming_calls, "Incoming calls (Telescope)")
		map("C", telescope.lsp_outgoing_calls, "Outgoing calls (Telescope)")
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
