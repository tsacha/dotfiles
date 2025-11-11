return {
	"stevearc/conform.nvim",
	event = { "BufReadPre", "BufNewFile" },
	cmd = { "ConformInfo" },
	opts = {
		notify_on_error = false,
		format_on_save = function(bufnr)
			-- Disable with a global or buffer-local variable
			if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
				return
			end
			return { timeout_ms = 500, lsp_fallback = true }
		end,
		formatters = {
			yamlfmt = {
				inherit = false,
				command = "yamlfmt",
				args = {
					"-formatter",
					"include_document_start=false",
					"-",
				},
			},
		},
		formatters_by_ft = {
			python = { "ruff_format" },
			go = { "gofmt" },
			lua = { "stylua" },
			fish = { "fish_indent" },
			helm = { "yamlfmt" },
			yaml = { "yamlfmt" },
		},
	},
	config = function(_, opts)
		require("conform").setup(opts)

		vim.api.nvim_create_user_command("FormatDisable", function(args)
			if args.bang then
				-- FormatDisable! will disable formatting just for this buffer
				vim.b.disable_autoformat = true
			else
				vim.g.disable_autoformat = true
			end
		end, {
			desc = "Disable autoformat-on-save",
			bang = true,
		})

		vim.api.nvim_create_user_command("FormatEnable", function()
			vim.b.disable_autoformat = false
			vim.g.disable_autoformat = false
		end, {
			desc = "Re-enable autoformat-on-save",
		})
	end,
}
-- vim: ts=2 sts=2 sw=2 et
