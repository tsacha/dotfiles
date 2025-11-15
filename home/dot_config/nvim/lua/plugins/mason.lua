return {
	{
		"mason-org/mason.nvim",
		dependencies = {
			{ "mason-org/mason-lspconfig.nvim" },
			{ "WhoIsSethDaniel/mason-tool-installer.nvim" },
		},
		lazy = false,
		opts = {},
		config = function()
			require("mason").setup()
			require("mason-lspconfig").setup({
				ensure_installed = {
					"basedpyright",
					"clangd",
					"gopls",
					"helm_ls",
					"lua_ls",
					"ruff",
					"stylua",
					"tofu_ls",
					"yamlls",
				},
			})
			require("mason-tool-installer").setup({
				ensure_installed = {
					"yamlfmt",
				},
			})
		end,
	},
}
