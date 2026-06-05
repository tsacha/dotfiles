local M = {}

M.plugin = { src = "https://github.com/deparr/tairiki.nvim" }

function M.apply()
	vim.o.termguicolors = true
	require("tairiki").setup({
		palette = vim.o.background == "light" and "light" or "dark",
		transparent = true,
		plugins = {
			gitsigns = true,
			telescope = true,
			treesitter = true,
			semantic_tokens = true,
		},
	})
	vim.cmd.colorscheme("tairiki")
end

return M
