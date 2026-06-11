local M = {}

M.plugin = { src = "https://github.com/shatur/neovim-ayu" }

function M.apply()
	vim.o.termguicolors = true
	require("ayu").setup({
		mirage = true,
	})

	vim.cmd.colorscheme("ayu")
end

return M
