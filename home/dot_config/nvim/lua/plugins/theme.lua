return {
	{
		"rose-pine/neovim",
		name = "rose-pine",
		config = function()
			vim.opt.termguicolors = true
			vim.cmd("colorscheme rose-pine")
		end,
	},
}
