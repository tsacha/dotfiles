vim.pack.add({ "https://github.com/folke/zen-mode.nvim" })
require("zen-mode").setup({
	window = {
		width = 120,
	},
	plugins = {
		tmux = { enabled = true },
	},
})
vim.keymap.set("n", "<leader>z", "<cmd>ZenMode<cr>", { desc = "Toggle Zen Mode" })
