require("kubectl").setup()
vim.keymap.set(
	"n",
	"<leader>k",
	'<cmd>lua require("kubectl").toggle({ tab = false })<cr>',
	{ noremap = true, silent = true }
)
