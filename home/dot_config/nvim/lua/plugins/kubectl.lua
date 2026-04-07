vim.pack.add({
	{ src = "https://github.com/tsacha/kubectl.nvim", version = "helm-4-compatibility" },
})
require("kubectl").setup()
vim.keymap.set(
	"n",
	"<leader>k",
	'<cmd>lua require("kubectl").toggle({ tab = false })<cr>',
	{ noremap = true, silent = true }
)
