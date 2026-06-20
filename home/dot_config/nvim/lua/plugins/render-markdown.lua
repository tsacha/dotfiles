vim.pack.add({ "https://github.com/MeanderingProgrammer/render-markdown.nvim" })
require("render-markdown").setup({
	-- On garde le wrap (pour voir la fin des lignes), mais on neutralise les
	-- options de continuation qui salissent le rendu (showbreak "↪ " et
	-- breakindentopt "list:-1"). Réglage conseillé par la doc du plugin.
	win_options = {
		showbreak = { default = vim.o.showbreak, rendered = "  " },
		breakindent = { default = vim.o.breakindent, rendered = true },
		breakindentopt = { default = vim.o.breakindentopt, rendered = "" },
	},
	-- Force les bordures haut/bas en lignes virtuelles au lieu de réutiliser
	-- les lignes vides adjacentes (qui cassent la bordure quand il n'y en a pas).
	pipe_table = {
		border_virtual = true,
	},
})
vim.keymap.set("n", "<leader>m", "<cmd>RenderMarkdown toggle<cr>", { desc = "Toggle Render Markdown" })
