vim.pack.add({
	{ src = "https://github.com/rose-pine/neovim" },

	{ src = "https://github.com/neovim/nvim-lspconfig" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter" },

	{ src = "https://github.com/saghen/blink.cmp", branch = "1.*" },
})

vim.cmd("colorscheme rose-pine")

--- Core configuration
require("core.keymap")
require("core.options")
require("core.lsp")

--- Major plugins
require("plugins.blink")
require("plugins.treesitter")
require("plugins.conform")
require("plugins.telescope")

--- Minor plugins
require("plugins.gitsigns")
require("plugins.neoscroll")

--- Languages plugins
require("plugins.helm")
