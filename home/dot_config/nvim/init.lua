vim.pack.add({
	{ src = "https://github.com/rose-pine/neovim" },

	{ src = "https://github.com/neovim/nvim-lspconfig" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter" },

	{ src = "https://github.com/saghen/blink.cmp" },
	{ src = "https://github.com/stevearc/conform.nvim" },
	{ src = "https://github.com/nvim-lua/plenary.nvim" },

	{ src = "https://github.com/nvim-telescope/telescope-fzf-native.nvim", run = "make" },
	{ src = "https://github.com/nvim-telescope/telescope.nvim" },
	{ src = "https://github.com/nvim-telescope/telescope-file-browser.nvim", run = "make" },
	{ src = "https://github.com/saghen/blink.download" },
})

vim.cmd("colorscheme rose-pine")

require("vim._core.ui2").enable({})

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
require("plugins.guess-indent")

--- Languages plugins
require("plugins.helm")
