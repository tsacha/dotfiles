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

	{ src = "https://github.com/lewis6991/gitsigns.nvim" },
	{ src = "https://github.com/karb94/neoscroll.nvim" },

	{ src = "https://github.com/towolf/vim-helm" },

	{ src = "https://github.com/tsacha/kubectl.nvim", version = "helm-4-compatibility" },
	--{ src = "https://github.com/ramilito/kubectl.nvim", version = "main" },
	{ src = "https://github.com/saghen/blink.download" },
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
require("plugins.kubectl")
