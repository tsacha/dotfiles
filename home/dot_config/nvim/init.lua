local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
vim.g.coq_settings = {
  auto_start = 'shut-up',
  keymap = {
    pre_select = true
  },
  clients = {
    snippets = {
      warn = {}
    }
  }
}

require("lazy").setup({
  "tsacha/bepo.nvim",
  "sainnhe/gruvbox-material",
  {
      "sontungexpt/sttusline",
      branch = "table_version",
      dependencies = {
          "nvim-tree/nvim-web-devicons",
      },
      event = { "BufEnter" },
      config = function(_, opts)
          require("sttusline").setup()
      end,
  },
  "lewis6991/gitsigns.nvim",
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { 'nvim-lua/plenary.nvim' }
  },
  { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
  },
  "cljoly/telescope-repo.nvim",
  "ahmedkhalf/project.nvim",
  "max397574/better-escape.nvim",
  "nanotee/zoxide.vim",
  "ggandor/leap.nvim",
  "williamboman/mason.nvim",
  "williamboman/mason-lspconfig.nvim",
  "neovim/nvim-lspconfig",
  {"ms-jpq/coq_nvim", branch="coq"},
  "nmac427/guess-indent.nvim",
  "nvim-treesitter/nvim-treesitter",
  {
    "utilyre/barbecue.nvim",
    name = "barbecue",
    version = "*",
    dependencies = {
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons", -- optional dependency
    },
    opts = {},
  },
  {
    "rcarriga/nvim-dap-ui", dependencies = {"mfussenegger/nvim-dap"}
  },
  {
    "ray-x/go.nvim",
    dependencies = {  -- optional packages
      "ray-x/guihua.lua",
      "neovim/nvim-lspconfig",
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      require("go").setup()
    end,
    event = {"CmdlineEnter"},
    ft = {"go", 'gomod'},
    build = ':lua require("go.install").update_all_sync()' -- if you need to install/update all binaries
  }
})

-- Base settings
vim.wo.relativenumber = true

---- Remove trailing whitespaces
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
    pattern = {"*"},
    callback = function(ev)
        save_cursor = vim.fn.getpos(".")
        vim.cmd([[%s/\s\+$//e]])
        vim.fn.setpos(".", save_cursor)
    end,
})

-- Theme - https://felix-kling.de/blog/2021/linux-toggle-dark-mode.html
local colorFile = vim.fn.expand('~/.vimrc.color')
local function reload()
	vim.cmd("source ".. colorFile)
end

local w = vim.loop.new_fs_event()
local on_change
local function watch_file(fname)
	w:start(fname, {}, vim.schedule_wrap(on_change))
end
on_change = function()
	reload()
	-- Debounce: stop/start.
	w:stop()
	watch_file(colorFile)
end

-- reload vim config when background changes
watch_file(colorFile)
reload()

vim.cmd([[colorscheme gruvbox-material]])

vim.g.gruvbox_material_background = 'medium'
vim.opt.termguicolors = true
if os.getenv('theme') == 'light' then
  vim.o.background = 'light'
end

-- Relative numbers
vim.wo.relativenumber = true
vim.wo.number = true

--- Keybinds
local function map(mode, lhs, rhs, opts)
  local options = { noremap=true, silent=true }
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

vim.g.mapleader = ' '
map('n', 'wk', ':close')
map('n', '<leader>s', ':w<CR>')
map('n', '<leader>q', ':q<CR>')
map('n', '<leader>Q', ':qa!<CR>')

-- Bépo
require("bepo").setup()
require("bepo").digits()


-- Git
require('gitsigns').setup()

-- Guess-indent
require('guess-indent').setup()

--- LSP
require("mason").setup()
require("mason-lspconfig").setup {
    ensure_installed = { "gopls", "terraformls", "ansiblels" }
}


local lsp = require("lspconfig")
local coq = require "coq"
lsp.gopls.setup(coq.lsp_ensure_capabilities({}))

-- Terraform
require'lspconfig'.terraformls.setup{}
vim.api.nvim_create_autocmd({"BufWritePre"}, {
  pattern = {"*.tf", "*.tfvars"},
  callback = function()
    vim.lsp.buf.format()
  end,
})

-- Golang
require('go').setup()
-- Run gofmt + goimport on save

local format_sync_grp = vim.api.nvim_create_augroup("GoImport", {})
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*.go",
  callback = function()
   require('go.format').goimport()
  end,
  group = format_sync_grp,
})

-- Treesitter
require'nvim-treesitter.configs'.setup {
  ensure_installed = "all",
  indent = {
    enable = true
  },
  highlight = {
    enable = true
  },
}

--- Projects
require("project_nvim").setup()

--- Telescope
require('telescope').setup {
  defaults = {
    layout_strategy = "horizontal",
    layout_config = {
      horizontal = { width = 0.9 },
    },
    preview = {
      hide_on_startup = false
    }
  },
  extensions = {
    file_browser = {
      theme = "ivy",
      hijack_netrw = false,
      mappings = {},
    },
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
    },
    repo = {
      settings = {
        auto_lcd = true,
      },
      list = {
        search_dirs = {
          "~/Git",
        },
      },
    },
  }
}
require("telescope").load_extension("fzf")
require("telescope").load_extension("repo")
require("telescope").load_extension("file_browser")

local telescope = require('telescope.builtin')
local extensions = require('telescope').extensions

vim.keymap.set('n', '<leader>fr', extensions.repo.list, {})
vim.keymap.set('n', '<leader>fg', telescope.git_files, {})
vim.keymap.set('n', '<leader>ff', telescope.find_files, {})
vim.keymap.set('n', '<leader>fs', telescope.current_buffer_fuzzy_find, {})
vim.keymap.set('n', '<leader>fb', telescope.buffers, {})
vim.keymap.set('n', '<leader>fl', telescope.lsp_document_symbols, {})
vim.keymap.set('n', '<leader>y', telescope.registers, {})

--- Better escape
require("better_escape").setup()

-- Line

-- Leap & Bepo
require("leap").add_default_mappings()
vim.keymap.set({'n', 'x', 'o'}, 'k', '<Plug>(leap-forward-to)')
vim.keymap.set({'n', 'x', 'o'}, 'K', '<Plug>(leap-backward-to)')
vim.keymap.set({'n', 'x', 'o'}, 'gk', '<Plug>(leap-from-window)')
