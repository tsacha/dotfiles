package.path = os.getenv('HOME') .. "/.config/nvim/?.lua;" .. package.path
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  "sainnhe/gruvbox-material",
  "lewis6991/gitsigns.nvim",
  "tpope/vim-fugitive",
  "rcarriga/nvim-notify",
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { 'nvim-lua/plenary.nvim' }
  },
  { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
  },
  "max397574/better-escape.nvim",
  "nanotee/zoxide.vim",
  "ggandor/leap.nvim",
  "williamboman/mason.nvim",
  "williamboman/mason-lspconfig.nvim",
  "nvim-neotest/nvim-nio",
  "mfussenegger/nvim-dap",
  "jay-babu/mason-nvim-dap.nvim",
  "rcarriga/nvim-dap-ui",
  "neovim/nvim-lspconfig",
  "nvim-treesitter/nvim-treesitter",
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' }
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
  },
  {
    'mrcjkb/rustaceanvim',
    version = '^4', -- Recommended
    lazy = false, -- This plugin is already lazy
  },
  {
    "pmizio/typescript-tools.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {},
  },
  "hrsh7th/cmp-nvim-lsp",
  "hrsh7th/nvim-cmp",
  "dcampos/nvim-snippy",
  "dcampos/cmp-snippy",
  {
    "johmsalas/text-case.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    config = function()
      require("textcase").setup({})
      require("telescope").load_extension("textcase")
    end,
    keys = {
      "ga", -- Default invocation prefix
      { "ga.", "<cmd>TextCaseOpenTelescope<CR>", mode = { "n", "v" }, desc = "Telescope" },
    },
  },
  "nmac427/guess-indent.nvim",
  {
    "gbprod/yanky.nvim",
    opts = {},
  },
  { "rose-pine/neovim", name = "rose-pine" }
})

-- Base settings
vim.opt.autochdir = true
vim.wo.relativenumber = true
vim.o.termguicolors = true

---- Remove trailing whitespaces
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
    pattern = {"*"},
    callback = function(ev)
        save_cursor = vim.fn.getpos(".")
        vim.cmd([[%s/\s\+$//e]])
        vim.fn.setpos(".", save_cursor)
    end,
})

require("toggle-dark-mode")

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
map('n', '<leader>tt', ':tabnew<CR>')

-- Yanky
vim.keymap.set({"n","x"}, "y", "<Plug>(YankyYank)")

-- Line
require('lualine').setup {}

-- Git
require('gitsigns').setup()
vim.keymap.set('n', '<leader>gg', ":Git<CR>", {})

-- Autocompletion
local cmp = require('cmp')
cmp.setup {
  snippet = {
    expand = function(args)
      require('snippy').expand_snippet(args.body) -- For `snippy` users.
    end
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
  }),
  sources = {
    { name = 'nvim_lsp' },
    { name = 'snippy' }
  }
}
local capabilities = require('cmp_nvim_lsp').default_capabilities()

--- LSP
--vim.lsp.inlay_hint.enable()

require("mason").setup()
require("mason-lspconfig").setup {
    ensure_installed = {
      "gopls",
      "terraformls",
      "ansiblels",
      "bufls",
      "tsserver",
      "svelte",
      "yamlls",
      "basedpyright",
      "ruff",
      "ruff_lsp",
      "rust_analyzer"
    }
}

local lsp = require("lspconfig")

---- YAML
lsp.yamlls.setup {
  settings = {
    yaml = {
      validate = true,
      -- disable the schema store
      schemaStore = {
        enable = false,
        url = "",
      },
      -- manually select schemas
      schemas = {
        ['https://json.schemastore.org/kustomization.json'] = 'kustomization.{yml,yaml}',
      }
    }
  }
}

---- Debuggers
require("debuggers")

---- Golang
require("go")
lsp.gopls.setup({capabilities = capabilities})

---- Rust
vim.g.rustaceanvim = {
  -- Plugin configuration
  tools = {},
  -- LSP configuration
  server = {
    on_attach = function(client, bufnr)
      -- you can also put keymaps in here
    end,
    default_settings = {
      -- rust-analyzer language server configuration
      ['rust-analyzer'] = {
      },
    },
  },
  -- DAP configuration
  dap = {},
}

---- Terraform
lsp.terraformls.setup{}

--- Web things
lsp.svelte.setup{}
lsp.bufls.setup{}

---- Python
require('lspconfig').ruff_lsp.setup {}
require('lspconfig').basedpyright.setup {
  settings = {
    basedpyright = {
      analysis = {
        typeCheckingMode = "basic"
      }
    },
    python = {
      analysis = {
        -- Ignore all files for analysis to exclusively use Ruff for linting
        ignore = { '*' },
      },
    },
  },
}

--- Manual indent ---
local g = vim.g
local o = vim.o
local opt = vim.opt

opt.tabstop = 4
opt.smartindent = true
opt.shiftwidth = 4
opt.expandtab = true

--- Auto indent
require('guess-indent').setup {}
vim.api.nvim_create_autocmd({"BufWritePre"}, {
  pattern = {"*.tf", "*.tfvars", "*.ts", "*.svelte", "*.py"},
  callback = function()
    vim.lsp.buf.format()
  end,
})

-- Golang
require('go')


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
      hijack_netrw = true,
      mappings = {},
      follow_symlinks = true,
    },
    fzf = {
      fuzzy = false,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
    }
  }
}

require("telescope").load_extension("fzf")
require("telescope").load_extension("file_browser")
require("telescope").load_extension("yank_history")

local telescope = require('telescope.builtin')
local extensions = require('telescope').extensions

vim.keymap.set('n', '<leader>fg', telescope.git_files, {})
vim.keymap.set('n', '<leader>ff', extensions.file_browser.file_browser, {})
vim.keymap.set('n', '<leader>fb', telescope.buffers, {})
vim.keymap.set('n', '<leader>m', telescope.marks, {})
vim.keymap.set('n', '<leader>y', extensions.yank_history.yank_history, {})
vim.keymap.set('n', '<leader>fs', telescope.current_buffer_fuzzy_find, {})
function live_grep_git_dir()
  local git_dir = vim.fn.system(string.format("git -C %s rev-parse --show-toplevel", vim.fn.expand("%:p:h")))
  git_dir = string.gsub(git_dir, "\n", "") -- remove newline character from git_dir
  local opts = {
    cwd = git_dir,
  }
  require('telescope.builtin').live_grep(opts)
end
vim.keymap.set('n', '<leader>fS', ":lua live_grep_git_dir()<CR>", {})

vim.keymap.set('n', '<leader>ll', telescope.lsp_document_symbols, {})
vim.keymap.set('n', '<leader>ld', telescope.lsp_definitions, {})
vim.keymap.set('n', '<leader>lr', telescope.lsp_references, {})
vim.keymap.set('n', '<leader>lR', ":LspRestart<CR>", {})
vim.keymap.set('n', '<leader>li', telescope.lsp_incoming_calls, {})
vim.keymap.set('n', '<leader>lo', telescope.lsp_outgoing_calls, {})

--- Better escape
require("better_escape").setup()

-- Leap
require("leap").add_default_mappings()
