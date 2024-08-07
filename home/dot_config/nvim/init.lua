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

---
require('base')

---
require("toggle-dark-mode")

--- Better escape
require("better_escape").setup()

-- Leap
require("leap").add_default_mappings()

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
map('n', '<leader>h', ':split<CR>')
map('n', '<leader>v', ':vsplit<CR>')
map('n', '<leader>o', '<C-w>w')

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
lsp.gopls.setup({capabilities = capabilities})

---- Debuggers
require("debuggers")

---- Golang
require("go-config")

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

--- Auto indent
require('guess-indent').setup {}
vim.api.nvim_create_autocmd({"BufWritePre"}, {
  pattern = {"*.tf", "*.tfvars", "*.ts", "*.svelte", "*.py"},
  callback = function()
    vim.lsp.buf.format()
  end,
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


--- Telescope
require('telescope-config')

