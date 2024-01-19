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
  "mfussenegger/nvim-dap",
  "jay-babu/mason-nvim-dap.nvim",
  "rcarriga/nvim-dap-ui",
  "neovim/nvim-lspconfig",
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
  "hrsh7th/cmp-nvim-lsp",
  "hrsh7th/nvim-cmp",
  "dcampos/nvim-snippy",
  "dcampos/cmp-snippy",
})

-- Base settings
vim.opt.autochdir = true
vim.wo.relativenumber = true
vim.o.termguicolors = true
if vim.g.neovide then
local keymapopts = {
  silent = true,
  noremap = true
}
  vim.keymap.set({"n", "v"}, "<S-Insert>", "\"*p", keymapOpts)
  vim.keymap.set({"n", "v"}, "<D-v>", "\"*p", keymapOpts)
  vim.keymap.set({"n", "v"}, "<D-c>", "\"*y", keymapOpts)
  vim.keymap.set({"n", "v"}, "<D-x>", "\"*x", keymapOpts)
  vim.g.neovide_cursor_animation_length = 0
  vim.g.neovide_scroll_animation_length = 0
  vim.o.guifont = "Iosevka Nerd Font Mono:h16"
end

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
vim.keymap.set('n', '<leader>gg', ":Git<CR>", {})

-- Guess-indent
require('guess-indent').setup()

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
require("mason").setup()
require("mason-lspconfig").setup {
    ensure_installed = { "gopls", "terraformls", "ansiblels" }
}

local lsp = require("lspconfig")

---- Golang
lsp.gopls.setup({capabilities = capabilities})

---- Terraform
lsp.terraformls.setup{}
vim.api.nvim_create_autocmd({"BufWritePre"}, {
  pattern = {"*.tf", "*.tfvars"},
  callback = function()
    vim.lsp.buf.format()
  end,
})

-- Debuggers
require("mason-nvim-dap").setup({
    ensure_installed = { "delve" }
})

require("dapui").setup({
  icons = { expanded = "▾", collapsed = "▸" },
  mappings = {
    open = "o",
    remove = "d",
    edit = "e",
    repl = "r",
    toggle = "t",
  },
  expand_lines = vim.fn.has("nvim-0.7"),
  layouts = {
    {
      elements = {
        "scopes",
      },
      size = 0.3,
      position = "right"
    },
    {
      elements = {
        "repl",
        "breakpoints"
      },
      size = 0.3,
      position = "bottom",
    },
  },
  floating = {
    max_height = nil,
    max_width = nil,
    border = "single",
    mappings = {
      close = { "q", "<Esc>" },
    },
  },
  windows = { indent = 1 },
  render = {
    max_type_length = nil,
  },
})

local dap_ok, dap = pcall(require, "dap")
local dap_ui_ok, ui = pcall(require, "dapui")

if not (dap_ok and dap_ui_ok) then
  require("notify")("nvim-dap or dap-ui not installed!", "warning") -- nvim-notify is a separate plugin, I recommend it too!
  return
end

vim.fn.sign_define('DapBreakpoint', { text = '🐞' })

-- Start debugging session
vim.keymap.set("n", "<leader>ds", function()
  dap.continue()
  ui.toggle({})
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-w>=", false, true, true), "n", false) -- Spaces buffers evenly
end)

-- Set breakpoints, get variable values, step into/out of functions, etc.
vim.keymap.set("n", "<leader>dl", require("dap.ui.widgets").hover)
vim.keymap.set("n", "<leader>dc", dap.continue)
vim.keymap.set("n", "<leader>db", dap.toggle_breakpoint)
vim.keymap.set("n", "<leader>dn", dap.step_over)
vim.keymap.set("n", "<leader>di", dap.step_into)
vim.keymap.set("n", "<leader>do", dap.step_out)
vim.keymap.set("n", "<leader>dC", function()
  dap.clear_breakpoints()
  require("notify")("Breakpoints cleared", "warn")
end)

-- Close debugger and clear breakpoints
vim.keymap.set("n", "<leader>dq", function()
  dap.clear_breakpoints()
  ui.toggle({})
  dap.terminate()
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-w>=", false, true, true), "n", false)
  require("notify")("Debugger session ended", "warn")
end)

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

local dap_ok, dap = pcall(require, "dap")
if not (dap_ok) then
  print("nvim-dap not installed!")
  return
end

require('dap').set_log_level('INFO') -- Helps when configuring DAP, see logs with :DapShowLog

dap.configurations = {
    go = {
      {
        type = "go", -- Which adapter to use
        name = "Debug", -- Human readable name
        request = "launch", -- Whether to "launch" or "attach" to program
        program = "${file}", -- The buffer you are focused on when running nvim-dap
      },
    }
}
dap.adapters.go = {
  type = "server",
  port = "${port}",
  executable = {
    command = vim.fn.stdpath("data") .. '/mason/bin/dlv',
    args = { "dap", "-l", "127.0.0.1:${port}" },
  },
}

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
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
    }
  }
}

require("telescope").load_extension("fzf")
require("telescope").load_extension("file_browser")

local telescope = require('telescope.builtin')
local extensions = require('telescope').extensions

vim.keymap.set('n', '<leader>fg', telescope.git_files, {})
vim.keymap.set('n', '<leader>fG', telescope.git_status, {})
vim.keymap.set('n', '<leader>fF', telescope.find_files, {})
vim.keymap.set('n', '<leader>ff', extensions.file_browser.file_browser, {})
vim.keymap.set('n', '<leader>fb', telescope.buffers, {})
vim.keymap.set('n', '<leader>y', telescope.registers, {})
vim.keymap.set('n', '<leader>m', telescope.marks, {})
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
vim.keymap.set('n', '<leader>li', telescope.lsp_incoming_calls, {})
vim.keymap.set('n', '<leader>lo', telescope.lsp_outgoing_calls, {})
--- Better escape
require("better_escape").setup()

-- Leap & Bepo
require("leap").add_default_mappings()
vim.keymap.set({'n', 'x', 'o'}, 'k', '<Plug>(leap-forward-to)')
vim.keymap.set({'n', 'x', 'o'}, 'K', '<Plug>(leap-backward-to)')
vim.keymap.set({'n', 'x', 'o'}, 'gk', '<Plug>(leap-from-window)')
