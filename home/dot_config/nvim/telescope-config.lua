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
