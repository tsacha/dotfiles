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

-- Relative numbers
vim.wo.relativenumber = true
vim.wo.number = true

--- Manual indent ---
local g = vim.g
local o = vim.o
local opt = vim.opt

opt.tabstop = 4
opt.smartindent = true
opt.shiftwidth = 4
opt.expandtab = true
