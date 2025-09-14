-- Base settings
vim.opt.autochdir = false
vim.o.termguicolors = true

---- Remove trailing whitespaces
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
    pattern = { "*" },
    callback = function()
        local save_cursor = vim.fn.getpos(".")
        vim.cmd([[%s/\s\+$//e]])
        vim.fn.setpos(".", save_cursor)
    end,
})

--- Keep cursor position after yank
local keymap_set = vim.keymap.set
local win_get_cursor = vim.api.nvim_win_get_cursor
local win_set_cursor = vim.api.nvim_win_set_cursor
local create_autocmd = vim.api.nvim_create_autocmd

keymap_set({ "n", "x" }, "y", function()
    vim.b.cursor_pre_yank = win_get_cursor(0)
    return "y"
end, { expr = true })

keymap_set("n", "Y", function()
    vim.b.cursor_pre_yank = win_get_cursor(0)
    return "y$"
end, { expr = true })

create_autocmd("TextYankPost", {
    callback = function()
        if vim.v.event.operator == "y" and vim.b.cursor_pre_yank then
            win_set_cursor(0, vim.b.cursor_pre_yank)
            vim.b.cursor_pre_yank = nil
        end
    end,
})

-- Relative numbers
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.tabstop = 4
vim.opt.smartindent = true
vim.opt.shiftwidth = 4
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.mouse = "a"
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.signcolumn = "yes"
vim.opt.updatetime = 250
vim.opt.timeoutlen = 1000
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
vim.opt.inccommand = "split"
vim.opt.cursorline = true
vim.opt.scrolloff = 10
