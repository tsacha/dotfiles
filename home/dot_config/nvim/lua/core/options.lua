vim.opt.autochdir = false
vim.o.termguicolors = true

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
vim.opt.scrolloff = 10
vim.opt.conceallevel = 0

-- Wrap
vim.opt.wrap = true
vim.opt.linebreak = true -- coupe sur un mot entier, pas au milieu
vim.opt.breakindent = true -- la ligne wrappée garde l'indentation d'origine
vim.opt.breakindentopt = "list:-1" -- aligne la continuation sous le texte des listes
vim.opt.showbreak = "↪ "
