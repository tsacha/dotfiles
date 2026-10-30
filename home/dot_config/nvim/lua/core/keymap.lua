vim.g.mapleader = " "
vim.g.maplocalleader = " "

local function map(mode, lhs, rhs, opts)
	local options = { noremap = true, silent = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

for _, lhs in ipairs({ "<Leader>c" }) do
	map("n", lhs, '"+yy')
	map("x", lhs, '"+y')
end

map("n", "<leader>s", ":w<CR>")
map("n", "<leader>q", ":q<CR>")
map("n", "<leader>Q", ":qa!<CR>")
map("n", "<leader>tt", ":tabnew<CR>")
map("n", "<leader>tn", ":tabprevious<CR>")
map("n", "<leader>tr", ":tabnext<CR>")
map("n", "<leader>m", ":vsplit<CR>")
map("n", "<leader>p", ":split<CR>")
map("n", "<leader>o", "<C-w>w")
