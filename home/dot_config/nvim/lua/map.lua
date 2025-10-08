local function map(mode, lhs, rhs, opts)
	local options = { noremap = true, silent = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

map("n", "<leader>s", ":w<CR>")
map("n", "<leader>q", ":q<CR>")
map("n", "<leader>Q", ":qa!<CR>")
map("n", "<leader>tt", ":tabnew<CR>")
map("n", "<leader>tp", ":tabprevious<CR>")
map("n", "<leader>tn", ":tabnext<CR>")
map("n", "<leader>h", ":vsplit<CR>")
map("n", "<leader>v", ":split<CR>")
map("n", "<leader>o", "<C-w>w")
