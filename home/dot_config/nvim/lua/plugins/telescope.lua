require("telescope").setup({
	defaults = {
		layout_strategy = "horizontal",
		layout_config = {
			horizontal = { width = 0.9 },
		},
	},
	extensions = {
		file_browser = {
			theme = "ivy",
			hijack_netrw = true,
		},
	},
})

local map = vim.keymap.set
map("n", "<leader>fs", function()
	require("telescope.builtin").current_buffer_fuzzy_find({})
end)

map("n", "<leader>fS", function()
	local root = vim.fs.root(0, ".git") or vim.uv.cwd()
	require("telescope.builtin").live_grep({
		cwd = root,
	})
end)

map("n", "<leader>fb", function()
	require("telescope.builtin").buffers({})
end)

map("n", "<leader>fg", function()
	require("telescope.builtin").git_files({
		show_untracked = true,
	})
end)

map("n", "<leader>fm", function()
	require("telescope.builtin").marks({})
end)
map("n", "<leader>fr", function()
	require("telescope.builtin").registers({})
end)

map("n", "<leader>dd", function()
	require("telescope.builtin").diagnostics({})
end)

map("n", "<leader>fF", function()
	require("telescope.builtin").find_files()
end)

map("n", "<leader>ff", function()
	require("telescope").extensions.file_browser.file_browser({
		path = "%:p:h",
		no_ignore = true,
		grouped = true,
		follow_symlinks = true,
	})
end)
