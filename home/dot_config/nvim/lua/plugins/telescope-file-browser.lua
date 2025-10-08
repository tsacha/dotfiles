return {
	"nvim-telescope/telescope-file-browser.nvim",
	build = "make",
	dependencies = { "nvim-lua/plenary.nvim" },
	config = function()
		require("telescope").setup({
			extensions = {
				file_browser = {
					theme = "ivy",
					hijack_netrw = true,
				},
			},
		})
		require("telescope").load_extension("file_browser")
	end,
}
