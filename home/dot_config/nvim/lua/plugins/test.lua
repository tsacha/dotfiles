return {
	"nvim-neotest/neotest",
	dependencies = {
		"nvim-neotest/nvim-nio",
		"nvim-lua/plenary.nvim",
		"antoinemadec/FixCursorHold.nvim",
		"nvim-treesitter/nvim-treesitter",
		{
			"fredrikaverpil/neotest-golang",
			dependencies = {
				"leoluz/nvim-dap-go",
			},
		},
	},
	config = function()
		local neotest_golang_opts = {
			runner = "gotestsum",
			go_test_args = {
				"-v",
				"-race",
				"-count=1",
				"-coverprofile=" .. vim.fn.getcwd() .. "/coverage.out",
			},
		}
		require("neotest").setup({
			adapters = {
				require("neotest-golang")(neotest_golang_opts),
			},
			status = { virtual_text = true },
			output = { open_on_run = true },
		})
	end,
	keys = {
		{
			"<leader>na",
			function()
				require("neotest").run.attach()
			end,
			desc = "test attach",
		},
		{
			"<leader>nf",
			function()
				require("neotest").run.run(vim.fn.expand("%"))
			end,
			desc = "test file",
		},
		{
			"<leader>nA",
			function()
				require("neotest").run.run(vim.uv.cwd())
			end,
			desc = "test all files",
		},
		{
			"<leader>nN",
			function()
				require("neotest").run.run({ suite = true })
			end,
			desc = "test suite",
		},
		{
			"<leader>nn",
			function()
				require("neotest").run.run()
			end,
			desc = "test nearest",
		},
		{
			"<leader>nl",
			function()
				require("neotest").run.run_last()
			end,
			desc = "test last",
		},
		{
			"<leader>nu",
			function()
				require("neotest").summary.toggle()
			end,
			desc = "test summary",
		},
		{
			"<leader>no",
			function()
				require("neotest").output.open({ enter = true, auto_close = true })
			end,
			desc = "test output",
		},
		{
			"<leader>nO",
			function()
				require("neotest").output_panel.toggle()
			end,
			desc = "test output panel",
		},
		{
			"<leader>nk",
			function()
				require("neotest").run.stop()
			end,
			desc = "kill test",
		},
	},
}
