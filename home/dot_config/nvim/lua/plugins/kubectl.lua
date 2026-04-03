--return {
--	{
--		"tsacha/kubectl.nvim",
--		branch = "main",
--		dependencies = "saghen/blink.download",
--		config = function()
--			require("kubectl").setup()
--			vim.keymap.set("n", "<leader>k", function()
--				require("kubectl").toggle({ tab = false })
--			end, { desc = "Toggle kubectl.nvim" })
--		end,
--	},
--}

return {
	{
		--"tsacha/kubectl.nvim",
		--branch = "main",
		"ramilito/kubectl.nvim",
		version = "2.*",
		-- OR build from source, requires nightly: https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust
		-- build = 'make build',
		-- OR if you use nix, build from source with:
		-- build = 'nix run .#build-plugin',
		dependencies = "saghen/blink.download",
		config = function()
			require("kubectl").setup()
			vim.keymap.set(
				"n",
				"<leader>k",
				'<cmd>lua require("kubectl").toggle({ tab = false })<cr>',
				{ noremap = true, silent = true }
			)
		end,
	},
}
