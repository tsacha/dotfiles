return {
	"greggh/claude-code.nvim",
	dependencies = {
		"nvim-lua/plenary.nvim", -- Required for git operations
	},
	config = function()
		require("claude-code").setup()
	end,
	keys = {
		{ "<leader>cl", "<cmd>ClaudeCode<CR>", desc = "claude code" },
	},
}
