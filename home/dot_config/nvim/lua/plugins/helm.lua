vim.pack.add({ "https://github.com/qvalentin/helm-ls.nvim" })
require("helm-ls").setup({
	conceal_templates = {
		-- enable the replacement of templates with virtual text of their current values
		-- note: for better wrapping support, set `vim.opt.conceallevel = 2`
		enabled = false, -- this might change to false in the future
	},
})
