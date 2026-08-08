-- Hard wrap auto à 80 colonnes pendant la frappe
vim.opt_local.textwidth = 80
vim.opt_local.formatoptions:append("t")

-- <leader>w : toggle du wrap auto pour ce buffer
vim.keymap.set("n", "<leader>w", function()
	if vim.bo.textwidth == 0 then
		vim.bo.textwidth = 80
		vim.opt_local.formatoptions:append("t")
		vim.notify("Auto-wrap 80 : ON")
	else
		vim.bo.textwidth = 0
		vim.opt_local.formatoptions:remove("t")
		vim.notify("Auto-wrap 80 : OFF")
	end
end, { buffer = true, silent = true, desc = "Toggle auto-wrap 80" })
