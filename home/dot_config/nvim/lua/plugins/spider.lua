vim.pack.add({ "https://github.com/chrisgrieser/nvim-spider" })

for _, motion in ipairs({ "w", "e", "b" }) do
	vim.keymap.set({ "n", "o", "x" }, motion, function()
		require("spider").motion(motion)
	end, { desc = "Spider-" .. motion })
end
