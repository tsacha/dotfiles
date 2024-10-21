return {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "make",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
        require("telescope").setup({
            extensions = {
                fzf = {
                    fuzzy = false,
                },
            },
        })
        require("telescope").load_extension("fzf")
    end,
}
