return {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    keys = {
        {
            "<leader>fs",
            mode = { "n" },
            function()
                require('telescope.builtin').current_buffer_fuzzy_find({})
            end
        },
        {
            "<leader>fb",
            mode = { "n" },
            function()
                require('telescope.builtin').buffers({})
            end
        },
        {
            "<leader>fg",
            mode = { "n" },
            function()
                require('telescope.builtin').git_files({})
            end
        },
        {
            "<leader>fm",
            mode = { "n" },
            function()
                require('telescope.builtin').marks({})
            end
        }
    },
    config = function()
        require("telescope").setup({
            defaults = {
                layout_strategy = "horizontal",
                layout_config = {
                    horizontal = { width = 0.9 },
                },
            },
        })
    end,
}
