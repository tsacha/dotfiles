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
            "<leader>fS",
            mode = { "n" },
            function()
                require('telescope.builtin').live_grep({
                    cwd = Snacks.git.get_root()
                })
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
                require('telescope.builtin').git_files({
                    show_untracked = true
                })
            end
        },
        {
            "<leader>fm",
            mode = { "n" },
            function()
                require('telescope.builtin').marks({})
            end
        },
        {
            "<leader>dd",
            mode = { "n" },
            function()
                require('telescope.builtin').diagnostics({})
            end
        },
        {
            "<leader>ff",
            mode = { "n" },
            function()
                local extensions = require('telescope').extensions
                extensions.file_browser.file_browser({
                    path = "%:p:h",
                    no_ignore = true,
                    grouped = true,
                    follow_symlinks = true
                })
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
            }
        })
    end,
}
