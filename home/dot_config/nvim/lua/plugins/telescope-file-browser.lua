return {
    "nvim-telescope/telescope-file-browser.nvim",
    build = "make",
    keys = {
        {
            "<leader>ff",
            mode = { "n" },
            function()
                local extensions = require('telescope').extensions
                extensions.file_browser.file_browser({
                    no_ignore = true,
                    grouped = true
                })
            end
        }
    },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
        require("telescope").setup {
            extensions = {
                file_browser = {
                    theme = "ivy",
                    hijack_netrw = true,
                },
            },
        }
        require("telescope").load_extension("file_browser")
    end,
}
