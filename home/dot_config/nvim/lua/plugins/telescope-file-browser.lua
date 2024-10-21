return {
    "nvim-telescope/telescope-file-browser.nvim",
    build = "make",
    keys = {
        {
            "<leader>ff",
            mode = { "n" },
            function()
                local extensions = require('telescope').extensions
                extensions.file_browser.file_browser({})
            end
        }

    },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
        require("telescope").load_extension("file_browser")
    end,
}
