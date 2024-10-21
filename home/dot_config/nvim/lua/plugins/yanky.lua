return {
    "gbprod/yanky.nvim",
    keys = {
        { "y",     mode = { "n", "x" }, "<Plug>(YankyYank)" },
        { "p",     mode = { "n", "x" }, "<Plug>(YankyPutAfter)" },
        { "P",     mode = { "n", "x" }, "<Plug>(YankyPutBefore)" },
        { "gp",    mode = { "n", "x" }, "<Plug>(YankyGPutAfter)" },
        { "gP",    mode = { "n", "x" }, "<Plug>(YankyGPutBefore)" },
        { "<C-p>", mode = { "n" },      "<Plug>(YankyPreviousEntry)" },
        { "<C-n>", mode = { "n" },      "<Plug>(YankyNextEntry)" },
        {
            "<leader>fy",
            mode = { "n" },
            function()
                local extensions = require('telescope').extensions
                extensions.yank_history.yank_history({})
            end
        }
    },
    config = function()
        require("yanky").setup({})
        require("telescope").load_extension("yank_history")
        vim.keymap.set("n", "<leader>y", require("telescope").extensions.yank_history.yank_history, {})
    end,
}
