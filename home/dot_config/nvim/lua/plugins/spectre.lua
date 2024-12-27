return {
    "nvim-pack/nvim-spectre",
    dependencies = { "nvim-lua/plenary.nvim" },
    keys = {
        { "<leader>cc", mode = { "n" }, function() require("spectre").toggle() end,                            desc = "Spectre Toggle" },
        { "<leader>cw", mode = { "n" }, function() require("spectre").open_visual({ select_word = true }) end, desc = "Spectre Toggle" },
        { "<leader>cw", mode = { "v" }, function() require("spectre").open_visual() end,                       desc = "Spectre Toggle" },
    },
    config = function()
        require("spectre").setup({})
    end,
}
