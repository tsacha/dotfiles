local show_coverage = false
local function toggle_coverage()
    local coverage = require("coverage")
    show_coverage = not show_coverage
    coverage.load(show_coverage)
end

return {
    "andythigpen/nvim-coverage",
    version = "*",
    config = function()
        require("coverage").setup({
            auto_reload = true,
        })
    end,
    keys = {
        {
            "<leader>tc",
            function()
                toggle_coverage()
            end,
            desc = "test coverage"
        },
        {
            "<leader>tC",
            function()
                require("coverage").load()
                require("coverage").summary()
            end,
            desc = "test coverage"
        },
    }
}
