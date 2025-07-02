return {
    "leoluz/nvim-dap-go",
    config = function()
        require('dap-go').setup({
            dap_configurations = {}
        })
    end,
    keys = {
        { "<leader>dt", function() require("dap-go").debug_test() end,      desc = "Debug Test" },
        { "<leader>dT", function() require("dap-go").debug_last_test() end, desc = "Debug Last Test" },
    },
}
