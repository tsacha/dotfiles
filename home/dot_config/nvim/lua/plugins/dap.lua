return {
    "mfussenegger/nvim-dap",
    dependencies = {
        "nvim-neotest/nvim-nio",
        --"igorlfs/nvim-dap-view",
        "rcarriga/nvim-dap-ui",
        "theHamsta/nvim-dap-virtual-text",
        "leoluz/nvim-dap-go",
    },
    config = function()
        require("nvim-dap-virtual-text").setup()
        require('dap-go').setup({
            dap_configurations = {
                {
                    -- Must be "go" or it will be ignored by the plugin
                    type = "go",
                    name = "Attach remote",
                    mode = "remote",
                    request = "attach",
                    port = "2345"
                }
            },
        })
        --local dap, dapui = require("dap"), require("dap-view")
        local dap, dapui = require("dap"), require("dapui")
        dapui.setup({
            layouts = {
                {
                    elements = {
                        { id = "console", size = 0.5 },
                        { id = "repl",    size = 0.5 },
                    },
                    position = "bottom",
                    size = 5,
                },
                {
                    elements = {
                        { id = "scopes",      size = 0.60 },
                        { id = "watches",     size = 0.30 },
                        { id = "stacks",      size = 0.05 },
                        { id = "breakpoints", size = 0.05 },
                    },
                    position = "left",
                    size = 60,
                },
            },
            winbar = {
                sections = { "console", "watches", "scopes", "exceptions", "breakpoints", "threads", "repl" },
                default_section = "scopes",
            }
        })

        dap.listeners.before.attach.dapui_config = function()
            dapui.open()
        end
        dap.listeners.before.launch.dapui_config = function()
            dapui.open()
        end
        dap.listeners.before.event_terminated.dapui_config = function()
            --dapui.close()
        end
        dap.listeners.before.event_exited.dapui_config = function()
            --dapui.close()
        end
    end,
    keys = {
        { "<leader>db", function() require("dap").toggle_breakpoint() end,                                           desc = "Toggle Breakpoint" },
        { "<leader>dl", function() require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end, desc = "Toggle Logpoint" },
        { "<leader>dc", function() require("dap").continue() end,                                                    desc = "Continue" },
        {
            "<leader>dg",
            function()
                require("dap").run({
                    type = "go",
                    name = "Attach remote",
                    mode = "remote",
                    request = "attach",
                    port = "2345"
                })
            end,
            desc = "Run Go Remote"
        },
        { "<leader>dr", function() require("dap").restart() end,                                        desc = "Restart" },
        { "<leader>de", function() require('dapui').elements.watches.add(vim.fn.expand('<cword>')) end, desc = "Eval" },
        { "<C-j>",      function() require("dap").step_over() end,                                      desc = "Step Over" },
        { "<C-l>",      function() require("dap").step_into() end,                                      desc = "Step Into" },
        { "<C-k>",      function() require("dap").step_out() end,                                       desc = "Step Into" },
        { "<leader>dk", function() require("dap").terminate() end,                                      desc = "Terminate" },
        { "<leader>du", function() require("dapui").toggle() end,                                       desc = "UI" },
        { "<leader>dt", function() require("dap-go").debug_test() end,                                  desc = "Debug Test" },
        { "<leader>dT", function() require("dap-go").debug_last_test() end,                             desc = "Debug Last Test" },
    },
}
