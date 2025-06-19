return {
    "mfussenegger/nvim-dap",
    dependencies = {
        "nvim-neotest/nvim-nio",
        "igorlfs/nvim-dap-view",
        "theHamsta/nvim-dap-virtual-text",
    },
    config = function()
        require("nvim-dap-virtual-text").setup()
        local dap, dapui = require("dap"), require("dap-view")
        dapui.setup({
            winbar = {
                sections = { "console", "watches", "scopes", "exceptions", "breakpoints", "threads", "repl" },
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
        {
            "<leader>db",
            function() require("dap").toggle_breakpoint() end,
            desc = "Toggle Breakpoint"
        },
        {
            "<leader>dl",
            function() require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end,
            desc = "Toggle Logpoint"
        },
        {
            "<leader>dc",
            function() require("dap").continue() end,
            desc = "Continue"
        },
        {
            "<leader>de",
            function() require("dap-view").add_expr() end,
            desc = "Eval"
        },
        {
            "<leader>do",
            function() require("dap").step_over() end,
            desc = "Step Over"
        },
        {
            "<leader>di",
            function() require("dap").step_into() end,
            desc = "Step Into"
        },
        {
            "<leader>dT",
            function() require("dap").terminate() end,
            desc = "Terminate"
        },
        {
            "<leader>du",
            function() require("dap-view").toggle() end,
            desc = "UI"
        }
    },
}
