return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "antoinemadec/FixCursorHold.nvim",
    "nvim-treesitter/nvim-treesitter",
    {
      "fredrikaverpil/neotest-golang",
      dependencies = {
        "leoluz/nvim-dap-go",
      },
    },
  },
  config = function()
    local neotest_golang_opts = {
      runner = "gotestsum",
      go_test_args = {
        "-v",
        "-race",
        "-count=1",
        "-coverprofile=" .. vim.fn.getcwd() .. "/coverage.out",
      }
    }
    require("neotest").setup({
      adapters = {
        require("neotest-golang")(neotest_golang_opts)
      },
      status = { virtual_text = true },
      output = { open_on_run = true },
    })
  end,
  keys = {
    { "<leader>ta", function() require("neotest").run.attach() end,                                     desc = "test attach" },
    { "<leader>tf", function() require("neotest").run.run(vim.fn.expand("%")) end,                      desc = "test file" },
    { "<leader>tA", function() require("neotest").run.run(vim.uv.cwd()) end,                            desc = "test all files" },
    { "<leader>tS", function() require("neotest").run.run({ suite = true }) end,                        desc = "test suite" },
    { "<leader>tr", function() require("neotest").run.run() end,                                        desc = "test nearest" },
    { "<leader>tl", function() require("neotest").run.run_last() end,                                   desc = "test last" },
    { "<leader>ts", function() require("neotest").summary.toggle() end,                                 desc = "test summary" },
    { "<leader>to", function() require("neotest").output.open({ enter = true, auto_close = true }) end, desc = "test output" },
    { "<leader>tO", function() require("neotest").output_panel.toggle() end,                            desc = "test output panel" },
    { "<leader>tk", function() require("neotest").run.stop() end,                                       desc = "kill test" },
  },
}
