return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    git = { enabled = true },
    notifier = { enabled = true },
    explorer = { enabled = true,
    },
  },
  keys = {
    {
      "<leader>fe",
      mode = { "n" },
      function()
        Snacks.explorer.open({
          auto_close = true,
        })
      end
    },

    {
      "<leader>fr",
      mode = { "n" },
      function()
        Snacks.explorer.reveal(Snacks.git.get_root())
      end
    }
  }
}
