return {
  {
    "rose-pine/neovim",
    name = "rose-pine",
    config = function()
      vim.opt.termguicolors = true
      vim.cmd("colorscheme rose-pine")
    end,
  }
}
-- vim: ts=2 sts=2 sw=2 et
