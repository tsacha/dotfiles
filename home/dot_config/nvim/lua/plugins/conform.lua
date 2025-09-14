return {
    { -- Autoformat
        "stevearc/conform.nvim",
        event = { "BufWritePre" },
        cmd = { "ConformInfo" },
        opts = {
            notify_on_error = false,
            formatters_by_ft = {
                lua = { "stylua" },
                fish = { "fish_indent" },
            },
        },
    },
}
-- vim: ts=2 sts=2 sw=2 et
