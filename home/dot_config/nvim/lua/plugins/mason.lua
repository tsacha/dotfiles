return {
    {
        'williamboman/mason.nvim',
        dependencies = {
            { 'williamboman/mason-lspconfig.nvim' },
        },
        lazy = false,
        opts = {},
        config = function()
            require("mason").setup()
            require('mason-lspconfig').setup({
                ensure_installed = {
                    "ansiblels",
                    "basedpyright",
                    "gopls",
                    "helm_ls",
                    "lua_ls",
                    "ruff",
                    "rust_analyzer",
                    "tofu_ls",
                    "ts_ls",
                    "yamlls",
                },
            })
        end
    },
}
