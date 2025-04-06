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
                    "lua_ls",
                    "gopls",
                    "terraformls",
                    "ansiblels",
                    "ts_ls",
                    "yamlls",
                    "basedpyright",
                    "ruff",
                    "rust_analyzer",
                    "helm_ls"
                },
            })
        end
    },
}
