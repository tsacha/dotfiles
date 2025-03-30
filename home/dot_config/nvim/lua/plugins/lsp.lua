return {
    {
        'williamboman/mason.nvim',
        lazy = false,
        opts = {},
    },
    -- LSP
    {
        'neovim/nvim-lspconfig',
        cmd = { 'LspInfo', 'LspInstall', 'LspStart' },
        event = { 'BufReadPre', 'BufNewFile' },
        dependencies = {
            { 'williamboman/mason.nvim' },
            { 'williamboman/mason-lspconfig.nvim' },
        },
        config = function()
            local lsp_defaults = require('lspconfig').util.default_config

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
                handlers = {
                    function(server_name)
                        require('lspconfig')[server_name].setup({})
                    end,
                }
            })

            require('lspconfig').yamlls.setup {
                settings = {
                    yaml = {
                        schemas = {
                            kubernetes = "templates/**",
                            ["http://json.schemastore.org/github-workflow"] = ".github/workflows/*",
                            ["http://json.schemastore.org/github-action"] = ".github/action.{yml,yaml}",
                            ["https://raw.githubusercontent.com/ansible/ansible-lint/main/src/ansiblelint/schemas/ansible.json#/$defs/tasks"] = "roles/tasks/*.{yml,yaml}",
                            ["http://json.schemastore.org/kustomization"] = "kustomization.{yml,yaml}",
                            ["https://raw.githubusercontent.com/ansible/ansible-lint/main/src/ansiblelint/schemas/ansible.json#/$defs/playbook"] = "*play*.{yml,yaml}",
                            ["https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json"] = "*docker-compose*.{yml,yaml}",
                        },
                        validate = false,
                        format = { enable = true },
                        completion = true,
                        hover = true,
                    }
                }
            }
        end
    }
}
