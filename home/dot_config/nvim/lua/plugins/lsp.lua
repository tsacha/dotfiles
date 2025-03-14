return {
    {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v4.x',
        lazy = true,
        config = false,
    },
    {
        'williamboman/mason.nvim',
        lazy = false,
        opts = {},
    },

    -- Autocompletion
    {
        'hrsh7th/nvim-cmp',
        event = 'InsertEnter',
        config = function()
            local cmp = require('cmp')

            cmp.setup({
                sources = {
                    { name = 'nvim_lsp' },
                },
                mapping = cmp.mapping.preset.insert({
                    ['<CR>'] = cmp.mapping.confirm({ select = true }),
                    ['<C-Space>'] = cmp.mapping.complete(),
                    ['<C-u>'] = cmp.mapping.scroll_docs(-4),
                    ['<C-d>'] = cmp.mapping.scroll_docs(4),
                }),
                snippet = {
                    expand = function(args)
                        vim.snippet.expand(args.body)
                    end,
                },
            })
        end
    },

    -- LSP
    {
        'neovim/nvim-lspconfig',
        cmd = { 'LspInfo', 'LspInstall', 'LspStart' },
        event = { 'BufReadPre', 'BufNewFile' },
        dependencies = {
            { 'hrsh7th/cmp-nvim-lsp' },
            { 'williamboman/mason.nvim' },
            { 'williamboman/mason-lspconfig.nvim' },
        },
        init = function()
            -- Reserve a space in the gutter
            -- This will avoid an annoying layout shift in the screen
            vim.opt.signcolumn = 'yes'
        end,
        config = function()
            local lsp_defaults = require('lspconfig').util.default_config

            -- Add cmp_nvim_lsp capabilities settings to lspconfig
            -- This should be executed before you configure any language server
            lsp_defaults.capabilities = vim.tbl_deep_extend(
                'force',
                lsp_defaults.capabilities,
                require('cmp_nvim_lsp').default_capabilities()
            )

            -- LspAttach is where you enable features that only work
            -- if there is a language server active in the file
            vim.api.nvim_create_autocmd('LspAttach', {
                desc = 'LSP actions',
                callback = function(event)
                    local opts = { buffer = event.buf }

                    vim.keymap.set('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
                    vim.keymap.set({ 'n', 'x' }, '<F3>', '<cmd>lua vim.lsp.buf.format({async = true})<cr>', opts)
                    vim.keymap.set('n', '<F4>', '<cmd>lua vim.lsp.buf.code_action()<cr>', opts)

                    local telescope = require('telescope.builtin')
                    vim.keymap.set('n', '<leader>ll', telescope.lsp_document_symbols, {})
                    vim.keymap.set('n', '<leader>ld', telescope.lsp_definitions, {})
                    vim.keymap.set('n', '<leader>lr', telescope.lsp_references, {})
                    vim.keymap.set('n', '<leader>lR', ":LspRestart<CR>", {})
                    vim.keymap.set('n', '<leader>li', telescope.lsp_incoming_calls, {})
                    vim.keymap.set('n', '<leader>lo', telescope.lsp_outgoing_calls, {})
                end,
            })

            require('mason-lspconfig').setup({
                ensure_installed = {
                    -- "buf_ls", https://github.com/williamboman/mason-lspconfig.nvim/pull/485
                    "lua_ls",
                    "gopls",
                    "terraformls",
                    "ansiblels",
                    "ts_ls",
                    "svelte",
                    "yamlls",
                    "basedpyright",
                    "ruff",
                    "rust_analyzer",
                    "helm_ls"
                },
                handlers = {
                    -- this first function is the "default handler"
                    -- it applies to every language server without a "custom handler"
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
