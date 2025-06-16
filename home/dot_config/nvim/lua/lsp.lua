-- This is where you enable features that only work
-- if there is a language server active in the file
vim.api.nvim_create_autocmd("LspAttach", {
    desc = "LSP actions",
    callback = function(event)
        local opts = { buffer = event.buf }
        local telescope = require('telescope.builtin')

        vim.keymap.set("n", "K", "<cmd>lua vim.lsp.buf.hover()<cr>", opts)

        vim.keymap.set('n', '<leader>ll', telescope.lsp_document_symbols, {})
        vim.keymap.set('n', '<leader>ld', telescope.lsp_definitions, {})
        vim.keymap.set('n', '<leader>lr', telescope.lsp_references, {})
        vim.keymap.set('n', '<leader>lR', ":LspRestart<CR>", {})
        vim.keymap.set('n', '<leader>li', telescope.lsp_incoming_calls, {})
        vim.keymap.set('n', '<leader>lo', telescope.lsp_outgoing_calls, {})
        vim.keymap.set('n', '<leader>lm', telescope.lsp_implementations, {})

        vim.keymap.set('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
        vim.keymap.set({ 'n', 'x' }, '<F3>', '<cmd>lua vim.lsp.buf.format({async = true})<cr>', opts)
        vim.keymap.set('n', '<F4>', '<cmd>lua vim.lsp.buf.code_action()<cr>', opts)
    end,
})

-- Setup language servers.
vim.lsp.config("*", {
    capabilities = require("blink.cmp").get_lsp_capabilities({}),
    root_markers = { ".git" },
})
vim.diagnostic.config({
    virtual_lines = { current_line = true }
})


-- Enable each language server by filename under the lsp/ folder
vim.lsp.enable({
    "basedpyright",
    "gopls",
    "helm_ls",
    "lua_ls",
    "tofu_ls",
    "yamlls",
})
