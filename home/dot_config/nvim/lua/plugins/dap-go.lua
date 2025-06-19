return {
    "leoluz/nvim-dap-go",
    config = function()
        require('dap-go').setup({
            dap_configurations = {}
        })
    end
}
