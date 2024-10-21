return {
    { -- Adds git related signs to the gutter, as well as utilities for managing changes
        "ggandor/leap.nvim",
        config = function()
            require("leap").add_default_mappings()
        end,
    },
}
-- vim: ts=2 sts=2 sw=2 et
