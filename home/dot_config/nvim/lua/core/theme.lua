local M = {}

M.plugin = { src = "https://github.com/rose-pine/neovim", name = "rose-pine" }

function M.apply()
	vim.o.termguicolors = true
	require("rose-pine").setup({
		variant = "auto", -- rose-pine-dawn on light background, main on dark
		dark_variant = "main",
		-- stylua: ignore
		highlight_groups = {
			CursorLineNr                = { fg = "gold", bg = "base", bold = true },
			FloatTitle                  = { fg = "foam", bold = true },
			LineNr                      = { fg = "highlight_high" },

			-- Floats / completion popup
			Pmenu                       = { fg = "text",          bg = "base" },
			PmenuBorder                 = { fg = "muted",         bg = "base" },
			PmenuMatch                  = { fg = "iris",          bold = true },
			PmenuSel                    = { bg = "highlight_med", bold = true },

			-- Reverse text for diagnostics
			DiagnosticVirtualTextError  = { bg = "love", fg = "base", italic = true },
			DiagnosticVirtualTextWarn   = { bg = "gold", fg = "base", italic = true },
			DiagnosticVirtualTextInfo   = { bg = "foam", fg = "base", italic = true },
			DiagnosticVirtualTextHint   = { bg = "iris", fg = "base", italic = true },

			-- render-markdown.nvim
			RenderMarkdownCode          = { bg = "highlight_low" },
			RenderMarkdownCodeBorder    = { bg = "highlight_med" },
			RenderMarkdownCodeInline    = { fg = "foam", bg = "highlight_low" },
			RenderMarkdownTableHead     = { fg = "highlight_med" },
			RenderMarkdownTableRow      = { fg = "highlight_med" },

			-- Treesitter markup (markdown, docs, ...)
			['@markup.heading']         = { fg = "pine", bold = true },
			['@markup.heading.1']       = { fg = "gold", bold = true },
			['@markup.heading.2']       = { fg = "pine", bold = true },
			['@markup.heading.3']       = { fg = "rose", bold = true },
			['@markup.heading.4']       = { fg = "foam", bold = true },
			['@markup.heading.5']       = { fg = "iris", bold = true },
			['@markup.heading.6']       = { fg = "love", bold = true },
			['@markup.strong']          = { fg = "pine", bold = true },
			['@markup.italic']          = { fg = "pine", italic = true },
			['@markup.quote']           = { fg = "iris", italic = true },
			['@markup.raw']             = { fg = "foam", bg = "highlight_low" },
			['@markup.list']            = { fg = "foam" },
			['@markup.raw.block']       = { fg = "foam" },
			['@module']                 = { fg = "text" },
			['@string.documentation']   = { fg = "muted" },
			['@variable.builtin']       = { fg = "text" },

			-- LSP semantic tokens
			['@lsp.type.variable']                     = { link = "@lsp" },
			["@lsp.typemod.class.defaultLibrary"]      = { link = "@type.builtin" },
			["@lsp.typemod.enum.defaultLibrary"]       = { link = "@type.builtin" },
			["@lsp.typemod.enumMember.defaultLibrary"] = { link = "@constant.builtin" },
			["@lsp.typemod.function.defaultLibrary"]   = { link = "@function.builtin" },
			["@lsp.typemod.keyword.async"]             = { link = "@keyword.coroutine" },
			["@lsp.typemod.keyword.injected"]          = { link = "@keyword" },
			["@lsp.typemod.macro.defaultLibrary"]      = { link = "@function.builtin" },
			["@lsp.typemod.method.defaultLibrary"]     = { link = "@function.builtin" },
			["@lsp.typemod.operator.injected"]         = { link = "@operator" },
			["@lsp.typemod.string.injected"]           = { link = "@string" },
			["@lsp.typemod.struct.defaultLibrary"]     = { link = "@type.builtin" },
			["@lsp.typemod.variable.callable"]         = { link = "@function" },
			["@lsp.typemod.variable.defaultLibrary"]   = { link = "@variable.builtin" },
			["@lsp.typemod.variable.injected"]         = { link = "@variable" },
			["@lsp.typemod.variable.static"]           = { link = "@constant" },
		},
	})

	vim.cmd.colorscheme("rose-pine")
end

return M
