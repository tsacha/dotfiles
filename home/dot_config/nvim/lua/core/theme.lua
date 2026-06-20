local M = {}

M.plugin = { src = "https://github.com/shatur/neovim-ayu" }

function M.apply()
	vim.o.termguicolors = true
	require("ayu").setup({
		mirage = true,
		terminal = true,
		overrides = function()
			local c = require("ayu.colors")
			-- stylua: ignore
			return {
				CursorLineNr                = { fg = c.accent, bg = c.bg, bold = true },
				FloatTitle                  = { fg = c.tag,    bold = true },
				LineNr                      = { fg = c.guide_active },

				-- Floats / completion popup
				Pmenu                       = { fg = c.fg,           bg = c.bg },
				PmenuBorder                 = { fg = c.comment,      bg = c.bg },
				PmenuMatch                  = { fg = c.regexp,       bold = true },
				PmenuSel                    = { bg = c.selection_bg, reverse = false, bold = true },

				-- Reverse text for diagnostics
				DiagnosticVirtualTextError  = { bg = c.error,   fg = c.line, italic = true },
				DiagnosticVirtualTextWarn   = { bg = c.keyword, fg = c.line, italic = true },
				DiagnosticVirtualTextInfo   = { bg = c.tag,     fg = c.line, italic = true },
				DiagnosticVirtualTextHint   = { bg = c.regexp,  fg = c.line, italic = true },

				-- render-markdown.nvim
				RenderMarkdownCode          = { bg = c.selection_inactive },
				RenderMarkdownCodeBorder    = { bg = c.selection_bg },
				RenderMarkdownCodeInline    = { fg = c.tag, bg = c.selection_inactive },
				RenderMarkdownTableHead     = { fg = c.selection_bg },
				RenderMarkdownTableRow      = { fg = c.selection_bg },

				-- Treesitter markup (markdown, docs, ...)
				['@markup.heading']         = { fg = c.keyword,  bold = true },
				['@markup.heading.1']       = { fg = c.accent,   bold = true },
				['@markup.heading.2']       = { fg = c.keyword,  bold = true },
				['@markup.heading.3']       = { fg = c.markup,   bold = true },
				['@markup.heading.4']       = { fg = c.entity,   bold = true },
				['@markup.heading.5']       = { fg = c.regexp,   bold = true },
				['@markup.heading.6']       = { fg = c.string,   bold = true },
				['@markup.strong']          = { fg = c.keyword,  bold = true },
				['@markup.italic']          = { fg = c.keyword,  italic = true },
				['@markup.quote']           = { fg = c.constant, italic = true },
				['@markup.raw']             = { fg = c.tag,      bg = c.selection_inactive },
				['@markup.list']            = { fg = c.vcs_added },
				['@markup.raw.block']       = { fg = c.tag },
				['@module']                 = { fg = c.fg },
				['@string.documentation']   = { fg = c.lsp_inlay_hint },
				['@variable.builtin']       = { fg = c.fg },

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
			}
		end,
	})

	vim.cmd.colorscheme("ayu")
end

return M
