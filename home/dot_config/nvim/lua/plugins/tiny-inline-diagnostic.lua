require("tiny-inline-diagnostic").setup({
	preset = "classic",
	options = {
		show_source = {
			enabled = true, -- Enable showing source names
			if_many = true, -- Only show source if multiple sources exist for the same diagnostic
		},
		set_arrow_to_diag_color = true,
		add_messages = {
			display_count = true,
		},

		-- Show all diagnostics on the current cursor line, not just those under the cursor
		show_all_diags_on_cursorline = false,

		-- Only show diagnostics when the cursor is directly over them, no fallback to line diagnostics
		show_diags_only_under_cursor = false,
	},
})
