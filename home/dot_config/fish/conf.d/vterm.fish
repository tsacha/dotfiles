if test -n "$INSIDE_EMACS"
    functions --copy fish_prompt vterm_old_fish_prompt
    function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
        printf "%b" (string join "\n" (vterm_old_fish_prompt))
        vterm_prompt_end
    end
end
