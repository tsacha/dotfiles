function rgv
    # Set the first argument as the search pattern
    set -l search
    if set -q argv[1]
        set search $argv[1]
        # Remove the first argument so the rest can be passed to rg
        set --erase argv[1]
    end

    # The main command pipeline
    set -l color_scheme (get_color_scheme)
    if test "$color_scheme" = light
        set -g theme "--theme=rose-pine-dawn"
    else
        set -g theme "--theme=rose-pine"
    end
    commandline -j (
    string join " " (
      rg --color=always --line-number --no-heading --smart-case $argv "$search" |
        fzf --delimiter ':' --ansi \
          --preview "bat -p --color=always $theme {1} --highlight-line {2}" \
          --preview-window '~8,+{2}-5' | awk -F ':' '{print "nvim "$1 " +" $2}'
      )
    )
end
