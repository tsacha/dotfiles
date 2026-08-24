function s --description 'Pick a session, path, or repo to open — repos with no session yet go through wa'
    set -l names_rows
    set -l git_rows
    set -l other_rows
    for row in (sesh list --icons --hide-duplicates)
        set -l name (string split -m1 -f2 ' ' -- $row)
        set -l path (string replace -r '^~' $HOME -- $name)
        if test -d $path/.git
            set -a git_rows (printf '%s %s' '[33m[0m' $name)
        else if test -d $path
            set -a other_rows $row
        else
            set -a names_rows $row
        end
    end

    set -l pick (printf '%s\n' $names_rows $git_rows $other_rows | fzf --ansi --no-sort --reverse --height 60% \
        --border --border-label ' sessions ' --prompt '  ' --pointer '▸')
    test -n "$pick"; or return

    set -l name (string split -m1 -f2 ' ' -- $pick)
    set -l path (string replace -r '^~' $HOME -- $name)

    set -l sessions (tmux list-sessions -F '#{session_path}' 2>/dev/null)
    if test -d $path/.git; and not contains -- $path $sessions
        wa -C $path
    else
        sesh connect $pick
    end
end
