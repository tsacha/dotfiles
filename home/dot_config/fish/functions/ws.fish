function ws --description 'Pick a worktree across every project and open its tmux session'
    set -l dirs (__wt_dirs)

    if not set -q dirs[1]
        echo 'ws: no worktree anywhere' >&2
        return 1
    end

    set -l paths (tmux list-sessions -F '#{session_path}' 2>/dev/null)
    set -l rows
    for dir in $dirs
        set -l repo (string replace -r '__worktrees$' '' (path dirname $dir) | path basename)
        set -l mark ' '
        contains -- $dir $paths; and set mark '●'
        set -a rows (printf '%s %-24s %s\t%s' $mark $repo (path basename $dir) $dir)
    end

    set -l pick (printf '%s\n' $rows | fzf --ansi --no-sort --reverse --height 60% --border \
        --border-label ' worktrees ' --prompt '  ' --pointer '▸' \
        --delimiter \t --with-nth 1 | string split -f2 \t)

    string length -q "$pick[1]"; and __wt_open $pick[1]
end
