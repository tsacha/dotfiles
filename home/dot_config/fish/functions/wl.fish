function wl --description 'List the worktrees of the current repo and their tmux sessions'
    set -l root (__wt_root) || return 1
    set -l paths (tmux list-sessions -F '#{session_path}' 2>/dev/null)
    set -l here
    set -q TMUX; and set here (tmux display-message -p '#{session_path}' 2>/dev/null)

    for dir in (git -C $root worktree list --porcelain | string replace -rf '^worktree ' '')
        set -l branch (git -C $dir branch --show-current)
        test -n "$branch"; or set branch '(detached)'
        set -l mark ' '
        contains -- $dir $paths; and set mark '●'
        test "$dir" = "$here"; and set mark '▸'
        printf '%s %-32s %s\n' $mark $branch (string replace -- $HOME '~' $dir)
    end
end
