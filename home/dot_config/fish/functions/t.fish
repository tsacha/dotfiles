function t --description 'Open the tmux session for the current repo, else scratch'
    # --show-toplevel, not __wt_root: inside a worktree we want its own session,
    # which is what wa/wd already name it after.
    if set -l root (git rev-parse --show-toplevel 2>/dev/null)
        __wt_open $root
    else
        tmux new-session -A -s scratch -c ~
    end
end
