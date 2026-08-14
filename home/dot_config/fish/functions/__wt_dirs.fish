function __wt_dirs --description 'Print every worktree directory, across all projects'
    set -l dirs $HOME/Git/*__worktrees/* $HOME/Git/Work/*__worktrees/*
    set -q dirs[1]; or return 1 # path filter would fall back to reading stdin
    path filter -d -- $dirs | path sort
end
