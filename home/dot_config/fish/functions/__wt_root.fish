function __wt_root --description 'Print the main repo root, even from inside a worktree'
    set -l common (git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
    or return 1
    dirname $common
end
