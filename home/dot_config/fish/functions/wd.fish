function wd --description 'Purge a worktree, its branch and its tmux session'
    set -l root (__wt_root) || return 1
    set -l branch $argv[1]
    set -l dir

    if string length -q $branch
        set dir $root'__worktrees'/(__wt_name $branch)
    else
        set dir (git rev-parse --show-toplevel) || return 1
        if test "$dir" = "$root"
            echo 'wd: this is the main clone, pass a branch name' >&2
            return 1
        end
        set branch (git branch --show-current)
    end

    string match -q "$dir*" $PWD; and cd $root

    git -C $root worktree remove --force $dir || return 1
    if test -n "$branch"
        git -C $root branch -D $branch
        tmux kill-session -t=(__wt_name $branch) 2>/dev/null
    end
end
