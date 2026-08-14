function wa --description 'Add a git worktree and open it in a tmux session'
    set -l branch $argv[1]
    if not string length -q $branch
        set branch (__wt_pick) || return 1
        string length -q $branch; or return 1
    end

    set -l root (__wt_root) || return 1
    set -l name (__wt_name $branch)
    set -l dir $root'__worktrees'/$name

    if not test -d $dir
        git -C $root worktree prune
        if git -C $root show-ref --verify --quiet refs/heads/$branch
            git -C $root worktree add $dir $branch || return 1
        else if git -C $root show-ref --verify --quiet refs/remotes/origin/$branch
            git -C $root worktree add --track -b $branch $dir origin/$branch || return 1
        else
            git -C $root worktree add -b $branch $dir || return 1
        end
        test -f $root/.env; and cp $root/.env $dir/.env
    end

    __wt_open $dir
end
