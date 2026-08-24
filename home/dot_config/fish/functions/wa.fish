function wa --description 'Add a git worktree and open it in a tmux session'
    argparse 'C=' -- $argv
    or return 1

    set -l root
    if set -q _flag_C
        set root $_flag_C
    else
        set root (__wt_root) || set root (__wt_repo_pick) || return 1
    end

    set -l branch $argv[1]
    if not string length -q $branch
        set branch (__wt_pick $root) || return 1
        string length -q $branch; or return 1
    end

    set -l dir
    set -l wt_path
    for line in (git -C $root worktree list --porcelain)
        if string match -q 'worktree *' -- $line
            set wt_path (string sub -s 10 -- $line)
        else if test "$line" = "branch refs/heads/$branch"
            set dir $wt_path
            break
        end
    end

    if not set -q dir[1]
        set -l name (__wt_name $branch)
        set dir $root'__worktrees'/$name

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
