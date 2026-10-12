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
    set -l pr
    set -l query
    if string match -qr '^#?\d+$' -- $branch
        # A number is a PR on GitHub, otherwise (or when there is no such PR) a
        # search term, such as a ticket number found in branch names or PR titles.
        set query (string trim -l -c '#' -- $branch)
        set branch
        set -l remote (git -C $root remote get-url origin 2>/dev/null)
        if command -q gh; and string match -q '*github.com*' -- $remote
            set branch (gh pr view $query -R $remote --json headRefName --jq .headRefName 2>/dev/null)
            and set pr $query
        end
    end
    if not string length -q "$branch"
        set -l picked (__wt_pick $root $query) || return 1
        set branch $picked[1]
        set pr $picked[2]
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
        set -l name $pr
        string length -q $name; or set name (__wt_name $branch)
        set dir $root'__worktrees'/$name

        git -C $root worktree prune

        if not git -C $root show-ref --verify --quiet refs/heads/$branch
            and not git -C $root show-ref --verify --quiet refs/remotes/origin/$branch
            git -C $root fetch --no-tags origin \
                "+refs/heads/$branch:refs/remotes/origin/$branch" 2>/dev/null
        end

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
