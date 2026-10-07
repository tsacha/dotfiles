function wd --description 'Purge a worktree, its branch and its tmux session'
    set -l root (__wt_root) || return 1
    set -l branch $argv[1]
    set -l dir

    if string match -qr '^#?\d+$' -- $branch
        set dir $root'__worktrees'/(string trim -l -c '#' -- $branch)
        set branch (git -C $dir branch --show-current) || return 1
    else if string length -q $branch
        set -l wt_path
        for line in (git -C $root worktree list --porcelain)
            if string match -q 'worktree *' -- $line
                set wt_path (string sub -s 10 -- $line)
            else if test "$line" = "branch refs/heads/$branch"
                set dir $wt_path
                break
            end
        end
        set -q dir[1]; or set dir $root'__worktrees'/(__wt_name $branch)
    else
        set dir (git rev-parse --show-toplevel) || return 1
        if test "$dir" = "$root"
            echo 'wd: this is the main clone, pass a branch name' >&2
            return 1
        end
        set branch (git branch --show-current)
    end

    if test "$PWD" = "$dir"; or string match -q "$dir/*" $PWD
        cd $root
    end

    # Look the session up by path before the directory goes away: a repo with the
    # same basename elsewhere may well hold a similar-looking name.
    set -l session (__wt_session_at $dir)

    git -C $root worktree remove --force $dir || return 1
    # The session belongs to the directory, so a detached worktree loses it too.
    set -q session[1]; and tmux kill-session -t=$session 2>/dev/null
    if test -n "$branch"
        git -C $root branch -D $branch
    end
end
