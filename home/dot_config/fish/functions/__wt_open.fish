function __wt_open --description 'Attach or switch to the tmux session for a worktree directory'
    set -l dir (path resolve $argv[1])
    set -l name (__wt_session $dir)

    # A session is identified by its path, the name is only a label — so realign
    # the label instead of creating a second session for the same directory.
    set -l current (__wt_session_at $dir)
    if set -q current[1]
        test "$current" = "$name"; or tmux rename-session -t=$current $name
    else
        tmux new-session -ds $name -c $dir
    end

    if set -q TMUX
        tmux switch-client -t=$name
    else
        tmux attach -t=$name
    end
end
