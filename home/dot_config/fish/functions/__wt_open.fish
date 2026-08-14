function __wt_open --description 'Attach or switch to the tmux session for a worktree directory'
    set -l dir $argv[1]
    set -l name (__wt_name (path basename $dir))

    tmux has-session -t=$name 2>/dev/null; or tmux new-session -ds $name -c $dir
    if set -q TMUX
        tmux switch-client -t $name
    else
        tmux attach -t $name
    end
end
