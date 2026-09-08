function __wt_session_at --description 'Print the tmux session sitting at a directory, if any'
    set -l dir (path resolve $argv[1])

    for line in (tmux list-sessions -F '#{session_path}'\t'#{session_name}' 2>/dev/null)
        set -l parts (string split -m1 \t -- $line)
        if test "$parts[1]" = "$dir"
            echo $parts[2]
            return 0
        end
    end
    return 1
end
