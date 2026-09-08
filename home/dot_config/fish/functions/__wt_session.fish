function __wt_session --description 'Turn a directory into a tmux session name unique to that path'
    set -l dir (path resolve $argv[1])

    # prompt_pwd already knows how to shorten a path; -d/-D are passed so the name
    # never moves with $fish_prompt_pwd_dir_length. tmux targets treat . and : as
    # separators, / and ~ are fine.
    set -l name (string replace -ra '[.:]' - (prompt_pwd -d 1 -D 1 $dir))

    # Two paths can still shorten to the same thing (~/Work/zzz/x vs ~/WoW/zig/x).
    # If the name belongs to another directory, spell the path out in full.
    if tmux has-session -t=$name 2>/dev/null
        set -l owner (__wt_session_at $dir)
        if not contains -- $name $owner
            set name (string replace -ra '[.:]' - (prompt_pwd -D 99 $dir))
        end
    end

    echo $name
end
