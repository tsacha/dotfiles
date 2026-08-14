function __wt_name --description 'Turn a branch name into a tmux-addressable session name'
    string replace -ra '[/.:]' - $argv[1]
end
