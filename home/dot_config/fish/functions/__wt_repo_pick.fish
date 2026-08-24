function __wt_repo_pick --description 'Pick a project repo root, across every project'
    set -l gits $HOME/Git/*/.git $HOME/Git/Work/*/.git
    set -q gits[1]; or return 1 # path dirname would fall back to reading stdin
    set -l roots (path dirname $gits | path sort)

    printf '%s\n' $roots | fzf --no-sort --reverse --height 60% --border \
        --border-label ' repo ' --prompt '  ' --pointer '▸'
end
