function s --description 'Pick a tmux session with sesh'
    set -l session (sesh list --icons | fzf --ansi --no-sort --reverse --height 60% \
        --border --border-label ' sessions ' --prompt '  ' --pointer '▸')
    test -n "$session"; and sesh connect $session
end
