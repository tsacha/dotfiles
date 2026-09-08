#!/usr/bin/env fish
# Store the final session ID in status-bar order for the separator in the theme.
# Usage: sessions.fish refresh <socket>
if test "$argv[1]" != refresh; or test -z "$argv[2]"
    echo 'sessions.fish: expected refresh <socket>' >&2
    exit 1
end
set -l socket $argv[2]
set -l order '#{S:#{?#{==:#{session_name},scratch},#{session_id} ,}}#{S:#{?#{==:#{session_name},scratch},,#{session_id} }}'
set -l ids (string split -n ' ' -- (tmux -S "$socket" display-message -p "$order"))
set -q ids[1]; or exit 0
tmux -S "$socket" set-option -g @last-session-id $ids[-1]
