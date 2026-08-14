#!/usr/bin/env fish
# The status line iterates sessions with #{S:}, which walks them by id, not by
# name like switch-client -n/-p. Ask tmux for that same order so both agree.
#
#   sessions.fish next|prev <current>  print the neighbour, wrapping around
#   sessions.fish set-last             store the last session in @last-session

set -l names (tmux display-message -p '#{S:#{session_name}
}')
set -e names[-1] # display-message -p adds a newline of its own
set -l n (count $names)
test $n -gt 0; or exit 0

set -l i (contains -i -- "$argv[2]" $names)
set -q i[1]; or set i 1

switch $argv[1]
    case next
        set i (math "$i % $n + 1")
    case prev
        set i (math $i - 1)
        test $i -eq 0; and set i $n
    case set-last
        tmux set -g @last-session $names[-1]
        exit 0
    case '*'
        echo "sessions.fish: unknown command '$argv[1]'" >&2
        exit 1
end

echo $names[$i]
