#!/usr/bin/env fish
# switch-client -n/-p walk sessions by name; the status line walks them by id.
# Resolve the neighbour in id order so both agree.

set -l dir $argv[1]
set -l current $argv[2]

set -l sessions (tmux list-sessions -F '#{session_id} #{session_name}' | sort -n -k1.2 | cut -d' ' -f2-)
set -l n (count $sessions)
test $n -gt 0; or exit

set -l i (contains -i -- $current $sessions)
test -n "$i"; or set i 1

if test "$dir" = prev
    if test $i -eq 1
        set i $n
    else
        set i (math $i - 1)
    end
else
    if test $i -eq $n
        set i 1
    else
        set i (math $i + 1)
    end
end

echo $sessions[$i]
