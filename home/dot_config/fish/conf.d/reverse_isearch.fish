function __fzf_reverse_isearch
    history merge
    history -z | eval fzf --read0 --print0 --height 40% --tiebreak=index --toggle-sort=ctrl-r -q '(commandline)' | read -lz result
    and commandline -- $result
    commandline -f repaint
end

bind \cr __fzf_reverse_isearch
