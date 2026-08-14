function wl --description 'List the worktrees of the current repo and their tmux sessions'
    set -l root (__wt_root) || return 1
    set -l paths (tmux list-sessions -F '#{session_path}' 2>/dev/null)
    set -l here
    set -q TMUX; and set here (tmux display-message -p '#{session_path}' 2>/dev/null)

    set -l dir
    set -l branch
    # A porcelain record ends on a blank line; the extra '' closes the last one.
    for line in (git -C $root worktree list --porcelain) ''
        switch $line
            case 'worktree *'
                set dir (string replace 'worktree ' '' -- $line)
                set branch '(detached)'
            case 'branch *'
                set branch (string replace 'branch refs/heads/' '' -- $line)
            case ''
                test -n "$dir"; or continue
                set -l mark ' '
                contains -- $dir $paths; and set mark '●'
                test "$dir" = "$here"; and set mark '▸'
                printf '%s %-32s %s\n' $mark $branch (string replace -- $HOME '~' $dir)
                set dir ''
        end
    end
end
