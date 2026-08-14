function __wt_pick --description 'Pick a branch for a worktree: open PRs on GitHub, remote branches otherwise'
    set -l root (__wt_root) || return 1
    set -l rows

    if command -q gh; and string match -q '*github.com*' -- (git -C $root remote get-url origin 2>/dev/null)
        set rows (gh pr list --limit 100 --json number,title,headRefName 2>/dev/null |
            jq -r '.[] | "#\(.number)  \(.title)\t\(.headRefName)"')
    end

    if not set -q rows[1]
        set rows (git -C $root for-each-ref --sort=-committerdate refs/remotes/origin \
            --format='%(refname:lstrip=3)  (%(committerdate:relative))%09%(refname:lstrip=3)' |
            string match -rv '\tHEAD$')
    end

    if not set -q rows[1]
        echo 'wa: nothing to pick from' >&2
        return 1
    end

    printf '%s\n' $rows | fzf --ansi --no-sort --reverse --height 60% --border \
        --border-label ' worktree ' --prompt '  ' --pointer '▸' \
        --delimiter \t --with-nth 1 | string split -f2 \t
end
