function __wt_pick --description 'Pick a branch for a worktree: existing worktrees first, then open PRs or remote branches'
    set -l root $argv[1]
    string length -q $root; or set root (__wt_root) || return 1

    set -l icon_current (printf '\033[92m\033[0m')  # nf-oct-check, green
    set -l icon_worktree (printf '\033[94m\033[0m') # nf-oct-git_branch, blue
    set -l icon_local (printf '\033[33m\033[0m')    # nf-oct-git_branch, yellow
    set -l icon_branch (printf '\033[36m\033[0m')   # nf-oct-git_branch, cyan
    set -l icon_pr (printf '\033[95m\033[0m')       # nf-oct-git_pull_request, magenta

    set -l rows
    set -l wt_path
    for line in (git -C $root worktree list --porcelain)
        if string match -q 'worktree *' -- $line
            set wt_path (string sub -s 10 -- $line)
        else if string match -q 'branch refs/heads/*' -- $line
            set -l b (string replace 'branch refs/heads/' '' -- $line)
            set -l icon $icon_worktree
            set -l label worktree
            if test "$wt_path" = "$root"
                set icon $icon_current
                set label current
            end
            set -a rows (printf '%s %s  (%s)\t%s' $icon $b $label $b)
        end
    end

    set -l seen (string match -rg '\t(.*)$' -- $rows)

    for b in (git -C $root for-each-ref --sort=-committerdate --format='%(refname:short)' refs/heads/)
        contains -- $b $seen; and continue
        set -a rows (printf '%s %s  (local)\t%s' $icon_local $b $b)
        set -a seen $b
    end

    set -l more

    set -l origin (git -C $root remote get-url origin 2>/dev/null)

    if command -q gh; and string match -q '*github.com*' -- $origin
        set more (gh pr list -R $origin --limit 100 --json number,title,headRefName \
            --template '{{range .}}#{{.number}}  {{.title}}{{"\t"}}{{.headRefName}}{{"\n"}}{{end}}' 2>/dev/null)
    end

    if not set -q more[1]
        git -C $root fetch --prune --no-tags origin 2>/dev/null
        set more (git -C $root for-each-ref --sort=-committerdate refs/remotes/origin \
            --format='%(refname:lstrip=3)  (%(committerdate:relative))%09%(refname:lstrip=3)' |
            string match -rv '\tHEAD$')
    end

    for row in $more
        set -l b (string match -rg '\t(.*)$' -- $row)
        contains -- $b $seen; and continue
        set -l icon $icon_branch
        string match -q '#*' -- $row; and set icon $icon_pr
        set -a rows (printf '%s %s' $icon $row)
    end

    if not set -q rows[1]
        echo 'wa: nothing to pick from' >&2
        return 1
    end

    printf '%s\n' $rows | fzf --ansi --no-sort --reverse --height 60% --border \
        --border-label ' worktree ' --prompt '  ' --pointer '▸' \
        --delimiter \t --with-nth 1 | string split -f2 \t
end
