function __wt_pick --description 'Pick a branch for a worktree: existing worktrees first, then open PRs or remote branches'
    set -l root $argv[1]
    string length -q $root; or set root (__wt_root) || return 1

    # Where the branch lives: colors the icon when there is no PR. Existing worktrees are in bold.
    set -l loc_names current worktree local remote
    set -l loc_colors 92 94 33 36
    set -l loc_glyphs '' '' '' '' # nf-oct-check, nf-oct-git_branch

    # PR states, colored like GitHub: open green, draft grey, merged purple, closed red.
    set -l pr_state_names OPEN DRAFT MERGED CLOSED
    set -l pr_state_colors 92 90 95 91
    set -l pr_state_glyphs '' '' '' '' # nf-oct-git_pull_request, _draft, git_merge, _closed

    set -l e_branch
    set -l e_loc
    set -l e_extra
    set -l wt_path
    for line in (git -C $root worktree list --porcelain)
        if string match -q 'worktree *' -- $line
            set wt_path (string sub -s 10 -- $line)
        else if string match -q 'branch refs/heads/*' -- $line
            set -a e_branch (string replace 'branch refs/heads/' '' -- $line)
            if test "$wt_path" = "$root"
                set -a e_loc current
            else
                set -a e_loc worktree
            end
            set -a e_extra ''
        end
    end

    for b in (git -C $root for-each-ref --sort=-committerdate --format='%(refname:short)' refs/heads/)
        contains -- $b $e_branch; and continue
        set -a e_branch $b
        set -a e_loc local
        set -a e_extra ''
    end

    set -l pr_numbers
    set -l pr_titles
    set -l pr_branches
    set -l pr_states
    set -l pr_open
    set -l repo (string match -rg 'github\.com[:/]([^/]+)/(.+?)(?:\.git)?$' -- (git -C $root remote get-url origin 2>/dev/null))
    if command -q gh; and set -q repo[2]
        # One request for the open PRs plus the latest PR of each local branch, however old.
        set -l fields '{ nodes { number state isDraft headRefName title } }'
        set -l order 'orderBy: {field: CREATED_AT, direction: DESC}'
        set -l vars '$owner: String!, $name: String!'
        set -l body "open: pullRequests(states: OPEN, first: 100, $order) $fields"
        set -l args -f owner=$repo[1] -f name=$repo[2]
        for k in (seq (count $e_branch))
            set -a vars "\$h$k: String!"
            set -a body "b$k: pullRequests(headRefName: \$h$k, first: 1, $order) $fields"
            set -a args -f h$k=$e_branch[$k]
        end
        set -l query "query($(string join ', ' $vars)) { repository(owner: \$owner, name: \$name) { $body } }"

        for line in (gh api graphql $args -f query=$query \
                --jq '.data.repository[].nodes[] | [.number, .state, .isDraft, .headRefName, .title] | map(tostring) | join("\t")' 2>/dev/null)
            set -l parts (string split -m4 \t -- $line)
            set -l state $parts[2]
            test "$state $parts[3]" = 'OPEN true'; and set state DRAFT
            test $parts[2] = OPEN; and set -a pr_open $parts[4]
            set -a pr_numbers $parts[1]
            set -a pr_states $state
            set -a pr_branches $parts[4]
            set -a pr_titles $parts[5]
        end
    end

    if set -q pr_branches[1]
        for b in $pr_open
            contains -- $b $e_branch; and continue
            set -a e_branch $b
            set -a e_loc remote
            set -a e_extra ''
        end
    else
        git -C $root fetch --prune --no-tags origin 2>/dev/null
        for line in (git -C $root for-each-ref --sort=-committerdate refs/remotes/origin \
                --format='%(refname:lstrip=3)%09%(committerdate:relative)' | string match -rv '^HEAD\t')
            set -l parts (string split -m1 \t -- $line)
            contains -- $parts[1] $e_branch; and continue
            set -a e_branch $parts[1]
            set -a e_loc remote
            set -a e_extra $parts[2]
        end
    end

    # Hidden: a merged or closed PR, or no PR and already in the default
    # branch. A worktree is only hidden for its PR, since a fresh one has nothing on top yet.
    set -l merged
    set -l default (git -C $root symbolic-ref -q --short refs/remotes/origin/HEAD)
    and set merged (git -C $root branch --merged $default --format='%(refname:short)')
    set -l e_pr
    set -l done
    set -l width 0
    for k in (seq (count $e_branch))
        set -l i (contains -i -- $e_branch[$k] $pr_branches); or set i 0
        set -l is_done false
        if test $i -gt 0
            contains -- $pr_states[$i] MERGED CLOSED; and set is_done true
        else if test $e_loc[$k] = local
            contains -- $e_branch[$k] $merged; and set is_done true
        end
        if $is_done; and test $e_loc[$k] != current
            set -a done $k
            continue
        end
        set -a e_pr $i
        test $i -gt 0; and set width (math "max($width, $(string length -- $pr_numbers[$i]))")
    end
    if set -q done[1]
        set -e "e_branch[$done]"
        set -e "e_loc[$done]"
        set -e "e_extra[$done]"
    end

    if not set -q e_branch[1]
        echo 'wa: nothing to pick from' >&2
        return 1
    end

    # Branches without a PR line up with the PR titles, past "<number> - ".
    set -l indent 0
    test $width -gt 0; and set indent (math $width + 3)

    # Rows are "<display>\t<branch>\t<pr number or empty>"; only the display is shown.
    set -l rows
    for k in (seq (count $e_branch))
        set -l b $e_branch[$k]
        set -l l (contains -i -- $e_loc[$k] $loc_names)
        set -l reset (printf '\033[0m')
        set -l fg (printf '\033[39m') # back to the default color without dropping the bold
        set -l icon (printf "\033[$loc_colors[$l]m$loc_glyphs[$l]")$reset
        set -l text "$(string repeat -n $indent ' ')$b"
        set -l pr
        set -l i $e_pr[$k]
        if test $i -gt 0
            set pr $pr_numbers[$i]
            set -l s (contains -i -- $pr_states[$i] $pr_state_names)
            set -l color (printf "\033[$pr_state_colors[$s]m")
            set icon (printf "$color$pr_state_glyphs[$s]")$reset
            set text (printf '%s%*s%s - %s' $color $width $pr $fg $pr_titles[$i])
        end
        test -n "$e_extra[$k]"; and set text "$text  ($e_extra[$k])"
        contains -- $e_loc[$k] current worktree; and set text (printf '\033[1m')$text
        set -a rows (printf '%s %s%s\t%s\t%s' $icon $text $reset $b $pr)
    end

    printf '%s\n' $rows | fzf --ansi --no-sort --reverse --height 60% --border \
        --border-label ' worktree ' --prompt '  ' --pointer '▸' \
        --delimiter \t --with-nth 1 | string split -f2,3 \t
end
