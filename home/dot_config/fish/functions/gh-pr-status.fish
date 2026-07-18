function gh-pr-status
    # Precompute colors once (avoids a set_color subshell per line)
    set -l c_approved (set_color green)
    set -l c_review (set_color yellow)
    set -l c_changes (set_color red)
    set -l c_pending (set_color brblack)
    set -l c_repo (set_color brblue)
    set -l c_age (set_color cyan)
    set -l c_reset (set_color normal)

    # --cache: réponse mise en cache 60s -> les appels suivants sont ~20x plus rapides
    gh api graphql --cache 60s -f query='
    query {
      search(query: "author:@me is:open is:pr is:unmerged archived:false", type: ISSUE, first: 100) {
        nodes {
          ... on PullRequest {
            url
            createdAt
            reviewDecision
            repository { nameWithOwner isArchived }
          }
        }
      }
    }' \
        --jq '
          .data.search.nodes
          | map(select(.repository.isArchived | not))
          | sort_by(.createdAt) | reverse
          | .[]
          | (now - (.createdAt | fromdateiso8601)) as $s
          | (if $s < 86400 then "\(($s / 3600) | floor)h" else "\(($s / 86400) | floor)d" end) as $age
          | "\(.reviewDecision // "PENDING")\t\($age)\t\(.repository.nameWithOwner)\t\(.url)"
        ' |
        while read -d \t -l decision age repo url
            set -l color $c_pending
            switch $decision
                case APPROVED
                    set color $c_approved
                case CHANGES_REQUESTED
                    set color $c_changes
                case REVIEW_REQUIRED
                    set color $c_review
            end

            # One printf per line, no subshells
            printf '%s%-20s%s %s%4s%s %s%-40s%s %s\n' \
                $color $decision $c_reset \
                $c_age $age $c_reset \
                $c_repo $repo $c_reset \
                $url
        end
end
