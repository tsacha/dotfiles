function fish_prompt
    set -l last_status $status

    set_color blue
    echo -n (prompt_pwd)

    # fish_git_prompt forks four processes to print just the branch; each exec costs ~6ms under Jamf Protect.
    set -l branch (command git rev-parse --abbrev-ref HEAD 2>/dev/null)
    test "$branch" = HEAD; and set branch (command git rev-parse --short HEAD)
    if test -n "$branch"
        set_color yellow
        echo -n " ($branch)"
    end

    if test $last_status -eq 0
        set_color green
    else
        set_color red
    end
    echo -n " ❯ "

    set_color normal
end

function fish_right_prompt
    set -l kubeconfig $KUBECONFIG
    if test -z "$kubeconfig"
        set kubeconfig ~/.kube/config
    end
    test -f "$kubeconfig"; or return

    set -l k8s (yq '.current-context as $curr|$curr, ((.contexts[]|select(.name==$curr).context.namespace) // "")' $kubeconfig 2>/dev/null)
    set -l k8s_context $k8s[1]
    test -z "$k8s_context"; or test "$k8s_context" = null; and return

    set -l k8s_ns $k8s[2]
    if test -z "$k8s_ns" -o "$k8s_ns" = default
        set k8s_ns ""
    else
        set k8s_ns "/$k8s_ns"
    end

    set -l ctx_color 4481d0
    if string match -q "*prod*" "$k8s_context"
        set ctx_color ff5252
    else if string match -q "*dev*" "$k8s_context"
        set ctx_color 7ed321
    else if string match -q "*shared*" "$k8s_context"
        set ctx_color a389d4
    end

    set_color $ctx_color
    echo -n "☸ $k8s_context$k8s_ns"
    set_color normal
end
