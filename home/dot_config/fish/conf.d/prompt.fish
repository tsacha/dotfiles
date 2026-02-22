function fish_prompt
    set -l last_status $status

    set_color blue
    echo -n (prompt_pwd)

    set_color yellow
    echo -n (fish_git_prompt)

    if test $last_status -eq 0
        set_color green
    else
        set_color red
    end
    echo -n " ❯ "

    set_color normal
end

function fish_right_prompt
    set -l k8s_context (kubectl config current-context 2>/dev/null)
    test -z "$k8s_context"; and return

    set -l ctx_color 4481d0
    if string match -q "*prod*" "$k8s_context"
        set ctx_color ff5252
    else if string match -q "*dev*" "$k8s_context"
        set ctx_color 7ed321
    else if string match -q "*shared*" "$k8s_context"
        set ctx_color a389d4
    end

    set_color $ctx_color
    echo -n "☸  $k8s_context"
    set_color normal
end
