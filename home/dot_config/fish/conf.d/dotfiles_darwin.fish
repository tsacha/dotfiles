abbr --add ta task

set -gx SSH_AUTH_SOCK ~/.gnupg/S.gpg-agent.ssh

if not contains /opt/homebrew/bin $fish_user_paths
    fish_add_path /opt/homebrew/bin
end

set -l brew_prefix /opt/homebrew

if test -d $brew_prefix/share/fish/completions
    if not contains $brew_prefix/share/fish/completions $fish_complete_path
        set -p fish_complete_path $brew_prefix/share/fish/completions
    end
end

if test -d $brew_prefix/share/fish/vendor_completions.d
    if not contains $brew_prefix/share/fish/vendor_completions.d $fish_complete_path
        set -p fish_complete_path $brew_prefix/share/fish/vendor_completions.d
    end
end
