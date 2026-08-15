set -U fish_greeting ""

set -gx EDITOR nvim

fish_add_path $HOME/.local/bin
fish_add_path $HOME/.cargo/bin

alias cat="bat -pp --theme-dark=theme-dark --theme-light=theme-light"
alias ls="eza --long --header --git"
alias ag="ag -f"
alias cm="chezmoi -S ~/Git/dotfiles -c ~/Git/dotfiles/chezmoi.toml --persistent-state ~/.config/chezmoi/chezmoistate.boltdb"
alias cms="chezmoi -S ~/Git/security -c ~/Git/security/chezmoi.toml --persistent-state ~/.config/chezmoi/chezmoistate-security.boltdb"
alias gg="cd (git rev-parse --show-toplevel)"
alias gs="git status"

abbr --add l ls
abbr --add n nvim
abbr --add y yazi
abbr --add t 'tmux new-session -A -s scratch -c ~'
abbr --add tf tofu
abbr --add gu gitu
abbr --add gdd 'git -c core.pager=delta -c delta.side-by-side=true diff'
abbr --add gdf 'GIT_EXTERNAL_DIFF=difft git diff'

bind ctrl-backspace backward-kill-token
bind alt-backspace backward-kill-word
bind \cr __fzf_reverse_isearch

zoxide init fish | source
