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
alias t=tmux

abbr --add l ls
abbr --add n nvim
abbr --add y yazi
abbr --add tf tofu
abbr --add gu gitu
abbr --add gdd 'git -c core.pager=delta -c delta.side-by-side=true diff'
abbr --add gdf 'GIT_EXTERNAL_DIFF=difft git diff'
# Named apart from their trigger: kube.fish already owns the abbr name "wa".
abbr --add wt-switch --regex wa -- 'wt switch'
abbr --add wt-remove --regex wd -- 'wt remove'
abbr --add wt-list --regex wl -- 'wt list'

bind ctrl-backspace backward-kill-token
bind alt-backspace backward-kill-word
bind \cr __fzf_reverse_isearch

zoxide init fish | source
command -q wt; and wt config shell init fish | source
