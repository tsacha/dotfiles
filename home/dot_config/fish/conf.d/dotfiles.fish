alias pass="gopass"
alias ls="exa --long --header --git"
alias l="ls"
alias cm="chezmoi -S ~/Git/dotfiles -c ~/Git/dotfiles/chezmoi.toml --persistent-state ~/.config/chezmoi/chezmoistate.boltdb"
alias cms="chezmoi -S ~/Git/Security -c ~/Git/Security/chezmoi.toml --persistent-state ~/.config/chezmoi/chezmoistate-security.boltdb"

export EDITOR=emacs
export TERM=xterm-256color
export XDG_CURRENT_DESKTOP=X-Generic

fish_add_path /opt/homebrew/bin
set -U fish_greeting "🐟"
