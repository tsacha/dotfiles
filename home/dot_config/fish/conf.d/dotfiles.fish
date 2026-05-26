set -U fish_greeting ""

set -gx EDITOR nvim

fish_add_path $HOME/.local/bin

alias cat="bat -pp --theme-dark=rose-pine --theme-light=rose-pine-dawn"
alias ls="eza --long --header --git"
alias ag="ag -f"
alias cm="chezmoi -S ~/Git/dotfiles -c ~/Git/dotfiles/chezmoi.toml --persistent-state ~/.config/chezmoi/chezmoistate.boltdb"
alias cms="chezmoi -S ~/Git/security -c ~/Git/security/chezmoi.toml --persistent-state ~/.config/chezmoi/chezmoistate-security.boltdb"
alias gg="cd (git rev-parse --show-toplevel)"
alias gs="git status"

abbr --add l ls
abbr --add n nvim
abbr --add y yazi
abbr --add tf tofu
abbr --add t tmux
abbr --add lg lazygit
abbr --add gcmai 'git commit -m (gcmsg_ai)'

bind ctrl-backspace backward-kill-token
bind alt-backspace backward-kill-word
bind \cr __fzf_reverse_isearch

zoxide init fish | source
