if type -q ~/.bun/bin/bun
    set --export BUN_INSTALL "$HOME/.bun"
    set --export PATH $BUN_INSTALL/bin $PATH
end

if test -x /opt/homebrew/opt/bun/bin/bun
    set --export BUN_INSTALL /opt/homebrew/opt/bun
    set --export PATH $BUN_INSTALL/bin $PATH
end
