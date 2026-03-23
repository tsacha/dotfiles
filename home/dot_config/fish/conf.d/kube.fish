set -gx K9S_CONFIG_DIR $HOME/.config/k9s
set -gx PATH $PATH $HOME/.krew/bin

abbr --add k kubectl
abbr --add ku kubie

set -l k_shortcuts \
    gp="get pods" \
    gd="get deployments" \
    gs="get services" \
    gn="get nodes" \
    gcr="get cronjobs" \
    gj="get jobs" \
    gns="get namespaces" \
    lo="logs -f"

for pair in $k_shortcuts
    set -l name (string split -m 1 "=" $pair)[1]
    set -l value (string split -m 1 "=" $pair)[2]
    abbr --add $name --command kubectl $value
end
abbr --add n --command kubectl -- --namespace
abbr --add w --command kubectl -- --watch
