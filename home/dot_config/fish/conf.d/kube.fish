set -gx K9S_CONFIG_DIR $HOME/.config/k9s
set -gx PATH $PATH $HOME/.krew/bin

abbr --add k kubectl

if test -f ~/.config/fish/conf.d/kube_clusters.fish
    source ~/.config/fish/conf.d/kube_clusters.fish
    for i in (seq 1 2 (count $__k8s_clusters))
        set -l key $__k8s_clusters[$i]
        set -l name $__k8s_clusters[(math $i + 1)]

        abbr --add k$key "KUBECONFIG=~/.kube/clusters/$name.yaml"
        abbr --add k{$key}k "KUBECONFIG=~/.kube/clusters/$name.yaml kubectl"
        alias k{$key}c="KUBECONFIG=~/.kube/clusters/$name.yaml kubie ctx $name -n (kubectl get ns -o jsonpath='{range .items[*]}{.metadata.name}{\"\\n\"}{end}' | fzf --tac)"
    end
end

set -l k_shortcuts \
    g="get" \
    lo="logs -f" \
    oj="-o=json" \
    oy="-o=yaml" \
    wa="--watch" \
    n="--namespace" \
    nt="-o=yaml | kubectl neat | yq -C"

for pair in $k_shortcuts
    set -l name (string split -m 1 "=" $pair)[1]
    set -l value (string split -m 1 "=" $pair)[2]
    abbr --add $name --command kubectl -- $value
end
