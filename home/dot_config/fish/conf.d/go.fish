set -x GOPATH $HOME/go
fish_add_path $GOPATH/bin

abbr dlvs dlv debug -l :2345 --accept-multiclient --headless
abbr dlvc dlv connect localhost:2345
