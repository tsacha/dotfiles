abbr --add ta go-task

if type -q go-task
    set -gx GO_TASK_PROGNAME go-task
    go-task --completion fish | source
end

set -gx SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
