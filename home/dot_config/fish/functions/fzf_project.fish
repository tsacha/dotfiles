function fzf_project
    set -l dir
    if string match $PWD $HOME
        set dir (
            begin
                find $HOME/Git -mindepth 1 -maxdepth 1 -type d
                find $HOME/Git/Work -mindepth 1 -maxdepth 1 -type d
            end | fzf
        )
        if string length -q $dir
            cd $dir
        end
    else
        set -l file (fd --type f --strip-cwd-prefix | fzf -0)
        if string length -q $file
            commandline -j (string join " " $argv[1] $argv[2] $file)
        end
    end
end
