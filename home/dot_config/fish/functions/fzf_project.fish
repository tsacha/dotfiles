function fzf_project
    set -l dir
    if test "$PWD" = "$HOME"
        set -l projects (path filter -d -- $HOME/Git/* $HOME/Git/Work/*) (__wt_dirs)
        set dir (string join \n $projects | fzf)
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
