if type -q kubecolor
    function kubectl --wraps kubectl
        command kubecolor $argv
    end

    function k --wraps kubectl
        command kubecolor $argv
    end

    function kubecolor --wraps kubectl
        command kubecolor $argv
    end
end
