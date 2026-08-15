function get_color_scheme
    switch (uname)
        case Darwin
            set -l mode (osascript -e "tell app \"System Events\" to tell appearance preferences to get dark mode" | string trim)
            if test "$mode" = "true"
                echo "dark"
            else
                echo "light"
            end
        case Linux
            if set -q WSL_DISTRO_NAME; or set -q WSL_INTEROP
                if reg.exe query 'HKCU\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize' /v AppsUseLightTheme 2>/dev/null | string match -q '*0x0*'
                    echo "dark"
                else
                    echo "light"
                end
            else if command -q gsettings
                set -l scheme (gsettings get org.gnome.desktop.interface color-scheme | string trim)
                if test "$scheme" = "'prefer-dark'"
                    echo "dark"
                else if test "$scheme" = "'prefer-light'"
                    echo "light"
                else
                    echo "light"
                end
            else
                echo "light"
            end
        case '*'
            echo "light"
    end
end
