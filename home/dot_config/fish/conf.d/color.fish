function get_color_scheme
    # Check the operating system
    switch (uname)
        case Darwin # macOS
            set -l mode (osascript -e "tell app \"System Events\" to tell appearance preferences to get dark mode" | string trim)
            if test "$mode" = "true"
                echo "dark"
            else
                echo "light"
            end
        case Linux
            # Check if gsettings is available (for GNOME-based environments)
            if command -q gsettings
                set -l scheme (gsettings get org.gnome.desktop.interface color-scheme | string trim)
                if test "$scheme" = "'prefer-dark'"
                    echo "dark"
                else if test "$scheme" = "'prefer-light'"
                    echo "light"
                else
                    # Fallback for other schemes or unset values
                    echo "light"
                end
            else
                # Fallback for non-GNOME Linux environments
                echo "light"
            end
        case '*' # Other/Unsupported OS
            echo "light" # Default to light mode
    end
end

