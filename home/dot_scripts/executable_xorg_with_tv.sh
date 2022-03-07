#!/bin/sh
xrandr \
    --output HDMI-0 --mode 3840x2160 --pos 0x200 --rotate normal \
    --output DP-0 --primary --mode 3440x1440 --pos 5280x560 --rate 143.92 --rotate normal \
    --output DP-1 --off \
    --output DP-2 --mode 1920x1080 --pos 8720x920 --rotate normal \
    --output DP-3 --off \
    --output DP-4 --mode 2560x1440 --pos 3840x0 --rate 144.0 --rotate left \
    --output DP-5 --off

feh --bg-fill $HOME/Pictures/wallpapers/gris/01.png --bg-fill $HOME/Pictures/wallpapers/gris/02.png --bg-fill $HOME/Pictures/wallpapers/gris/03.png --bg-fill $HOME/Pictures/wallpapers/gris/04.png
nvidia-settings --load-config-only
