#!/bin/sh
if [ ! -d /sys/class/power_supply/BAT0 ]; then
    xrandr \
        --output HDMI-0 --off \
        --output DP-0 --primary --mode 3440x1440 --pos 5280x560 --rate 143.92 --rotate normal \
        --output DP-1 --off \
        --output DP-2 --mode 1920x1080 --pos 8720x920 --rotate normal \
        --output DP-3 --off \
        --output DP-4 --mode 2560x1440 --pos 3840x0 --rate 144.00 --rotate left \
        --output DP-5 --off

    feh --bg-fill /home/sacha/Pictures/wallpapers/gris/01.png --bg-fill /home/sacha/Pictures/wallpapers/gris/02.png --bg-fill /home/sacha/Pictures/wallpapers/gris/03.png
    nvidia-settings --load-config-only
fi
