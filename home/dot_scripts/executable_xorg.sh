#!/bin/sh
if [ "$(sudo virt-what)" != "hyperv" ]; then
    xrandr \
        --output DP-4 --mode 3440x1440 --pos 200x1080   --rate 143.92 --rotate normal --primary \
        --output DP-2 --mode 1920x1080 --pos 0x0        --rate 143.98 --rotate normal \
        --output DP-0 --mode 1920x1080 --pos 1920x0     --rate 143.85 --rotate normal \
        --output DP-1 --off \
        --output HDMI-0 --off \
        --output DP-3 --off \
        --output DP-5 --off
    nvidia-settings --load-config-only
    systemctl --user start picom
else
    xrandr --output Virtual-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal
fi
