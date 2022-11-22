#!/bin/sh
if [ ! -d /sys/class/power_supply/BAT0 ]; then
    xrandr \
        --output HDMI-0 --off \
        --output DP-0 --mode 3440x1440 --pos 0x0 --rate 143.92 --rotate normal \
        --output DP-1 --off \
        --output DP-2 --mode 1920x1080 --pos 3440x0 --rate 143.92 --rotate normal \
        --output DP-3 --off \
        --output DP-4 --off \
        --output DP-5 --off
    nvidia-settings --load-config-only
fi
