#!/bin/sh
xrandr \
    --output HDMI-0 --primary --mode 3840x2160 --rotate normal \
    --output DP-0 --off \
    --output DP-1 --off \
    --output DP-2 --off \
    --output DP-3 --off \
    --output DP-4 --off \
    --output DP-5 --off

feh --bg-fill /home/sacha/Pictures/wallpapers/gris/01.png
nvidia-settings --load-config-only
