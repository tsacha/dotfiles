#!/bin/sh
if [ ! -d /sys/class/power_supply/BAT0 ]; then

	xrandr \
		--output HDMI-0 --mode 1920x1080 --pos 4880x920 --rotate normal --rate 60.00 \
		--output DP-0 --mode 2560x1440 --pos 0x0 --rotate left --rate 144.00 \
		--output DP-1 --off --output DP-2 --primary --mode 3440x1440 --pos 1440x560 --rotate normal --rate 143.92 \
		--output DP-3 --off \
		--output DP-4 --off \
		--output DP-5 --off

	feh --bg-fill /home/sacha/Pictures/wallpapers/gris/01.png --bg-fill /home/sacha/Pictures/wallpapers/gris/02.png --bg-fill /home/sacha/Pictures/wallpapers/gris/03.png
	nvidia-settings --load-config-only
fi
