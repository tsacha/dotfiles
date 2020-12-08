#!/bin/sh
if [ ! -d /sys/class/power_supply/BAT0 ]; then
	xrandr \
		--output DP-2 --primary --mode 2560x1440 --rate 144 \
		--output HDMI-0 --mode 1920x1080 --rate 60 --right-of DP-2 \
		--output DVI-D-0 --mode 1920x1080 --rate 60 --left-of DP-2 \
		--output DP-5 --off \
		--output DP-4 --off \
		--output DP-3 --off \
		--output DP-1 --off
	feh --bg-fill /home/sacha/Pictures/wallpapers/gris/01.png --bg-fill /home/sacha/Pictures/wallpapers/gris/02.png --bg-fill /home/sacha/Pictures/wallpapers/gris/03.png
	nvidia-settings --load-config-only
fi
