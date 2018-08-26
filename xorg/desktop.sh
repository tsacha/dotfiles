#!/bin/sh
xrandr \
	--output DP-0 --primary --mode 2560x1440 --rate 144 \
	--output DP-2 --mode 1920x1080 --rate 60 --right-of DP-0 \
	--output DVI-D-0 --mode 1920x1080 --rate 60 --left-of DP-0 \
	--output HDMI-0 --mode 3840x2160 --rate 60 --left-of DVI-D-0 \
	--output DP-5 --off \
	--output DP-4 --off \
	--output DP-3 --off \
	--output DP-1 --off \
