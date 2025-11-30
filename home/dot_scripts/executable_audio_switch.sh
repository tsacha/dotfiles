#!/usr/bin/env bash

# Define the two sinks to switch between
SINK1="alsa_output.usb-iFi__by_AMR__iFi__by_AMR__HD_USB_Audio_0003-00.analog-stereo"
SINK2="alsa_output.pci-0000_11_00.6.iec958-stereo"

# Get current default sink
current_sink=$(pactl get-default-sink)

# Switch to the other sink
if [ "$current_sink" = "$SINK1" ]; then
    new_sink="$SINK2"
else
    new_sink="$SINK1"
fi

# Set new default sink
pactl set-default-sink "$new_sink"

# Move all currently playing streams to new sink
pactl list short sink-inputs | awk '{print $1}' | while read -r stream; do
    pactl move-sink-input "$stream" "$new_sink" 2>/dev/null
done

echo "Switched to: $new_sink"
