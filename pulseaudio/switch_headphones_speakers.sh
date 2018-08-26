#!/bin/sh
id_card=$(pacmd list-cards | grep -B6 'alsa\.card_name = "Xonar STX"' | head -n 1 | awk -F ': ' '{ print $2 }')
profile_card=$(pacmd list-cards | grep -A24 'alsa\.card_name = "Xonar STX"' | grep "active profile" | awk -F ': ' '{ print $2 }' | sed -E 's/[<>]//g')

if [ $profile_card == "output:analog-stereo" ]; then
  pacmd set-card-profile $id_card output:iec958-stereo
else
  pacmd set-card-profile $id_card output:analog-stereo
fi
