#!/usr/bin/env fish
fisher install jethrokuan/z
fisher install jhillyerd/plugin-git
fisher install IlanCosman/tide@v6

tide configure \
    --auto \
    --style=Lean \
    --prompt_colors='True color' \
    --show_time=No \
    --lean_prompt_height='One line' \
    --prompt_spacing=Compact \
    --icons='Few icons' \
    --transient=No

if contains aws $tide_right_prompt_items
    set -Ue tide_right_prompt_items[(contains -i aws $tide_right_prompt_items)]
end
