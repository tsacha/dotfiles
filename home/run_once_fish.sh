#!/usr/bin/env fish
fisher install jethrokuan/z
fisher install jhillyerd/plugin-git
fisher install IlanCosman/tide@v6
fisher install meaningful-ooo/sponge

tide configure \
    --auto \
    --style=Lean \
    --prompt_colors='True color' \
    --show_time=No \
    --lean_prompt_height='One line' \
    --prompt_spacing=Compact \
    --icons='Few icons' \
    --transient=Yes

if contains aws $tide_right_prompt_items
    set -Ue tide_right_prompt_items[(contains -i aws $tide_right_prompt_items)]
end
set -U tide_pwd_color_anchors $tide_pwd_color_dirs
set -U tide_pwd_color_truncated_dirs $tide_pwd_color_dirs
set -U tide_git_color_branch 3d8b00
set -U tide_git_color_upstream 3d8b00
set -U tide_git_color_stash 3d8b00
set -U tide_character_color 3d8b00
