set -q LANG; or set -gx LANG C.UTF-8

if command -q wslview
    set -gx BROWSER wslview
end
