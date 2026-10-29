# Loaded on first call: `wt config shell init fish` costs ~70ms, too much for every shell the async prompt spawns.
function wt
    command wt config shell init fish | source
    wt $argv
end
