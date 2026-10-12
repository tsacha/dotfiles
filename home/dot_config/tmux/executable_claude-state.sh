#!/bin/sh
# Claude Code hook: stores the agent state on its tmux pane for the status bar.
# Usage: claude-state.sh working|blocked|done|idle|off|tool
[ -n "${TMUX_PANE:-}" ] || exit 0

state=$1
if [ "$state" = tool ]; then
	state=working
	grep -Eq '"tool_name": *"AskUserQuestion"' && state=blocked
fi
if [ "$state" = done ]; then
	# A turn that ends under the user's eyes has nothing left to catch up on.
	visible=$(tmux display -p -t "$TMUX_PANE" '#{&&:#{pane_active},#{&&:#{window_active},#{session_attached}}}')
	[ "$visible" = 1 ] && state=idle
fi

if [ "$state" = off ]; then
	tmux set -pu -t "$TMUX_PANE" @claude-state
else
	tmux set -p -t "$TMUX_PANE" @claude-state "$state"
fi
