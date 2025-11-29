#!/bin/bash

if [ "$1" = "--help" ] || [ "$1" = "-h" ] || [ -z "$1" ]; then
    echo "Usage: brightness.sh <action> [value]"
    echo ""
    echo "Arguments:"
    echo "  action    increase or decrease"
    echo "  value     brightness adjustment amount (default: 8)"
    echo ""
    echo "Example: brightness.sh increase 10"
    exit 0
fi

action="$1"
value="${2:-8}"

if [ "$action" = "increase" ]; then
    ddcutil --display=1 setvcp 10 + "$value" &
    ddcutil --display=2 setvcp 10 + "$value" &
elif [ "$action" = "decrease" ]; then
    ddcutil --display=1 setvcp 10 - "$value" &
    ddcutil --display=2 setvcp 10 - "$value" &
fi

wait
