#!/usr/bin/sh

WINDOWS=$(swaymsg -t get_tree | jq -r '.nodes | .[] | .nodes | . [] | select(.nodes != null) | .nodes | .[] | select(.name != null) | "\(.id?) \(.name?)"')

TARGET=$(echo "$WINDOWS" | wofi --dmenu --cache-file /dev/null --prompt "Switch to window" --insensitive)

if [ -n "$TARGET" ]; then
    ID=$(echo "$TARGET" | cut -d ' ' -f1)
    swaymsg [con_id="$ID"] focus
fi
