#!/usr/bin/sh

WINDOWS=$(swaymsg -t get_tree | jq -re '.nodes | .[] | .nodes | . [] | select(.nodes != null) | .nodes | .[] | select(.name != null) | [.id, .name, (.window_properties.class? | select(.!= null)), .app_id? | select(.!=null)] | "\(.[0])\t \(.[2]) \t \(.[1])"')

COLUMNS=$(echo "$WINDOWS" | column -t -s '	')

TARGET=$(echo "$COLUMNS" | wofi --dmenu --cache-file /dev/null --prompt "Switch to window" --insensitive --location top --lines 10)

if [ -n "$TARGET" ]; then
    ID=$(echo "$TARGET" | awk '{print $1}')
    swaymsg [con_id="$ID"] focus
fi
