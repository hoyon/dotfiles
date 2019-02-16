#!/bin/sh

# Script to open new terminal windows in the same directory if currently focused window is a terminal

XPROP=$(xprop -id $(xdotool getwindowfocus))

# Get the window class
CLASS=$(echo "$XPROP" | grep -oP "WM_CLASS\(STRING\)\ \=\ \"\K\w+")

if [ "$CLASS" = "termite" ]; then
    # If focused window is termite, get it's PID
    PID=$(echo "$XPROP" | grep -oP "_NET_WM_PID\(CARDINAL\)\ =\ \K\w+")
    # Get the PID of zsh so that it's cwd can be retrieved
    CHILD_PID=$(pgrep -P $PID)
    CWD=$(readlink "/proc/$CHILD_PID/cwd")
    nohup env GDK_BACKEND=x11 termite -e "/usr/bin/fish -C 'cd $CWD'" > /dev/null 2>&1 &
else
    # Focused window is not termite. Open termite in home directory
    nohup env GDK_BACKEND=x11 termite -e /usr/bin/fish > /dev/null 2>&1 &
fi
