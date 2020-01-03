#!/bin/sh

# Script to open new terminal windows in the same directory if currently focused window is a terminal

terminator

# XPROP=$(xprop -id "$(xdotool getwindowfocus)")
# 
# # Get the window class
# CLASS=$(echo "$XPROP" | grep -oP "WM_CLASS\(STRING\)\ \=\ \"\K\w+")
# 
# if [ "$CLASS" = "terminator" ]; then
#     # If focused window is termite, get its PID
#     PID=$(echo "$XPROP" | grep -oP "_NET_WM_PID\(CARDINAL\)\ =\ \K\w+")
#     # Get the PID of shell so that it's cwd can be retrieved
#     CHILD_PID=$(pgrep -P "$PID" | grep fish)
#     CWD=$(readlink "/proc/$CHILD_PID/cwd")
#     nohup env GDK_BACKEND=x11 terminator -x /usr/bin/fish -d "$CWD" > /dev/null 2>&1 &
# else
#     # Focused window is not termite. Open termite in home directory
#     nohup env GDK_BACKEND=x11 terminator -x /usr/bin/fish > /dev/null 2>&1 &
# fi
