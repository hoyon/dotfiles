#!/usr/bin/env sh

kill -USR1 "$(pgrep -f 'mediaplayer.py' | head -n 1)"
