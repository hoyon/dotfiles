#!/bin/sh

cat <(playerctl status --follow & playerctl --player playerctld metadata --format '{{title}}' --follow) | while read -r line; do
    players=$(playerctl --list-all 2>&1)
    if [[ "$players" != "No players found" ]]; then
        playerctl --player playerctld metadata --format '{{status}}	{{ artist }} - {{ title }}	{{playerName}}' |\
            jq --raw-input --unbuffered --compact-output 'split("\t") | {status: .[0], text: ((if (.[0] == "Paused") then " " else "" end) + .[1])}'
    else
        echo ""
    fi
done
