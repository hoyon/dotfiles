#!/bin/sh

playerctl --player playerctld metadata --format '{{status}}	{{ artist }} - {{ title }}	{{playerName}}' --follow | jq --raw-input --unbuffered --compact-output 'split("\t") | {status: .[0], text: ((if (.[0] == "Paused") then " " else "" end) + .[1])}'
