#!/bin/sh

mullvad status listen | while read -r line; do
    status=$(echo $line | awk '{ print $3 }')
    case $status in
        Connected)
            hostname=$(curl --silent https://am.i.mullvad.net/json | jq -r '.mullvad_exit_ip_hostname')
            output=$hostname
            ;;
        Disconnected)
            output=""
            echo ""
            continue
            ;;
        *)
            output=$status
            ;;
    esac
    jq --null-input --unbuffered --compact-output --arg output "$output" '{text: $output}'
done
