#!/bin/bash

# Assumes only 1 Spotify window is open.
spotify_id=""
function handle {
    # echo "HIRAM: $1" | systemd-cat
    if [[ ${1:0:10} == "openwindow" && $1 == *"Spotify"* ]]; then
        spotify_id=$(echo ${1:12} | cut -d, -f1)
        pkill -RTMIN+5 waybar
    elif [[ ${1:0:11} == "closewindow" && ${1:13} == $spotify_id ]]; then
        spotify_id=""
        # Wait for the process to fully exit before reloading waybar.
        sleep 1
        pkill -RTMIN+5 waybar
    fi
}

#echo "HIRAM: STARTING" | systemd-cat
socat - "UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock" | while read -r line; do handle "$line"; done
