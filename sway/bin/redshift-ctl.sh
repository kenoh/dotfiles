#!/bin/bash

set -euo pipefail

FN=~/.config/gammastep/config.ini

case "$1" in
    [0-1].[0-9])
        NAME=brightness-night
        VAL="$1"
        ;;
    [0-9][0-9][0-9][0-9])
        NAME=temp-night
        VAL="$1"
        ;;
    *)
        exit;
        ;;
esac
     
sed -i "s/^$NAME.*/$NAME=$VAL/" "$FN"
killall -w gammastep || true
# notify-send 'Gammastep status' "$(gammastep -p)"
nohup gammastep -P -r
