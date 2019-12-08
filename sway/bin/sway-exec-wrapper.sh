#!/bin/sh

name="$1"
which $name >/dev/null 2>&1 || swaynag --message "Message from $(realpath $0): Error running ${@}"
log="$HOME/.k-sway_exec-$name.log"

echo "STARTING at $(date)" >> $log
"${@}" >> $log 2>&1
echo "STOPPING at $(date)" >> $log
