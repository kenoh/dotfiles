#!/bin/sh

name="$1"
which $name >/dev/null 2>&1 || swaynag --message "Message from $(realpath $0): Error running ${@}"
log="$HOME/.k-sway_exec-$name.log"

echo "STARTING at $(date)" >> $log
trap "echo \"STOPPING at \$(date)\" >> $log" EXIT
"${@}" >> $log 2>&1 ; echo "PROCESS EXITED with return code $?." >> $log
