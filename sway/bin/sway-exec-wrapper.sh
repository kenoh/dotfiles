#!/bin/sh

name="$1"
which $name >/dev/null 2>&1 || swaynag --message "Message from $(realpath $0): Error running ${@}"
log="$HOME/.k-sway_exec-$name.log"

echo "STARTING at $(date)" >> $log
trap "echo \"STOPPING at \$(date)\" >> $log" EXIT
set -x
"${@}" 2>&1 | while read X; do D="$(date -Iseconds)"; echo "$X" | sed -u "s/^/[$D]  /" >> $log; done; echo "PROCESS EXITED with return code $?." >> $log
