#!/bin/bash

LOG="/tmp/recur-sh---$1.log"
touch "$LOG"
COUNT=0
COUNT_LIMIT=3
PROC="$1"

handle_trap() {
	echo "~~~$ Recur trapped at $(date -Is)" >> "$LOG"
}
trap handle_trap EXIT

exit_on_limit() {
    echo "~~~~ Recur ending after restart count limit reached." >> "$LOG"
    notify-send -u critical "Recur for process '$PROC' ended after too many repeats."
    exit
}

recur() {
	echo "~~~^ Starting at $(date -Is)" >> "$LOG"
	"$@" >> "$LOG" 2>&1
	RC="$?"
	echo "~~~. Ending (rc = $RC) at $(date -Is)" >> "$LOG"
    COUNT=$((COUNT + 1))
    test "$COUNT" -gt "$COUNT_LIMIT" && exit_on_limit
	recur "$@"
}

case "x$1" in
	"x") echo "usage: command [args...]" ;;
	*) recur "$@" ;;
esac
