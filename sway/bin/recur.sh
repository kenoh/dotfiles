#!/bin/bash

LOG="$HOME/tmp/$1.log"
touch "$LOG"

handle_trap() {
	echo "~~~$ Recur trapped at $(date -Is)" >> "$LOG"
}
trap handle_trap EXIT

recur() {
	echo "~~~^ Starting at $(date -Is)" >> "$LOG"
	"$@" >> "$LOG" 2>&1
	RC="$?"
	echo "~~~. Ending (rc = $RC) at $(date -Is)" >> "$LOG"
	recur "$@"
}

case "x$1" in
	"x") echo "usage: command [args...]" ;;
	*) recur "$@" ;;
esac
