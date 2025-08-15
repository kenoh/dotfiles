#!/bin/bash
#
# Kenoh's SwayWM startup scripts manager.
#
# Run from Sway's config file (without args). The script makes a tmux server per Sway session.
#
# When run with args they are passed to the correct tmux server and session, e.g.:
#    ./startup.sh attach
# to interactively attach the session.
#
# In case you want to quickly restart the particular script, run (in that tmux window):
#    :respawnw
#
# 

export TERMINAL=kitty

TSRV="${SWAYSOCK##*/}"  # sock's filename without directories
TSES="0"  # tmux session (hardcoded, no need for more)

SELF=$(readlink -fn "$0")

DEADIGNORE=60
COUNT=0
COUNT_LIMIT=3
STARTED=$SECONDS

tm() {
	tmux -L "$TSRV" "${@}"
}

init() {
	echo "Make server..."
	tm has-session -t "$TSES" || tm new-session -d -s "$TSES"
	echo "Set remain-on-exit..."
	tm set-option -g remain-on-exit
	for f in ~/.config/sway/startup.d/*; do
		echo "Run $f"
		tm new-window -S -n "${f##*/}" "$SELF" "--run" "$f"
	done
}

log() {
	echo "${@}" >&2
}

run() {
	f="$1"; shift
	n="${f##*/}"; shift
	
	while true; do
		log "~~~^ Starting at $(date -Is)."
		"$f"
		RC="$?"
		log "~~~$ Died with RC = $RC at $(date -Is)."
		if [ "$(( ( SECONDS - STARTED ) < DEADIGNORE ))" = "1" ]; then
			COUNT=$(( COUNT + 1 ))
			if [ "$COUNT" -gt "$COUNT_LIMIT" ]; then
				swaynag \
				-m "$n failed!" \
				-Z $(printf '%q --attach %q' "$SELF" "$n")
				exit 1
			fi
			log "~~~@ Restarting with COUNT = $COUNT at $(date -Is)."
		else
			COUNT=0
			log "~~~@ Restarting with COUNT reset to 0 at $(date -Is)."
		fi
	done
}

attach() {
	tm attach-session -t "$TSES" \; find-window "$1"
}


case "$1" in
	"")
		init
		;;
	"--run")
		shift;
		run "${@}"
		;;
	"--attach")
		shift
		attach "${@}"
		;;
	*)
		tm "${@}"
		;;
esac
