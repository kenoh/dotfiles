#!/bin/sh

name="$1"
logprefix=/tmp

session="$DESKTOP_SESSION"
case "$session" in
	i3)
		nag=i3-nagbar
		;;
	sway)
		nag=swaynag
		;;
esac

which $name >/dev/null 2>&1 || swaynag --message "Message from $(realpath $0): Error running ${@}"
log="$logprefix/sway-exec-wrapper---$session---${name//\//___}.log"

echo "STARTING at $(date)" >> $log
trap "echo \"STOPPING at \$(date)\" >> $log" EXIT
#set -x
exitcode=-1
"${@}" 2>&1 | \
	while read X; do 
		D="$(date -Iseconds)";
		echo "$X" | sed -u "s/^/[$D]  /" >> $log;
	done;
pipe0="${PIPESTATUS[0]}";

echo "PROCESS EXITED with return code $pipe0." >> $log
exit $pipe0
