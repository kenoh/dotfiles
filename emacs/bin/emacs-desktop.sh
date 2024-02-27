#!/bin/bash
set -x
set -e
trap "sleep 99999" ERR

FILENAME="$1"

msg() {
	notify-send "$1"
}

ec() {
	emacsclient -c -n -a "emacs --daemon --debug-init" -F "'(fullscreen . maximize)" "$@" || msg "Failed to start emacs via $0" && false
}

ec "$@"

# NOTE: removing the following since the -a param for emacsclient should do the job.
# ec "$1" || {
# 	{
# 		emacs --daemon --debug-init || msg "Could not run emacs daemon" && false
# 		ec "$1" || msg "Started an emacs daemon but connecting to it failed" && false
# 	} && {
# 		ec "$1" || msg "Started an emacs daemon but connecting to it failed" && false
# 	}
# }

