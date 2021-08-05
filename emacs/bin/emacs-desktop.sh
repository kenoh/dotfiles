#!/bin/bash
set -x
set -e
trap "sleep 99999" ERR

FILENAME="$1"

msg() {
	notify-send "$1"
}

ec() {
	/usr/bin/emacsclient -c -n -a "" -F "'(fullscreen . maximize)" $1
}

ec "$1" || {
	{
		emacs --daemon || msg "Could not run emacs daemon" && false
	} && {
		ec "$1" || msg "Started an emacs daemon but connecting to it failed" && false
	}
}

