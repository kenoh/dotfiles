#!/bin/sh
case "$1" in
    dark|light)
        ln -fs $1.conf $HOME/.config/kitty/current-theme.conf && kitten @ load-config || sleep 99999
        ;;
    *)
        echo ERROR: unsupported argument. See contents of "$0". >&2
        ;;
esac
