#!/bin/sh

set -e
set -x

if [ -f stow-skel-dirs ]; then
    xargs -a stow-skel-dirs -I{} mkdir -pv ../{}
fi

stow "$@"
