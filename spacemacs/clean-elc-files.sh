#!/bin/bash
test -d .emacs.d || exit 2

cd .emacs.d
find -type f -name '*.elc' | xargs rm
