#!/bin/bash
{
    cat ~/.config/kanshi/config \
        | sed -En 's#^profile\s+([^\s]+)\s+.*$#kanshictl switch \1# p';
    echo "kanshictl reload";
} | "$@" | xargs -I {} -exec sh -c "{}"


