#!/bin/sh

grim -g "$(slurp -p)" -t ppm - \
    | magick convert - -format '%[pixel:p{0,0}]' txt:- \
    | { \
        D=$(cat); \
        notify-send -t 5000 Pick "$D"; \
        echo "$D" \
            | grep -o '#[0-9A-Za-z]\{6\}' \
            | tr -d '\n' \
            | wl-copy; \
    }
