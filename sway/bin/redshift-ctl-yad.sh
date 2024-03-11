#!/bin/bash
RS="redshift-ctl.sh"
yad \
    --use-interp='sh -c "%s"' \
    --button="cold:$RS 6500" \
    --button="hot:$RS 2600" \
    --button="dim:$RS 0.6" \
    --button="undim:$RS 1.0" \
    --button='QUIT:kill $YAD_PID'
