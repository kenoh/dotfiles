#!/bin/sh

function vpn {
  vpn="$(ifconfig | grep "tun0")"
  if [ -n "$vpn" ]; then
    vpn="🔒"
    class=on
  else
    vpn="🔓"
    class=off
  fi
   printf '{"text": "%s", "tooltip": "", "class": "%s"}\n' "$vpn" "$class"

 }

 vpn

 ip monitor link | while read -r line; do vpn; done

 # credits: https://github.com/valebes/Dots/blob/7803a0715be43c520eeddbafdefca9844818f4ed/.config/waybar/waybar-vpn
