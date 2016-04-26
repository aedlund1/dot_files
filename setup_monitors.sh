#! /bin/bash

xrandr --output DP-2 --auto --rotate left 
xrandr --output DP-1 --auto --right-of DP-2 
find /home/$USER/images/ -type f -name "*.jpg" | sort -R | tail -n 1 | while read file; do
    feh --bg-fill $file
done
