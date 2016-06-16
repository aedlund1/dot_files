#! /bin/bash

xrandr --output DP-2 --auto --rotate left 
xrandr --output DP-1 --auto --right-of DP-2 
feh --bg-center --randomize /home/$USER/Images/backgrounds/* 
#done
