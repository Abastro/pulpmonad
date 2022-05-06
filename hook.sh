#!/bin/sh
status-notifier-watcher &
pasystray &
nm-applet --sm-disable --indicator &
blueman-applet &
synapse -s &

# SSH key
export SSH_ASKPASS="/usr/bin/ssh-askpass"
ssh-add &
