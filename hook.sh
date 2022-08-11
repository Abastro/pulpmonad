#!/bin/sh

echo "Starting hooks"

if [ -z "$(pgrep status-notifier)" ] ; then
  status-notifier-watcher &
fi

if [ -z "$(pgrep pasystray)" ] ; then
  pasystray &
fi

if [ -z "$(pgrep nm-applet)" ] ; then
  nm-applet --sm-disable --indicator &
fi

if [ -z "$(pgrep blueman-applet)" ] ; then
  blueman-applet &
fi

if [ -z "$(pgrep synapse)" ] ; then
  synapse -s &
fi

if [ -z "$(pgrep parcellite)" ] ; then
  parcellite -d &
fi

# SSH key
export SSH_ASKPASS="/usr/bin/ssh-askpass"
ssh-add &
