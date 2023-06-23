#!/bin/sh

echo "Starting hooks"

# Status notifier watcher for the taskbar
if [ -z "$(pgrep status-notifier)" ] ; then
  status-notifier-watcher &
fi

# NetworkManager applet
if [ -z "$(pgrep nm-applet)" ] ; then
  nm-applet --sm-disable --indicator &
fi

# BlueMan applet
if [ -z "$(pgrep blueman-applet)" ] ; then
  blueman-applet &
fi

# Application Launcher
if [ -z "$(pgrep synapse)" ] ; then
  synapse -s &
fi

# Clipboard Manager
if [ -z "$(pgrep parcellite)" ] ; then
  parcellite -d &
fi

# SSH key
export SSH_ASKPASS="/usr/bin/ssh-askpass"
ssh-add &
