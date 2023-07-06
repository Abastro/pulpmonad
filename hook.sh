#!/bin/sh

echo "Starting hooks"

# NetworkManager applet
# TODO Somehow does not register correctly on first run after boot 
if [ -z "$(pgrep nm-applet)" ] ; then
  nm-applet --sm-disable --indicator &
fi

# Blueman applet
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
