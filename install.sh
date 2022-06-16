#!/bin/sh
cfg="$XMONAD_CONFIG_DIR"

sudo apt install light pasystray network-manager-gnome blueman synapse ssh-askpass-gnome

if ! command -v status-notifier-watcher; then
  cabal install status-notifier-item
fi

# Copy greeter badge in place
if [ -d "/usr/share/slick-greeter/badges" ]; then
  sudo cp "$cfg/asset/badge/pulpmonad.svg" "/usr/share/slick-greeter/badges"
  sudo chmod a+r "/usr/share/slick-greeter/badges/pulpmonad.svg"
fi
