#!/bin/sh
cfg="$XMONAD_CONFIG_DIR"

if ! command -v status-notifier-watcher; then
  cabal install status-notifier-item
fi

# TODO Fonts?

# Copy greeter badge in place
if [ -d "/usr/share/slick-greeter/badges" ]; then
  sudo cp "$cfg/asset/badge/pulpmonad.svg" "/usr/share/slick-greeter/badges"
  sudo chmod a+r "/usr/share/slick-greeter/badges/pulpmonad.svg"
fi
