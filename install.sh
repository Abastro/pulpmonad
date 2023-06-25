#!/bin/sh
cfg="$XMONAD_CONFIG_DIR"

# TODO Fonts?

# Copy greeter badge in place
if [ -d "/usr/share/slick-greeter/badges" ]; then
  echo "Inserting greeter badges."
  sudo cp "$cfg/asset/badge/pulpmonad.svg" "/usr/share/slick-greeter/badges"
  sudo chmod a+r "/usr/share/slick-greeter/badges/pulpmonad.svg"
fi
