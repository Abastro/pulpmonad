#!/bin/sh
PROF_ID="pulpmonad"
BAR_ID="pulp-taskbar"

config="$XMONAD_CONFIG_DIR"
data="$XMONAD_DATA_DIR"

cd "$config" || exit

# Cabal installation
cabal install "exe:$PROF_ID" "exe:$BAR_ID" \
  --installdir="$XMONAD_CACHE_DIR" --install-method=copy \
  --overwrite-policy=always
ln -sf "$XMONAD_CACHE_DIR/$PROF_ID" "$XMONAD_CACHE_DIR/$XMONAD_NAME"

# Copy assets & styles
cp -R "$config/asset" "$data"
cp -R "$config/styles" "$data"
cp -R "$config/ui" "$data"
