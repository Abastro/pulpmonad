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

# Copy configurations
xdgcfg="$HOME/.config"
cp -T "$config/cfg/.gtkrc-2.0" "$xdgcfg/gtk-2.0/.gtkrc-2.0"
cp -T "$config/cfg/gtk3-settings.ini" "$xdgcfg/gtk-3.0/settings.ini"
cp -T "$config/cfg/gtk4-settings.ini" "$xdgcfg/gtk-4.0/settings.ini"

# Copy assets & styles
cp -R "$config/asset" "$data"
cp -R "$config/styles" "$data"
cp -R "$config/ui" "$data"
