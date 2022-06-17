#!/bin/sh
cfgid="pulpmonad"
barid="pulp-taskbar"

config="$XMONAD_CONFIG_DIR"
cache="$XMONAD_CACHE_DIR"
data="$XMONAD_DATA_DIR"
xmonad_name="xmonad-$ENV_ARCH-$ENV_OS"

# Cabal installation
cabal install "exe:$cfgid" "exe:$barid" \
  --installdir="$cache" --install-method=copy \
  --overwrite-policy=always
ln -sf "$cache/$cfgid" "$cache/$xmonad_name"

# Copy configurations
xdgcfg="$HOME/.config"
cp -T "$config/cfg/.gtkrc-2.0" "$xdgcfg/gtk-2.0/.gtkrc-2.0"
cp -T "$config/cfg/settings.ini" "$xdgcfg/gtk-3.0/settings.ini"

# Copy assets & styles
cp -R "$config/asset" "$data"
cp -R "$config/styles" "$data"
