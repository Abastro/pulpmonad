#!/bin/sh
cfgid="pulpmonad"
barid="pulpbar"

cache="$XMONAD_CACHE_DIR"
xmonad_name="xmonad-$ENV_ARCH-$ENV_OS"

cabal install "exe:$cfgid" "exe:$barid" \
  --installdir="$cache" --install-method=copy \
  --overwrite-policy=always
ln -sf "$cache/$cfgid" "$cache/$xmonad_name"
