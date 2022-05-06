#!/bin/sh
cache="$XMONAD_CACHE_DIR"
xmonad_name="xmonad-$ENV_ARCH-$ENV_OS"

exec "$cache/$xmonad_name" > "$XMONAD_LOG_DIR/xmonad.log" 2> "$XMONAD_LOG_DIR/xmonad.err"
