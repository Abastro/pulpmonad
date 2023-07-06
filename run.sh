#!/bin/sh
# Deprecated

XMONAD_EXE_PATH="$XMONAD_CACHE_DIR/xmonad-$ENV_ARCH-$ENV_OS"

cp -T "$XMONAD_LOG_DIR/xmonad.log" "$XMONAD_LOG_DIR/xmonad-1.log"
cp -T "$XMONAD_LOG_DIR/xmonad.err" "$XMONAD_LOG_DIR/xmonad-1.err"

"$XMONAD_EXE_PATH" > "$XMONAD_LOG_DIR/xmonad.log" 2> "$XMONAD_LOG_DIR/xmonad.err"
