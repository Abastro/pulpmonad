#!/bin/sh
config="$XMONAD_CONFIG_DIR"
cache="$XMONAD_CACHE_DIR"
xmonad_name="xmonad-$ENV_ARCH-$ENV_OS"

xdgcfg="$HOME/.config"
export GTK2_RC_FILES="$xdgcfg/gtk-2.0/.gtkrc-2.0"

chmod +x "$config/hook.sh"
"$config/hook.sh" &

echo "Running XMonad"
exec "$cache/$xmonad_name" > "$XMONAD_LOG_DIR/xmonad.log" 2> "$XMONAD_LOG_DIR/xmonad.err"
