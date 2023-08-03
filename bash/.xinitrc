#!/bin/bash
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)


exec dbus-launch -mm emacs --debug-init --use-exwm
