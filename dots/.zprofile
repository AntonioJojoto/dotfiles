export PATH="$PATH:/home/tony/bin"
export PATH="$PATH:/home/tony/.emacs.d/bin"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi
