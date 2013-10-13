# When exiting the topmost shell, clear the console
if [ "$SHLVL" = 1 ] && type clear &>/dev/null
then
    clear
fi
