# Defined in /home/hoyon/.config/fish/config.fish @ line 11
function ec --description Emacsclient
	command emacsclient -n $argv >/dev/null
    wmctrl -x -a emacs
end
