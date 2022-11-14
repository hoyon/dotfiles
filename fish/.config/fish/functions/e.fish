function e --description="Terminal emacs"
    if test "$TERM" = "xterm-kitty"
        kitty @ set-window-title --temporary "emacs -nw $argv"
    end

    if type -q systemctl && systemctl --user --quiet is-active emacs.service
        env TERM=xterm-24bits emacsclient -nw $argv
    else
        env TERM=xterm-24bits emacs -nw $argv
    end
end

