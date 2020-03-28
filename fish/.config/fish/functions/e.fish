function e --description="Terminal emacs"
    if test "$TERM" = "xterm-kitty"
        kitty @ set-window-title --temporary "emacs"
    end
    env TERM=xterm-24bits emacs -nw $argv
end

