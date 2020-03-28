function e
    kitty @ set-window-title --temporary "emacs"
    env TERM=xterm-24bits emacs -nw $argv
end

