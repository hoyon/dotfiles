function ec --description Emacsclient
    set file (echo $argv | cut -d':' -f1)
    set line (echo $argv | cut -d':' -f2 -s)

    if test -n "$line"
        set line_flag "+$line"
    end

    command emacsclient -n $line_flag $file >/dev/null
    if test "$status" -eq 0
        if type -fq sway
            swaymsg "[title=\"emacs\"] focus"
        else
            wmctrl -x -a emacs
        end
    end
end
