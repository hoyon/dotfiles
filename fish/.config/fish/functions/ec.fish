function ec --description="Open in emacs"
    if test -z "$argv"
        echo "Usage: ec filename[:line]"
        return 1
    end

    set file (echo $argv | cut -d':' -f1)
    set line (echo $argv | cut -d':' -f2 -s)

    if test -n "$line" && ! test "$line" -ge 0 2> /dev/null
        echo "Invalid line number"
        return 1
    end

    if test -n "$line"
        set line_flag "+$line"
    end

    command emacsclient -n $line_flag $file >/dev/null
    if test "$status" -eq 0
        if type -fq sway
            swaymsg "[title=\"Emacs\"] focus" > /dev/null || swaymsg "[class=\"Emacs\"] focus" > /dev/null
        else
            wmctrl -x -a emacs
        end
    end
end
