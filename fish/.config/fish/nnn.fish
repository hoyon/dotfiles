set -x NNN_USE_EDITOR 1
set -x NNN_BMS "d:~/Downloads/;c:~/san/code;s:~/Stuff"

# From https://github.com/jarun/nnn/blob/master/misc/quitcd/quitcd.fish
function n --description 'support nnn quit and change directory'
    # Block nesting of nnn in subshells
    if [ (expr $NNNLVL + 0) -ge 1 ]
        echo "nnn is already running"
        return
    end

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "-x" as in:
    #    set NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    if test -n "$XDG_CONFIG_HOME"
        set -x NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
    else
        set -x NNN_TMPFILE "$HOME/.config/nnn/.lastd"
    end

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn -e $argv

    if test -e $NNN_TMPFILE
        source $NNN_TMPFILE
        rm $NNN_TMPFILE
    end
end
