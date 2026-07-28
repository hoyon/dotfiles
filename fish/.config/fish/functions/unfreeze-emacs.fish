function unfreeze-emacs --description "Break a wedged GUI Emacs out of a stuck NSApp event loop with SIGUSR2"
    set -l pids $argv

    if test (count $pids) -eq 0
        set pids (pgrep -x Emacs)
    end

    if test (count $pids) -eq 0
        echo "No Emacs process found."
        return 1
    end

    if test (count $pids) -gt 1
        echo "Multiple Emacs processes found; pass one explicitly:"
        ps -o pid,etime,command -p (string join , $pids)
        return 1
    end

    echo "Sending SIGUSR2 to Emacs (pid $pids)"
    kill -USR2 $pids; or return 1
    echo "Press q in the frame to dismiss the debugger."
end
