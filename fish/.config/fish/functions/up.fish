function up --description 'bring system up to date'
    set host (hostname)

    if test "$host" = "hoyon-desktop"
        yay -Syu --combinedupgrade $argv
    else if test "$host" = "hoyon-thinkpad"
        sudo apt-get update
        sudo apt upgrade
    end

    git -C ~/.dotfiles pull
    git -C ~/.emacs.d pull
end
