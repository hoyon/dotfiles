function up --description 'bring system up to date'
    if test "$hostname" = "hoyon-desktop"
        yay -Syu --combinedupgrade $argv
    else if test "$hostname" = "hoyon-thinkpad"
        sudo apt-get update
        sudo apt upgrade
    end

    git -C ~/.dotfiles pull
    git -C ~/.emacs.d pull
end
