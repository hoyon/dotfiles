set fish_greeting ""

# Colourful man in less
set -x LESS_TERMCAP_mb (printf "\\e[1;91m")    # start blinking
set -x LESS_TERMCAP_md (printf "\\e[1;91m")    # start bold
set -x LESS_TERMCAP_me (printf "\\e[0m")       # end mode
set -x LESS_TERMCAP_so (printf "\\e[1;40;93m") # start standout
set -x LESS_TERMCAP_se (printf "\\e[0m")       # end standout
set -x LESS_TERMCAP_us (printf "\\e[1;92m")    # start underlining
set -x LESS_TERMCAP_ue (printf "\\e[0m")       # end underlining

set SHELL /usr/bin/fish
set -x PAGER less

if type -q nvim
    set -x EDITOR nvim
    set -x VISUAL nvim
else
    set -x EDITOR vim
    set -x VISUAL vim
end

set -x ERL_AFLAGS "-kernel shell_history enabled"

if type -q fd
  set -x FZF_DEFAULT_COMMAND 'fd --type f'
end

abbr -ag gst "git status"
abbr -ag gco "git checkout"
abbr -ag gc "git commit -v"
abbr -ag gca "git commit -va"
abbr -ag gp "git push"
abbr -ag ga "git add --all"
abbr -ag gd "git diff HEAD | vim +'set buftype=nofile' -"
abbr -ag gdm "git diff master | vim +'set buftype=nofile' -"
abbr -ag gdms "git diff master --stat"
abbr -ag gcm "git checkout master; git pull"
abbr -ag gb "git branch"
abbr -ag gbs "git checkout (git branch | cut -c 3- | fzf)"
abbr -ag gdf "git diff (git merge-base --fork-point master) | vim +'set buftype=nofile' -"
abbr -ag gdfs "git diff (git merge-base --fork-point master) --stat"

abbr -ag vimless "vim +'set buftype=nofile' -"

if test -z "$SSH_ENV"
    set -x SSH_ENV $HOME/.ssh/environment
end

if not __ssh_agent_is_started
    __ssh_agent_start
end

# device specific config
if test "$hostname" = "hoyon-desktop"
    set -x PATH /home/hoyon/.cargo/bin/ /home/hoyon/bin /home/hoyon/.local/bin /home/hoyon/.yarn/bin ~/.nimble/bin ~/.pi/pi/bin ~/san/go/bin $PATH
    set -x GOPATH /home/hoyon/san/go
else if test "$hostname" = "hoyon-work"
    set -x PATH /home/hoyon/.local/bin /home/hoyon/.yarn/bin $PATH
else if test "$hostname" = "penguin"
    source ~/.asdf/asdf.fish
end
