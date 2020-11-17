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

function git-default-branch
    git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'
end

abbr -ag gst "git status"
abbr -ag gco "git checkout"
abbr -ag gc "git commit -v"
abbr -ag gca "git commit -va"
abbr -ag gp "git push"
abbr -ag ga "git add --all"
abbr -ag gd "git diff HEAD | vim +'set buftype=nofile' -"
abbr -ag gdm "git diff (git-default-branch) | vim +'set buftype=nofile' -"
abbr -ag gdms "git diff (git-default-branch) --stat"
abbr -ag gcm "git checkout (git-default-branch); git pull"
abbr -ag gb "git branch"
abbr -ag gbs "git checkout (git branch | cut -c 3- | fzf)"
abbr -ag gdf "git diff (git merge-base --fork-point (git-default-branch)) | vim +'set buftype=nofile' -"
abbr -ag gdfs "git diff (git merge-base --fork-point (git-default-branch)) --stat"

abbr -ag vimless "vim +'set buftype=nofile' -"

abbr -ag rg "rg -S"

if test -z "$SSH_ENV"
    set -x SSH_ENV $HOME/.ssh/environment
end

if not __ssh_agent_is_started
    __ssh_agent_start
end

function __path_var
    if not string match -qe $argv[1] $PATH
        set -x PATH $argv[1] $PATH
    end
end

# device specific config
if test "$hostname" = hoyon-desktop
    __path_var /home/hoyon/.cargo/bin
    __path_var /home/hoyon/bin
    __path_var /home/hoyon/.local/bin
    __path_var /home/hoyon/.yarn/bin
    __path_var /home/hoyon/.nimble/bin
    __path_var /home/hoyon/.pi/pi/bin
    __path_var /home/hoyon/san/go/bin

    set -x GOPATH /home/hoyon/san/go
else if test "$hostname" = hoyon-work
    __path_var /home/hoyon/.local/bin
    __path_var /home/hoyon/.yarn/bin

else if test "$hostname" = hoyon-arch
    __path_var /home/hoyon/.local/bin
    __path_var /home/hoyon/.gem/ruby/2.7.0/bin

    source /opt/asdf-vm/asdf.fish
end
