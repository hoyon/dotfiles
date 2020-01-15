set fish_greeting ""

# Colourful man in less
set -x LESS_TERMCAP_mb (printf "\033[01;31m")
set -x LESS_TERMCAP_md (printf "\033[01;31m")
set -x LESS_TERMCAP_me (printf "\033[0m")
set -x LESS_TERMCAP_se (printf "\033[0m")
set -x LESS_TERMCAP_so (printf "\033[01;44;33m")
set -x LESS_TERMCAP_ue (printf "\033[0m")
set -x LESS_TERMCAP_us (printf "\033[01;32m")

set SHELL /usr/bin/fish
set -x EDITOR vim
set -x VISUAL vim

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
abbr -ag gdf "git diff (git merge-base --fork-point master) | vim +'set buftype=nofile' -"
abbr -ag gdfs "git diff (git merge-base --fork-point master) --stat"

# Event hooks to calculate command running time
function _undistract_begin --on-event fish_preexec
    set -g _undistract_timestamp (date +%s)
end

function _undistract_end --on-event fish_postexec
    set -g _undistract_last (math (date +%s) - $_undistract_timestamp)
    if test $_undistract_last -gt 5
        printf "\a"
    end
end

if test -z "$SSH_ENV"
    set -x SSH_ENV $HOME/.ssh/environment
end

if not __ssh_agent_is_started
    __ssh_agent_start
end

# device specific config
if test "$hostname" = "hoyon-desktop"
    set -x PATH /home/hoyon/.cargo/bin/ /home/hoyon/bin /home/hoyon/.local/bin /home/hoyon/.yarn/bin ~/.nimble/bin $PATH
else if test "$hostname" = "hoyon-work"
    set -x PATH /home/hoyon/.local/bin /home/hoyon/.yarn/bin $PATH
else if test "$hostname" = "penguin"
    source ~/.asdf/asdf.fish
end

source ~/.config/fish/nnn.fish
