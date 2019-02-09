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

set -x GPODDER_HOME ~/.config/gpodder
set -x GPODDER_DOWNLOAD_DIR ~/Stuff/Podcasts

if type -q fd
  set -x FZF_DEFAULT_COMMAND 'fd --type f'
end

abbr -a gst "git status"
abbr -a gco "git checkout"
abbr -a gc "git commit -v"
abbr -a gca "git commit -va"
abbr -a gp "git push"
abbr -a ga "git add --all"
abbr -a gd "git diff HEAD | vim +'set buftype=nofile' -"

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
    setenv SSH_ENV $HOME/.ssh/environment
end

set -g _host "$USER@"(cat /etc/hostname)

if not __ssh_agent_is_started
    __ssh_agent_start
end

if test (hostname) = "hoyon-desktop"
    set -x PATH /home/hoyon/.cargo/bin/ /home/hoyon/bin /home/hoyon/.local/bin $PATH
else if test (hostname) = "hoyon-thinkpad"
    set -x PATH /home/hoyon/.local/bin $PATH
end
