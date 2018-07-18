set fish_greeting ""

function zathura
    command nohup zathura $argv >/dev/null 2>&1 &
end

function evince
    command nohup evince $argv >/dev/null 2>&1 &
end

function ec --description="Emacsclient"
    command emacsclient -n $argv >/dev/null
    wmctrl -x -a emacs
    #if pgrep i3
        #command i3-msg '[title="emacs@hoyon-desktop"] focus' >/dev/null 2>&1
    #end
end

function pc --description="fishy pkg-config"
    command pkg-config $argv | string split " "
end

function nohup
    command nohup $argv > /dev/null 2>&1 &
end

function randman
    apropos . | shuf -n 1 | awk '{print $1}' | xargs man
end

function f
    command nohup nautilus . > /dev/null 2>&1 &
end

# Colourful man in less
set -x LESS_TERMCAP_mb (printf "\033[01;31m")  
set -x LESS_TERMCAP_md (printf "\033[01;31m")  
set -x LESS_TERMCAP_me (printf "\033[0m")  
set -x LESS_TERMCAP_se (printf "\033[0m")  
set -x LESS_TERMCAP_so (printf "\033[01;44;33m")  
set -x LESS_TERMCAP_ue (printf "\033[0m")  
set -x LESS_TERMCAP_us (printf "\033[01;32m")

set PATH /home/hoyon/.cargo/bin/ /home/hoyon/.go/bin/ /home/hoyon/bin /home/hoyon/.gem/ruby/2.5.0/bin/ /home/hoyon/.local/bin $PATH

set SHELL /usr/bin/fish
set -x EDITOR vim
set -x VISUAL vim
set -x GOPATH /home/hoyon/.go
set -x RUSTC_WRAPPER /home/hoyon/.cargo/bin/sccache

alias ls "exa --group-directories-first --color=auto --colour-scale"
alias l "ls"
alias ll "ls -l"
alias la "ls -la"
alias time "time --portability"
alias pgrep "pgrep -l"
alias gst "git status"
alias tty-clock "tty-clock -bDBc"
alias make "make -j4"
alias tree "exa -T --colour=always --colour-scale | less -Fr"
alias t "tree"
alias vim "nvim"
alias gdb "gdb -q"
alias bc "bc -ql"

function fish_user_key_bindings
    fzf_key_bindings
end

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

# asdf
source ~/.asdf/asdf.fish
