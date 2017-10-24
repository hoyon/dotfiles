#!/bin/zsh

if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
    return
fi

# emacsclient alias function
ec() {
    emacsclient "$@" >/dev/null &!;
    # Change focus to emacs window
    i3-msg '[title="emacs@hoyon-desktop"] focus' > /dev/null;
    #if [ $? -ne 0 ]; then
	#echo "Open emacs first"
    #fi
}

# open nvim if it exists, else open vim
vim() {
    if hash nvim 2>/dev/null; then
        nvim "$@"
    else
        command vim "$@"
    fi
}

# always open zathura in seperate process
zathura() {
    command zathura "$@" >/dev/null 2>&1 &!;
}

## fzf
# fh - repeat history
fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}

# fkill - kill process
fkill() {
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    kill -${1:-9} $pid
  fi
}

# colourful man pages
man() {
    LESS_TERMCAP_md=$'\e[01;31m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    command man "$@"
}

# Exports
export EDITOR='nvim'
export VISUAL='nvim'
export BROWSER='firefox'
export PAGER='less'
export MANPAGER='less'

export GCC_COLORS=auto

export PATH=/home/hoyon/.local/bin:/home/hoyon/dotfiles/bin:/home/hoyon/.cargo/bin:$PATH
export RUST_SRC_PATH=/usr/src/rust/src
export CARGO_HOME=/home/hoyon/.cargo

export FZF_DEFAULT_COMMAND='ag -g ""'

export NVIM_TUI_ENABLE_CURSOR_SHAPE=1

export HISTSIZE=8192
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_DUPS

export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

export VDPAU_DRIVER=radeonsi

# Use /bin/time instead of zsh built in time
disable -r time

# Disable hanging terminal with C-s
stty -ixon

# Colorful ls
eval $(dircolors ~/.dircolors)

# init autocompletion
fpath+=~/dotfiles/zsh/completions
autoload -U compinit
compinit

# Enable zmv
autoload -U zmv

# Use emacs mode. Vi mode on the command line confuses me
bindkey -e
