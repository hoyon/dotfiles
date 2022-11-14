set fish_greeting ""

# Colourful man in less
set -x LESS_TERMCAP_mb (printf "\\e[1;91m")    # start blinking
set -x LESS_TERMCAP_md (printf "\\e[1;91m")    # start bold
set -x LESS_TERMCAP_me (printf "\\e[0m")       # end mode
set -x LESS_TERMCAP_so (printf "\\e[1;40;93m") # start standout
set -x LESS_TERMCAP_se (printf "\\e[0m")       # end standout
set -x LESS_TERMCAP_us (printf "\\e[1;92m")    # start underlining
set -x LESS_TERMCAP_ue (printf "\\e[0m")       # end underlining

set SHELL (which fish)
set -x PAGER less

if type -q nvim
    set -x EDITOR nvim
    set -x VISUAL nvim
else
    set -x EDITOR vim
    set -x VISUAL vim
end

set -x ERL_AFLAGS "-kernel shell_history enabled"

set -x THEFUCK_PRIORITY "git_hook_bypass=1100"

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
abbr -ag gfm "git fetch origin (git-default-branch):(git-default-branch)"

abbr -ag vimless "vim +'set buftype=nofile' -"
abbr -ag rg "rg -S"
abbr -ag tf "terraform"
abbr -ag kssh "kitty +kitten ssh"


if test -z "$SSH_ENV"
    set -x SSH_ENV $HOME/.ssh/environment
end

if not __ssh_agent_is_started
    __ssh_agent_start
end

# set fish opts if ls --color=auto is supported
if command ls --color=auto / >/dev/null 2>/dev/null
    if command ls --hyperlink=auto / >/dev/null 2>/dev/null
        set -g __fish_ls_color_opt --color=auto --hyperlink=auto --group-directories-first
    else
        set -g __fish_ls_color_opt --color=auto
    end
end

set -x BAT_THEME ansi

# device specific config
if test "$hostname" = hoyon-work
    fish_add_path -g /home/hoyon/.yarn/bin

else if test "$hostname" = Ho-Yons-MacBook-Pro.local
    eval (/opt/homebrew/bin/brew shellenv)
    fish_add_path -g ~/.cargo/bin
    fish_add_path -g (python3 -m site --user-base)/bin
end

if test -e ~/.asdf/asdf.fish
    source ~/.asdf/asdf.fish
else if test -e /opt/asdf-vm/asdf.fish
    source /opt/asdf-vm/asdf.fish
end

if test -d ~/.local/bin
    fish_add_path -g ~/.local/bin
end

if test -d ~/.nimble/bin
    fish_add_path -g ~/.nimble/bin
end

# fzf.fish bindings
set --universal fzf_fish_custom_keybindings
bind \ct _fzf_search_directory

# fish colours
set -g fish_color_command brgreen
set -g fish_color_comment yellow
set -g fish_color_error brred
set -g fish_color_param brblue
set -g fish_color_operator brcyan
set -g fish_color_redirection bryellow

if type -q starship
    starship init fish | source
end

# switch to /bin/sh for dumb terminals (eg emacs tramp mode)
if test "$TERM" = "dumb"
    exec /bin/sh
end

