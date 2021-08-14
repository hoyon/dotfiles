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

# set fish opts if ls --color=auto is supported
if command ls --color=auto / >/dev/null 2>/dev/null
    set -g __fish_ls_color_opt --color=auto --hyperlink=auto
end

# device specific config
if test "$hostname" = hoyon-desktop
    fish_add_path /home/hoyon/.cargo/bin
    fish_add_path /home/hoyon/bin
    fish_add_path /home/hoyon/.local/bin
    fish_add_path /home/hoyon/.yarn/bin
    fish_add_path /home/hoyon/.nimble/bin
    fish_add_path /home/hoyon/.pi/pi/bin
    fish_add_path /home/hoyon/san/go/bin

    set -x GOPATH /home/hoyon/san/go
else if test "$hostname" = hoyon-work
    fish_add_path /home/hoyon/.local/bin
    fish_add_path /home/hoyon/.yarn/bin

else if test "$hostname" = hoyon-arch
    fish_add_path /home/hoyon/.local/bin
    fish_add_path /home/hoyon/.gem/ruby/2.7.0/bin

    source /opt/asdf-vm/asdf.fish
end

# fzf.fish bindings
set --universal fzf_fish_custom_keybindings
bind \ct '_fzf_search_directory'

# tide prompt
set -g tide_character_color magenta
set -g tide_pwd_color_dirs cyan
set -g tide_pwd_color_anchors brcyan
