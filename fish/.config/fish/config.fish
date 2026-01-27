set fish_greeting ""

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
set -x CPM_SOURCE_CACHE "$HOME/.cache/CPM" # CMake Package Manager
set -x GODOT "$HOME/.config/godotenv/godot/bin"

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

if test -d /opt/homebrew
    eval (/opt/homebrew/bin/brew shellenv)
    fish_add_path -g /opt/homebrew/opt/rustup/bin
    fish_add_path -g ~/.cargo/bin
    fish_add_path -g (python3 -m site --user-base)/bin
end

if test -e ~/.asdf/asdf.fish
    source ~/.asdf/asdf.fish
else if test -e /opt/asdf-vm/asdf.fish
    source /opt/asdf-vm/asdf.fish
end

if test -z $ASDF_DATA_DIR
    set _asdf_shims "$HOME/.asdf/shims"
else
    set _asdf_shims "$ASDF_DATA_DIR/shims"
end

# Do not use fish_add_path (added in Fish 3.2) because it
# potentially changes the order of items in PATH
if not contains $_asdf_shims $PATH
    set -gx --prepend PATH $_asdf_shims
end
set --erase _asdf_shims

if test -e /nix/var/nix/profiles/default/etc/profile.d/nix.fish
    source /nix/var/nix/profiles/default/etc/profile.d/nix.fish
    direnv hook fish | source
end

if test -d ~/.local/bin
    fish_add_path -g ~/.local/bin
end

if test -d ~/.nimble/bin
    fish_add_path -g ~/.nimble/bin
end

if test -d ~/kde/src/kdesrc-build
    fish_add_path -g ~/kde/src/kdesrc-build
end

if test -d ~/.dotnet/tools
    fish_add_path -g ~/.dotnet/tools
end

if test -d ~/go/bin
    fish_add_path -g ~/go/bin
end

if test -d ~/.yarn/bin
    fish_add_path -g ~/.yarn/bin
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

# fish-colored-man colours
set -g man_blink -o red
set -g man_bold -o magenta
set -g man_standout -b white 586e75
set -g man_underline -u 586e75

if type -q starship
    starship init fish | source
end

# switch to /bin/sh for dumb terminals (eg emacs tramp mode)
if test "$TERM" = "dumb"
    exec /bin/sh
end

if test -e ~/.config/fish/secrets.fish
    source ~/.config/fish/secrets.fish
end
