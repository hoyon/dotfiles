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

# --- PATH setup ---

if test -d /opt/homebrew; and not set -q HOMEBREW_PREFIX
    eval (/opt/homebrew/bin/brew shellenv)
end

set _extra_paths \
    ~/.local/bin \
    ~/.cargo/bin \
    /opt/homebrew/opt/rustup/bin \
    ~/go/bin \
    ~/.nimble/bin \
    ~/.yarn/bin

for p in $_extra_paths
    if test -d $p
        fish_add_path -g $p
    end
end
set --erase _extra_paths

# asdf — must be last to ensure shims take priority
if test -e ~/.asdf/asdf.fish
    source ~/.asdf/asdf.fish
else if test -e /opt/asdf-vm/asdf.fish
    source /opt/asdf-vm/asdf.fish
end

if type -q asdf
    if test -z $ASDF_DATA_DIR
        set _asdf_shims "$HOME/.asdf/shims"
    else
        set _asdf_shims "$ASDF_DATA_DIR/shims"
    end

    # Remove and re-prepend to maintain correct ordering in nested shells (eg tmux)
    if set -l idx (contains -i -- $_asdf_shims $PATH)
        set -e PATH[$idx]
    end
    set -gx --prepend PATH $_asdf_shims
    set --erase _asdf_shims
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
