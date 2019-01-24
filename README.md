# My Dotfiles

Use GNU Stow to install configs for programs

Repo must be cloned in subdirectory of home folder, for example `~/.dotfiles`

To install configs for a program, `cd` into the dotfiles directory and run
```
stow <directory>
```

Stow will create symlinks corresponding to files inside the corresponding
program directory in the parent directory.
