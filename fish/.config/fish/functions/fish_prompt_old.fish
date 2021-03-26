set -g fish_prompt_pwd_dir_length 0

function _print_in_color
  set -l string $argv[1]
  set -l color  $argv[2]

  if test -n (string trim $string)
    set_color $color
    printf $string
    set_color normal
  end
end

function _prompt_color_for_status
  if test $argv[1] -eq 0
    echo magenta
  else
    echo red
  end
end

set -g __fish_git_prompt_show_informative_status
set -g __fish_git_prompt_showdirtystate
set -g __fish_git_prompt_showuntrackedfiles
set -g __fish_git_prompt_showupstream
set -g __fish_git_prompt_showcolorhints
set -g __fish_git_prompt_char_dirtystate '*'
set -g __fish_git_prompt_char_stagedstate +

function fish_prompt_old
  set -l last_status $status

  _print_in_color "\n"(prompt_pwd) blue

  fish_vcs_prompt

  # Show hostname if ssh
  if test -n "$SSH_CONNECTION"
      _print_in_color " $USER@$hostname" green
  end

  # Show last running time if greater than 5
  if test $CMD_DURATION -gt 5000
      _print_in_color " "(display_time (math $CMD_DURATION / 1000)) yellow
  end

  _print_in_color "\n❯ " (_prompt_color_for_status $last_status)
end
