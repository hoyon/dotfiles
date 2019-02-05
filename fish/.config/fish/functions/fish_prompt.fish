set -g fish_prompt_pwd_dir_length 0

function _pwd_with_tilde
  #echo $PWD | sed 's|^'$HOME'\(.*\)$|~\1|'
  prompt_pwd
end

function _in_git_directory
  git rev-parse --git-dir > /dev/null 2>&1
end

function _git_branch_name_or_revision
  set -l branch (git symbolic-ref HEAD 2> /dev/null | sed -e 's|^refs/heads/||')
  set -l revision (git rev-parse HEAD 2> /dev/null | cut -b 1-7)

  if test (count $branch) -gt 0
    echo $branch
  else
    echo $revision
  end
end

function _git_tag
  set -l tag (git describe 2> /dev/null)
  if test (count $tag) -gt 0
      echo $tag
  else
      echo ""
  end
end

function _git_upstream_configured
  git rev-parse --abbrev-ref @"{u}" > /dev/null 2>&1
end

function _git_behind_upstream
  test (git rev-list --right-only --count HEAD...@"{u}" 2> /dev/null) -gt 0
end

function _git_ahead_of_upstream
  test (git rev-list --left-only --count HEAD...@"{u}" 2> /dev/null) -gt 0
end

function _git_upstream_status
  set -l arrows

  if _git_upstream_configured
    if _git_behind_upstream
      set arrows "$arrows⇣"
    end

    if _git_ahead_of_upstream
      set arrows "$arrows⇡"
    end
  end

  echo $arrows
end

function _git_is_dirty
    command git diff --shortstat --quiet --exit-code > /dev/null
end

function _git_untracked_files 
    set untracked (git ls-files --others --exclude-standard | wc -l)
    return $untracked
end

function _git_staged_files
    set files (git diff --staged --name-only | wc -l)
    return $files
end

function _git_dirty
    set -l flags
    if not _git_is_dirty
        set flags "$flags*"
    end
    if not _git_untracked_files
        set flags "$flags+"
    end
    if not _git_staged_files
        set flags "$flags!"
    end
    echo $flags
end

function _print_in_color
  set -l string $argv[1]
  set -l color  $argv[2]

  set_color $color
  printf $string
  set_color normal
end

function _prompt_color_for_status
  if test $argv[1] -eq 0
    echo magenta
  else
    echo red
  end
end

function fish_prompt
  set -l last_status $status

  _print_in_color "\n"(_pwd_with_tilde) blue

  if _in_git_directory
    _print_in_color " "(_git_branch_name_or_revision) grey
    _print_in_color " "(_git_tag) grey
    _print_in_color (_git_dirty) red
    _print_in_color " "(_git_upstream_status) cyan
  end

  # Show hostname if ssh
  if test -n "$SSH_CONNECTION"
      _print_in_color " [$_host]" green
  end

  # Show last running time if greater than 5
  set -q _undistract_last; or set _undistract_last 0
  if test $_undistract_last -gt 5
      _print_in_color " "(display_time $_undistract_last) yellow
  end

  _print_in_color "\n❯ " (_prompt_color_for_status $last_status)
end
