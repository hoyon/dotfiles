complete --no-files --command gwts --arguments '(git worktree list | tail -n +2 | awk -F"[][]" \'{print $2}\')'
