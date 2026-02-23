complete --no-files --command gwtd --arguments '(git worktree list | tail -n +2 | awk -F"[][]" \'{print $2}\')'
