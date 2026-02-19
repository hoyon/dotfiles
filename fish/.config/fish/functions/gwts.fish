function gwts --description "Switch to a different git worktree"
    set -l current_root (git rev-parse --show-toplevel 2>/dev/null)
    set -l worktree (git worktree list | grep -v "^$current_root " | fzf --prompt="Switch to worktree: " | awk '{print $1}')
    if test -z "$worktree"
        return 1
    end

    cd "$worktree"
end
