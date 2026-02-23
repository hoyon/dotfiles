function gwts --description "Switch to a different git worktree"
    set -l current_root (git rev-parse --show-toplevel 2>/dev/null)

    if test (count $argv) -gt 0
        set -l path (git worktree list | awk -v b="[$argv[1]]" '$NF == b {print $1}')
        if test -z "$path"
            echo "No worktree found for branch: $argv[1]"
            return 1
        end
        cd "$path"
    else
        set -l worktree (git worktree list | grep -v "^$current_root " | fzf --prompt="Switch to worktree: " | awk '{print $1}')
        if test -z "$worktree"
            return 1
        end
        cd "$worktree"
    end
end
