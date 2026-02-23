function gwtd --description "Delete a git worktree using fzf"
    set -l current_root (git rev-parse --show-toplevel 2>/dev/null)
    set -l main_worktree (git worktree list | head -1 | awk '{print $1}')
    set -l worktrees

    if test (count $argv) -gt 0
        for name in $argv
            set -l path (git worktree list | awk -v b="[$name]" '$NF == b {print $1}')
            if test -z "$path"
                echo "No worktree found for branch: $name"
                return 1
            end
            set -a worktrees "$path"
        end
    else if test "$current_root" != "$main_worktree"
        read -l -P "Delete current worktree ($current_root)? [y/N] " confirm
        if test "$confirm" = y -o "$confirm" = Y
            set worktrees "$current_root"
        else
            echo "Aborted."
            return 1
        end
    else
        set -l worktree (git worktree list | tail -n +2 | fzf --prompt="Select worktree to delete: " | awk '{print $1}')
        if test -z "$worktree"
            return 1
        end
        set worktrees "$worktree"
    end

    if contains -- "$current_root" $worktrees
        cd "$main_worktree"
    end

    for worktree in $worktrees
        echo "Deleting worktree: $worktree"
        if not git worktree remove "$worktree"
            read -l -P "Deleting worktree failed. Force delete? [y/N] " confirm
            if test "$confirm" = y -o "$confirm" = Y
                git worktree remove --force "$worktree"
            else
                echo "Skipped: $worktree"
            end
        end
    end
end
