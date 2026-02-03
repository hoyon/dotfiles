function gwtd --description "Delete a git worktree using fzf"
    set -l current_root (git rev-parse --show-toplevel 2>/dev/null)
    set -l main_worktree (git worktree list | head -1 | awk '{print $1}')
    set -l worktree

    if test "$current_root" != "$main_worktree"
        read -l -P "Delete current worktree ($current_root)? [y/N] " confirm
        if test "$confirm" = y -o "$confirm" = Y
            set worktree "$current_root"
            cd "$main_worktree"
        else
            echo "Aborted."
            return 1
        end
    else
        set worktree (git worktree list | tail -n +2 | fzf --prompt="Select worktree to delete: " | awk '{print $1}')
        if test -z "$worktree"
            return 1
        end
    end

    echo "Deleting worktree: $worktree"
    if not git worktree remove "$worktree"
        read -l -P "Deleting worktree failed. Force delete? [y/N] " confirm
        if test "$confirm" = y -o "$confirm" = Y
            git worktree remove --force "$worktree"
        else
            echo "Aborted."
            return 1
        end
    end
end
