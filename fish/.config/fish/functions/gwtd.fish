function gwtd --description "Delete a git worktree using fzf"
    set -l worktree (git worktree list | tail -n +2 | fzf --prompt="Select worktree to delete: " | awk '{print $1}')

    if test -z "$worktree"
        return 1
    end

    echo "Deleting worktree: $worktree"
    git worktree remove "$worktree"
end
