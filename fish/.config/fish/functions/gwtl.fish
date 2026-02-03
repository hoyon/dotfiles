function gwtl --description "List git worktrees with their commits"
    set -l main_branch (git-default-branch)

    for line in (git worktree list)
        set -l path (echo $line | awk '{print $1}')
        set -l branch (echo $line | awk '{print $3}' | tr -d '[]')

        echo (set_color --bold cyan)$branch(set_color normal) (set_color --dim)$path(set_color normal)

        if test "$branch" != "$main_branch"
            set -l commits (git log --oneline $main_branch..$branch 2>/dev/null)
            if test -n "$commits"
                for commit in $commits
                    echo "  $commit"
                end
            else
                set -l changes (git -C $path diff --stat 2>/dev/null)
                if test -n "$changes"
                    echo (set_color yellow)"  (uncommitted changes)"(set_color normal)
                    for change in $changes
                        echo "  $change"
                    end
                else
                    echo (set_color --dim)"  (no commits ahead of $main_branch)"(set_color normal)
                end
            end
        end
        echo
    end
end
