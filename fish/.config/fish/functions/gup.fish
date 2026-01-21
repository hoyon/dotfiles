function gup
    set -l default_branch (git-default-branch)
    or return 1

    set -l stashed 0
    if not git diff --quiet || not git diff --cached --quiet
        git stash push -m "gup: auto-stash"
        set stashed 1
    end

    set -l fork_point (git merge-base --fork-point $default_branch)
    git fetch origin $default_branch:$default_branch
    and git rebase $default_branch

    if test -n "$fork_point"
        if test "$fork_point" = (git rev-parse $default_branch)
            echo "Already up to date"
        else
            echo ""
            echo "Changes from $default_branch:"
            git diff --stat $fork_point $default_branch
        end
    end

    if test $stashed -eq 1
        git stash pop
    end
end
