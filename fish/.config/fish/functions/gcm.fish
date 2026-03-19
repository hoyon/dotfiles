function gcm
    set -l default_branch (git-default-branch)
    or return 1

    set -l stashed 0
    if not git diff --quiet || not git diff --cached --quiet
        git stash push -m "gcm: auto-stash"
        set stashed 1
    end

    git checkout $default_branch
    and git pull

    if test $stashed -eq 1
        git stash pop
    end
end
