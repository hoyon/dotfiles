function git-default-branch
    git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'

    if test $pipestatus[1] -ne 0
        echo "Try running this command to sync default branch with origin" 1>&2
        echo "git remote set-head origin --auto" 1>&2
        return 1
    end
end
