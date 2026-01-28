function gwt
    if test -z "$argv"
        echo "Usage: gwt new-branch-name"
        return 1
    end

    set -l branch $argv[1]
    set -l repo (basename (pwd))

    git fetch origin main
    git worktree add ../$repo-$branch origin/main -b $branch; or return 1

    if test "_build"
        cp -r _build ../$repo-$branch/_build
    end

    if test "_build"
        cp -r deps ../$repo-$branch/deps
    end

    cd ../$repo-$branch
end
