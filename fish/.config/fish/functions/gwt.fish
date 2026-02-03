function gwt --description "Create a new git worktree from origin/main or a PR"
    argparse 'pr' -- $argv
    or return 1

    if test -z "$argv"
        if set -q _flag_pr
            echo "Usage: gwt --pr <pr-number>"
        else
            echo "Usage: gwt <new-branch-name>"
        end
        return 1
    end

    set -l repo (basename (pwd))
    set -l worktree

    if set -q _flag_pr
        set -l pr_number $argv[1]
        set -l pr_info (gh pr view $pr_number --json headRefName,title -q '.headRefName + "\t" + .title')
        or return 1
        set -l branch (echo $pr_info | cut -f1)
        set -l title (echo $pr_info | cut -f2)
        set -l slug (echo $title | string lower | string replace -ra '[^a-z0-9]+' '-' | string replace -r '^-|-$' '' | string sub -l 30)
        set worktree ../$repo-pr-$pr_number-$slug
        git fetch origin $branch
        git worktree add $worktree origin/$branch; or return 1
    else
        set -l branch $argv[1]
        set worktree ../$repo-$branch
        git fetch origin main
        git worktree add $worktree origin/main -b $branch; or return 1
    end

    if test -d "_build"
        cp -r _build $worktree/_build
    end

    if test -d "deps"
        cp -r deps $worktree/deps
    end

    set -l had_node_modules (test -d "node_modules" && echo 1)

    if test -f ".env"
        ln -s (pwd)/.env $worktree/.env
    end

    cd $worktree

    if test -n "$had_node_modules"
        npm i
    end
end
