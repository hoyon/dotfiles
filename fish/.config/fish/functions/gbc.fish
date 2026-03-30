function gbc --description "Interactively prune local branches with last commit over a month ago"
    set -l cutoff (date -v-1m +%s 2>/dev/null || date -d '1 month ago' +%s)
    set -l worktree_branches (git worktree list --porcelain | string replace -rf '^branch refs/heads/(.+)' '$1')

    for branch in (git for-each-ref --sort=committerdate --format='%(refname:short)' refs/heads/)
        test $branch = main; or test $branch = master; and continue
        contains $branch $worktree_branches; and continue

        set -l epoch (git log -1 --format='%ct' $branch)
        test $epoch -ge $cutoff; and continue

        set -l date (git log -1 --format='%cr' $branch)
        set -l subject (git log -1 --format='%s' $branch)
        printf '\033[32m%-20s\033[0m %-50s %s\n' $date $branch $subject

        read -P 'Delete? [y/N] ' -n 1 -l answer
        if test "$answer" = y
            git branch -D $branch
        end
    end
end
