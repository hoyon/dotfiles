function gwt --description "Create a new git worktree from origin/main or a PR"
    argparse 'pr' -- $argv
    or return 1

    if test -z "$argv"
        if set -q _flag_pr
            echo "Usage: gwt --pr <pr-number>"
        else
            echo "Usage: gwt <dir-name> [branch-name]"
        end
        return 1
    end

    set -l repo (basename (pwd))
    set -l worktree
    set -l branch

    if set -q _flag_pr
        set -l pr_number $argv[1]
        set -l pr_info (gh pr view $pr_number --json headRefName,title -q '.headRefName + "\t" + .title')
        or return 1
        set branch (echo $pr_info | cut -f1)
        set -l title (echo $pr_info | cut -f2)
        set -l slug (echo $title | string lower | string replace -ra '[^a-z0-9]+' '-' | string replace -r '^-|-$' '' | string sub -l 30)
        set worktree ../$repo-pr-$pr_number-$slug
        git fetch origin $branch
        git worktree add $worktree origin/$branch; or return 1
    else
        set -l dir_name $argv[1]
        set branch (test (count $argv) -ge 2 && echo $argv[2] || echo $argv[1])
        set worktree ../$repo-$dir_name
        git fetch origin main
        git worktree add $worktree origin/main -b $branch; or return 1
    end

    set -l cp_flags -r
    switch (uname)
        case Darwin
            set -a cp_flags -c
        case Linux
            set -a cp_flags --reflink=auto
    end

    if test -d "_build"
        cp $cp_flags _build $worktree/_build
    end

    if test -d "deps"
        cp $cp_flags deps $worktree/deps
    end

    set -l had_node_modules (test -d "node_modules" && echo 1)

    if test -f ".env"
        ln -s (pwd)/.env $worktree/.env
    end

    if test -f "dslr.toml"
        # Clone dev DB for worktree isolation
        set -l db_name (echo $branch | string replace -a '/' '_')
        set -l db_url "postgres://postgres:postgres@localhost:5432"
        set -l new_db "$db_name"_server_dev

        echo "Creating branch database: $new_db"
        psql -h localhost -p 5432 -U postgres -q -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = 'server_dev' AND pid <> pg_backend_pid();" > /dev/null
        createdb -h localhost -p 5432 -U postgres -T server_dev $new_db
        DB_NAME=$db_name mix ecto.migrate

        echo "url = '$db_url/$new_db'" > $worktree/dslr.toml
        echo "DB_NAME=$db_name" > $worktree/.env.local
    end

    cd $worktree

    if test -n "$had_node_modules"
        npm i
    end
end
