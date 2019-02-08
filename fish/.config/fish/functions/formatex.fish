function formatex
    git diff --name-only --cached | egrep '\.ex$|\.exs$' | xargs mix format
end
