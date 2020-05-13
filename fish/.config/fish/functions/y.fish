function y -d "Yarn run in assets directory"
    yarn --cwd (git rev-parse --show-toplevel)/assets $argv
end
