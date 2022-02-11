function aws-profile --description="Set AWS_PROFILE env variable"
    if test (count $argv) -lt 1;
        set -ug AWS_PROFILE
    else
        set -xg AWS_PROFILE $argv[1]
    end
end
