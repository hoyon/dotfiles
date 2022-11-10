function aws-profile --description="Set AWS_PROFILE env variable"
    if test (count $argv) -lt 1;
        set -ug AWS_PROFILE
    else
        set profile "$argv[1]"
        if grep -q "\[profile $profile\]" ~/.aws/config;
            set -xg AWS_PROFILE $argv[1]
        else
            echo "Profile $profile not found in ~/.aws/config"
            return 1
        end
    end
end
