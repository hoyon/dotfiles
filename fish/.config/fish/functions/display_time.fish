function display_time
    set -l time $argv[1]
    set -l days (math --scale 0 $time "/ 60 / 60 / 24")
    set -l hours (math --scale 0 $time "/ 60 / 60 % 24")
    set -l minutes (math --scale 0 $time "/ 60 % 60")
    set -l seconds (math --scale 0 $time "% 60")

    if test $days -ne 0
        printf '%dd ' $days
    end
    if test $hours -ne 0
        printf '%dh ' $hours
    end
    if test $minutes -ne 0
        printf '%dm ' $minutes
    end
    if test $seconds -ne 0
        printf '%ds' $seconds
    end
end
