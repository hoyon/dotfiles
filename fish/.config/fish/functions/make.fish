# Defined in - @ line 1
function make
    set -l ncpu
    if command -q nproc
        set ncpu (nproc)
    else
        set ncpu (sysctl -n hw.ncpu)
    end
    set -l j_arg "-j$ncpu"
    command make $j_arg $argv;
end
