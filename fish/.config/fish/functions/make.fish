# Defined in - @ line 1
function make
    set -l j_arg "-j"(nproc)
    command make $j_arg $argv;
end
