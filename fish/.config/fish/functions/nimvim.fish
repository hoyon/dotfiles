function nimvim --description "View nim cached output in vim"
    clang-format $argv -style="{ColumnLimit: 0}" | sed '/#line/d' | sed '/nimfr_/d' | sed '/nimln_/d' | vim +'set buftype=nofile' +'set ft=cpp' -
end
