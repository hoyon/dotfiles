function vimdiff
	if type -q nvim
        nvim -d $argv
    else
        command vimdiff $argv
    end
end
