function vim
	if type -q nvim
        nvim $argv
    else
        command vim $argv
    end
end
