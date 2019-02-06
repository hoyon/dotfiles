# Defined in /home/hoyon/.config/fish/config.fish @ line 27
function vim
	if type -q nvim
        nvim $argv
    else
        vim $argv
    end
end
