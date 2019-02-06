# Defined in /home/hoyon/.config/fish/config.fish @ line 16
function pc --description 'fishy pkg-config'
	command pkg-config $argv | string split " "
end
