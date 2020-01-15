function pc --description 'fishy pkg-config'
	command pkg-config $argv | string split -n " "
end
