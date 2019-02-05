# Defined in - @ line 1
function server --description 'alias server python -m SimpleHTTPServer'
	python -m SimpleHTTPServer $argv;
end
