# Defined in - @ line 1
function server --description 'start server in current directory' -a 'port'
    string length -q -- $port; or set port 8000
    if type -q livereload
        livereload -p $port
    else
        python -m http.server $port;
    end
end
