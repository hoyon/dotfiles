# Defined in - @ line 1
function server --description 'start server in current directory' -a 'port'
    string length -q -- $port; or set port 8000
    if type -q livereload
        livereload -p $port
    else
        python2 -m SimpleHTTPServer $port;
    end
end
