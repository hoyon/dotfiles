function gen-secret --description "Generate a random alphanumeric secret"
    set -l length 32
    if test (count $argv) -gt 0
        set length $argv[1]
    end

    set -l result ""
    while test (string length "$result") -lt $length
        set result "$result"(openssl rand -base64 32 | tr -d '+/=\n')
    end
    echo $result | cut -c1-$length
end
