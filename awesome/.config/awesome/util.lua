local util = {}
util.run_cmd = function(cmd)
    local file = io.popen(cmd, "r")
    local output = file:read("*all")
    file:close()
    return output 
end

util.file_exists = function(name)
    local f = io.open(name, "r")
    if f~= nil then
        io.close(f)
        return true
    else
        return false
    end
end

util.trim = function(s)
    return (s:gsub("^%s*(.-)%s*$", "%1"))
end

return util
