local util = require("util")
local awful = require("awful")

-- string for commands without args
-- table for commands with args
local startup_home = {
    --"compton",
    {"ibus-daemon", "-x", "-d"},
    --{"xflux", "-l 51 -g 0 -k 3000"},
    "redshift",
    --"synergys",
    "unclutter",
    {"volnoti", "-t 1"},
}

local running = function(cmd)
    local out = util.run_cmd("pgrep -x -c " .. cmd)
    return tonumber(out) ~= 0
end

local start_programs = function(programs)
    for _,i in pairs(programs) do
        if type(i) == "string" then
            if not running(i) then
                awful.spawn(i, false)
            end
        elseif type(i) == "table" then
            if not running(i[1]) then
                awful.spawn(i[1] .. " " .. i[2], false)
            end
        end
    end
end

-- Only start programs which aren't already running
local startup = function()
    hostname = io.popen("uname -n"):read()

    if hostname == "hoyon-desktop" then
        start_programs(startup_home)
    end
end

return startup
