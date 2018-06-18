local awful = require("awful")

local pressKey = function(key)
    local focused = awful.screen.focused().index
    local otherIndex = (focused % 2) + 1
    local other = screen[otherIndex]
    if #other.clients > 0 then
        local id = other.clients[1].window
        local focused = client.focus.window
        local cmd1 = "xdotool windowactivate --sync " .. id
        local cmd2 = "sleep 0.05 && xdotool key " .. key .. " && sleep 0.05"
        local cmd3
        if focused then
            cmd3 = "xdotool windowactivate --sync " .. focused
        else
            cmd3 = "true"
        end

        awful.spawn.easy_async("sh -c '" .. cmd1 .. " && " .. cmd2 .. " && " .. cmd3 .. "'", function() end)
    end
end

return pressKey
