local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")

volume_widget = wibox.widget.textbox()
volume_widget:set_align("right")
volume_widget:buttons(awful.util.table.join(
    awful.button({}, 1, function() volume_widget:mute(true) end),
    awful.button({}, 4, function() volume_widget:up(true) end),
    awful.button({}, 5, function() volume_widget:down(true) end),
    awful.button({}, 2, function() awful.spawn("pavucontrol") end)
))

function volume_widget:update_volume()
    awful.spawn.easy_async("amixer get Master", function(stdout)
        local volume = string.match(stdout, "(%d?%d?%d)%%")
        volume = tonumber(volume)
        volume = string.format("% 3d", volume)

        local status = string.match(stdout, "%[(o[^%]]*)%]")

        local volume_string = ""
        if string.find(status, "on", 1, true) then
            volume_string = " " .. volume .. "%"
        else
            volume_string = " " .. volume_string .. "Mute"
        end
        self:set_markup(volume_string .. " | ")
    end)
end

function volume_widget:show_volnoti()
    awful.spawn.easy_async("amixer get Master", function(stdout)
        local volume = string.match(stdout, "(%d?%d?%d)%%")
        volume = tonumber(volume)
        local volume_string = string.format("% 3d", volume)
        -- check mute
        if string.find(stdout, "off") then
            awful.spawn("volnoti-show -m " .. volume_string, false)
        else
            awful.spawn("volnoti-show " .. volume_string, false)
        end
    end)
end

function volume_widget:up(hide)
    awful.spawn.easy_async("amixer set Master 5%+", function()
        self:update_volume()
        if not hide then
            self:show_volnoti()
        end
    end)
end

function volume_widget:down(hide)
    awful.spawn.easy_async("amixer set Master 5%-", function()
        self:update_volume()
        if not hide then
            self:show_volnoti()
        end
    end)
end

function volume_widget:mute(hide)
    awful.spawn.easy_async("amixer set Master toggle", function()
        self:update_volume()
        if not hide then
            self:show_volnoti()
        end
    end)
end

 --Update every 5 seconds
gears.timer.start_new(5, function()
    volume_widget:update_volume()
    return true
end)

return volume_widget
