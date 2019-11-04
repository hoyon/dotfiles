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

sink_id = ""

awful.spawn.easy_async("/home/hoyon/.config/awesome/pulse-sink.sh", function(stdout)
    sink_id = " --id " .. stdout .. " "
end)

function status_command()
    return "pulsemixer" .. sink_id .. " --get-volume --get-mute"
end

function volume_widget:update_volume()
    awful.spawn.easy_async(status_command(), function(stdout)
        
        local volume = string.match(stdout, "%d+")
        volume = tonumber(volume)
        volume = string.format("% 3d", volume)

        local status = tonumber(string.sub(stdout, -2))

        local volume_string = ""
        if status == 0 then
            volume_string = " " .. volume .. "%"
        else
            volume_string = " Mute"
        end
        self:set_markup(volume_string .. " | ")
    end)
end

function volume_widget:show_volnoti()
    awful.spawn.easy_async(status_command(), function(stdout)
        local volume = string.match(stdout, "%d+")
        volume = tonumber(volume)
        volume = string.format("% 3d", volume)

        local status = tonumber(string.sub(stdout, -2))

        -- check mute
        if status == 0 then
            awful.spawn("volnoti-show " .. volume, false)
        else
            awful.spawn("volnoti-show -m " .. volume, false)
        end
    end)
end

function volume_widget:up(hide)
    local command = "pulsemixer" .. sink_id .. " --change-volume +5"
    awful.spawn.easy_async(command, function()
        self:update_volume()
        if not hide then
            self:show_volnoti()
        end
    end)
end

function volume_widget:down(hide)
    local command = "pulsemixer" .. sink_id .. " --change-volume -5"
    awful.spawn.easy_async(command, function()
        self:update_volume()
        if not hide then
            self:show_volnoti()
        end
    end)
end

function volume_widget:mute(hide)
    local command = "pulsemixer" .. sink_id .. " --toggle-mute"
    awful.spawn.easy_async(command, function()
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
