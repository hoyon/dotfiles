local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local dbus = require("dbus")
local util = require("util")

media_widget = wibox.widget.textbox()
media_widget:set_align("right")
media_widget:buttons(awful.util.table.join(
    awful.button({}, 1, function() media_widget:toggle() end)
))

awful.spawn("/home/hoyon/.config/awesome/media/kill.sh")

file = io.popen("/home/hoyon/.config/awesome/media/freeport.sh")
local port = file:read('*all')
file:close()

awful.spawn.with_line_callback("/home/hoyon/.config/awesome/media/mpris_wrapper.sh " .. port, {
    stdout = function(line)
        if line == '' then
            media_widget:set_markup('')
        else
            media_widget:set_markup(line .. ' | ')
        end
    end,
    stderr = function(line)
        media_widget:set_markup(line)
    end })

function media_widget:action(cmd)
    awful.spawn.easy_async("/home/hoyon/.config/awesome/media/send_command.sh " .. port .. " ".. cmd,
        function()
            --self:update()
        end)
end

function media_widget:toggle()
    self:action("pause")
end

function media_widget:next()
    self:action("next")
end

function media_widget:prev()
    self:action("previous")
end

function media_widget:rotate()
    self:action("rotate")
end

--Update every second
--gears.timer.start_new(1, function()
    --media_widget:update()
    --return true
--end)

return media_widget
