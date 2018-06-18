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

awful.spawn('killall mpris_wrapper.sh')
awful.spawn.with_line_callback("/home/hoyon/.config/awesome/mpris_wrapper.sh", {
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

--function media_widget:update()
    --awful.spawn.easy_async("playerctl status",
        --function(stdout, stderr, reason, code)
            --if code == 0 then
                --local status = ""
                --if stdout:find("Stopped") then
                    --media_widget:set_markup("")
                    --return
                --elseif stdout:find("Playing") then
                    --status = " "
                --elseif stdout:find("Paused") then
                    --status = " "
                --end
                --awful.spawn.easy_async("playerctl metadata artist",
                    --function(artist)
                        --artist = util.trim(artist)
                        --awful.spawn.easy_async("playerctl metadata title",
                        --function(title)
                            --title = util.trim(title)
                            --local media_string = ""
                            --if artist == "" then
                                --media_string = status .. title
                            --elseif title == "" then
                                --media_string = status .. artist
                            --else
                                --media_string = status .. artist .. " - " .. title
                            --end
                            --max_length = 70
                            --if media_string:len() > max_length then
                                --media_string = media_string:sub(0, max_length) .. "…"
                            --end
                            --media_widget:set_markup(media_string .. " | ")
                        --end)
                    --end)
            --else
                --media_widget:set_markup("");
            --end
        --end)
--end

function media_widget:action(cmd)
    awful.spawn.easy_async("playerctl " .. cmd,
        function()
            --self:update()
        end)
end

function media_widget:toggle()
    self:action("play-pause")
end

function media_widget:next()
    self:action("next")
end

function media_widget:prev()
    self:action("previous")
end

--Update every second
--gears.timer.start_new(1, function()
    --media_widget:update()
    --return true
--end)

return media_widget
