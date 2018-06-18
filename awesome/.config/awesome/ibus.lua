local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local util = require("util")

ibus_widget = wibox.widget.textbox()
ibus_widget:set_align("right")
local lang_file = "/tmp/hoyon-lang"
local eng_str = "eng"
local jpn_str = "jpn"
local cur;

-- Write current lang to /tmp/hoyon-lang
if not util.file_exists(lang_file) then
    local file = io.open(lang_file, "w")
    file:write(eng_str)
    file:close()
    cur = eng_str
else
    local file = io.open(lang_file)
    local lang = file:read("*a")
    file:close()
    cur = lang
end

function ibus_widget:update()
    ibus_widget:set_markup(cur .. " | ")
end

ibus_widget:update()

function ibus_widget:switch()
    --util.run_cmd("xdotool --clearmodifiers key Henkan")
    root.fake_input("keyrelease", 100)
    if cur == eng_str then
        cur = jpn_str
    else
        cur = eng_str
    end
    ibus_widget:update()
end

return ibus_widget
