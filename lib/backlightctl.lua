local backlightctl = {}

local is_wayland = os.getenv("WAYLAND_DISPLAY") ~= nil
local is_xorg = (not is_wayland) and (os.getenv("DISPLAY") ~= nil)

local xbacklightGet = function()
    local handle = io.popen("xbacklight -get")
    local data = handle:read("*all")
    handle:close() -- TODO: handle return code
    return data:gsub("^%s*(.-)%s*$", "%1")
end

local lightGet = function()
    local handle = io.popen("light")
    local data = handle:read("*all")
    handle:close() -- TODO: handle return code
    return data:gsub("^%s*(.-)%s*$", "%1")
end

local xbacklightSet = function(data)
    if type(data) ~= "number" then
        error("data should be a number (found " .. type(data) .. ")")
    end

    local handle = io.popen("xbacklight -set " .. data)
    handle:close() -- TODO: handle return code
end

local lightSet = function(data)
    if type(data) ~= "number" then
        error("data should be a number (found " .. type(data) .. ")")
    end

    local handle = io.popen("light -s sysfs/backlight/auto -v3 -S " .. data)
    handle:close() -- TODO: handle return code
end

function backlightctl.get()
    if is_xorg then
        return xbacklightGet()
    else
        return lightGet()
    end
end

function backlightctl.set(data)
    if is_xorg then xbacklightSet(data) else lightSet(data) end
end

function backlightctl.inc_dec(amount, limits)
    local lower_limit = limits.lower_limit or error("`limits` must have a `lower_limit` field")
    local upper_limit = limits.upper_limit or 100

    local current = backlightctl.get()
    backlightctl.set(math.min(math.max(lower_limit, current + amount), upper_limit))
end

return backlightctl
