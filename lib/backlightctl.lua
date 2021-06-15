local backlightctl = {}

function backlightctl.get()
    local handle = io.popen("xbacklight -get")
    local data = handle:read("*all")
    handle:close() -- TODO: handle return code
    return data:gsub("^%s*(.-)%s*$", "%1")
end

function backlightctl.set(data)
    if type(data) ~= "number" then
        error("data should be a number (found " .. type(data) .. ")")
    end

    local handle = io.popen("xbacklight -set " .. data)
    handle:close() -- TODO: handle return code
end

function backlightctl.inc_dec(amount, limits)
    local lower_limit = limits.lower_limit or error("`limits` must have a `lower_limit` field")
    local upper_limit = limits.upper_limit or 100

    local current = backlightctl.get()
    backlightctl.set(math.min(math.max(lower_limit, current + amount), upper_limit))
end

return backlightctl
