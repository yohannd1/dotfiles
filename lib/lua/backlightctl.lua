local stringTrim = function(str)
  return str:gsub("^%s*(.-)%s*$", "%1")
end

local runCommand = function(command)
  local handle = io.popen(command)
  handle:close() -- TODO: handle return code
end

local getCommandOutput = function(command)
  local handle = io.popen(command)
  local data = handle:read("*all")
  handle:close() -- TODO: handle return code
  return stringTrim(data)
end

local XbacklightBackend = {}
XbacklightBackend.get = function()
  return tonumber(getCommandOutput("xbacklight -get"))
end
XbacklightBackend.set = function(value)
  assert(type(value) == "number", "expected number, got " .. type(value) .. ")")
  runCommand("xbacklight -set " .. value)
end
XbacklightBackend.inc_dec = function(amount, limits)
  local lower_limit = limits.lower_limit or error("`limits` must have a `lower_limit` field")
  local upper_limit = limits.upper_limit or 100

  local current = XbacklightBackend.get()
  XbacklightBackend.set(math.min(math.max(lower_limit, current + amount), upper_limit))
end

local LightBackend = {}
LightBackend.get = function()
  local str = getCommandOutput("light")
  return tonumber(str)
end
LightBackend.set = function(value)
  assert(type(value) == "number", "expected number, got " .. type(value) .. ")")
  runCommand("light -s sysfs/backlight/auto -v3 -S " .. value)
end
LightBackend.inc_dec = function(amount, limits)
  -- FIXME: actually use the limits... it's glitched.
  amount = amount * 0.4
  local args = (amount > 0) and ("-A " .. amount) or ("-U " .. -amount)
  print("light " .. args)
  runCommand("light " .. args)
end

return LightBackend
