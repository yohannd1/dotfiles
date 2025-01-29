local M = {}

M.is_wayland = os.getenv("WAYLAND_DISPLAY") ~= nil
M.is_xorg = (not is_wayland) and (os.getenv("DISPLAY") ~= nil)

return M
