local api = vim.api
local cmd = api.nvim_command
local call = api.nvim_call_function

local function rifle(on_windows, plus)
    local r = api.nvim_buf_get_var(0, "rifle")
    local filename = api.nvim_buf_get_var(0, "filename")

    local tempfile = nil
    if on_windows then
        tempfile = os.getenv("HOME")
        if tempfile == nil then
            tempfile = os.getenv("APPDATA")
        end
        if (tempfile == nil) and plus then
            print("Could not generate tempfile")
            return
        end
        tempfile = tempfile .. '\\tmp_output.exe'
    else
        tempfile = os.getenv("F_TEMP")
        if tempfile == nil then
            tempfile = "/tmp"
        end
        tempfile = tempfile .. '/tmp_output'
    end

    if on_windows then
        r = r.win
    else
        r = r.std
    end

    if r.body == nil then
        print("[Rifle] main command (.body key) not found.")
        return
    end

    cmd("split")
    cmd("wincmd j")
    cmd("enew")

    local rifle_cmd = ""
    if r.before ~= nil then
        rifle_cmd = rifle_cmd .. r.before .. " && "
    end

    rifle_cmd = rifle_cmd .. r.body

    if plus then
        if r.plus ~= nil then
            rifle_cmd = rifle_cmd .. " && " .. r.plus
        end
    end

    if r.after ~= nil then
        rifle_cmd = rifle_cmd .. "; " .. r.after
    end

    rifle_cmd = string.gsub(rifle_cmd, "%%f", filename)
    rifle_cmd = string.gsub(rifle_cmd, "%%o", tostring(tempfile))
    rifle_cmd = string.gsub(rifle_cmd, "'", '"')

    call("termopen", {rifle_cmd})
    cmd("normal i")
end

return {
    rifle = rifle,
}
