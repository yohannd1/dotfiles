local api = vim.api
local cmd = api.nvim_command
local call = api.nvim_call_function

-- Function: (string) -> boolean
-- Checks if a file exists.
-- Args:
--   filename ::= the path to the file to check.
function io.exists(filename)
    local f = io.open(filename, "r")
    if f ~= nil then
        io.close(f)
        return true
    else
        return false
    end
end

-- Function: () -> string
-- Generates a temp file and returns it. Returns nil if the process was
-- not successful.
local function gen_tmp()
    local base = nil

    if on_windows then
        local base = os.getenv("HOME") or os.getenv("APPDATA")
        if io.exists(base) then os.remove(base) end
        return base and (base.."\\prog.exe")
    else
        local base = os.getenv("F_TEMP") or "/tmp"
        if io.exists(base) then os.remove(base) end
        return base .. "/prog"
    end
end

-- Function: (string) -> boolean
-- The main part of the plugin. Returns a boolean indicating whether the
-- operation was successful or not.
-- Args:
--   command ::= the key of the rifle dict that contains the command.
local function rifle(command, _use_termup, _bspc)
    local r = api.nvim_buf_get_var(0, "rifle")
    local fname = api.nvim_buf_get_var(0, "filename")
    local use_termup = _use_termup ~= 0

    -- Check if specified command really exists
    if r[command] == nil then
        print("Rifle: key'"..command.."' not found in b:rifle.")
        return false
    end

    local rifle_cmd = ""

    -- Maybe use termup.
    if use_termup then
        rifle_cmd = rifle_cmd .. 'rifle-run "'
    end

    rifle_cmd = rifle_cmd .. r[command]
    rifle_cmd = rifle_cmd:gsub("%%f", fname)

    -- Only mess with gen_tmp() if it really is needed (%o is mentioned
    -- in the command)
    if r[command]:find("%%o") then
        local output = gen_tmp()
        if type(output) ~= "string" then
            print("Rifle: could not generate output file.")
            return false
        end
        rifle_cmd = rifle_cmd:gsub("%%o", output)
    end

    -- The closing quote
    if use_termup then
        rifle_cmd = rifle_cmd .. '"'
    end

    if use_termup then
        -- if _bspc ~= 0 then
        --     call("jobstart", {"bspc node -p south"})
        -- end
        call("jobstart", {rifle_cmd})
    else
        cmd("split")
        cmd("wincmd j")
        cmd("enew")
        call("termopen", {rifle_cmd})
        cmd("normal i")
    end
end

return {
    rifle = rifle,
}
