local vim = _G.vim
local M = {}

M.os = {}
M.os.is_android = vim.fn.isdirectory("/sdcard") ~= 0
M.os.is_windows = (vim.fn.has("win32") or vim.fn.has("win64")) ~= 0

M.parseEscapeCode = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

M.overrideTableWith = function(dest, src)
  for k, v in pairs(src) do
    dest[k] = v
  end
end

-- TODO: is this the most efficient way
M.splitIter = function(str, separator)
    local length = str:len()

    local start_p = 1
    local end_p = 1

    return function()
        while true do
            if start_p > length then
                return nil
            end

            if separator == "" then
                local i = end_p
                end_p = end_p + 1
                start_p = end_p
                return str:sub(i, i)
            elseif (str:sub(end_p, end_p) == separator) then
                local s = start_p
                local e = end_p

                end_p = end_p + 1
                start_p = start_p + 1

                return str:sub(s, e-1)
            end

            end_p = end_p + 1
        end
    end
end

M.enumerateIter = function(iter)
    error("unimplemented") -- TODO
end

M.pathAppend = function(dir)
    local p = vim.env.PATH
    if not p:find(dir) then
        vim.env.PATH = p .. (M.os.is_windows and ";" or ":") .. dir
    end
end

M.string = {}
M.string.endsWith = function(haystack, suffix)
    return string.sub(haystack, -#suffix) == suffix
end

M._features = {}

-- TODO: "inspect" function

return M
