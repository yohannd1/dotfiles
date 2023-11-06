-- A custom require function because neovim struggles sometimes
local module_cache = {}

_G.useConfModule = function(module_name, options)
    local options = options or {}
    if not options.force and module_cache[module_name] then
        return module_cache[module_name]
    end

    local config_root = assert(vim.g.config_root, "config root not specified")
    local path = config_root .. "/lua/cfg/" .. module_name .. ".lua"
    local module_data = assert(loadfile(path))()
    module_cache[module_name] = module_data
    return module_data
end

_G.clearConfCache = function()
    for k, _ in pairs(module_cache) do
        module_cache[k] = nil
    end
end

return nil
