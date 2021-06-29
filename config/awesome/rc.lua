local user = {}
_G.user = user

user.cfg_root = (
    debug.getinfo(1, "S").source
    :gsub("^@", "")
    :gsub("^(.*)(/.*)$", "%1")
)

user.fennel_load_opts = { correlate = true }

-- Set up fennel
local fennel = require("fennel")
table.insert(package.loaders or package.searchers,
             fennel.makeSearcher(user.fennel_load_opts))

user.cfgPathFor = function(path)
    return user.cfg_root .. "/" .. path
end

user.loadFnlConfig = function(path)
    return fennel.dofile(user.cfgPathFor(path), user.fennel_load_opts)
end

user.loadFnlConfig("init.fnl")
