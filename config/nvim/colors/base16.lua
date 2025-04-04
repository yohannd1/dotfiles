-- base16 theme

local vim = _G.vim
local has = function(x) return vim.fn.has(x) ~= 0 end

vim.cmd.hi("clear")
vim.cmd.syntax("reset")
vim.g.colors_name = "base16"

local has_xres =
  (vim.env.DISPLAY ~= nil) and (vim.fn.executable("xgetres") ~= 0)

local use_true_colors =
  has("gui_running") or (vim.b.base16_use_true_colors or 0) ~= 0

local xgetres = function(res)
  local c = vim.system({"xgetres", res}, {text = true}):wait()
  assert(c.code == 0, "failed to run xgetres (code ~= 0)")
  return vim.fn.split(c.stdout)[1]
end

local loadXresColorMap = function()
  local cmap = {}
  for i = 0, 15 do
    local id = ("%02X"):format(i)
    local res = ("nvim.base%02X"):format(i)
    cmap[id] = xgetres(res)
  end
  return cmap
end

local DEFAULT_GUI_COLORS = {
  ["00"] = "#f2e5bc",
  ["01"] = "#ebdbb2",
  ["02"] = "#d5c4a1",
  ["03"] = "#bdae93",
  ["04"] = "#665c54",
  ["05"] = "#504945",
  ["06"] = "#3c3836",
  ["07"] = "#282828",
  ["08"] = "#9d0006",
  ["09"] = "#af3a03",
  ["0A"] = "#b57614",
  ["0B"] = "#79740e",
  ["0C"] = "#427b58",
  ["0D"] = "#076678",
  ["0E"] = "#8f3f71",
  ["0F"] = "#d65d0e",
}

-- initialize bases table
local bases = { tty = {}, gui = {} }
bases.tty["NONE"] = "NONE"
bases.gui["NONE"] = "NONE"
for i = 0, 15 do
  local id = ("%02X"):format(i)
  bases.tty[id] = ""
  bases.gui[id] = ""
end

if use_true_colors then
  local color_map = vim.b.base16_true_color_map or (has_xres and loadXresColorMap()) or DEFAULT_GUI_COLORS

  for i = 0, 15 do
    local id = ("%02X"):format(i)
    bases.gui[id] = color_map[id]
    bases.tty[id] = "NONE" -- disable tty coloring
  end
else
  -- not on GUI - let's use the terminal-native coloring, assuming its colors match each base16 color
  for i = 0, 15 do
    local id_hex = ("%02X"):format(i)
    local id_int = ("%02d"):format(i)

    bases.tty[id_hex] = id_int
    bases.gui[id_hex] = "NONE" -- disable gui coloring
  end
end

local hl = function(group, fg_code, bg_code, attr, guisp)
  fg_code = fg_code or ""
  bg_code = bg_code or ""
  attr = attr or ""
  guisp = guisp or ""

  local hargs = {}
  local set = function(k, v)
    table.insert(hargs, ("%s=%s"):format(k, v))
  end

  if fg_code ~= "" then
    set("guifg", bases.gui[fg_code])
    set("ctermfg", bases.tty[fg_code])
  end

  if bg_code ~= "" then
    set("guibg", bases.gui[bg_code])
    set("ctermbg", bases.tty[bg_code])
  end

  if attr ~= "" then
    set("gui", attr)
    set("cterm", attr)
  end

  if guisp ~= "" then
    set("guisp", "#" .. guisp)
  end

  local command = ("hi %s %s"):format(group, table.concat(hargs, " "))
  vim.cmd(command)
end

-- rainbow parens config
if vim.g.rainbow_active then
  local colors = {"09", "0A", "0B", "0D", "0E"}

  local conf = vim.b.rainbow_conf or {}
  if use_true_colors then
    conf.guifgs = vim.fn.map(colors, function(_, id)
      return bases.gui[id]
    end)
  else
    conf.ctermfgs = vim.fn.map(colors, function(_, id)
      return bases.tty[id]
    end)
  end
  vim.b.rainbow_conf = conf
end

-- Vim editor colors
if use_true_colors then
  hl("Normal",      "05",   "00",   "",     "")
else
  hl("Normal",      "05",   "NONE", "",     "")
end

-- Standard semantics
hl("Bold",          "NONE", "NONE", "bold", "")
hl("Debug",         "08",   "NONE", "", "")
hl("Directory",     "0D",   "NONE", "", "")
hl("Error",         "08",   "00",   "undercurl", "")
hl("ErrorMsg",      "08",   "NONE", "", "")
hl("Exception",     "08",   "NONE", "", "")
hl("FoldColumn",    "0C",   "02",   "", "")
hl("Folded",        "05",   "01",   "italic", "")
hl("IncSearch",     "01",   "09",   "none", "")
hl("Italic",        "NONE", "NONE", "italic", "")
hl("Macro",         "08",   "NONE", "", "")
hl("MatchParen",    "NONE", "0D",   "", "")
hl("ModeMsg",       "0B",   "NONE", "", "")
hl("MoreMsg",       "0B",   "NONE", "", "")
hl("Question",      "0D",   "NONE", "", "")
hl("Search",        "01",   "0A",   "", "")
hl("Substitute",    "01",   "0A",   "none", "")
hl("SpecialKey",    "03",   "NONE", "", "")
hl("TooLong",       "08",   "NONE", "", "")
hl("Underlined",    "NONE", "NONE", "undercurl", "")
hl("Visual",        "NONE", "02", "", "")
hl("VisualNOS",     "08",   "NONE", "", "")
hl("WarningMsg",    "08",   "NONE", "", "")
hl("WildMenu",      "08",   "NONE", "", "")
hl("Title",         "0D",   "NONE", "none", "")
hl("Conceal",       "0D",   "00",   "", "")
hl("Cursor",        "00",   "05",   "", "")
hl("NonText",       "03",   "NONE", "", "")
hl("LineNr",        "04",   "NONE", "", "")
hl("SignColumn",    "03",   "01",   "", "")
hl("StatusLine",    "03",   "NONE",   "none", "")
hl("StatusLineNC",  "03",   "NONE",   "none", "")
hl("VertSplit",     "02",   "02",   "none", "")
hl("ColorColumn",   "01",   "NONE", "", "")
hl("CursorColumn",  "01",   "NONE", "", "")
hl("CursorLine",    "NONE", "01",   "NONE", "")
hl("CursorColumn",  "NONE", "01",   "NONE", "")
hl("CursorLineNr",  "NONE", "NONE", "italic", "")
hl("QuickFixLine",  "0A",   "02", "", "")
hl("PMenu",         "05",   "01",   "none", "")
hl("PMenuSel",      "01",   "05",   "", "")
hl("TabLine",       "05",   "01",   "none", "")
hl("TabLineFill",   "03",   "NONE", "none", "")
hl("TabLineSel",    "0A",   "01",   "bold,italic", "")
hl("MatchParen",    "00",   "03",   "none", "")

-- Custom semantics
hl("Dimmed", "03", "NONE", "", "")

-- Standard semantics - language definitions
hl("Boolean",      "09", "NONE", "", "")
hl("Character",    "08", "NONE", "", "")
hl("Comment",      "03", "NONE", "italic", "")
hl("Conditional",  "0E", "NONE", "", "")
hl("Constant",     "09", "NONE", "", "")
hl("Define",       "0E", "NONE", "none", "")
hl("Delimiter",    "0F", "NONE", "", "")
hl("Float",        "09", "NONE", "", "")
hl("Function",     "0D", "NONE", "", "")
hl("Identifier",   "05", "NONE", "none", "")
hl("Include",      "0D", "NONE", "", "")
hl("Keyword",      "0E", "NONE", "bold", "")
hl("Label",        "0A", "NONE", "", "")
hl("Number",       "09", "NONE", "", "")
hl("Operator",     "05", "NONE", "none", "")
hl("PreProc",      "0A", "NONE", "", "")
hl("Repeat",       "0A", "NONE", "", "")
hl("Special",      "0C", "NONE", "", "")
hl("SpecialChar",  "0F", "NONE", "", "")
hl("Statement",    "08", "NONE", "", "")
hl("StorageClass", "0A", "NONE", "", "")
hl("String",       "0B", "NONE", "", "")
hl("Structure",    "0E", "NONE", "", "")
hl("Tag",          "0A", "NONE", "", "")
hl("Todo",         "0A", "01",   "", "")
hl("Type",         "0A", "NONE", "none", "")
hl("Typedef",      "0A", "NONE", "", "")
hl("Ignore",       "02", "NONE", "", "")

-- C highlighting
hl("cOperator",    "0C", "NONE", "", "")
hl("cPreCondit",   "0E", "NONE", "", "")

-- C# highlighting
hl("csClass",                 "0A", "NONE", "", "")
hl("csAttribute",             "0A", "NONE", "", "")
hl("csModifier",              "0E", "NONE", "", "")
hl("csType",                  "08", "NONE", "", "")
hl("csUnspecifiedStatement",  "0D", "NONE", "", "")
hl("csContextualStatement",   "0E", "NONE", "", "")
hl("csNewDecleration",        "08", "NONE", "", "")

-- CSS highlighting
hl("cssBraces",      "05", "NONE", "", "")
hl("cssClassName",   "0E", "NONE", "", "")
hl("cssColor",       "0C", "NONE", "", "")

-- Diff highlighting
hl("DiffAdd",      "0B", "01", "", "")
hl("DiffChange",   "03", "01", "", "")
hl("DiffDelete",   "08", "01", "", "")
hl("DiffText",     "0D", "01", "", "")
hl("DiffAdded",    "0B", "00", "", "")
hl("DiffFile",     "08", "00", "", "")
hl("DiffNewFile",  "0B", "00", "", "")
hl("DiffLine",     "0D", "00", "", "")
hl("DiffRemoved",  "08", "00", "", "")

-- Git highlighting
hl("gitcommitOverflow",       "08", "NONE", "", "")
hl("gitcommitSummary",        "0B", "NONE", "", "")
hl("gitcommitComment",        "03", "NONE", "", "")
hl("gitcommitUntracked",      "03", "NONE", "", "")
hl("gitcommitDiscarded",      "03", "NONE", "", "")
hl("gitcommitSelected",       "03", "NONE", "", "")
hl("gitcommitHeader",         "0E", "NONE", "", "")
hl("gitcommitSelectedType",   "0D", "NONE", "", "")
hl("gitcommitUnmergedType",   "0D", "NONE", "", "")
hl("gitcommitDiscardedType",  "0D", "NONE", "", "")
hl("gitcommitBranch",         "09", "NONE", "bold", "")
hl("gitcommitUntrackedFile",  "0A", "NONE", "", "")
hl("gitcommitUnmergedFile",   "08", "NONE", "bold", "")
hl("gitcommitDiscardedFile",  "08", "NONE", "bold", "")
hl("gitcommitSelectedFile",   "0B", "NONE", "bold", "")

-- GitGutter highlighting
hl("GitGutterAdd",           "0B", "01", "", "")
hl("GitGutterChange",        "0D", "01", "", "")
hl("GitGutterDelete",        "08", "01", "", "")
hl("GitGutterChangeDelete",  "0E", "01", "", "")

-- HTML highlighting
hl("htmlBold",    "0A", "NONE", "bold", "")
hl("htmlItalic",  "0E", "NONE", "italic", "")
hl("htmlEndTag",  "05", "NONE", "", "")
hl("htmlTag",     "05", "NONE", "", "")

-- JavaScript highlighting
hl("javaScript",       "05", "NONE", "", "")
hl("javaScriptBraces", "05", "NONE", "", "")
hl("javaScriptNumber", "09", "NONE", "", "")
vim.cmd([[ hi link javaScriptIdentifier Keyword ]])

-- pangloss/vim-javascript highlighting
hl("jsOperator",          "0D", "NONE", "", "")
hl("jsStatement",         "0E", "NONE", "", "")
hl("jsReturn",            "0E", "NONE", "", "")
hl("jsThis",              "08", "NONE", "", "")
hl("jsClassDefinition",   "0A", "NONE", "", "")
hl("jsFunction",          "0E", "NONE", "", "")
hl("jsFuncName",          "0D", "NONE", "", "")
hl("jsFuncCall",          "0D", "NONE", "", "")
hl("jsClassFuncName",     "0D", "NONE", "", "")
hl("jsClassMethodType",   "0E", "NONE", "", "")
hl("jsRegexpString",      "0C", "NONE", "", "")
hl("jsGlobalObjects",     "0A", "NONE", "", "")
hl("jsGlobalNodeObjects", "0A", "NONE", "", "")
hl("jsExceptions",        "0A", "NONE", "", "")
hl("jsBuiltins",          "0A", "NONE", "", "")

-- Mail highlighting
hl("mailQuoted1",  "0A", "NONE", "", "")
hl("mailQuoted2",  "0B", "NONE", "", "")
hl("mailQuoted3",  "0E", "NONE", "", "")
hl("mailQuoted4",  "0C", "NONE", "", "")
hl("mailQuoted5",  "0D", "NONE", "", "")
hl("mailQuoted6",  "0A", "NONE", "", "")
hl("mailURL",      "0D", "NONE", "", "")
hl("mailEmail",    "0D", "NONE", "", "")

-- Markdown highlighting
hl("markdownCode",              "0B", "NONE", "", "")
hl("markdownError",             "05", "00", "", "")
hl("markdownCodeBlock",         "0B", "NONE", "", "")
hl("markdownHeadingDelimiter",  "0D", "NONE", "", "")

-- NERDTree highlighting
hl("NERDTreeDirSlash",  "0D", "NONE", "", "")
hl("NERDTreeExecFile",  "05", "NONE", "", "")

-- PHP highlighting
hl("phpMemberSelector",  "05", "NONE", "", "")
hl("phpComparison",      "05", "NONE", "", "")
hl("phpParent",          "05", "NONE", "", "")
hl("phpMethodsVar",      "0C", "NONE", "", "")

-- Nim highlighting
vim.cmd([[ hi link nimOperator Keyword ]])
vim.cmd([[ hi link nimOP9 Keyword ]])
vim.cmd([[ hi link nimOP5 Keyword ]])
vim.cmd([[ hi link nimOP4 Keyword ]])
vim.cmd([[ hi link nimOP3 Keyword ]])

-- Gdscript highlighting
vim.cmd([[ hi link gdscriptOperator Keyword ]])

-- Python highlighting
hl("pythonOperator",  "0E", "NONE", "", "")
hl("pythonRepeat",    "0E", "NONE", "", "")
hl("pythonInclude",   "0E", "NONE", "", "")
hl("pythonStatement", "0E", "NONE", "", "")

-- Ruby highlighting
hl("rubyAttribute",               "0D", "NONE", "", "")
hl("rubyConstant",                "0A", "NONE", "", "")
hl("rubyInterpolationDelimiter",  "0F", "NONE", "", "")
hl("rubyRegexp",                  "0C", "NONE", "", "")
hl("rubySymbol",                  "0B", "NONE", "", "")
hl("rubyStringDelimiter",         "0B", "NONE", "", "")

-- SASS highlighting
hl("sassidChar",     "08", "NONE", "", "")
hl("sassClassChar",  "09", "NONE", "", "")
hl("sassInclude",    "0E", "NONE", "", "")
hl("sassMixing",     "0E", "NONE", "", "")
hl("sassMixinName",  "0D", "NONE", "", "")

-- Signify highlighting
hl("SignifySignAdd",     "0B", "01", "", "")
hl("SignifySignChange",  "0D", "01", "", "")
hl("SignifySignDelete",  "08", "01", "", "")

-- Spelling highlighting
hl("SpellBad",     "NONE", "08",   "undercurl", "")
hl("SpellLocal",   "NONE", "NONE", "undercurl", "")
hl("SpellCap",     "NONE", "NONE", "undercurl", "")
hl("SpellRare",    "NONE", "NONE", "undercurl", "")

-- Startify highlighting
hl("StartifyBracket",  "03", "NONE", "", "")
hl("StartifyFile",     "07", "NONE", "", "")
hl("StartifyFooter",   "03", "NONE", "", "")
hl("StartifyHeader",   "0B", "NONE", "", "")
hl("StartifyNumber",   "09", "NONE", "", "")
hl("StartifyPath",     "03", "NONE", "", "")
hl("StartifySection",  "0E", "NONE", "", "")
hl("StartifySelect",   "0C", "NONE", "", "")
hl("StartifySlash",    "03", "NONE", "", "")
hl("StartifySpecial",  "03", "NONE", "", "")

-- Java highlighting
hl("javaOperator",     "0D", "NONE", "", "")

-- Zig highlighting
hl("zigComparatorWord", "0C", "NONE", "", "")
vim.cmd([[ hi link zigStringDelimiter Delimiter ]])
vim.cmd([[ hi link zigMultilineStringDelimiter Delimiter ]])
vim.cmd([[ hi link zigMultilineStringDelimiter Delimiter ]])

-- Typescript highlighting
vim.cmd([[ hi link typescriptVariable Function ]])

-- Lua highlighting
vim.cmd([[ hi link luaOperator Keyword ]])
vim.cmd([[ hi link luaIn Keyword ]])

-- Clap (https://github.com/liuchengxu/vim-clap) highlighting
hl("ClapInput",            "NONE", "02",   "", "")
hl("ClapDisplay",          "NONE", "00",   "NONE", "")
hl("ClapSelected",         "NONE", "01",   "NONE", "")
hl("ClapCurrentSelection", "NONE", "01",   "NONE", "")
hl("ClapMatches",          "0A",   "00",   "NONE", "")
hl("ClapMatches1",         "0A",   "00",   "NONE", "")
hl("ClapMatches2",         "0A",   "00",   "NONE", "")
hl("ClapMatches3",         "0A",   "00",   "NONE", "")
hl("ClapMatches4",         "0A",   "00",   "NONE", "")
hl("ClapMatches5",         "0A",   "00",   "NONE", "")
hl("ClapMatches6",         "0A",   "00",   "NONE", "")
hl("ClapMatches7",         "0A",   "00",   "NONE", "")
hl("ClapMatches8",         "0A",   "00",   "NONE", "")
hl("ClapNoMatchesFound",   "08",   "NONE", "NONE", "")
-- See also: ClapPreview ClapDefaultSelected ClapDefaultCurrentSelection

-- Vimwiki
vim.cmd([[ hi link VimwikiXDone Dimmed ]])
-- hl("VimwikiXDone",         "03", "NONE",   "", "")

-- Acrylic
hl("acrTaskTodo",          "0B",    "01",   "", "")
hl("acrUrl",               "09",    "NONE",   "italic", "")
hl("acrRefInner",          "0C",    "NONE",   "bold", "")
hl("acrRefDelimiter",      "03",    "NONE",   "", "")
vim.cmd([[ hi link acrTaskDone Dimmed ]])

-- Illuminate
hl("illuminatedWord",      "NONE", "02",   "", "")
