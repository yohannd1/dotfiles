" base16 theme
" Based off base16-vim (https://github.com/chriskempson/base16-vim)

hi clear
syntax reset
let g:colors_name = "base16"

let s:is_win = has("win32") || has("win64")
let s:is_linux = has("unix") && !has("macunix")
let s:is_mac = has("macunix")
let s:is_android = isdirectory("/sdcard")
let s:is_tty = $DISPLAY == "" && !g:is_android
let s:is_gui = has("gui_running") || exists("g:GuiFont") || (has("nvim") && nvim_list_uis()[0]["rgb"] == v:true)

let s:TTY = 0
let s:GUI = 1
let s:bases = {}

function s:xgetres(string)
    return split(system("xgetres " .. a:string), "\n")[0]
endfunction

let s:bases["00"] = ["", ""]
let s:bases["01"] = ["", ""]
let s:bases["02"] = ["", ""]
let s:bases["03"] = ["", ""]
let s:bases["04"] = ["", ""]
let s:bases["05"] = ["", ""]
let s:bases["06"] = ["", ""]
let s:bases["07"] = ["", ""]
let s:bases["08"] = ["", ""]
let s:bases["09"] = ["", ""]
let s:bases["0A"] = ["", ""]
let s:bases["0B"] = ["", ""]
let s:bases["0C"] = ["", ""]
let s:bases["0D"] = ["", ""]
let s:bases["0E"] = ["", ""]
let s:bases["0F"] = ["", ""]

if s:is_gui
    " if on GUI
    if s:is_linux && executable("xgetres")
        let s:bases["00"][s:GUI] = s:xgetres("nvim.base00")
        let s:bases["01"][s:GUI] = s:xgetres("nvim.base01")
        let s:bases["02"][s:GUI] = s:xgetres("nvim.base02")
        let s:bases["03"][s:GUI] = s:xgetres("nvim.base03")
        let s:bases["04"][s:GUI] = s:xgetres("nvim.base04")
        let s:bases["05"][s:GUI] = s:xgetres("nvim.base05")
        let s:bases["06"][s:GUI] = s:xgetres("nvim.base06")
        let s:bases["07"][s:GUI] = s:xgetres("nvim.base07")
        let s:bases["08"][s:GUI] = s:xgetres("nvim.base08")
        let s:bases["09"][s:GUI] = s:xgetres("nvim.base09")
        let s:bases["0A"][s:GUI] = s:xgetres("nvim.base0A")
        let s:bases["0B"][s:GUI] = s:xgetres("nvim.base0B")
        let s:bases["0C"][s:GUI] = s:xgetres("nvim.base0C")
        let s:bases["0D"][s:GUI] = s:xgetres("nvim.base0D")
        let s:bases["0E"][s:GUI] = s:xgetres("nvim.base0E")
        let s:bases["0F"][s:GUI] = s:xgetres("nvim.base0F")
    else
        let s:bases["00"][s:GUI] = "#f2e5bc"
        let s:bases["01"][s:GUI] = "#ebdbb2"
        let s:bases["02"][s:GUI] = "#d5c4a1"
        let s:bases["03"][s:GUI] = "#bdae93"
        let s:bases["04"][s:GUI] = "#665c54"
        let s:bases["05"][s:GUI] = "#504945"
        let s:bases["06"][s:GUI] = "#3c3836"
        let s:bases["07"][s:GUI] = "#282828"
        let s:bases["08"][s:GUI] = "#9d0006"
        let s:bases["09"][s:GUI] = "#af3a03"
        let s:bases["0A"][s:GUI] = "#b57614"
        let s:bases["0B"][s:GUI] = "#79740e"
        let s:bases["0C"][s:GUI] = "#427b58"
        let s:bases["0D"][s:GUI] = "#076678"
        let s:bases["0E"][s:GUI] = "#8f3f71"
        let s:bases["0F"][s:GUI] = "#d65d0e"
    endif

    let s:bases["00"][s:TTY] = "NONE"
    let s:bases["01"][s:TTY] = "NONE"
    let s:bases["02"][s:TTY] = "NONE"
    let s:bases["03"][s:TTY] = "NONE"
    let s:bases["04"][s:TTY] = "NONE"
    let s:bases["05"][s:TTY] = "NONE"
    let s:bases["06"][s:TTY] = "NONE"
    let s:bases["07"][s:TTY] = "NONE"
    let s:bases["08"][s:TTY] = "NONE"
    let s:bases["09"][s:TTY] = "NONE"
    let s:bases["0A"][s:TTY] = "NONE"
    let s:bases["0B"][s:TTY] = "NONE"
    let s:bases["0C"][s:TTY] = "NONE"
    let s:bases["0D"][s:TTY] = "NONE"
    let s:bases["0E"][s:TTY] = "NONE"
    let s:bases["0F"][s:TTY] = "NONE"
else
    " if not on GUI
    let s:bases["00"][s:TTY] = "00"
    let s:bases["01"][s:TTY] = "01"
    let s:bases["02"][s:TTY] = "02"
    let s:bases["03"][s:TTY] = "03"
    let s:bases["04"][s:TTY] = "04"
    let s:bases["05"][s:TTY] = "05"
    let s:bases["06"][s:TTY] = "06"
    let s:bases["07"][s:TTY] = "07"
    let s:bases["08"][s:TTY] = "08"
    let s:bases["09"][s:TTY] = "09"
    let s:bases["0A"][s:TTY] = "10"
    let s:bases["0B"][s:TTY] = "11"
    let s:bases["0C"][s:TTY] = "12"
    let s:bases["0D"][s:TTY] = "13"
    let s:bases["0E"][s:TTY] = "14"
    let s:bases["0F"][s:TTY] = "15"

    let s:bases["00"][s:GUI] = "NONE"
    let s:bases["01"][s:GUI] = "NONE"
    let s:bases["02"][s:GUI] = "NONE"
    let s:bases["03"][s:GUI] = "NONE"
    let s:bases["04"][s:GUI] = "NONE"
    let s:bases["05"][s:GUI] = "NONE"
    let s:bases["06"][s:GUI] = "NONE"
    let s:bases["07"][s:GUI] = "NONE"
    let s:bases["08"][s:GUI] = "NONE"
    let s:bases["09"][s:GUI] = "NONE"
    let s:bases["0A"][s:GUI] = "NONE"
    let s:bases["0B"][s:GUI] = "NONE"
    let s:bases["0C"][s:GUI] = "NONE"
    let s:bases["0D"][s:GUI] = "NONE"
    let s:bases["0E"][s:GUI] = "NONE"
    let s:bases["0F"][s:GUI] = "NONE"
endif

function! s:hl(group, fg_code, bg_code, attr, guisp)
  if a:fg_code != "NONE"
    exec "hi " . a:group . " guifg=" . s:bases[a:fg_code][s:GUI]
    exec "hi " . a:group . " ctermfg=" . s:bases[a:fg_code][s:TTY]
  endif
  if a:bg_code != "NONE"
    exec "hi " . a:group . " guibg=" . s:bases[a:bg_code][s:GUI]
    exec "hi " . a:group . " ctermbg=" . s:bases[a:bg_code][s:TTY]
  endif
  if a:attr != ""
    exec "hi " . a:group . " gui=" . a:attr . " cterm=" . a:attr
  endif
  if a:guisp != ""
    exec "hi " . a:group . " guisp=#" . a:guisp
  endif
endfunction

if has_key(g:, "rainbow_active") " rainbow parens configuration
    let colors = ["09", "0A", "0B", "0D", "0E"]

    if !has_key(g:, "rainbow_conf")
        let g:rainbow_conf = {}
    end

    if s:is_gui
        let rainbow_conf.guifgs = map(colors, {_, n -> s:bases[n][s:GUI]})
    else
        let rainbow_conf.ctermfgs = map(colors, {_, n -> s:bases[n][s:TTY]})
    end
end

" Vim editor colors
if s:is_gui
  call s:hl("Normal",      "05",   "00",   "",     "")
else
  call s:hl("Normal",      "05",   "NONE", "",     "")
endif

call s:hl("Bold",          "NONE", "NONE", "bold", "")
call s:hl("Debug",         "08",   "NONE", "", "")
call s:hl("Directory",     "0D",   "NONE", "", "")
call s:hl("Error",         "08",   "00",   "undercurl", "")
call s:hl("ErrorMsg",      "08",   "NONE", "", "")
call s:hl("Exception",     "08",   "NONE", "", "")
call s:hl("FoldColumn",    "0C",   "02",   "", "")
call s:hl("Folded",        "05",   "02",   "", "")
call s:hl("IncSearch",     "01",   "09",   "none", "")
call s:hl("Italic",        "NONE", "NONE", "italic", "")
call s:hl("Macro",         "08",   "NONE", "", "")
call s:hl("MatchParen",    "NONE", "0D",   "", "")
call s:hl("ModeMsg",       "0B",   "NONE", "", "")
call s:hl("MoreMsg",       "0B",   "NONE", "", "")
call s:hl("Question",      "0D",   "NONE", "", "")
call s:hl("Search",        "01",   "0A",   "", "")
call s:hl("Substitute",    "01",   "0A",   "none", "")
call s:hl("SpecialKey",    "03",   "NONE", "", "")
call s:hl("TooLong",       "08",   "NONE", "", "")
call s:hl("Underlined",    "08",   "NONE", "", "")
call s:hl("Visual",        "NONE", "02", "", "")
call s:hl("VisualNOS",     "08",   "NONE", "", "")
call s:hl("WarningMsg",    "08",   "NONE", "", "")
call s:hl("WildMenu",      "08",   "NONE", "", "")
call s:hl("Title",         "0D",   "NONE", "none", "")
call s:hl("Conceal",       "0D",   "00",   "", "")
call s:hl("Cursor",        "00",   "05",   "", "")
call s:hl("NonText",       "03",   "NONE", "", "")
call s:hl("LineNr",        "04",   "NONE", "", "")
call s:hl("SignColumn",    "03",   "01",   "", "")
call s:hl("StatusLine",    "03",   "02",   "none", "")
call s:hl("StatusLineNC",  "03",   "01",   "none", "")
call s:hl("VertSplit",     "02",   "02",   "none", "")
call s:hl("ColorColumn",   "01",   "NONE", "", "")
call s:hl("CursorColumn",  "01",   "NONE", "", "")
call s:hl("CursorLine",    "NONE", "01",   "NONE", "")
call s:hl("CursorColumn",  "NONE", "01",   "NONE", "")
call s:hl("CursorLineNr",  "NONE", "NONE", "italic", "")
call s:hl("QuickFixLine",  "01",   "NONE", "", "")
call s:hl("PMenu",         "05",   "01",   "none", "")
call s:hl("PMenuSel",      "01",   "05",   "", "")
call s:hl("TabLine",       "05",   "01",   "none", "")
call s:hl("TabLineFill",   "03",   "NONE", "none", "")
call s:hl("TabLineSel",    "0A",   "01",   "bold,italic", "")
call s:hl("MatchParen",    "00",   "03",   "none", "")

" Standard syntax highlighting
call s:hl("Boolean",      "09", "NONE", "", "")
call s:hl("Character",    "08", "NONE", "", "")
call s:hl("Comment",      "03", "NONE", "italic", "")
call s:hl("Conditional",  "0E", "NONE", "", "")
call s:hl("Constant",     "09", "NONE", "", "")
call s:hl("Define",       "0E", "NONE", "none", "")
call s:hl("Delimiter",    "0F", "NONE", "", "")
call s:hl("Float",        "09", "NONE", "", "")
call s:hl("Function",     "0D", "NONE", "", "")
call s:hl("Identifier",   "05", "NONE", "none", "")
call s:hl("Include",      "0D", "NONE", "", "")
call s:hl("Keyword",      "0E", "NONE", "bold", "")
call s:hl("Label",        "0A", "NONE", "", "")
call s:hl("Number",       "09", "NONE", "", "")
call s:hl("Operator",     "05", "NONE", "none", "")
call s:hl("PreProc",      "0A", "NONE", "", "")
call s:hl("Repeat",       "0A", "NONE", "", "")
call s:hl("Special",      "0C", "NONE", "", "")
call s:hl("SpecialChar",  "0F", "NONE", "", "")
call s:hl("Statement",    "08", "NONE", "", "")
call s:hl("StorageClass", "0A", "NONE", "", "")
call s:hl("String",       "0B", "NONE", "", "")
call s:hl("Structure",    "0E", "NONE", "", "")
call s:hl("Tag",          "0A", "NONE", "", "")
call s:hl("Todo",         "0A", "01",   "", "")
call s:hl("Type",         "0A", "NONE", "none", "")
call s:hl("Typedef",      "0A", "NONE", "", "")
call s:hl("Ignore",       "02", "NONE", "", "")

" C highlighting
call s:hl("cOperator",    "0C", "NONE", "", "")
call s:hl("cPreCondit",   "0E", "NONE", "", "")

" C# highlighting
call s:hl("csClass",                 "0A", "NONE", "", "")
call s:hl("csAttribute",             "0A", "NONE", "", "")
call s:hl("csModifier",              "0E", "NONE", "", "")
call s:hl("csType",                  "08", "NONE", "", "")
call s:hl("csUnspecifiedStatement",  "0D", "NONE", "", "")
call s:hl("csContextualStatement",   "0E", "NONE", "", "")
call s:hl("csNewDecleration",        "08", "NONE", "", "")

" CSS highlighting
call s:hl("cssBraces",      "05", "NONE", "", "")
call s:hl("cssClassName",   "0E", "NONE", "", "")
call s:hl("cssColor",       "0C", "NONE", "", "")

" Diff highlighting
call s:hl("DiffAdd",      "0B", "01", "", "")
call s:hl("DiffChange",   "03", "01", "", "")
call s:hl("DiffDelete",   "08", "01", "", "")
call s:hl("DiffText",     "0D", "01", "", "")
call s:hl("DiffAdded",    "0B", "00", "", "")
call s:hl("DiffFile",     "08", "00", "", "")
call s:hl("DiffNewFile",  "0B", "00", "", "")
call s:hl("DiffLine",     "0D", "00", "", "")
call s:hl("DiffRemoved",  "08", "00", "", "")

" Git highlighting
call s:hl("gitcommitOverflow",       "08", "NONE", "", "")
call s:hl("gitcommitSummary",        "0B", "NONE", "", "")
call s:hl("gitcommitComment",        "03", "NONE", "", "")
call s:hl("gitcommitUntracked",      "03", "NONE", "", "")
call s:hl("gitcommitDiscarded",      "03", "NONE", "", "")
call s:hl("gitcommitSelected",       "03", "NONE", "", "")
call s:hl("gitcommitHeader",         "0E", "NONE", "", "")
call s:hl("gitcommitSelectedType",   "0D", "NONE", "", "")
call s:hl("gitcommitUnmergedType",   "0D", "NONE", "", "")
call s:hl("gitcommitDiscardedType",  "0D", "NONE", "", "")
call s:hl("gitcommitBranch",         "09", "NONE", "bold", "")
call s:hl("gitcommitUntrackedFile",  "0A", "NONE", "", "")
call s:hl("gitcommitUnmergedFile",   "08", "NONE", "bold", "")
call s:hl("gitcommitDiscardedFile",  "08", "NONE", "bold", "")
call s:hl("gitcommitSelectedFile",   "0B", "NONE", "bold", "")

" GitGutter highlighting
call s:hl("GitGutterAdd",           "0B", "01", "", "")
call s:hl("GitGutterChange",        "0D", "01", "", "")
call s:hl("GitGutterDelete",        "08", "01", "", "")
call s:hl("GitGutterChangeDelete",  "0E", "01", "", "")

" HTML highlighting
call s:hl("htmlBold",    "0A", "NONE", "bold", "")
call s:hl("htmlItalic",  "0E", "NONE", "italic", "")
call s:hl("htmlEndTag",  "05", "NONE", "", "")
call s:hl("htmlTag",     "05", "NONE", "", "")

" JavaScript highlighting
call s:hl("javaScript",       "05", "NONE", "", "")
call s:hl("javaScriptBraces", "05", "NONE", "", "")
call s:hl("javaScriptNumber", "09", "NONE", "", "")
hi link javaScriptIdentifier Keyword

" pangloss/vim-javascript highlighting
call s:hl("jsOperator",          "0D", "NONE", "", "")
call s:hl("jsStatement",         "0E", "NONE", "", "")
call s:hl("jsReturn",            "0E", "NONE", "", "")
call s:hl("jsThis",              "08", "NONE", "", "")
call s:hl("jsClassDefinition",   "0A", "NONE", "", "")
call s:hl("jsFunction",          "0E", "NONE", "", "")
call s:hl("jsFuncName",          "0D", "NONE", "", "")
call s:hl("jsFuncCall",          "0D", "NONE", "", "")
call s:hl("jsClassFuncName",     "0D", "NONE", "", "")
call s:hl("jsClassMethodType",   "0E", "NONE", "", "")
call s:hl("jsRegexpString",      "0C", "NONE", "", "")
call s:hl("jsGlobalObjects",     "0A", "NONE", "", "")
call s:hl("jsGlobalNodeObjects", "0A", "NONE", "", "")
call s:hl("jsExceptions",        "0A", "NONE", "", "")
call s:hl("jsBuiltins",          "0A", "NONE", "", "")

" Mail highlighting
call s:hl("mailQuoted1",  "0A", "NONE", "", "")
call s:hl("mailQuoted2",  "0B", "NONE", "", "")
call s:hl("mailQuoted3",  "0E", "NONE", "", "")
call s:hl("mailQuoted4",  "0C", "NONE", "", "")
call s:hl("mailQuoted5",  "0D", "NONE", "", "")
call s:hl("mailQuoted6",  "0A", "NONE", "", "")
call s:hl("mailURL",      "0D", "NONE", "", "")
call s:hl("mailEmail",    "0D", "NONE", "", "")

" Markdown highlighting
call s:hl("markdownCode",              "0B", "NONE", "", "")
call s:hl("markdownError",             "05", "00", "", "")
call s:hl("markdownCodeBlock",         "0B", "NONE", "", "")
call s:hl("markdownHeadingDelimiter",  "0D", "NONE", "", "")

" NERDTree highlighting
call s:hl("NERDTreeDirSlash",  "0D", "NONE", "", "")
call s:hl("NERDTreeExecFile",  "05", "NONE", "", "")

" PHP highlighting
call s:hl("phpMemberSelector",  "05", "NONE", "", "")
call s:hl("phpComparison",      "05", "NONE", "", "")
call s:hl("phpParent",          "05", "NONE", "", "")
call s:hl("phpMethodsVar",      "0C", "NONE", "", "")

" Nim highlighting
hi link nimOperator Keyword
hi link nimOP9 Keyword
hi link nimOP5 Keyword
hi link nimOP4 Keyword
hi link nimOP3 Keyword

" Gdscript highlighting
hi link gdscriptOperator Keyword

" Python highlighting
call s:hl("pythonOperator",  "0E", "NONE", "", "")
call s:hl("pythonRepeat",    "0E", "NONE", "", "")
call s:hl("pythonInclude",   "0E", "NONE", "", "")
call s:hl("pythonStatement", "0E", "NONE", "", "")

" Ruby highlighting
call s:hl("rubyAttribute",               "0D", "NONE", "", "")
call s:hl("rubyConstant",                "0A", "NONE", "", "")
call s:hl("rubyInterpolationDelimiter",  "0F", "NONE", "", "")
call s:hl("rubyRegexp",                  "0C", "NONE", "", "")
call s:hl("rubySymbol",                  "0B", "NONE", "", "")
call s:hl("rubyStringDelimiter",         "0B", "NONE", "", "")

" SASS highlighting
call s:hl("sassidChar",     "08", "NONE", "", "")
call s:hl("sassClassChar",  "09", "NONE", "", "")
call s:hl("sassInclude",    "0E", "NONE", "", "")
call s:hl("sassMixing",     "0E", "NONE", "", "")
call s:hl("sassMixinName",  "0D", "NONE", "", "")

" Signify highlighting
call s:hl("SignifySignAdd",     "0B", "01", "", "")
call s:hl("SignifySignChange",  "0D", "01", "", "")
call s:hl("SignifySignDelete",  "08", "01", "", "")

" Spelling highlighting
call s:hl("SpellBad",     "NONE", "NONE", "undercurl", "")
call s:hl("SpellLocal",   "NONE", "NONE", "undercurl", "")
call s:hl("SpellCap",     "NONE", "NONE", "undercurl", "")
call s:hl("SpellRare",    "NONE", "NONE", "undercurl", "")

" Startify highlighting
call s:hl("StartifyBracket",  "03", "NONE", "", "")
call s:hl("StartifyFile",     "07", "NONE", "", "")
call s:hl("StartifyFooter",   "03", "NONE", "", "")
call s:hl("StartifyHeader",   "0B", "NONE", "", "")
call s:hl("StartifyNumber",   "09", "NONE", "", "")
call s:hl("StartifyPath",     "03", "NONE", "", "")
call s:hl("StartifySection",  "0E", "NONE", "", "")
call s:hl("StartifySelect",   "0C", "NONE", "", "")
call s:hl("StartifySlash",    "03", "NONE", "", "")
call s:hl("StartifySpecial",  "03", "NONE", "", "")

" Java highlighting
call s:hl("javaOperator",      "0D", "NONE", "", "")

" Zig highlighting
call s:hl("zigComparatorWord",           "0C", "NONE", "", "")
hi link zigStringDelimiter Delimiter
hi link zigMultilineStringDelimiter Delimiter
hi link zigMultilineStringDelimiter Delimiter

" Typescript highlighting
hi link typescriptVariable Function

" Lua highlighting
hi link luaOperator Keyword
hi link luaIn Keyword

" Clap (https://github.com/liuchengxu/vim-clap) highlighting
call s:hl("ClapInput",            "NONE", "02",   "", "")
call s:hl("ClapDisplay",          "NONE", "00",   "NONE", "")
call s:hl("ClapSelected",         "NONE", "01",   "NONE", "")
call s:hl("ClapCurrentSelection", "NONE", "01",   "NONE", "")
call s:hl("ClapMatches",          "0A",   "00",   "NONE", "")
call s:hl("ClapMatches1",         "0A",   "00",   "NONE", "")
call s:hl("ClapMatches2",         "0A",   "00",   "NONE", "")
call s:hl("ClapMatches3",         "0A",   "00",   "NONE", "")
call s:hl("ClapMatches4",         "0A",   "00",   "NONE", "")
call s:hl("ClapMatches5",         "0A",   "00",   "NONE", "")
call s:hl("ClapMatches6",         "0A",   "00",   "NONE", "")
call s:hl("ClapMatches7",         "0A",   "00",   "NONE", "")
call s:hl("ClapMatches8",         "0A",   "00",   "NONE", "")
call s:hl("ClapNoMatchesFound",   "08",   "NONE", "NONE", "")
" See also: ClapPreview ClapDefaultSelected ClapDefaultCurrentSelection

" Vimwiki
call s:hl("VimwikiXTodo",         "0B", "01",   "", "")
call s:hl("VimwikiXDone",         "03", "NONE",   "", "")

" Illuminate
call s:hl("illuminatedWord",      "NONE", "02",   "", "")
