" Based off base16-vim (https://github.com/chriskempson/base16-vim)

" Color definitions
let s:base00 = "00"
let s:base01 = "01"
let s:base02 = "02"
let s:base03 = "03"
let s:base04 = "04"
let s:base05 = "05"
let s:base06 = "06"
let s:base07 = "07"
let s:base08 = "08"
let s:base09 = "09"
let s:base0A = "10"
let s:base0B = "11"
let s:base0C = "12"
let s:base0D = "13"
let s:base0E = "14"
let s:base0F = "15"

" Theme setup
hi clear
syntax reset
let g:colors_name = "base16"

" Highlighting function
" Optional variables are attributes and guisp
function! g:Base16Highlight(group, guifg, guibg, ctermfg, ctermbg, ...)
  let l:attr = get(a:, 1, "")
  let l:guisp = get(a:, 2, "")

  if a:guifg != ""
    exec "hi " . a:group . " guifg=#" . a:guifg
  endif
  if a:guibg != ""
    exec "hi " . a:group . " guibg=#" . a:guibg
  endif
  if a:ctermfg != ""
    exec "hi " . a:group . " ctermfg=" . a:ctermfg
  endif
  if a:ctermbg != ""
    exec "hi " . a:group . " ctermbg=" . a:ctermbg
  endif
  if l:attr != ""
    exec "hi " . a:group . " gui=" . l:attr . " cterm=" . l:attr
  endif
  if l:guisp != ""
    exec "hi " . a:group . " guisp=#" . l:guisp
  endif
endfunction

fun <sid>hi(group, ctermfg, ctermbg, attr, guisp)
  call g:Base16Highlight(a:group, "", "", a:ctermfg, a:ctermbg, a:attr, a:guisp)
endfun

" Vim editor colors
call <sid>hi("Normal",        s:base05, "", "", "")
call <sid>hi("Bold",          "", "", "bold", "")
call <sid>hi("Debug",         s:base08, "", "", "")
call <sid>hi("Directory",     s:base0D, "", "", "")
call <sid>hi("Error",         s:base00, s:base08, "", "")
call <sid>hi("ErrorMsg",      s:base08, "", "", "")
call <sid>hi("Exception",     s:base08, "", "", "")
call <sid>hi("FoldColumn",    s:base0C, s:base02, "", "")
call <sid>hi("Folded",        s:base03, s:base02, "", "")
call <sid>hi("IncSearch",     s:base01, s:base09, "none", "")
call <sid>hi("Italic",        "", "", "none", "")
call <sid>hi("Macro",         s:base08, "", "", "")
call <sid>hi("MatchParen",    "", s:base03,  "", "")
call <sid>hi("ModeMsg",       s:base0B, "", "", "")
call <sid>hi("MoreMsg",       s:base0B, "", "", "")
call <sid>hi("Question",      s:base0D, "", "", "")
call <sid>hi("Search",        s:base01, s:base0A,  "", "")
call <sid>hi("Substitute",    s:base01, s:base0A, "none", "")
call <sid>hi("SpecialKey",    s:base03, "", "", "")
call <sid>hi("TooLong",       s:base08, "", "", "")
call <sid>hi("Underlined",    s:base08, "", "", "")
call <sid>hi("Visual",        "", s:base02, "", "")
call <sid>hi("VisualNOS",     s:base08, "", "", "")
call <sid>hi("WarningMsg",    s:base08, "", "", "")
call <sid>hi("WildMenu",      s:base08, "", "", "")
call <sid>hi("Title",         s:base0D, "", "none", "")
call <sid>hi("Conceal",       s:base0D, s:base00, "", "")
call <sid>hi("Cursor",        s:base00, s:base05, "", "")
call <sid>hi("NonText",       s:base03, "", "", "")
call <sid>hi("LineNr",        s:base03, s:base01, "", "")
call <sid>hi("SignColumn",    s:base03, s:base01, "", "")
call <sid>hi("StatusLine",    s:base04, s:base02, "none", "")
call <sid>hi("StatusLineNC",  s:base03, s:base01, "none", "")
call <sid>hi("VertSplit",     s:base02, s:base02, "none", "")
call <sid>hi("ColorColumn",   s:base01, "none", "", "")
call <sid>hi("CursorColumn",  s:base01, "", "", "")
call <sid>hi("CursorLine",    "", s:base01, "NONE", "")
call <sid>hi("CursorLineNr",  "", s:base01, "italic", "")
call <sid>hi("QuickFixLine",  s:base01, "none", "", "")
call <sid>hi("PMenu",         s:base05, s:base01, "none", "")
call <sid>hi("PMenuSel",      s:base01, s:base05, "", "")
call <sid>hi("TabLine",       s:base03, s:base01, "none", "")
call <sid>hi("TabLineFill",   s:base03, "", "none", "")
call <sid>hi("TabLineSel",    s:base0B, s:base02, "none", "")

" Standard syntax highlighting
call <sid>hi("Boolean",      s:base09, "", "", "")
call <sid>hi("Character",    s:base08, "", "", "")
call <sid>hi("Comment",      s:base03, "", "italic", "")
call <sid>hi("Conditional",  s:base0E, "", "", "")
call <sid>hi("Constant",     s:base09, "", "", "")
call <sid>hi("Define",       s:base0E, "", "none", "")
call <sid>hi("Delimiter",    s:base0F, "", "", "")
call <sid>hi("Float",        s:base09, "", "", "")
call <sid>hi("Function",     s:base0D, "", "", "")
call <sid>hi("Identifier",   s:base08, "", "none", "")
call <sid>hi("Include",      s:base0D, "", "", "")
call <sid>hi("Keyword",      s:base0E, "", "", "")
call <sid>hi("Label",        s:base0A, "", "", "")
call <sid>hi("Number",       s:base09, "", "", "")
call <sid>hi("Operator",     s:base05, "", "none", "")
call <sid>hi("PreProc",      s:base0A, "", "", "")
call <sid>hi("Repeat",       s:base0A, "", "", "")
call <sid>hi("Special",      s:base0C, "", "", "")
call <sid>hi("SpecialChar",  s:base0F, "", "", "")
call <sid>hi("Statement",    s:base08, "", "", "")
call <sid>hi("StorageClass", s:base0A, "", "", "")
call <sid>hi("String",       s:base0B, "", "", "")
call <sid>hi("Structure",    s:base0E, "", "", "")
call <sid>hi("Tag",          s:base0A, "", "", "")
call <sid>hi("Todo",         s:base0A, s:base01, "", "")
call <sid>hi("Type",         s:base0A, "", "none", "")
call <sid>hi("Typedef",      s:base0A, "", "", "")

" C highlighting
call <sid>hi("cOperator",    s:base0C, "", "", "")
call <sid>hi("cPreCondit",   s:base0E, "", "", "")

" C# highlighting
call <sid>hi("csClass",                 s:base0A, "", "", "")
call <sid>hi("csAttribute",             s:base0A, "", "", "")
call <sid>hi("csModifier",              s:base0E, "", "", "")
call <sid>hi("csType",                  s:base08, "", "", "")
call <sid>hi("csUnspecifiedStatement",  s:base0D, "", "", "")
call <sid>hi("csContextualStatement",   s:base0E, "", "", "")
call <sid>hi("csNewDecleration",        s:base08, "", "", "")

" CSS highlighting
call <sid>hi("cssBraces",      s:base05, "", "", "")
call <sid>hi("cssClassName",   s:base0E, "", "", "")
call <sid>hi("cssColor",       s:base0C, "", "", "")

" Diff highlighting
call <sid>hi("DiffAdd",      s:base0B, s:base01, "", "")
call <sid>hi("DiffChange",   s:base03, s:base01, "", "")
call <sid>hi("DiffDelete",   s:base08, s:base01, "", "")
call <sid>hi("DiffText",     s:base0D, s:base01, "", "")
call <sid>hi("DiffAdded",    s:base0B, s:base00, "", "")
call <sid>hi("DiffFile",     s:base08, s:base00, "", "")
call <sid>hi("DiffNewFile",  s:base0B, s:base00, "", "")
call <sid>hi("DiffLine",     s:base0D, s:base00, "", "")
call <sid>hi("DiffRemoved",  s:base08, s:base00, "", "")

" Git highlighting
call <sid>hi("gitcommitOverflow",       s:base08, "", "", "")
call <sid>hi("gitcommitSummary",        s:base0B, "", "", "")
call <sid>hi("gitcommitComment",        s:base03, "", "", "")
call <sid>hi("gitcommitUntracked",      s:base03, "", "", "")
call <sid>hi("gitcommitDiscarded",      s:base03, "", "", "")
call <sid>hi("gitcommitSelected",       s:base03, "", "", "")
call <sid>hi("gitcommitHeader",         s:base0E, "", "", "")
call <sid>hi("gitcommitSelectedType",   s:base0D, "", "", "")
call <sid>hi("gitcommitUnmergedType",   s:base0D, "", "", "")
call <sid>hi("gitcommitDiscardedType",  s:base0D, "", "", "")
call <sid>hi("gitcommitBranch",         s:base09, "", "bold", "")
call <sid>hi("gitcommitUntrackedFile",  s:base0A, "", "", "")
call <sid>hi("gitcommitUnmergedFile",   s:base08, "", "bold", "")
call <sid>hi("gitcommitDiscardedFile",  s:base08, "", "bold", "")
call <sid>hi("gitcommitSelectedFile",   s:base0B, "", "bold", "")

" GitGutter highlighting
call <sid>hi("GitGutterAdd",           s:base0B, s:base01, "", "")
call <sid>hi("GitGutterChange",        s:base0D, s:base01, "", "")
call <sid>hi("GitGutterDelete",        s:base08, s:base01, "", "")
call <sid>hi("GitGutterChangeDelete",  s:base0E, s:base01, "", "")

" HTML highlighting
call <sid>hi("htmlBold",    s:base0A, "", "", "")
call <sid>hi("htmlItalic",  s:base0E, "", "", "")
call <sid>hi("htmlEndTag",  s:base05, "", "", "")
call <sid>hi("htmlTag",     s:base05, "", "", "")

" JavaScript highlighting
call <sid>hi("javaScript",        s:base05, "", "", "")
call <sid>hi("javaScriptBraces",  s:base05, "", "", "")
call <sid>hi("javaScriptNumber",  s:base09, "", "", "")

" pangloss/vim-javascript highlighting
call <sid>hi("jsOperator",          s:base0D, "", "", "")
call <sid>hi("jsStatement",         s:base0E, "", "", "")
call <sid>hi("jsReturn",            s:base0E, "", "", "")
call <sid>hi("jsThis",              s:base08, "", "", "")
call <sid>hi("jsClassDefinition",   s:base0A, "", "", "")
call <sid>hi("jsFunction",          s:base0E, "", "", "")
call <sid>hi("jsFuncName",          s:base0D, "", "", "")
call <sid>hi("jsFuncCall",          s:base0D, "", "", "")
call <sid>hi("jsClassFuncName",     s:base0D, "", "", "")
call <sid>hi("jsClassMethodType",   s:base0E, "", "", "")
call <sid>hi("jsRegexpString",      s:base0C, "", "", "")
call <sid>hi("jsGlobalObjects",     s:base0A, "", "", "")
call <sid>hi("jsGlobalNodeObjects", s:base0A, "", "", "")
call <sid>hi("jsExceptions",        s:base0A, "", "", "")
call <sid>hi("jsBuiltins",          s:base0A, "", "", "")

" Mail highlighting
call <sid>hi("mailQuoted1",  s:base0A, "", "", "")
call <sid>hi("mailQuoted2",  s:base0B, "", "", "")
call <sid>hi("mailQuoted3",  s:base0E, "", "", "")
call <sid>hi("mailQuoted4",  s:base0C, "", "", "")
call <sid>hi("mailQuoted5",  s:base0D, "", "", "")
call <sid>hi("mailQuoted6",  s:base0A, "", "", "")
call <sid>hi("mailURL",      s:base0D, "", "", "")
call <sid>hi("mailEmail",    s:base0D, "", "", "")

" Markdown highlighting
call <sid>hi("markdownCode",              s:base0B, "", "", "")
call <sid>hi("markdownError",             s:base05, s:base00, "", "")
call <sid>hi("markdownCodeBlock",         s:base0B, "", "", "")
call <sid>hi("markdownHeadingDelimiter",  s:base0D, "", "", "")

" NERDTree highlighting
call <sid>hi("NERDTreeDirSlash",  s:base0D, "", "", "")
call <sid>hi("NERDTreeExecFile",  s:base05, "", "", "")

" PHP highlighting
call <sid>hi("phpMemberSelector",  s:base05, "", "", "")
call <sid>hi("phpComparison",      s:base05, "", "", "")
call <sid>hi("phpParent",          s:base05, "", "", "")
call <sid>hi("phpMethodsVar",      s:base0C, "", "", "")

" Python highlighting
call <sid>hi("pythonOperator",  s:base0E, "", "", "")
call <sid>hi("pythonRepeat",    s:base0E, "", "", "")
call <sid>hi("pythonInclude",   s:base0E, "", "", "")
call <sid>hi("pythonStatement", s:base0E, "", "", "")

" Ruby highlighting
call <sid>hi("rubyAttribute",               s:base0D, "", "", "")
call <sid>hi("rubyConstant",                s:base0A, "", "", "")
call <sid>hi("rubyInterpolationDelimiter",  s:base0F, "", "", "")
call <sid>hi("rubyRegexp",                  s:base0C, "", "", "")
call <sid>hi("rubySymbol",                  s:base0B, "", "", "")
call <sid>hi("rubyStringDelimiter",         s:base0B, "", "", "")

" SASS highlighting
call <sid>hi("sassidChar",     s:base08, "", "", "")
call <sid>hi("sassClassChar",  s:base09, "", "", "")
call <sid>hi("sassInclude",    s:base0E, "", "", "")
call <sid>hi("sassMixing",     s:base0E, "", "", "")
call <sid>hi("sassMixinName",  s:base0D, "", "", "")

" Signify highlighting
call <sid>hi("SignifySignAdd",     s:base0B, s:base01, "", "")
call <sid>hi("SignifySignChange",  s:base0D, s:base01, "", "")
call <sid>hi("SignifySignDelete",  s:base08, s:base01, "", "")

" Spelling highlighting
call <sid>hi("SpellBad",     "", "", "undercurl", "")
call <sid>hi("SpellLocal",   "", "", "undercurl", "")
call <sid>hi("SpellCap",     "", "", "undercurl", "")
call <sid>hi("SpellRare",    "", "", "undercurl", "")

" Startify highlighting
call <sid>hi("StartifyBracket",  s:base03, "", "", "")
call <sid>hi("StartifyFile",     s:base07, "", "", "")
call <sid>hi("StartifyFooter",   s:base03, "", "", "")
call <sid>hi("StartifyHeader",   s:base0B, "", "", "")
call <sid>hi("StartifyNumber",   s:base09, "", "", "")
call <sid>hi("StartifyPath",     s:base03, "", "", "")
call <sid>hi("StartifySection",  s:base0E, "", "", "")
call <sid>hi("StartifySelect",   s:base0C, "", "", "")
call <sid>hi("StartifySlash",    s:base03, "", "", "")
call <sid>hi("StartifySpecial",  s:base03, "", "", "")

" Java highlighting
call <sid>hi("javaOperator",     s:base0D, "", "", "")

" Remove functions
delf <sid>hi
