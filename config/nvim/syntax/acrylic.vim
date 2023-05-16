" vim: sw=2 et

if exists("b:current_syntax") | finish | endif
let b:current_syntax = "acrylic"

setlocal foldmethod=syntax
setlocal indentexpr=indent(v:lnum) " dummy indent

" Keywords and stuff {{{
syn match acrSymbolS /\v\@(fold|end)@!/ nextgroup=acrSymbol
syn match acrSymbol /\v(\w+)/ contained

let s:builtins = ["set", "get", "ref"]
for builtin_name in s:builtins
  exec 'syn match acrBuiltin /\v\@' .. builtin_name .. '>/'
endfor

let s:builtins = ["-fold", "-id"]
for builtin_name in s:builtins
  exec 'syn match acrBuiltin /\v\%' .. builtin_name .. '>/'
endfor
" }}}

syn match acrHeaderOption /\v^\s*\%:(\w+)/

" Folding + @fold/@end {{{
" This one is reponsible for @fold foldings and also the highlighting of
" @fold and @end as builtins, because I couldn't get it to do it in any
" other way
syn region acrFoldSym matchgroup=acrBuiltin fold transparent
      \ start='\v^\@fold>\ze(.*):(\s*)$' end='\v^\@end>'

" I'm not fully sure what this means
" Origin: https://www.vim.org/scripts/script.php?script_id=2462
syn region acrFoldTag fold transparent
      \ start="\v\%-fold" end="\ze\%(\s*\n\)\+\%(\z1\s\)\@!."
" }}}

syn region acrComment start=/%%/ end=/$/

hi link acrSymbolS Function
hi link acrSymbol Function
hi link acrComment Comment
hi link acrBuiltin Keyword
hi link acrSymbol Function
hi link acrHeaderOption Function

syn match acrTaskTodo /\v^(\s*)?([*-]+\s+)?(\[ \]|\( \))/
syn match acrTaskDone /\v^(\s*)?([*-]+\s+)?(\[x\]|\(x\))/

let s:URL_CHARS_MATCH = '[a-zA-Z0-9/\-\.%_?#=&+~:]'
let s:VIMW_URL_REGEX = '\v\[\[(' . s:URL_CHARS_MATCH . '+)(\|.*)?\]\]'
call matchadd("acrDeprecatedRef", s:VIMW_URL_REGEX)

hi link acrDeprecatedRef Function
