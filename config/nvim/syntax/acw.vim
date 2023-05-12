" vim: sw=2 et

if exists("b:current_syntax") | finish | endif
let b:current_syntax = "acw"

setlocal foldmethod=syntax
setlocal indentexpr=indent(v:lnum) " dummy indent

" Keywords and stuff {{{
syn match acwSymbolS /\v\@(fold|end)@!/ nextgroup=acwSymbol
syn match acwSymbol /\v(\w+)/ contained

let s:builtins = ["set", "get"]
for builtin_name in s:builtins
  exec 'syn match acwBuiltin /\v\@' .. builtin_name .. '>/'
endfor

let s:builtins = ["-fold"]
for builtin_name in s:builtins
  exec 'syn match acwBuiltin /\v\%' .. builtin_name .. '>/'
endfor
" }}}

syn match acwHeaderOption /\v^\s*\%:(\w+)/

" Folding + @fold/@end {{{
" This one is reponsible for @fold foldings and also the highlighting of
" @fold and @end as builtins, because I couldn't get it to do it in any
" other way
syn region acwFoldSym matchgroup=acwBuiltin fold transparent
      \ start='\v^\@fold>\ze(.*):(\s*)$' end='\v^\@end>'

" I'm not fully sure what this means
" Origin: https://www.vim.org/scripts/script.php?script_id=2462
syn region acwFoldTag fold transparent
      \ start="\v\%-fold" end="\ze\%(\s*\n\)\+\%(\z1\s\)\@!."
" }}}

syn region acwComment start=/%%/ end=/$/
setlocal commentstring=\%%\ %s

hi link acwSymbolS Function
hi link acwSymbol Function
hi link acwComment Comment
hi link acwBuiltin Keyword
hi link acwSymbol Function
hi link acwHeaderOption Function

syn match acwTaskTodo /\v^(\s*)?([*-]+\s+)?(\[ \]|\( \))/
syn match acwTaskDone /\v^(\s*)?([*-]+\s+)?(\[x\]|\(x\))/

" TODO: get indent size from header (in plugin file, not here...)

" shit {{{
" FIXME: idk if I really want headings on here. I think I'll just make
" that a @func

" syn region acwHeading1 start=/\v^\*{1}/ end=/$/ contains=acwH3Init

" syn region acwHeading2 start=/\v^\*{2}/ end=/$/ contains=acwH2Init
" syn match acwH2Init /\v^\*{1}/ containedin=acwH2Init contained
" hi link acwH2Init Comment

" syn region acwHeading3 start=/\v^\*{3}/ end=/$/ contains=acwH3Init
" syn match acwH3Init /\v^\*{2}/ containedin=acwH3Init contained
" hi link acwH3Init Comment

" TODO: make this be variable on amount, so 4+ can work

" syn match Normal /\v\*[^*]/ conceal cchar=◉
" syn match Normal /\v\*\*/ms=s+1 conceal cchar=◎
"
" hi link acwHeading1 Keyword
" hi link acwHeading2 Function
" hi link acwHeading3 Delimiter
" hi link acwHeading4More Number
" }}}
