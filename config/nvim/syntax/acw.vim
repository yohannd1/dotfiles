if exists("b:current_syntax") | finish | endif
let b:current_syntax = "acw"

" TODO: make this work properly:
" syn region acwHeader start=/^/ end=/\n\n/ contains=acwHeaderOption
" hi link acwHeader Function

syn match acwFunction /\v\@(\w+)/
syn match acwHeaderOption /\v^\s*\%:(\w+)/

syn match acwInlineCall /\v^\s*\%\@(\w+)/

let s:builtins = []
for builtin_name in s:builtins
  exec 'syn match acwBuiltin /\v\@' .. builtin_name .. '>/'
endfor

hi def link acwLink Underlined

syn match acwDelimiter /\v[\[\]{}]/

syn region acwComment start=/^%>/ end=/$/
setlocal commentstring=\%>\ %s

hi link acwComment Comment
hi link acwBuiltin Keyword
hi link acwFunction Function
hi link acwInlineCall Function
hi link acwHeaderOption Function
hi link acwDelimiter Delimiter

hi link acwDocument Comment

" vim: sw=2 et
