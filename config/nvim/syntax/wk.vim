if exists("b:current_syntax")
  finish
endif

syn match wkKeyword /\v\%(header)/
syn match wkFunction /\v\@(\w)+/
syn match wkDelimiter /\v[(){}]/

syn region wkComment start=/^%>/ end=/$/
setlocal commentstring=\%>\ %s

hi link wkComment Comment
hi link wkKeyword Keyword
hi link wkFunction Function
hi link wkDelimiter Delimiter

let b:current_syntax = "wk"

" vim: sw=2 et
