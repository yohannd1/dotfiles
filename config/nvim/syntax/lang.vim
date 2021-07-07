if exists("b:current_syntax") | finish | endif
let b:current_syntax = "lang"

let s:keywords = "mut fn macro"
for keyword in split(s:keywords, " ")
  exec printf('syn match langKeyword /\v<%s>/', keyword)
endfor

syn match langNumber /\v[0-9]+/

syn match langBuiltin /\v\@(call)>/
syn match langBuiltin /\v<(_G)>/

" let s:keywords = "_G @call"
" for keyword in split(s:keywords, " ")
"   exec printf('syn match langBuiltin /<%s>/', keyword)
" endfor

" syn match langStringSugar /\v\.[A-Za-z_][A-Za-z0-9_]*/

hi link langKeyword Keyword
hi link langNumber Number
hi link langBuiltin Special
hi link langStringSugar String

" syn match langAssignment /\v((mut)?\s*)[A-Za-z_][A-Za-z0-9_]*\s*\:\=/
" syn match langAssignment /\v[A-Za-z_][A-Za-z0-9_]*\s*\=/
" " syn match langAssignment /\v[A-Za-z_][A-Za-z0-9_]*/
" hi link langAssignment Function

" syn region wkComment start=/^%>/ end=/$/
" setlocal commentstring=\%>\ %s

" vim: sw=2 et
