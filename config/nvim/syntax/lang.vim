if exists("b:current_syntax") | finish | endif
let b:current_syntax = "lang"

syn match langKeyword /\v<(fn|if|else|match|for|return|comptime|runtime)>/
syn match langDeclKeyword /\v<(let|var)>/
syn match langConstant /\v<(nil|true|false)>/

syn match langKeyword /\v\@/

syn match langOperator /?=/ " default argument operator (FIXME: is this needed?)
syn match langOperator /|>/ " left-to-right piping operator
syn match langOperator /<|/ " right-to-left piping operator
syn match langOperator /!/ " next-expr-is-arg operator
syn match langOperator /->/ " return operator

syn match langBooleanOperator /\v<(and|or|not)>/

syn match langDeclOperator /:=/

syn match langNumber /\v<[0-9]+>/

" syn match langBuiltin /\v\@(kind)>/
syn match langAttribute /\v\#(inline|macro)>/
syn match langBuiltin /\v<(_G)>/

syn match langStringSugar /\v\.[A-Za-z_][A-Za-z0-9_]*/
syn match langMethodSugar /\v\:[A-Za-z_][A-Za-z0-9_]*/

hi link langKeyword Keyword
hi link langNumber Number
hi link langBuiltin Special
hi link langAttribute Special
hi link langString String
hi link langStringSugar String
hi link langMethodSugar Function
hi link langComment Comment
hi link langOperator Operator
hi link langConstant Boolean
hi link langDeclOperator Function
hi link langDeclKeyword Function
hi link langBooleanOperator Special

" syn match langAssignment /\v((mut)?\s*)[A-Za-z_][A-Za-z0-9_]*\s*\:\=/
" syn match langAssignment /\v[A-Za-z_][A-Za-z0-9_]*\s*\=/
" " syn match langAssignment /\v[A-Za-z_][A-Za-z0-9_]*/
" hi link langAssignment Function
"
syn region langString start=/"/ end=/"/

syn region langComment start='\s*//' end=/$/
setlocal commentstring=//\ %s

" vim: sw=2 et
