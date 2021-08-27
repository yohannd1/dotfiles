if exists("b:current_syntax") | finish | endif
let b:current_syntax = "visualg"

syn match visualgFlowKeyword /\v<(algoritmo|fimalgoritmo|inicio|var)>/
syn match visualgFlowKeyword /\v<(se|entao|senao|fimse)>/
syn match visualgFlowKeyword /\v<(repita|ate)>/
syn match visualgFlowKeyword /\v<(para|de|fimpara)>/
syn match visualgFlowKeyword /\v<(enquanto|faca|fimenquanto)>/

syn match visualgDeclAssign /\v^\s*<[a-zA-Z_][a-zA-Z0-9_]*>(\s*,\s*<[a-zA-Z_][a-zA-Z0-9_]*>)*\s*(:|\<-)\s*/
syn match visualgType /\v<(caractere|inteiro|real)>/

syn match visualgConstant /\v<(verdadeiro|falso)>/
syn match visualgBooleanOperator /\v<(e|ou|xou)>/
syn match visualgNumber /\v<[0-9]+>/
syn match visualgBuiltin /\v<(escreva|escreval|leia)>/

syn region visualgString start=/"/ end=/"/

syn region visualgComment start='\s*//' end=/$/
setlocal commentstring=//\ %s

hi link visualgFlowKeyword Keyword
hi link visualgDeclAssign Function
hi link visualgString String
hi link visualgComment Comment
hi link visualgConstant Boolean
hi link visualgBooleanOperator Special
hi link visualgNumber Number
hi link visualgBuiltin Special
hi link visualgType Special

" vim: sw=2 et
