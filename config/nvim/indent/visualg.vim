if exists("b:did_indent") | finish | endif
let b:did_indent = 1

setlocal indentexpr=GetVisualgIndent(v:lnum)

function! GetVisualgIndent(lnum)
  let sw = &shiftwidth
  let current_line_n = a:lnum
  let current_line = getline(a:lnum)

  let prev_line_n = prevnonblank(a:lnum - 1)
  let prev_line = getline(prev_line_n)

  let closing_bracket_line = '\v^\s*[)\]}]+\s*$'

  let closing_opening_else_line = '\v^\s*\}\s*else\s*\{\s*$'
  let ends_with_open_bracket = '\v[{\[(]\s*$'

  if current_line =~ '\v^\s*<(algoritmo|var|inicio|fimalgoritmo)>'
      return 0
  endif

  let l:indent = indent(prev_line_n)
  if prev_line =~ '\v^\s*<(var|inicio|repita|enquanto|se|para|senao)>'
      let l:indent += sw
  elseif prev_line =~ '\v<(entao|faca)>$'
      return indent(prev_line_n) + sw
  endif

  if current_line =~ '\v^\s*<(fimse|fimenquanto|fimpara|ate|senao)>'
      let l:indent -= sw
  endif


  " current line is a closing bracket, or an else line
  if current_line =~ closing_bracket_line || current_line =~ closing_opening_else_line
    " if the previous line is an opening bracket, just return the same
    " indent as it
    if prev_line =~ ends_with_open_bracket || prev_line =~ closing_opening_else_line
      return indent(prev_line_n)
    end

    return indent(prev_line_n) - sw
  endif

  " previous line is a closing bracket
  if prev_line =~ closing_bracket_line
    return indent(prev_line_n)
  endif

  " previous line is an opening bracket, or an else line
  if prev_line =~ ends_with_open_bracket || prev_line =~ closing_opening_else_line
    return indent(prev_line_n) + sw
  endif

  return l:indent
endfunction

" vim: sw=2 et
