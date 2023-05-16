let s:URL_CHARS_MATCH = '[a-zA-Z0-9/\-\.%_?#=&+~:]'
let s:VIMW_URL_REGEX = '\v\[\[\zs(' . s:URL_CHARS_MATCH . '+)\ze'
let s:ACR_URL_REGEX = '\v\@ref\(\zs(' . s:URL_CHARS_MATCH . '+)\ze\)'

function! acrylic#find_ref(flags)
    let query = '\v(' . s:VIMW_URL_REGEX . '|' . s:ACR_URL_REGEX . ')'

    call search(query, a:flags)
endfunction

function! s:AcrTryGetUrl(word)
    " Vimwiki reminiscent refs
    let l:res = a:word->matchstr(s:VIMW_URL_REGEX)
    if l:res != ""
        return l:res
    endif

    " ACR refs
    let l:res = a:word->matchstr(s:ACR_URL_REGEX)
    if l:res != ""
        return l:res
    endif

    return ""
endfunction

function! acrylic#open_cword()
    let l:cword = expand("<cWORD>") " FIXME: isntead of this, just get matches in a line and check which one the cursor is currently inside.
    let l:result = s:AcrTryGetUrl(l:cword)

    if l:result == ""
        echo "Could not find suitable URL in current WORD"
        return
    endif

    if matchstr(l:result, '\v\.\zs(\w+)\ze$') != ""
        " There's an extension - don't question it and just open
        exec 'edit ' .. l:result
    else
        " There's no extension - check if the file exists. If not,
        " search for .wiki and lastly .acr.
        if filereadable(l:result . ".wiki")
            exec 'edit ' .. l:result . ".wiki"
        elseif filereadable(l:result . ".acr")
            exec 'edit ' .. l:result . ".acr"
        else
            exec 'edit ' .. l:result
        endif
    endif
endfunction
