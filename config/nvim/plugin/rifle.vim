if exists("g:loaded_rifle")
    finish
endif

let s:is_windows = (exists("g:is_windows") && g:is_windows == 1) || isdirectory('C:\')
let s:save_cpo = &cpo " Save user coptions
set cpo&vim " Reset them to their defaults

augroup rifle
    au!
    au BufNewFile,BufRead,BufWrite,BufEnter * let b:filename = expand("%")
augroup end

function! g:LaunchRifle(alt)
    if s:is_windows
        if !exists("b:rifle.win")
            echo "[Rifle] Windows Command (b:rifle.win) not found."
            return
        endif
        if a:alt
            lua require'rifle'.rifle(true, true)
        else
            lua require'rifle'.rifle(true, false)
        endif
    else
        if !exists("b:rifle.std")
            echo "[Rifle] Command (b:rifle.std) not found."
            return
        endif
        if a:alt
            lua require'rifle'.rifle(false, true)
        else
            lua require'rifle'.rifle(false, false)
        endif
    endif
endfunction

command! RifleRun call g:LaunchRifle(1)
command! Rifle call g:LaunchRifle(0)

let &cpo = s:save_cpo " Restore user coptions
unlet s:save_cpo " Remove temp var

let g:loaded_rifle = 1
