"
" Remember and restore Fcitx's state for each buffer separately when
" leaving/entering insert mode, while always switch Fcitx off in normal
" mode.
"
" Credit: https://github.com/lilydjwg/fcitx.vim
"

if &compatible || 
        \ exists("g:fcitx_loaded") ||
        \ (!exists('$DISPLAY') || exists('$SSH_TTY'))
    finish
endif

if executable('fcitx5-remote')
    let g:fcitx_remote = 'fcitx5-remote'
elseif executable('fcitx-remote')
    let g:fcitx_remote = 'fcitx-remote'
else
    finish
endif

function Fcitx2en()
    if system(g:fcitx_remote) == 2  " Fcitx is active
        let b:fcitx_toggle = 1
        call system(g:fcitx_remote . ' -c')  " inactivate
    endif
endfunction

function Fcitx2zh()
    try
        if b:fcitx_toggle == 1
            call system(g:fcitx_remote . ' -o')  " activate
            let b:fcitx_toggle = 0
        endif
    catch /fcitx_toggle/
        let b:fcitx_toggle = 0
    endtry
endfunction

autocmd InsertLeave * call Fcitx2en()
autocmd InsertEnter * call Fcitx2zh()

let g:fcitx_loaded = 1
