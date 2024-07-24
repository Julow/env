" Settings for the quickfix window

" Improve status line
setlocal noruler
setlocal statusline=%{exists('w:quickfix_title')?w:quickfix_title:''}%= " Set by some commands
setlocal statusline+=%=%t " Right part
setlocal statusline+=\ %l/%L%(\ (%{S_selection('n')})%) " Cursor position and selection

setlocal number

function! Quickfix_open_current(wincmd)
  let l:is_loclist = getwininfo(win_getid())[0]['loclist']
  let l:n = line(".")
  execute a:wincmd
  execute ((l:is_loclist == 1) ? l:n . "ll" : l:n . "cc")
endfunction

" Enter to open a quickfix entry in a new split
" C-w Enter to a new tabs
nnoremap <buffer> <Enter> :call Quickfix_open_current("new")<return>
nnoremap <buffer> <C-w><Enter> :call Quickfix_open_current("tab new") <bar> copen<return>

" C-Enter to open a quickfix entry and close the quickfix window
nnoremap <buffer> <C-Enter> :call Quickfix_open_current("new") <bar> cclose<return>

" Let the quickfix window resize like the other windows.
setlocal nowinfixheight

