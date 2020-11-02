" Nested folding
let g:markdown_fold_style = 'nested'

" Tabs to 2 spaces
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2

" Enable concealing
set conceallevel=2
let g:vim_markdown_conceal_code_blocks = 0

if !exists("*With_variable")
  function! With_variable(key, val, exc)
    if exists('g:' . a:key)
      let orig = get(g:, a:key)
      execute "let g:" . a:key . " = a:val"
      execute a:exc
      execute "let g:" . a:key . " = orig"
    else
      execute "let g:" . a:key . " = a:val"
      execute a:exc
      execute "unlet g:" . a:key
    endif
  endfunction
endif

nmap gf <Plug>Markdown_EditUrlUnderCursor
nmap gF :call With_variable('vim_markdown_edit_url_in', 'hsplit', 'normal gf')<return>
nmap <C-w>gf :call With_variable('vim_markdown_edit_url_in', 'tab', 'normal gf')<return>
