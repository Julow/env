" Plugin 'vim-markdown-folding'
let g:markdown_fold_style = 'nested'
let g:markdown_fold_override_foldtext = 0
let g:vim_markdown_folding_disabled = 1

" Tabs to 2 spaces
setlocal expandtab
setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2

" Break line at word boundaries
setlocal linebreak

" Indent in lists
let g:vim_markdown_new_list_item_indent = 2
" But disable automatic list bullets
let g:vim_markdown_auto_insert_bullets = 0

" Enable concealing
setlocal conceallevel=3
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

" Links

let g:vim_markdown_follow_anchor = 1

nmap <buffer> gf <Plug>Markdown_EditUrlUnderCursor
nmap <buffer> gF :call With_variable('vim_markdown_edit_url_in', 'hsplit', 'normal gf')<return>
nmap <buffer> <C-w>gf :call With_variable('vim_markdown_edit_url_in', 'tab', 'normal gf')<return>

" Returns a path equivalent to 'path' that is relative to 'relative_to_file'.
function! Path_relative_to(path, relative_to_file)
  let abspath = split(fnamemodify(a:path, ":."), "/")
  let absrelto = split(fnamemodify(a:relative_to_file, ":.:h"), "/")
  let l = min([ len(abspath), len(absrelto) ])
  let i = 0
  while i < l && abspath[i] == absrelto[i] | let i += 1 | endwhile
  return join(map(absrelto[i:], '".."') + abspath[i:], '/')
endfunction

function! Markdown_link_of_path(path, relative_to)
  let path = Path_relative_to(trim(a:path), a:relative_to)
  let name = fnamemodify(path, ":t:r")
  return "[" . name . "](" . path . ")"
endfunction

" Insert a markdown link. Expect a path in clipboard ("+) (either relative to
" the current directory or absolute)
nnoremap c<C-l> "=Markdown_link_of_path(@+, @%)<return>p
inoremap <C-l> <C-r>=Markdown_link_of_path(@+, @%)<return>
