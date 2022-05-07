" Don't expand tabs
setlocal noexpandtab
setlocal softtabstop=0
setlocal shiftwidth=0

" Show tabs, nice with g:tsv_padding = 1
setlocal listchars=tab:\ \ ⎸
setlocal list

" <Tab> and <S-Tab> to navigate between cells
nnoremap <Tab> f<Tab>l
nnoremap <S-Tab> F<Tab>:exe "norm!" ((search("	", "bs", line(".")) == 0) ? "^" : "l")<return>

" Return the width of each columns in a line (without the tab character)
function! TSV_column_widths(line)
  " Comment lines
  if a:line =~ '^#'
    return [0]
  endif
  let i = 0
  let tabs = []
  while 1
    let x = stridx(a:line, "	", i)
    if x == -1 | break | endif
    call add(tabs, x - i)
    let i = x + 1
  endwhile
  call add(tabs, len(a:line) - i)
  return tabs
endfunction

" Minimum cell width
let g:tsv_min_width = 4
" Virtual space added on the right of every cells
let g:tsv_padding = 1

let b:tsv_tabstops = []

" Update the vartabstop option according to a range of lines
function! TSV_update_tabstops(cell_widths)
  let column_n = max(map(copy(a:cell_widths), {i, ln -> len(ln)}))
  " Add an extra column, will be the tabstop for new columns
  let tabstops = repeat([ g:tsv_min_width ], column_n + 1)
  for ln in a:cell_widths
    if len(ln) > 1 " Ignore lines with no tabs
      let i = 0
      while i < len(ln)
        let tabstops[i] = max([ tabstops[i], ln[i] + 1 + g:tsv_padding ])
        let i += 1
      endwhile
    endif
  endfor
  " Update the option if needed
  if tabstops != b:tsv_tabstops
    let b:tsv_tabstops = tabstops
    let &l:vartabstop = join(tabstops, ",")
  endif
endfunction

" TODO:
" - efficient update of tabstops when text changes 

" Compute the width of each columns from the visible lines and update the
" vartabstop option.
" This is called only when a buffer is read or saved to disk.
" This operation is not that heavy but it can't run on every keystrokes
function! TSV_update()
  let cell_widths = map(range(line("w0"), line("w$")), {i, l -> TSV_column_widths(getline(l))})
  call TSV_update_tabstops(cell_widths)
endfunction

augroup tsv
  au!
  au BufWritePost <buffer> call TSV_update()
augroup END

call TSV_update()
