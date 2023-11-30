" Disable ocaml-vim mappings
let no_plugin_maps = 1

" Disable ocp-indent setting the comments option
let no_ocaml_comments = 1

" Set $OPAMSWITCH
let g:opam_set_switch = 1

" Error messages
let g:ocaml_compiler_compact_messages = 0

setlocal commentstring=(*\ %s\ *)

setlocal expandtab
setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2

" Formatting

setlocal fo-=c fo-=r fo-=o

" Alter the path so that ignore files stop working
setlocal formatprg=ocamlformat\ --no-disable\ --no-version-check\ --enable-outside-detected-project\ --ignore-invalid-option\ --wrap-comments\ --parse-docstrings\ --name\ %:h:S/_fmt/%:t:S\ -
" This is used instead of &formatprg when formatting an entire buffer
" &formatprg disables the 'disable' option and the version check
let b:full_formatprg = "ocamlformat --enable-outside-detected-project --name % -"

" Merlin

let g:merlin_split_method = "never"

" C-c: Clear merlin highlights
autocmd User ClearHighlight call merlin#StopHighlight()

if !exists('*MerlinLocateMli')
  function! MerlinLocateMli()
    let old_pref = g:merlin_locate_preference
    let g:merlin_locate_preference = 'mli'
    call merlin#Locate()
    let g:merlin_locate_preference = old_pref
  endfunction
endif

nmap <buffer> gD :call MerlinLocateMli()<return>

nmap <buffer> <Leader>e :MerlinErrorCheck<return>

" -

let g:ripple_repls = {
    \ "ocaml": ["dune exec -- ocaml", "", ";;", 0],
    \ }

" Highlight types
hi link ocamlTypeConstr Type
hi link ocamlTypeBuiltin Type
hi link ocamlTypeIdentifier Identifier
hi link ocamlTypeVar Special
hi link ocamlTypeAnyVar Special

" Override C-^ to switch between the .ml and .mli files.

function! Ocaml_alternate(fname)
  let l:base = fnamemodify(a:fname, ":.:r")
  if a:fname =~? ".mli$"
    return l:base . ".ml"
  else
    return l:base . ".mli"
  endif
endfunction

nnoremap <buffer> <C-^> :e <C-r>=Ocaml_alternate(@%)<return><return>
