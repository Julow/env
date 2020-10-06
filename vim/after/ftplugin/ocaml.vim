let no_plugin_maps = 1

setlocal commentstring=(*\ %s\ *)

nmap K :call ocpindex#print()<return>

set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2

set fo-=c fo-=r fo-=o

set formatprg=ocamlformat\ --enable-outside-detected-project\ --no-version-check\ --name\ %\ -

" ocamlformat, ocp-indent
let $PATH = $OPAM_SWITCH_PREFIX . '/bin:' . $PATH

" C-l: Clear merlin highlights
autocmd User ClearHighlight call merlin#StopHighlight()

" Default to dune
set makeprg=dune\ build
let g:runtestprg = "dune runtest"

nmap <Leader>e :MerlinErrorCheck<return>