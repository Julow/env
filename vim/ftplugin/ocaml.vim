let no_plugin_maps = 1

setlocal commentstring=(*\ %s\ *)

nmap K :call ocpindex#print()<return>

set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2

set formatprg=ocamlformat\ --enable-outside-detected-project\ --name\ %\ -

" ocamlformat, ocp-indent
let $PATH = $OPAM_SWITCH_PREFIX . '/bin:' . $PATH

" C-l: Clear merlin highlights
autocmd User ClearHighlight call merlin#StopHighlight()
