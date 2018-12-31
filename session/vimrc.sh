# Vim config

cat > "$HOME/.vimrc" <<"EOF"
filetype plugin on
filetype indent on

set encoding=utf8
set ignorecase
set hlsearch
set showmatch

set wildmenu

set mouse=a

syntax enable

set backspace=eol,start,indent

" Indent
set smarttab
set shiftwidth=4
set tabstop=4
set ai
set si

set wrap

" Fold
set foldenable
set foldmethod=indent
set foldlevelstart=99
nnoremap <space> za
EOF
