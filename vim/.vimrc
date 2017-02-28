" Lines picked from https://dougblack.io/words/a-good-vimrc.html

set nocompatible            " get rid of Vi compatibility mode. SET FIRST!
set ffs=unix,dos,mac        " Use Unix as the standard file type
set t_Co=256                " enable 256-color mode.

syntax enable               " enable syntax highlighting (previously syntax on).

" COLORS
colorscheme badwolf         " https://github.com/sjl/badwolf/

" TABS
set tabstop=4               " number of visual spaces per TAB
set softtabstop=4           " number of spaces in tab when editing
set expandtab               " tabs are spaces

" General UI
set number                  " show line numbers
set showcmd                 " show command in bottom bar
set cursorline              " highlight current line
filetype indent on          " load filetype-specific indent files
set wildmenu                " visual autocomplete for command menu
set lazyredraw              " redraw only when we need to.
set showmatch               " highlight matching [{()}]
set ruler                   " Always show info along bottom.
set wrap                    "Wrap lines

" SEARCH
set incsearch               " search as characters are entered
set hlsearch                " highlight matches
set ignorecase              " Make searches case-insensitive.

" FOLDING
set foldenable              " enable folding
set foldlevelstart=10       " open most folds by default
set foldnestmax=10          " 10 nested fold max
" nnoremap <space> za       " space open/closes folds
set foldmethod=indent       " fold based on indent level

" MOVEMENT
" move vertically by visual line
nnoremap j gj
nnoremap k gk

" BACKUP off
set nobackup
set nowb
set noswapfile
