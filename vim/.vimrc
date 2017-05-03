" Lines picked from https://dougblack.io/words/a-good-vimrc.html

set nocompatible            " get rid of Vi compatibility mode. SET FIRST!

" Start Vundle config
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
" call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'valloric/youcompleteme'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-sensible'
Plugin 'LucHermitte/lh-vim-lib'
Plugin 'LucHermitte/lh-dev'
Plugin 'LucHermitte/lh-brackets'
" And a lh-dev dependency, not required by lh-brackets
Plugin 'LucHermitte/lh-tags'
Plugin 'plasticboy/vim-markdown'
Plugin 'sjl/vitality.vim'
Plugin 'rdnetto/YCM-Generator'
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'kopischke/vim-stay'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

set ffs=unix,dos,mac        " Use Unix as the standard file type
set t_Co=256                " enable 256-color mode.

syntax enable               " enable syntax highlighting (previously syntax on).

" COLORS
set background=dark
colorscheme solarized

" TABS
set tabstop=4               " number of visual spaces per TAB
set softtabstop=4           " number of spaces in tab when editing
set shiftwidth=4
set expandtab               " tabs are spaces

" General UI
set number                  " show line numbers
set relativenumber 
set showcmd                 " show command in bottom bar
set cursorline              " highlight current line
filetype indent on          " load filetype-specific indent files
set wildmenu                " visual autocomplete for command menu
set lazyredraw              " redraw only when we need to.
set showmatch               " highlight matching [{()}]
set ruler                   " Always show info along bottom.
set wrap                    " Wrap lines

" SEARCH
set incsearch               " search as characters are entered
set hlsearch                " highlight matches
set ignorecase              " Make searches case-insensitive.

" FOLDING
set foldenable              " enable folding
set foldlevelstart=10       " open most folds by default
set foldnestmax=10          " 10 nested fold max
nnoremap <space> za       " space open/closes folds
set foldmethod=indent       " fold based on indent level
"au BufWinLeave ?* mkview     " save folds on exit
"au BufWinEnter ?* silent loadview " load folds on open

" MOVEMENT
" move vertically by visual line
nnoremap j gj
nnoremap k gk

" BACKUP off
set nobackup
set nowb
set noswapfile

" No markers for LH-brackets
let b:usemarks=0

" Built in fuzzy find from https://github.com/mcantor/no_plugins
set path+=**
" - Hit tab to :find by partial match
" - Use * to make it fuzzy
" - Use :b and type in unique part of path and enter to open up file in buffer

" Tweaks for netrw (file-browser) from https://github.com/mcantor/no_plugins 
let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=4  " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()

" Get off my lawn - helpful when learning Vim :)
nnoremap <Left> :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up> :echoe "Use k"<CR>
nnoremap <Down> :echoe "Use j"<CR>

" Save with sudo
command! -nargs=0 Sw w !sudo tee % > /dev/null
