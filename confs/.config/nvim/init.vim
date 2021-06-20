call plug#begin(stdpath('data') . '/plugged')
Plug 'unblevable/quick-scope'
Plug 'chaoren/vim-wordmotion'
call plug#end()

if exists('g:vscode')
    xmap gc  <Plug>VSCodeCommentary
    nmap gc  <Plug>VSCodeCommentary
    omap gc  <Plug>VSCodeCommentary
    nmap gcc <Plug>VSCodeCommentaryLine

    highlight QuickScopePrimary guifg='#afff5f' gui=underline ctermfg=155 cterm=underline
    highlight QuickScopeSecondary guifg='#5fffff' gui=underline ctermfg=81 cterm=underline
endif

set incsearch
set hlsearch
set ignorecase
set smartcase

" MOVEMENT
" move vertically by visual line
nnoremap j gj
nnoremap k gk

" Enter and backspaces moves through paragraphs in normal mode
nnoremap <BS> {
onoremap <BS> {
vnoremap <BS> {

