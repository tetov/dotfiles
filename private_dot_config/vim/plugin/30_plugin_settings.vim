" CamelCaseMotion
let g:camelcasemotion_key = '<leader>'

if !exists('g:vscode')
  colorscheme wombat

  " folding
  let g:markdown_folding=1

  " gitgutter
  highlight! link SignColumn LineNr
  let g:gitgutter_set_sign_backgrounds = 1

  " vimspector
  let g:vimspector_enable_mappings = 'HUMAN'

  " Settings for vim-stay, what to save between sessions
  set viewoptions=cursor,slash,unix

  " Pandoc
  " make markdown files not appears as pandoc files to w0rp/ale
  let g:pandoc#filetypes#handled = ["pandoc", "markdown"]
  let g:pandoc#folding#fold_yaml=1
  let g:pandoc#folding#fdc=0
  let g:pandoc#formatting#textwidth=80
  let g:pandoc#formatting#mode="a"

  " ALE
  let g:ale_fixers = {
  \   '*': ['remove_trailing_lines', 'trim_whitespace'],
  \   'python': ['autopep8', 'isort'],
  \   'css': ['prettier', 'stylelint'],
  \   'html': ['tidy'],
  \   'sh': ['shfmt'],
  \   'json': ['prettier'],
  \}

  nmap ]a :ALENextWrap<CR>
  nmap [a :ALEPreviousWrap<CR>
  nmap ]A :ALELast
  nmap [A :ALEFirst

  nmap <F8> <Plug>(ale_fix)
endif
