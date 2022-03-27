" Lightline
set noshowmode  " don't show vim mode in cmd line

let g:lightline = {
\   'colorscheme': 'wombat',
\   'active': {
\       'left': [
\           ['mode', 'paste'],
\           ['venv', 'gitbranch', 'filename', 'modified']
\       ],
\       'right': [
\           ['lineinfo'],
\           ['percent'],
\           ['fileformat', 'fileencoding', 'filetype'],
\           ['readonly', 'cocstatus', 'currentfunction'],
\           ['linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok']
\       ]
\   },
\   'component_function': {
\       'gitbranch': 'fugitive#head',
\       'venv': 'virtualenv#statusline',
\   },
\   'component_type': {
\       'buffers': 'tabsel',
\       'readonly': 'error',
\       'linter_checking': 'right',
\       'linter_infos': 'right',
\       'linter_warnings': 'warning',
\       'linter_errors': 'error',
\       'linter_ok': 'right',
\   },
\   'component_expand': {
\       'linter_checking': 'lightline#ale#checking',
\       'linter_infos': 'lightline#ale#infos',
\       'linter_warnings': 'lightline#ale#warnings',
\       'linter_errors': 'lightline#ale#errors',
\       'linter_ok': 'lightline#ale#ok',
\       'buffers':'lightline#bufferline#buffers'
\   },
\}

let g:lightline#ale#indicator_checking = "\uf110"
let g:lightline#ale#indicator_infos = "\uf129"
let g:lightline#ale#indicator_warnings = "\uf071"
let g:lightline#ale#indicator_errors = "\uf05e"
let g:lightline#ale#indicator_ok = "\uf00c"
