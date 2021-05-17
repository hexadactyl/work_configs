" Matt's nvim config
" UI CONFIG
set number relativenumber
set nowrap
set termguicolors
set cursorline
let mapleader=' '
set showcmd

" allow netrw plugins
set nocp                    " 'compatible' is not set
filetype plugin on          " plugins are enabled 
" PLUGIN MANAGEMENT
" auto-install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))                              
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim 
  autocmd VimEnter * PlugInstall
endif                                                                                                                                 

" install plugins
call plug#begin('~/.config/nvim/plugged') 
Plug 'vim-airline/vim-airline'
Plug 'kkvh/vim-docker-tools'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'morhetz/gruvbox'
Plug 'jacoborus/tender.vim'
Plug 'plasticboy/vim-markdown'
Plug 'christoomey/vim-tmux-navigator'
Plug 'tpope/vim-salve'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-vinegar'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'edkolev/tmuxline.vim'
Plug 'liuchengxu/vim-which-key'
" On-demand lazy load
Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }
Plug 'ryanoasis/vim-devicons'
Plug 'unblevable/quick-scope'
call plug#end()

" colorschemes
" tender
"colorscheme tender

" plugin config
" gruvbox
colorscheme gruvbox
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_italic = 1
let g:gruvbox_number_column = 'bg0'

" gitgutter
let g:gitgutter_realtime = 1
let g:gitgutter_eager = 1
set updatetime=250

" airline
let g:airline_powerline_fonts = 1
let g:airline_left_sep = "\uE0BC"
let g:airline_left_alt_sep = "\uE0BD"
let g:airline_right_sep = "\uE0BE"
let g:airline_right_alt_sep = "\uE0BF"
let g:airline#extensions#fzf#enabled = 1

" tmuxline
let g:tmuxline_powerline_separators = 1
let g:tmuxline_separators = {
    \ 'left' : ' ',
    \ 'left_alt': ' ',
    \ 'right' : ' ',
    \ 'right_alt' : ' ',
    \ 'space' : ' '}

" fzf
let g:fzf_layout = { 'down': '40%' }

" salve
let g:salve_auto_start_repl = 1

" which-key
"nnoremap <silent> <leader> :WhichKey '<Space>'<CR>

" KEY BINDING CONFIG
" split commands
nmap <silent> <C-V> :wincmd v<CR>
nmap <silent> <C-S> :wincmd s<CR>

" fzf commands
nmap <silent> <C-O> :Files<CR>
nmap <silent> <C-G> :Rg<CR>

" netrw commands
nmap <silent> _ :Vexplore<CR>
autocmd FileType netrw nmap <buffer> h -
autocmd FileType netrw nmap <buffer> l <CR>

" quick scope
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
"highlight QuickScopePrimary guifg='#afff5f' gui=underline
"highlight QuickScopeSecondary guifg='#5fffff' gui=underline
"highlight QuickScopePrimary gui=underline ctermfg=155 cterm=underline
"highlight QuickScopeSecondary gui=underline ctermfg=81 cterm=underline

" FIXME
"autocmd FileType netrw nmap <silent> <C-H> :wincmd h<CR>
"autocmd FileType netrw nmap <silent> <C-J> :wincmd j<CR>
"autocmd FileType netrw nmap <silent> <C-K> :wincmd k<CR>
"autocmd FileType netrw nmap <silent> <C-l> :wincmd l<CR>
