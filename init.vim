" vim config
set number relativenumber
set nowrap
set termguicolors
set cursorline

" remap split commands
nmap <silent> <C-V> :wincmd v<CR>
nmap <silent> <C-S> :wincmd s<CR>

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
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'edkolev/tmuxline.vim'
Plug 'liuchengxu/vim-which-key'
" On-demand lazy load
Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }
call plug#end()

" colorschemes
" tender
"colorscheme tender

" gruvbox
colorscheme gruvbox
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_italic = 1
let g:gruvbox_number_column = 'bg0'

" plugin config
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

" key mappings
nmap <silent> <C-O> :Files
