" vim config
set number relativenumber
set nowrap

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
call plug#end()

colorscheme tender
let g:gitgutter_realtime = 1
let g:gitgutter_eager = 1
set updatetime=250
