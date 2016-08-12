set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'tmhedberg/SimpylFold'
Plugin 'tpope/vim-fugitive'
Plugin 'kien/ctrlp.vim'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'webfd/vim-erlang'
Plugin 'bling/vim-airline'
Plugin 'lokaltog/powerline'
Plugin 'vim-airline/vim-airline-themes'

colorscheme vividchalk
syntax on

set number
set tabstop=4
set shiftwidth=4
set autoindent
set expandtab
set cursorline
set showmatch
set splitbelow
set splitright
set laststatus=2

let python_highlight_all = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_theme='molokai'

imap jj <ESC>

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
map <C-n> :NERDTreeTabsToggle<CR>
map <C-t> :tabnew<CR>
map :apache :0r ~/.vim/apache.txt

call vundle#end()
filetype plugin indent on

