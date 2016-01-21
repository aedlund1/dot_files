set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'tmhedberg/SimpylFold'
Plugin 'tpope/vim-fugitive'

syntax on
set number
set tabstop=4
set shiftwidth=4
set autoindent
set expandtab
set cursorline
set showmatch
let python_highlight_all = 1
imap jj <ESC>

call vundle#end()
filetype plugin indent on
