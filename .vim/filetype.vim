" Blosxom files are html, but can't be called .html
" because then they interfere with headers, footers etc.
au BufRead,BufNewFile *.blx    set filetype=html
