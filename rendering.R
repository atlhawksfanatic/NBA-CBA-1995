# Rendering the book

# https://statnmap.com/2017-11-13-enable-code-folding-in-bookdown-and-blogdown/

bookdown::render_book("index.Rmd", "bookdown::gitbook")
# bookdown::render_book("index.Rmd")
browseURL("docs/index.html") # to view it

bookdown::render_book("index.Rmd", "bookdown::pdf_document2")
