library(rmarkdown)

render('regions_dyn.Rmd',
       output_format = github_document())
