Sys.setlocale(category="LC_ALL",locale="uk_UA.utf8" )

library(rmarkdown)

render('regions_dyn.Rmd',
       output_format = github_document())

render('histograms.Rmd',
       output_format = github_document())
