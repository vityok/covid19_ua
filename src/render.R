Sys.setlocale(category="LC_ALL",locale="uk_UA.utf8" )

library(rmarkdown)

render('regions_dyn.Rmd',
       output_format = github_document())

render('histograms.Rmd',
       output_format = github_document())


render('forecast.Rmd',
       output_format = github_document())

render('forecast_arima.Rmd',
       output_format = github_document())

render('forecast_fable.Rmd',
       output_format = github_document())

render('forecast_prophet.Rmd',
       output_format = github_document())
