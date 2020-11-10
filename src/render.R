Sys.setlocale(category="LC_ALL",locale="uk_UA.utf8" )

library(rmarkdown)

# area_dyn <- read_csv('https://raw.github.com/VasiaPiven/covid19_ua/master/covid19_by_area_type_hosp_dynamics.csv')

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

render('forecast_hw.Rmd',
       output_format = github_document())

render('forecast_prophet.Rmd',
       output_format = github_document())

render('reading_csse_ts.Rmd',
       output_format = github_document())

render('decompose.Rmd',
       output_format = github_document())

render_pdf <- function () {
    render('regions_dyn.Rmd',
           output_format = pdf_document())
}
