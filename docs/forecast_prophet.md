Побудова прогнозів із використанням Facebook Prophet
================

Це лише заготовка, сторінка роботі.

``` r
library(tidyverse)
library(prophet)
```

    ## Loading required package: Rcpp

    ## Loading required package: rlang

    ## 
    ## Attaching package: 'rlang'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     %@%, as_function, flatten, flatten_chr, flatten_dbl, flatten_int,
    ##     flatten_lgl, flatten_raw, invoke, list_along, modify, prepend,
    ##     splice

Prophet
=======

<https://facebook.github.io/prophet/docs/quick_start.html#r-api>

First we read in the data and create the outcome variable. As in the Python API, this is a dataframe with columns ds and y, containing the date and numeric value respectively. The ds column should be YYYY-MM-DD for a date, or YYYY-MM-DD HH:MM:SS for a timestamp. As above, we use here the log number of views to Peyton Manning’s Wikipedia page, available here.

``` r
df <- read_csv('../covid19_by_area_type_hosp_dynamics.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   zvit_date = col_date(format = ""),
    ##   registration_area = col_character(),
    ##   priority_hosp_area = col_character(),
    ##   edrpou_hosp = col_character(),
    ##   legal_entity_name_hosp = col_character(),
    ##   legal_entity_lat = col_double(),
    ##   legal_entity_lng = col_double(),
    ##   person_gender = col_character(),
    ##   person_age_group = col_character(),
    ##   add_conditions = col_character(),
    ##   is_medical_worker = col_character(),
    ##   new_susp = col_double(),
    ##   new_confirm = col_double(),
    ##   active_confirm = col_double(),
    ##   new_death = col_double(),
    ##   new_recover = col_double()
    ## )

    ## Warning: 82 parsing failures.
    ##  row              col               expected  actual                                        file
    ## 1381 legal_entity_lat no trailing characters ,604941 '../covid19_by_area_type_hosp_dynamics.csv'
    ## 1381 legal_entity_lng no trailing characters ,271351 '../covid19_by_area_type_hosp_dynamics.csv'
    ## 3272 legal_entity_lat no trailing characters ,604941 '../covid19_by_area_type_hosp_dynamics.csv'
    ## 3272 legal_entity_lng no trailing characters ,271351 '../covid19_by_area_type_hosp_dynamics.csv'
    ## 5227 legal_entity_lat no trailing characters ,604941 '../covid19_by_area_type_hosp_dynamics.csv'
    ## .... ................ ...................... ....... ...........................................
    ## See problems(...) for more details.

``` r
df <- df %>%
    select(zvit_date, new_confirm) %>%
    group_by(zvit_date) %>%
    summarise(new_confirm = sum(new_confirm)) %>%
    filter(zvit_date > as.Date("2020-04-01"))

df <- df %>% rename(ds = zvit_date, y = new_confirm)

head(df)
```

    ## # A tibble: 6 x 2
    ##   ds             y
    ##   <date>     <dbl>
    ## 1 2020-04-02   125
    ## 2 2020-04-03   149
    ## 3 2020-04-04   167
    ## 4 2020-04-05   102
    ## 5 2020-04-06   189
    ## 6 2020-04-07   159

``` r
plot(df)
```

<img src="fig_forecast_prophet/unnamed-chunk-3-1.png" width="672" />

We call the prophet function to fit the model. The first argument is the historical dataframe. Additional arguments control how Prophet fits the data and are described in later pages of this documentation.

``` r
m <- prophet(df)
```

    ## Disabling yearly seasonality. Run prophet with yearly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

Predictions are made on a dataframe with a column ds containing the dates for which predictions are to be made. The make\_future\_dataframe function takes the model object and a number of periods to forecast and produces a suitable dataframe. By default it will also include the historical dates so we can evaluate in-sample fit.

``` r
future <- make_future_dataframe(m, periods = 14)
tail(future)
```

    ##            ds
    ## 62 2020-06-02
    ## 63 2020-06-03
    ## 64 2020-06-04
    ## 65 2020-06-05
    ## 66 2020-06-06
    ## 67 2020-06-07

As with most modeling procedures in R, we use the generic predict function to get our forecast. The forecast object is a dataframe with a column yhat containing the forecast. It has additional columns for uncertainty intervals and seasonal components.

``` r
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
```

    ##            ds     yhat yhat_lower yhat_upper
    ## 62 2020-06-02 519.6943   391.0029   647.2796
    ## 63 2020-06-03 586.2447   459.6499   718.2326
    ## 64 2020-06-04 537.1718   414.9003   654.9671
    ## 65 2020-06-05 560.1487   441.4266   676.8701
    ## 66 2020-06-06 520.7512   409.3582   634.7605
    ## 67 2020-06-07 393.3554   274.9416   519.8033

You can use the generic plot function to plot the forecast, by passing in the model and the forecast dataframe.

``` r
plot(m, forecast)
```

<img src="fig_forecast_prophet/unnamed-chunk-7-1.png" width="672" />

You can use the prophet\_plot\_components function to see the forecast broken down into trend, weekly seasonality, and yearly seasonality.

``` r
prophet_plot_components(m, forecast)
```

<img src="fig_forecast_prophet/unnamed-chunk-8-1.png" width="672" />

More details about the options available for each method are available in the docstrings, for example, via `?prophet` or `?fit.prophet`. This documentation is also available in the [reference manual](https://cran.r-project.org/web/packages/prophet/prophet.pdf) on CRAN.

[Повернутись на головну](index.html) або [повідомити про помилку]((https://github.com/vityok/covid19_ua/issues))
