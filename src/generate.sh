#!/bin/sh

Rscript render.R

cp *.md ../docs
cp -ra fig_regions_dyn ../docs
cp -ra fig_histograms_dyn ../docs
cp -ra fig_forecast ../docs
cp -ra fig_forecast_arima ../docs
cp -ra fig_forecast_fable ../docs
cp -ra fig_forecast_hw ../docs
cp -ra fig_forecast_prophet ../docs
cp -ra fig_reading_csse_ts ../docs
cp -ra fig_decompose ../docs

