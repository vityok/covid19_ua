#!/bin/sh

Rscript render.R

cp *.md ../docs
cp -ra fig_regions_dyn ../docs
cp -ra fig_histograms_dyn ../docs
cp -ra fig_forecast_arima ../docs
cp -ra fig_forecast_fable ../docs
cp -ra fig_forecast_prophet ../docs

