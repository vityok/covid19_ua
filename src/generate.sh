#!/bin/sh

Rscript render.R

cp *.md ../docs
cp -ra fig_regions_dyn ../docs
cp -ra fig_histograms_dyn ../docs

