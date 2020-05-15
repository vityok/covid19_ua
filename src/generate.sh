#!/bin/sh

Rscript render.R

cp *.md ../docs
cp -ra figures ../docs

