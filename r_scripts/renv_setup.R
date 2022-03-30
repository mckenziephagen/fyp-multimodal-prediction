# ---
# jupyter:
#   jupytext:
#     formats: ipynb,R:light
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.13.7
#   kernelspec:
#     display_name: R [conda env:anoth-R]
#     language: R
#     name: conda-env-anoth-R-r
# ---

source("renv/activate.R")

library(renv)

renv::hydrate() #this is weird but necessary

#might need this
install.packages('BiocManager')
BiocManager::install("rhdf5")

