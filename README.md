# FishData
Compile and harmonize catch rates from fishery-independent surveys of fish populations

[![Build Status](https://travis-ci.org/James-Thorson/FishData.svg?branch=master)](https://travis-ci.org/James-Thorson/FishData)

# Description
* Scrape and harmonize bottom trawl survey data from Alaska Fisheries Science Center and Northwest Fisheries Science Center
* Adds zeros (sampling occasions without catch of any given species) automatically
* Stores data locally to avoid excessive survey usage
* Hopefully with be moved to an rOpenSci project to ensure that code is applicable for multiple surveys, regions, and agencies

# Installation Instructions

```R
# Install and load devtools package
install.packages("devtools")

# Install package
devtools::install_github("james-thorson/FishData")
```
