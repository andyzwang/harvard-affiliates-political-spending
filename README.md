# Contribution In and Out of the Classroom: Harvard Faculty Political Spending

## Introduction

Harvard faculty tend to be very involved, both in teaching and in various political / social activities. I investigated to see if there's patterns in the political spending behavior of Harvard faculty, as well as created a handful of models to predict political expenditures for various factors. 

Check out the (app here!)[https://andyzwang.shinyapps.io/harvard-political-spending/]!

## Sources

Harvard faculty data was (painstakingly) gathered by hand from department websites, as well as from Harvard's (Faculty Finder)[http://facultyfinder.harvard.edu/search].

FEC data included (bulk data)[https://www.fec.gov/data/browse-data/?tab=bulk-data] as well as (individual contribution data)[https://www.fec.gov/introduction-campaign-finance/how-to-research-public-records/individual-contributions/].

The app is built on Shiny and R analysis.

## Github Repo

**harvard-political-spending**: Everything related to the Shiny app can be found in this folder. App.R is the main Shiny app, while raw-data contains the original data that I compiled or gathered

**collect.Rmd / html**: These two files contain the original data analysis I did from the raw data, as well as the less organized sources of most of the functions in the actual Shiny app.