
# IDEAFilter

Built upon [shinyDataFilter](https://github.com/dgkf/shinyDataFilter),
which was built on on top of [Joe Cheng](https://github.com/jcheng5)’s
excellent [R/Pharma 2018 shiny
demo](https://github.com/jcheng5/rpharma-demo). This package is required
when deploying [tidyCDISC](https://github.com/Biogen-Inc/tidyCDISC) or
running locally.

# Getting started

## Installation

``` r
# Install from Public github
# install.packages("devtools") # if needed
devtools::install_github("Biogen-Inc/IDEAFilter")

# # Install from internal RSPM repo, if an HPC users:
# options(repos = c(
#   CRAN = "https://cran.rstudio.com/",
#   ghe = "http://10.240.22.159:4242/Git-Biogen/latest")
# )
# options('repos') # to confirm "ghe" was added
# install.packages("IDEAFilter")
```

## Example App

Then, run this sample app to build filters with `IDEAFilter`:

``` r
library(shiny)
shinyAppFile(system.file("examples", "basic_app", "app.R", package = "IDEAFilter"))
```
