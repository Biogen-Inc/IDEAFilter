
# IDEAFilter

Built upon [shinyDataFilter](https://github.com/dgkf/shinyDataFilter),
which was built on on top of [Joe Cheng](https://github.com/jcheng5)’s
excellent [R/Pharma 2018 shiny
demo](https://github.com/jcheng5/rpharma-demo). This package exists
primarily because it’s leveraged by
[tidyCDISC](https://github.com/Biogen-Inc/tidyCDISC) which depends on
it.

# Getting started

## Installation

``` r
# Install from Public github
# install.packages("devtools") # if needed
devtools::install_github("Biogen-Inc/IDEAFilter")
```

## Example App

Then, run this sample app to build filters with `IDEAFilter`:

``` r
library(shiny)
shinyAppFile(system.file("examples", "basic_app", "app.R", package = "IDEAFilter"))
```
