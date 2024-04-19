## code to prepare `internal-data` dataset goes here
set.seed(5000)
vector_data <-
  dplyr::tibble(
    character = purrr::map_chr(1:50, ~ paste(sample(letters, 5, replace = TRUE), collapse = "")),
    date = as.Date("2021-03-02") + floor(50*runif(50, min = -1)),
    datetime = as.POSIXct("2021-02-02 11:54:56", format = "%Y-%m-%d %H:%M:%S") + floor((1:50*24*60*60 + runif(50, min = -1)*24*60*60)),
    factor_few = as.factor(sample(LETTERS[1:4], 50, replace = TRUE)),
    factor_many = as.factor(sample(LETTERS[1:10], 50, replace = TRUE)),
    logical = runif(50, min = -1) > 0,
    numeric_few = sample(1:5, 50, replace = TRUE),
    numeric_many = floor(100*runif(50)),
    unknown = purrr::map(1:50, ~ list(A = sample(letters, 5, replace = TRUE), B = sample(LETTERS, 5, replace = TRUE)))
  )
attr(vector_data$datetime, "tzone") <- "UTC"

usethis::use_data(vector_data, overwrite = TRUE, internal = TRUE)
