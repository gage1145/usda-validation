# Function for making Airtable query formulas
get_formula <- function(id, values) {
  make_string <- function(value) sprintf("OR({%s} = %s", id, value)
  sapply(values, make_string) |>
    paste(collapse=",") |>
    paste0(strrep(")", length(values)), collapse="")
}