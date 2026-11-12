#' Format a log-odds effect for LaTeX
#' 
#' This function calculates the multiplicative odds effect corresponding to a given additive
#' log-odds effect and formats it nicely for display in LaTeX.
#' 
#' @param effect a numeric vector
#' @param latex logical, should the negative symbol be repeated to match the width of the plus
#'   symbol? Defaults to FALSE.
#' 
#' @return a string vector
#' 
#' @export
format_log_odds_effect <- function(effect, latex = FALSE) {
  negative_symbol <- ifelse(latex, "--", "-")
  percentage_effect <- 100 * (exp(effect) - 1)
  formatted_effect <- paste0(
    ifelse(percentage_effect > 0, "+", negative_symbol),
    round(abs(percentage_effect)),
    "\\%"
  )
  return(formatted_effect)
}

#' Format a percentage for LaTeX
#' 
#' This function converts a numeric between 0 and 1 to a nicely formatted percentage for LaTeX.
#' 
#' @param percentage a numeric vector between 0 and 1
#' @param latex logical, should the % symbol be escaped for latex? Defaults to FALSE.
#' 
#' @return a string vector
#' 
#' @export
format_percentage <- function(percentage, digits = 0, latex = FALSE) {
  percent_symbol <- ifelse(latex, "\\%", "%")
  formatted_percentage <- dplyr::case_when(
    is.na(percentage) ~ NA_character_,
    TRUE ~ paste0(sprintf(paste0("%.", digits, "f"), 100 * percentage), percent_symbol)
  )
  return(formatted_percentage)
}

