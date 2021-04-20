#' Uses skimr to produce summary statistics
#' Minimal because it removes summary stats I don't tend to look at
#'
#' @param df: a tibble or dataframe
#'
#' @return: a skim_df

skim_minimal <- function(df, ..., show_data_completeness = TRUE){

  res <- df %>%
    skimr::skim(...) %>%
    dplyr::select(-numeric.p25, -numeric.p75)

  if(show_data_completeness == FALSE)
    res <- select(res, -n_missing, -complete_rate)

  return(res)

}
