#' Get participant information
#'
#' @export
#' @param df Data frame with 'p' and 'condition' columns
#' @param c String used to filter() 'condition' column
#' @return list('participants' = p, 'n' = count(p))
get_participants <- function(df, c) {
  p <-
    df %>%
    ungroup() %>%
    filter(condition == c) %>%
    select(p) %>%
    unique()
  list('participants' = p, 'n' = count(p))
}
