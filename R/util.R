#' Get participant information
#'
#' Summarise breath counting accuracy
#' @param df Data frame with 'p' and 'condition' columns
#' @param c String used to filter() 'condition' column
#' @export
#' @return list('participants' = p, 'n' = count(p))
#' @examples
#' meditation_list <- get_participants(ant_with_participants_df, 'meditation')
get_participants <- function(df, c) {
  p <-
    df %>%
    filter(condition == c) %>%
    select(p) %>%
    unique()
  list('participants' = p, 'n' = count(p))
}