#' Get participant information
#'
#' Summarise breath counting accuracy
#' @export
#' @param df Data frame with 'p' and 'condition' columns
#' @param c String used to filter() 'condition' column
#' @return list('participants' = p, 'n' = count(p))
#' @examples
#' meditation_list <- get_participants(ant_with_participants_df, 'meditation')
get_participants <- function(df, c) {
  p <-
    df %>%
    ungroup() %>%
    filter(condition == c) %>%
    select(p) %>%
    unique()
  list('participants' = p, 'n' = count(p))
}

#' Summarize data
#'
#' Calculates count, mean, standard deviation, standard error of the mean, and confidence interval.
#'
#' See http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
#'
#' @export
#' @param data Data frame
#' @param measurevar String containing name of a column that contains the variable to be summarized
#' @param groupvars Vector of strings naming columns that contain grouping variables
#' @param na.rm Boolean indicating whether to ignore NA's
#' @param conf.interval Number < 1 indicating percent range of the confidence interval (default=.95)
#' @return Data frame
#'
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )

  # Rename "mean" column
  datac <- rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}
