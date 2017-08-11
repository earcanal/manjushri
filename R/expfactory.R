library(jsonlite)
library(tidyverse)
library(stringr)

#' Process expfactory Attention Network Test
#' 
#' @param ant_file Path to ANT file
#' @param p Participant number
#' @param time Time-point at which participant completed ANT
#' @keywords expfactory ANT
#' @export
#' @return Data frame

process_ant <- function(ant_file, p, time) {
  if(!file.exists(ant_file)){
    return(data_frame(p=p,t=time, file=ant_file))
  }
  ant <- read.csv(ant_file, header = TRUE)
  ant %>% filter(exp_stage == 'test') %>%
    filter(trial_type == 'poldrack-single-stim') %>%
    filter(! is.na(correct_response)) %>%
    select(-feedback_duration,-trial_id,-trial_index,-internal_node_id,-text,-timing_post_trial,-view_history,-stimulus,-trial_type,-time_elapsed) %>%
    mutate(p = as.numeric(p)) %>%
    mutate(subject = as.factor(subject)) %>%
    mutate(file=ant_file, t=time)
}

## To compare speed of different solutions
# mb <- microbenchmark::microbenchmark(
#   ldply(surveys, process_surveys),
#   ldply(surveys, process_surveys2, paths),
#   process_surveys3(".")
# )

#' Process expfactory survey data
#' 
#' Process expfactory survey data
#' @param p Participant Number
#' @keywords expfactory
#' @export
#' @param survey Survey name
#' @return Data frame (long format)
#' # http://expfactory.readthedocs.io/en/latest/development.html#contributing-to-surveys
process_expfactory_survey <- function(path, survey) {
  p <- as.integer(gsub(".*/(\\d+)$", '\\1', path))
  f <- paste(path, "/", p, "_", survey, ".json", sep = "")
  if ( file.exists(f) ) {
    df <- jsonlite::read_json(f, simplifyVector = TRUE)
    df %>%
      mutate(
        p = p,
        qnum = as.numeric(gsub(".*(\\d+).*$", '\\1', name)) - 1,
        question = text,
        #question = parse_number(name) - 1,
        survey = survey) %>%
      select(survey, value, p, question)
  } else {   # FIXME: handle missing data files
    message(f, ': file not found')
  }
}

#' Process expfactory survey data
#'
#' @param base_path data path
#'
#' @return data frame with [survey, value, participant, question]
#'
#' @examples process_surveys3(base_path)
process_surveys3 <- function(base_path){
  all_json_files = list.files(base_path, pattern = ".*\\.json", recursive = TRUE)
  df <- NULL
  for (f in all_json_files ) {
    df <- bind_rows(
      df, 
      jsonlite::read_json(f, simplifyVector = TRUE) %>%
        mutate(file = f)
    )
  }
  
  df %>%
    mutate( 
      #      survey = str_extract(name, ".*(?=_survey?)"),    # use survey name from JSON, different result and as an alternative to next line
      survey = str_extract(file, "(?<=_).*?(?=.json)"), # use survey name from filename
      p = str_extract(file, "^[:digit:]*(?=/?)"),       # extract the participant number from the directory name 
      question = parse_number(name) - 1) %>%
    select(survey, value, p, question)
}
