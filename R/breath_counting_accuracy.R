#' Summarise breath counting accuracy
#'
#' Summarise breath counting accuracy
#' @param data data.frame returned by breath_counting_accuracy()
#' @param participants data.frame with column 'p' listing participants to process
#' @export
#' @return data.frame
summarise_breath_counting_accuracy <- function(data, participants) {
  bc_accuracy <- data %>%
    select(subject) %>%
    unique() %>%
    dplyr::rename(participant = subject) %>%
    mutate(total=0,correct=0,incorrect=0)
  expand.grid(p=participants$p) %>%
    rowwise() %>%
    do(., breath_counting_accuracy(.$p, data)) %>%
    arrange(p)
}

#' Convert Experiment Factory breath counting data to CSV
#'
#' Convert Experiment Factory breath counting data to CSV
#' @param path Path to data files
#' @param t Time point
#' @param exclude Participants to exclude
#' @export
expfactory_breath_counting_to_csv <- function(path, t, exclude) {
  # make a bc.csv consumable by breath_counting_accuracy()
  paths <-
    list.files(path,
               pattern = ".*_bc.csv",
               full.names = TRUE,
               recursive = TRUE)
  df <- expand.grid(path=paths, time=t, stringsAsFactors = FALSE) %>%
    rowwise() %>%
    mutate(p=as.integer(gsub(".*/.*/(\\d+)/.*$", '\\1', path))) %>%
    filter(! p %in% exclude ) %>%
    do(., manjushri::process_expfactory_bc_file(.$path, .$p))
  write.table(df, paste(path, '/bc.csv', sep=''), sep = ",", row.names = FALSE)
}

#' @export
process_expfactory_bc_file <- function(bc_file, s) {
  bc <- read.csv(bc_file, header = TRUE)
  bc %>% filter(trial_id == 'breath_counting') %>%
    select(trial_index,rt,key_press) %>%
    # column headers from original ePrime task
    dplyr::rename(Sample = trial_index, response = key_press) %>%
    mutate(Sample = Sample - 2, response = ifelse(response == 40, '{DOWNARROW}', '{RIGHTARROW}'),
           subject = s)
}

#' Convert ePrime breath counting data to CSV
#'
#' Convert ePrime breath counting data to CSV
#' @param path Path to ePrime data files
#' @export
eprime_breath_counting_to_csv <- function(path) {
  paths <- list.files(path, pattern = ".txt", full.names = TRUE, recursive = TRUE)
  df <- expand.grid(path=paths, stringsAsFactors=FALSE) %>%
    rowwise() %>%
    do(., manjushri::process_eprime_file(.$path))
  write.table(df, paste(path, '/bc.csv', sep=''), sep = ",", row.names = FALSE)
}

#' Process ePrime Breath Counting Data File
#'
#' Process ePrime breath counting data file
#' (Levinson, Stoll, Kindy, Merry, & Davidson, 2014)
#' @param path Path to ePrime data file
#' @keywords ePrime
#' @export
process_eprime_file <- function(path) {
  lines  <- rprime::read_eprime(path)
  frames <- rprime::FrameList(lines)
  frame1 <- frames[[1]]

  # trials occur at level 3
  frames     <- rprime::keep_levels(frames, 3)
  df         <- rprime::to_data_frame(frames)
  to_pick    <- c("Sample", "Wait4TUTSlide.RESP", "Wait4TUTSlide.RT")
  df         <- df[to_pick]
  df$subject <- frame1$Subject
  df <- dplyr::rename(df, response = Wait4TUTSlide.RESP, rt = Wait4TUTSlide.RT)
  # Add initial {DOWNARROW} to correct for a known issue in the ePrime script provided by Daniel Levinson,
  # which doesn't record the first keypress (I think) on the first trial.  Bug requires a bit more testing
  # to confirm it's the _initial_ keypress which is lost, but it definitely omits 1 {DOWNARROW} keypress.
  df <- add_row(df, .before = 1, Sample = 0, response = '{DOWNARROW}', rt = 0, subject = df$subject[1])
  df
}

#' Breath Counting Accuracy
#'
#' Process ePrime breath counting data
#' (Levinson, Stoll, Kindy, Merry, & Davidson, 2014)
#' @param p Participant number
#' @param bc_df Breath counting data frame
#' @keywords breath counting meditation
#' @export
breath_counting_accuracy <- function(p, bc_df) {
  df        <- filter(bc_df, subject == p)
  resp      <- 1
  row       <- 1
  total     <- 0
  correct   <- 0
  incorrect <- 0
  repeat {
    if (resp < 9) {
      if (df[row,'response'] == '{DOWNARROW}') { # correct
        resp <- resp + 1
      } else if (df[row,'response'] == '{RIGHTARROW}') {
        total     <- total + 1
        incorrect <- incorrect + 1
        resp <- 1 # reset count
      } else {
        # shouldn't get here
      }
    } else { # response >= 9
      # We only record complete 9 counts (i.e. any final partial count isn't included)
      if (df[row,'response'] == '{RIGHTARROW}') {
        if (resp == 9 ) { # correct!
          correct <- correct + 1
        }
        # -> signals end of the count set, regardless of whether it was on breath 9 or later
        total <- total + 1
        resp  <- 1
      } else if (df[row,'response'] == '{DOWNARROW}') {
        if (resp == 9 ) { # incorrect
          incorrect <- incorrect + 1
        }
        # keep counting responses until the next {RIGHTARROW} ends the count set
        resp <- resp + 1
      } else {
        # shouldn't get here
      }
    }
    row <- row + 1
    if (row > nrow(df)) {
      break
    }
  }
  data.frame(p,total,correct,incorrect)
}
