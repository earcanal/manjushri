#' Process ePrime Breath Counting Data File
#'
#' Process ePrime breath counting data file
#' (Levinson, Stoll, Kindy, Merry, & Davidson, 2014)
#' @param path Path to ePrime data file
#' @keywords ePrime
#' @export
#' @examples
#' breath_counting_accuracy(/path/to/file.txt)
process_eprime_file <- function(path) {
  lines  <- rprime::read_eprime(path)
  frames <- FrameList(lines)
  frame1 <- frames[[1]]

  # trials occur at level 3
  frames     <- keep_levels(frames, 3)
  df         <- to_data_frame(frames)
  to_pick    <- c("Sample", "Wait4TUTSlide.RESP", "Wait4TUTSlide.RT")
  df         <- df[to_pick]
  df$subject <- frame1$Subject
  df
}

#' Breath Counting Accuracy
#'
#' Process ePrime breath counting data
#' (Levinson, Stoll, Kindy, Merry, & Davidson, 2014)
#' @param p Participant number
#' @keywords breath counting meditation
#' @export
#' @examples
#' breath_counting_accuracy(1)

breath_counting_accuracy <- function(p) {
  df        <- filter(bc, subject == p)
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
    } else { # 9th response
      total <- total + 1
      if (df[row,'response'] == '{RIGHTARROW}') { # correct
        correct <- correct + 1
      } else if (df[row,'response'] == '{DOWNARROW}') {
        incorrect <- incorrect + 1
      } else {
        # shouldn't get here
      }
      resp <- 1 # reset count
    }
    row <- row + 1
    if (row > nrow(df)) {
      break
    }
  }
  # FIXME: ignore final partial count?
  data.frame(p,total,correct,incorrect,accuracy=(1-signif(incorrect/total, 2))*100)
}
