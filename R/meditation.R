library(tidyverse)

#' Calculate State Mindfulness Scale values
#'
#' @param df Data frame
#' @keywords State Mindfulness Scale
#' @export
#' @return Data frame
state_mindfulness_scale <- function(df) {
  # calculate SMS score (Tanay & Bernstein, 2013)
  # The 21 items (none reversed) are based on the 2014 instrument supplied by Bernstein
  df <- mutate(df, value = as.integer(df$value))
  sms_total <- sum(df$value)  # SMS total
  ## Factor scoring based on CFA (Tanay & Bernstein, 2013, Table 3)
  # Factor I (state mindfulness of mind)
  mind <- c('I was aware of different emotions that arose in me',
            'I tried to pay attention to pleasant and unpleasant sensations',
            'I found some of my experiences interesting',
            'I noticed many small details of my experience',
            'I felt aware of what was happening inside of me',
            'I noticed pleasant and unpleasant emotions',
            'I actively explored my experience in the moment',
            'I felt that I was experiencing the present moment fully',
            'I noticed pleasant and unpleasant thoughts',
            'I noticed emotions come and go',
            'I had moments when I felt alert and aware',
            'I felt closely connected to the present moment',
            'I noticed thoughts come and go',
            'I was aware of what was going on in my mind',
            'It was interesting to see the patterns of my thinking'
  )
  mind <- df %>%
    filter(question %in% mind)
  sms_mind <- sum(mind$value)
  # Factor II (state mindfulness of body)
  body <- c('I clearly physically felt what was going on in my body',
            'I changed my body posture and paid attention to the physical process of moving',
            'I noticed various sensations caused by my surroundings (e.g., heat, coolness, the wind on my face)',
            'I noticed physical sensations come and go',
            'I felt in contact with my body',
            'I noticed some pleasant and unpleasant physical sensations'
  )
  body <- df %>%
    filter(question %in% body)
  sms_body <- sum(body$value)

  return(data.frame(p = df[1,'p'], sms_total, sms_mind, sms_body))
}
