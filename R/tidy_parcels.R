#' tidy_parcels
#'
#' This function takes text, removes all alphanumeric characters, and tokenizes it for natural language processing. I.e., it returns a data frame with one word per row, and the parcel number that the word came from. Parcels could be sentences, pragraphcs, timespans, etc.
#' @param data a data frame with the columns "id" (e.g., individiual, text source, etc) and "parcel" (e.g., sentence, paragraph, message, words spoken during a timeframe, etc).
#' @return a data frame with the columns "id", "parcel_number" (incremental integer assigned to the parcels) and "word" (individual words extracted from each parcel).
#' @export
#' @examples
#' tidied_data <- tidy_parcels(data = reddit_suicide_data)

tidy_parcels <- function(data) {

  # dependencies
  require(tidyverse)
  require(tidytext)

  tidy_data <- data %>%
    # add parcel numbers
    mutate(parcel_number = row_number()) %>%

    # remove non alphanumber or punctuation characters
    mutate(parcel = gsub("[^[:alnum:][:punct:]///' ]", "", parcel)) %>%

    # convert text to tidy text: with one word per row, retaining parcel number
    dplyr::select(id, parcel_number, parcel) %>%
    tidytext::unnest_tokens(word, parcel)

  return(tidy_data)
}
