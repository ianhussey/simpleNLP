#' categorize_parcels
#'
#' This function takes tokenized text (e.g., the output from `tidy_parcels()`) and a applies your desired dictionary/lexicon of word categories to it. The frequency of words per category is calculatd along with the percentage of word per category per total words in the text parcel.
#' Three dictionaries are included: emotions (8 emotions), valence (positive vs. negative words) and relations (relational terms). The latter is in development. Any custom dictionary that includes the columns "category" and "word" can be used.
#' @param data a data frame with the columns "parcel_number" (e.g., sentence, paragraph, message, words spoken during a timeframe, etc) and "word" (indivdiual words stripped of non alphanumeric characters). I.e., the output from `tidy_parcels()`.
#' @param dictionary a data frame with the columns "category" and "word", representing the specific words that are assigned to superordinate categories. Notionally, a given word could be a member of multiple categories.
#' @return a data frame with the columns "parcel_number", "category", "count" (instances of words per category in each parcel), and "percent" (instances of words from this category as percentage of all words in the parcel).
#' @export
#' @examples
#' categorized_data <- reddit_suicide_data %>%
#'     tidy_parcels() %>%
#'     categorize_parcels(dictionary = relations)

categorize_parcels <- function(data, dictionary) {

  # dependencies
  require(tidyverse)
  require(tidytext)

  # extract dictionary category labels
  columns_to_gather <- dictionary %>%
    distinct(category) %>%
    mutate(category = as.character(category))

  columns_to_gather <- columns_to_gather$category

  # words per parcel
  words_per_parcel <- data %>%
    group_by(parcel_number) %>%
    dplyr::summarise(n = n())

  # parse according to dictionary
  parsed_data <- data %>%
    inner_join(dictionary, by = "word") %>%
    count(parcel_number, category) %>%
    spread(category, n, fill = 0) %>%
    left_join(words_per_parcel, by = "parcel_number") %>%
    gather(category, count, as.vector(columns_to_gather)) %>%
    mutate(percent = round(count/n, 3)*100) %>%
    dplyr::select(-n) %>%
    arrange(parcel_number)

  return(parsed_data)
}

