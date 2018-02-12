#' plot_frequencies
#'
#' Plots raw data, distributions, and means with 95% CIs. Takes output from `categorize_parcels()` and plots frequencies by dictionary categories.
#' @param data a data frame with the columns "parcel_number", "category" and "count".
#' @return a ggplot plot object.
#' @export
#' @examples
#' categorized_data <- reddit_suicide_data %>%
#'     tidy_parcels() %>%
#'     categorize_parcels(dictionary = relations)
#' plot_frequencies(data = categorized_data)

plot_frequencies <- function(data) {

  # dependencies
  require(tidyverse)
  require(plotrix)

  data_summary <- data %>%
    group_by(category) %>%
    dplyr::summarize(mean  = mean(count),
                     se = plotrix::std.error(count))

  plot <-
    ggplot(data = data_summary,
           aes(x = category, y = mean)) +
    geom_violin(data = categorized_data, aes(x = category, y = count),
                adjust = 1,
                width = .9,
                scale = "width") +
    #fill = "lightgrey"
    geom_jitter(data = categorized_data, aes(x = category, y = count),
                width = .45,
                height = 0,
                alpha = 0.05,
                color = "black") +
    geom_pointrange(aes(ymax = mean + (1.96*se),
                        ymin = mean - (1.96*se)),
                    color = "black") +
    ylab("Percentage of words") +
    xlab("Category") +
    scale_colour_grey() +
    theme_classic() +
    coord_flip()

  return(plot)
}
