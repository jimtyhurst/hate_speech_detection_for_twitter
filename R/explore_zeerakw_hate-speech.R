library(readr)
library(dplyr)
library(ggplot2)

# Summarizes data from file:
# https://github.com/ZeerakW/hatespeech/blob/master/NAACL_SRW_2016.csv
# in repository:
# https://github.com/ZeerakW/hatespeech
#
# See:
# Zeerak Waseem and Dirk Hovy. 2016.
# "Hateful Symbols or Hateful People? Predictive Features for Hate Speech Detection on Twitter", pp. 88-93.
# Proceedings of the NAACL Student Research Workshop, June 2016.
# San Diego, CA: Association for Computational Linguistics.
# http://www.aclweb.org/anthology/N16-2013

#' @export
explore_zeerakw_hate_speech <- function() {
  encoded_tweets <-
    read_csv(
      "https://raw.githubusercontent.com/ZeerakW/hatespeech/master/NAACL_SRW_2016.csv",
      col_names = c("tweet_id", "rating"),
      col_types = list(col_character(), col_character())
    ) %>%
    mutate(rating = as.factor(rating))

  encoded_tweets %>%
    group_by(rating) %>%
    summarize(total_tweets = n()) %>%
    ggplot(aes(x = rating, y = total_tweets)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = total_tweets, vjust = -0.25)) +
    xlab("Rating") +
    ylab("Quantity") +
    ggtitle("Number of tweets by rating (data: NAACL_SRW_2016.csv)") %>%
    print()

  return(encoded_tweets)
}
