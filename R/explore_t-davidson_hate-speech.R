library(readr)
library(dplyr)
library(ggplot2)

# Summarizes data from file:
# https://raw.githubusercontent.com/t-davidson/hate-speech-and-offensive-language/master/data/labeled_data.csv
# in repository:
# https://github.com/t-davidson/hate-speech-and-offensive-language
#
# See:
# Thomas Davidsoon, Dana Warmsley, Michael Macy, and Ingmar Weber. 2017.
# "Automated Hate Speech Detection and the Problem of Offensive Language".
# Proceedings of the 11th International AAAI Conference on Web and Social Media. ICWSM '17, Montreal, Canada.
# https://aaai.org/ocs/index.php/ICWSM/ICWSM17/paper/view/15665

#' @export
explore_t_davidson_hate_speech <- function() {
  encoded_tweets <- readr::read_csv("https://raw.githubusercontent.com/t-davidson/hate-speech-and-offensive-language/master/data/labeled_data.csv")
  names(encoded_tweets)[1] <- "seq_number"

  # Histogram of tweet lengths
  encoded_tweets %>%
    mutate(tweet_length = nchar(tweet)) %>%
    ggplot(aes(x = tweet_length)) +
    geom_histogram(binwidth = 1) +
    scale_x_continuous(name = "Number of characters in tweet",
                       limits = c(0, 300),
                       expand = c(0, 0)) +
    ylab("Number of tweets") +
    ggtitle("Length of tweets (data: labeled_data.csv)") %>%
    print()

  # Bar chart of the number of tweets in each class
  encoded_tweets %>%
    dplyr::group_by(class) %>%
    dplyr::summarize(total = n()) %>%
    ggplot(aes(x = factor(class), y = total)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = total, vjust = -0.25)) +
    scale_x_discrete(labels = c("hate speech", "offensive language", "neither")) +
    xlab("class") +
    ylab("quantity") +
    ggtitle("Number of tweets in each class (data: labeled_data.csv)") %>%
    print()

  return(encoded_tweets)
}
