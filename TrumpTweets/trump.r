# Set CRAN repository if not already set
if (getOption("repos")["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org/"))
}

# Required packages
required_packages <- c(
  "readr", "dplyr", "ggplot2", "lubridate", "tidytext", 
  "wordcloud2", "stringr", "scales", "tidyr",
  "htmlwidgets", "webshot2"
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) {
  cat("Installing missing packages:", paste(new_packages, collapse=", "), "\n")
  install.packages(new_packages)
  if ("webshot2" %in% new_packages) {
    cat("webshot2 installed. You may need to run webshot2::install_chromium() if PNG export fails.\n")
  }
} else {
  cat("All required packages are already installed.\n")
}

# Load packages
cat("Loading packages...\n")
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(tidytext)
  library(wordcloud2)
  library(stringr)
  library(scales)
  library(tidyr)
  library(htmlwidgets)
  library(webshot2)
})
cat("Packages loaded.\n")

file_path <- "TrumpTweets\\trumptweets.csv"
cat("Reading file:", file_path, "\n")

tryCatch({
  tweets_df <- read_csv(file_path, show_col_types = FALSE)
}, error = function(e) {
  message("Failed to read file. Check the path and filename.")
  message("Make sure the file '", file_path, "' exists.")
  message("Current working directory: ", getwd())
  message("Original error: ", e$message)
  stop(e)
})
cat("Data loaded.\n")

# Date column detection and processing
date_column_name <- NULL
if ("date" %in% colnames(tweets_df)) {
  date_column_name <- "date"
} else if ("created_at" %in% colnames(tweets_df)) {
  date_column_name <- "created_at"
}

if (!is.null(date_column_name)) {
  tweets_df <- tweets_df %>%
    mutate(
      datetime = parse_date_time(!!sym(date_column_name), 
        orders = c(
          "a b d H:M:S z Y", "Ymd HMS", "mdy HMS", "dmy HMS",
          "Y-m-d H:M:S", "m/d/Y H:M:S", "Y-m-d", "m/d/Y"
        ),
        tz = "UTC"
      ),
      year_month = floor_date(datetime, "month"),
      day_of_week = wday(datetime, label = TRUE, week_start = 1, locale = "en_US.UTF-8"),
      hour_of_day = hour(datetime)
    )
  cat("Datetime columns processed.\n")
  cat("Parsed dates:", sum(!is.na(tweets_df$datetime)), "of", nrow(tweets_df), "\n")
  cat("Sample datetimes:\n")
  print(head(tweets_df$datetime))
  cat("Problematic original dates:\n")
  print(head(tweets_df[[date_column_name]][is.na(tweets_df$datetime) & !is.na(tweets_df[[date_column_name]])]))
} else {
  warning("No date column found. Time-based analysis will be limited.")
}

cat("Adding tweet_length column...\n")
tweets_df <- tweets_df %>%
  mutate(tweet_length = nchar(as.character(content)))
cat("tweet_length column added.\n")

# Create output directory for plots if it doesn't exist
plots_dir <- "TrumpTweetsPlots"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

# Plot 1: Tweets per month
if ("year_month" %in% colnames(tweets_df) && "datetime" %in% colnames(tweets_df)) {
  tweets_per_month <- tweets_df %>%
    filter(!is.na(year_month)) %>%
    count(year_month)
  if (nrow(tweets_per_month) > 0) {
    p1 <- ggplot(tweets_per_month, aes(x = year_month, y = n)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "blue", size = 2) +
      labs(title = "Tweets per Month", x = "Month", y = "Number of Tweets") +
      scale_x_datetime(labels = date_format("%Y-%m")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p1)
    ggsave(file.path(plots_dir, "tweets_per_month.png"), plot = p1, width = 10, height = 6, dpi = 300)
    cat("Plot saved:", file.path(plots_dir, "tweets_per_month.png"), "\n")
  } else {
    cat("No data for monthly tweet count plot.\n")
  }
} else {
  cat("Missing 'year_month' or 'datetime' column for monthly tweet count plot.\n")
}

# Plot 2: Tweet length distribution
p2 <- ggplot(tweets_df, aes(x = tweet_length)) +
  geom_histogram(binwidth = 10, fill = "coral", color = "black") +
  labs(title = "Tweet Length Distribution", x = "Tweet Length (chars)", y = "Number of Tweets") +
  theme_minimal()
print(p2)
ggsave(file.path(plots_dir, "tweet_length_distribution.png"), plot = p2, width = 8, height = 6, dpi = 300)
cat("Plot saved:", file.path(plots_dir, "tweet_length_distribution.png"), "\n")

# Plot 3: Word cloud
text_for_cloud <- tweets_df %>%
  select(id, content) %>%
  mutate(content_clean = str_replace_all(as.character(content), "https?://\\S+|www\\.\\S+", "")) %>%
  mutate(content_clean = str_replace_all(content_clean, "@\\w+", "")) %>%
  mutate(content_clean = str_replace_all(content_clean, "#\\w+", "")) %>%
  unnest_tokens(word, content_clean)

data("stop_words")
words_cleaned <- text_for_cloud %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% c("amp", "realdonaldtrump", "t.co", "http", "https", "rt", "it's", "ð", "ñ", "â")) %>%
  filter(nchar(word) > 2)

word_counts <- words_cleaned %>%
  count(word, sort = TRUE)

cat("Generating word cloud...\n")
if (nrow(word_counts) > 0) {
  wordcloud2_fig <- wordcloud2(data = head(word_counts, 150), size = 0.7, color = 'random-dark')
  print(wordcloud2_fig)
  saveWidget(wordcloud2_fig, file.path(plots_dir, "wordcloud.html"), selfcontained = FALSE, title = "Trump Word Cloud")
  cat("Word cloud saved as", file.path(plots_dir, "wordcloud.html"), "\n")
  tryCatch({
    webshot2::webshot(file.path(plots_dir, "wordcloud.html"), file.path(plots_dir, "wordcloud.png"), delay = 2, vwidth = 1000, vheight = 800)
    cat("Word cloud saved as", file.path(plots_dir, "wordcloud.png"), "\n")
  }, error = function(e) {
    cat("Failed to save word cloud as PNG:", e$message, "\n")
    cat("Try running 'webshot2::install_chromium()' if needed.\n")
  })
} else {
  cat("No words to generate word cloud after filtering.\n")
}

# Plot 4: Top 20 words
if (nrow(word_counts) > 0) {
  p4 <- ggplot(head(word_counts, 20), aes(x = reorder(word, n), y = n)) +
    geom_col(fill = "skyblue") +
    coord_flip() +
    labs(title = "Top 20 Words", x = "Word", y = "Count") +
    theme_minimal()
  print(p4)
  ggsave(file.path(plots_dir, "top20_words.png"), plot = p4, width = 8, height = 7, dpi = 300)
  cat("Plot saved:", file.path(plots_dir, "top20_words.png"), "\n")
} else {
  cat("No data for Top 20 words plot.\n")
}

# Plot 5: Top hashtags
hashtags_df <- tweets_df %>%
  mutate(hashtags = str_extract_all(as.character(content), "#\\w+")) %>%
  filter(sapply(hashtags, length) > 0) %>%
  unnest(cols = c(hashtags)) %>%
  mutate(hashtags = str_to_lower(hashtags)) %>%
  count(hashtags, sort = TRUE)

if (nrow(hashtags_df) > 0) {
  p5 <- ggplot(head(hashtags_df, 20), aes(x = reorder(hashtags, n), y = n)) +
    geom_col(fill = "lightgreen") +
    coord_flip() +
    labs(title = "Top 20 Hashtags", x = "Hashtag", y = "Count") +
    theme_minimal()
  print(p5)
  ggsave(file.path(plots_dir, "top20_hashtags.png"), plot = p5, width = 8, height = 7, dpi = 300)
  cat("Plot saved:", file.path(plots_dir, "top20_hashtags.png"), "\n")
} else {
  cat("No hashtags found.\n")
}

# Plot 6: Top mentions
mentions_df <- tweets_df %>%
  mutate(mentions = str_extract_all(as.character(content), "@\\w+")) %>%
  filter(sapply(mentions, length) > 0) %>%
  unnest(cols = c(mentions)) %>%
  mutate(mentions = str_to_lower(mentions)) %>%
  count(mentions, sort = TRUE)

if (nrow(mentions_df) > 0) {
  p6 <- ggplot(head(mentions_df, 20), aes(x = reorder(mentions, n), y = n)) +
    geom_col(fill = "gold") +
    coord_flip() +
    labs(title = "Top 20 Mentions", x = "User", y = "Count") +
    theme_minimal()
  print(p6)
  ggsave(file.path(plots_dir, "top20_mentions.png"), plot = p6, width = 8, height = 7, dpi = 300)
  cat("Plot saved:", file.path(plots_dir, "top20_mentions.png"), "\n")
} else {
  cat("No mentions found.\n")
}

# Plot 7: Sentiment analysis (Bing)
if (nrow(words_cleaned) > 0) {
  bing_sentiments <- words_cleaned %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    count(sentiment, sort = TRUE)
  if (nrow(bing_sentiments) > 0) {
    p7 <- ggplot(bing_sentiments, aes(x = sentiment, y = n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Sentiment Analysis (Bing)", x = "Sentiment", y = "Word Count") +
      theme_minimal()
    print(p7)
    ggsave(file.path(plots_dir, "sentiment_bing.png"), plot = p7, width = 7, height = 5, dpi = 300)
    cat("Plot saved:", file.path(plots_dir, "sentiment_bing.png"), "\n")
  } else {
    cat("No data for sentiment analysis (Bing).\n")
  }
} else {
  cat("No cleaned words for sentiment analysis.\n")
}

# Plot 8: Sentiment over time
if ("datetime" %in% colnames(tweets_df) && "id" %in% colnames(words_cleaned)) {
  sentiment_with_dates <- words_cleaned %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    left_join(select(tweets_df, id, datetime), by = "id") %>%
    filter(!is.na(datetime)) %>%
    mutate(year_month = floor_date(datetime, "month")) %>%
    filter(!is.na(year_month)) %>%
    count(year_month, sentiment) %>%
    group_by(year_month, sentiment) %>%
    summarise(n = sum(n), .groups = 'drop') %>%
    group_by(year_month) %>%
    mutate(total_sentiment_words_month = sum(n)) %>%
    ungroup() %>%
    mutate(proportion = n / total_sentiment_words_month)
  if (nrow(sentiment_with_dates) > 0) {
    p8 <- ggplot(sentiment_with_dates, aes(x = year_month, y = n, fill = sentiment)) +
      geom_col(position = "stack") +
      labs(title = "Sentiment Over Time (Bing)", x = "Month", y = "Sentiment Word Count", fill = "Sentiment") +
      scale_x_datetime(labels = date_format("%Y-%m")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p8)
    ggsave(file.path(plots_dir, "sentiment_over_time.png"), plot = p8, width = 10, height = 6, dpi = 300)
    cat("Plot saved:", file.path(plots_dir, "sentiment_over_time.png"), "\n")
  } else {
    cat("No data for sentiment over time plot.\n")
  }
} else {
  cat("Missing columns for sentiment over time plot.\n")
}

# Plot 9: Tweets by day of week
if ("day_of_week" %in% colnames(tweets_df) && "datetime" %in% colnames(tweets_df)) {
  tweets_by_dow <- tweets_df %>%
    filter(!is.na(day_of_week)) %>%
    count(day_of_week)
  if(nrow(tweets_by_dow) > 0) {
    p9 <- ggplot(tweets_by_dow, aes(x = day_of_week, y = n, fill = day_of_week)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Tweets by Day of Week", x = "Day of Week", y = "Tweet Count") +
      theme_minimal()
    print(p9)
    ggsave(file.path(plots_dir, "tweets_by_dayofweek.png"), plot = p9, width = 8, height = 5, dpi = 300)
    cat("Plot saved:", file.path(plots_dir, "tweets_by_dayofweek.png"), "\n")
  } else {
    cat("No data for tweets by day of week plot.\n")
  }
} else {
  cat("Missing columns for tweets by day of week plot.\n")
}

# Plot 10: Tweets by hour
if ("hour_of_day" %in% colnames(tweets_df) && "datetime" %in% colnames(tweets_df)) {
  tweets_by_hour <- tweets_df %>%
    filter(!is.na(hour_of_day)) %>%
    count(hour_of_day)
  if(nrow(tweets_by_hour) > 0) {
    p10 <- ggplot(tweets_by_hour, aes(x = hour_of_day, y = n)) +
      geom_col(fill = "purple") +
      labs(title = "Tweets by Hour", x = "Hour", y = "Tweet Count") +
      scale_x_continuous(breaks = 0:23) +
      theme_minimal()
    print(p10)
    ggsave(file.path(plots_dir, "tweets_by_hour.png"), plot = p10, width = 9, height = 5, dpi = 300)
    cat("Plot saved:", file.path(plots_dir, "tweets_by_hour.png"), "\n")
  } else {
    cat("No data for tweets by hour plot.\n")
  }
} else {
  cat("Missing columns for tweets by hour plot.\n")
}

# Plot 11: Top bigrams
bigrams <- tweets_df %>%
  mutate(content_clean = str_replace_all(as.character(content), "https?://\\S+|www\\.\\S+", "")) %>%
  mutate(content_clean = str_replace_all(content_clean, "@\\w+", "")) %>%
  mutate(content_clean = str_replace_all(content_clean, "#\\w+", "")) %>%
  unnest_tokens(bigram, content_clean, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

if (nrow(bigrams) > 0) {
  bigrams_separated <- bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word1 %in% c("amp", "realdonaldtrump", "t.co", "http", "https", "ð", "ñ", "â")) %>%
    filter(!word2 %in% c("amp", "realdonaldtrump", "t.co", "http", "https", "ð", "ñ", "â")) %>%
    filter(nchar(word1) > 2 & nchar(word2) > 2)
  bigram_counts <- bigrams_filtered %>%
    count(word1, word2, sort = TRUE) %>%
    unite(bigram, word1, word2, sep = " ")
  if (nrow(bigram_counts) > 0) {
    p11 <- ggplot(head(bigram_counts, 20), aes(x = reorder(bigram, n), y = n)) +
      geom_col(fill = "darkcyan") +
      coord_flip() +
      labs(title = "Top 20 Bigrams", x = "Bigram", y = "Count") +
      theme_minimal()
    print(p11)
    ggsave(file.path(plots_dir, "top20_bigrams.png"), plot = p11, width = 8, height = 7, dpi = 300)
    cat("Plot saved:", file.path(plots_dir, "top20_bigrams.png"), "\n")
  } else {
    cat("No bigrams to display after filtering.\n")
  }
} else {
  cat("No bigrams to analyze.\n")
}

cat("All plots generated and saved.\n")
cat("Output files are in:\n")
cat(normalizePath(plots_dir), "\n")