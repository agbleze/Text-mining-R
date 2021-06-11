### sentiment analysis with bing
library(tidyr)
library(janeaustenr)
library(ggplot2)

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
#### plot data
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) + geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

## choose only the novel interested in
pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")

pride_prejudice
afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum()) %>%
  mutate(method = "AFINN")

bing <- pride_prejudice
bing%>%
  inner_join(get_sentiments("bing")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum()) %>%
  mutate(method = "bing")%>%
  #count(method, index = linenumber %/% 80, sentiment) %>%
  #spread(sentiment, n, fill = 0) %>%
  #mutate(sentiment = positive - negative)

bind_rows(afinn, bing) %>%
  ggplot(aes(index, sentiment, fill = method)) + geom_col(show.legend = FALSE) + 
  facet_wrap(~method, ncol = 1, scales = "free_y")

