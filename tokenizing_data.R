## unnest_tokens function
## converting text into tidy format
## load library
library(dplyr)
library(tidytext)
##example of text
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text
text_df <- data_frame(line = 1:4, text = text)
text_df%>%
  # break text into tokens (tokenization)
  unnest_tokens(word, text)
