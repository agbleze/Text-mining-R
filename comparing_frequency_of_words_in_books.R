## Word frequency
# load library
library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)
library(gutenbergr)
library(janeaustenr)
library(tidytext)
library(stringr)
library(scales)

## download data
hgwells <- gutenberg_download(c(35, 36, 5230, 159), mirror = "http://mirrors.xmission.com/gutenberg/")
View(hgwells)
data(stop_words) # downloads stop_words

# assign data to new variable and pipe it
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>% #tokenize the data
  anti_join(stop_words)  # remove stop words from the token
tidy_hgwells %>%
  count(word, sort = TRUE) ## calculate frequency of tokens and order in descending order


### download data for another 
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767), mirror = "http://mirrors.xmission.com/gutenberg/")
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
View(tidy_bronte)


# load dataset and store in variable
original_books <- austen_books()
#View(original_books)
#### group the book, add row numbers as line number and chapter number
#piping the dataset
original_books%>%
  #group the dataset based on the column name books
  group_by(book)%>%
  # add 2 columns to the data for line number and chapter number
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()
original_books
View(original_books)

### tokenize the data
# assign the preprocessed data to a variable and pipe
tidy_books <- original_books %>%
  # tokenize the data based on text column and ouptput tokens in word column 
  unnest_tokens(word, text)
tidy_books  
View(tidy_books) 

## removing stop_words
# load the dataset of stop_words
data(stop_words)
tidy_books %>%
  #remove stop_words from the preprocessed dataset
  anti_join(stop_words) %>%
  # count frequency words (token) and arrange in descending order
  count(word, sort = TRUE) %>%
  #View(tidy_books)
  # filter or reduce to words with frequecy greater than 600
  filter(n > 600) %>%
  # add column with word arranged in descending order
  mutate(word = reorder(word, n)) %>%
 
  frequency1 <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                         mutate(tidy_hgwells, author = "H.G. Wells"),
                         mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z]+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

ggplot(frequency1, aes(x = proportion, y = 'Jane Austen', color = abs('Jane Austen' - proportion))) +
  geom_abline(color = "gray40", lty = 2) + geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) + scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  face_wrap(~author, ncol = 2) + theme(legend.position = "none") +
  labs(y = "Jane Austen", x = NULL)
