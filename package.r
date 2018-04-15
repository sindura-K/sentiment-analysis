# load in the libraries we'll need
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
 
fileText <-("C:\Users\SINDURA K\Desktop\sindura\6thsem\ML\package\sona\1990_deKlerk.txt")
#fileText <- glue(read_file(file.choose()))
# remove any dollar signs (they're special characters in R)
fileText <- gsub("\\$", "", fileText) 
 
# tokenize
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)

tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds