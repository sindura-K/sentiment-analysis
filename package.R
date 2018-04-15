library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

sentiments <- data_frame()
files <- list.files(pattern = ".txt", recursive = TRUE)
for(i in files){
    sentiments <- rbind(sentiments, GetSentiment(i))
}

GetSentiment <- function(file){
	fileName <- glue("", file, sep = "")
	fileName <- trimws(fileName)
 	
	fileText <- glue(read_file(fileName))
	fileText <- gsub("\\$", "", fileText) 
 
	tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
	sentiment<-tokens %>%
      	inner_join(get_sentiments("afinn")) %>% # pull out only sentimen words
	      count(sentiment) %>% # count the # of positive & negative words
      	spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
	      mutate(sentiment = positive - negative) %>% # # of positive words - # of negative owrds
      	mutate(file = file) %>% # add the name of our file
	      mutate(year = as.numeric(str_match(file, "\\d{4}"))) %>% # add the year
      	mutate(president = str_match(file, "_(.*)")[2]) # add president
	return(sentiment)
}
ggplot(sentiments, aes(x = as.numeric(year), y = sentiment)) + 
  geom_point(aes(color = president))+ # add points to our plot, color-coded by president
  geom_smooth(method = "auto") # pick a method & fit a model
ggplot(sentiments, aes(x = president, y = sentiment, color = president)) + 
  geom_boxplot() # draw a boxplot for each president