library(tidyverse)
library(tidytext)
library(readtext)
file_paths <- system.file("MonasticTexts/")
monastictexts <- readtext(paste("MonasticTexts/", "*.txt", sep = ""))

library(stopwords)
library(quanteda)
??stopwords
head(stopwords::data_stopwords_ancient)

latinstopwords <- tibble(word = stopwords(language = "la", source = "ancient", simplify = TRUE))

tokenized.MT.full <- monastictexts %>% unnest_tokens(word, text) %>% as_tibble()
tokenized.MT.full <- tokenized.MT.full %>% anti_join(stop_words)
tokenized.MT.full.stopwords <- tokenized.MT.full %>% anti_join(latinstopwords)

library(dplyr)
# Combine English and Latin stopwords, making sure both have a 'word' column
englishstopwords <- stop_words %>% select(word)
completestopwords <- bind_rows(englishstopwords, latinstopwords) %>% distinct()
tokenized.MT.full.stopwords <- tokenized.MT.full %>% anti_join(completestopwords)




