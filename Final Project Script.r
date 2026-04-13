install.packages("tidytext")
install.packages("tidyverse")
install.packages("readtext")
library(tidyverse)
library(tidytext)
library(readtext)
file_paths <- system.file("MonasticTexts/")
monastictexts <- readtext(paste("MonasticTexts/", "*.txt", sep = ""))


install.packages("stopwords")
library(stopwords)
install.packages("quanteda")
library(quanteda)
??stopwords
head(stopwords::data_stopwords_ancient)

latinstopwords <- as_tibble(stopwords(language = "la", source = "ancient", simplify = TRUE))


tokenized.MT.full <- monastictexts %>% unnest_tokens(word, text)
tokenized.MT.full <- tokenized.MT.full %>% anti_join(stop_words)
tokenized.MT.full.stopwords <- tokenized.MT.full %>% anti_join(stopwords(language = "la", source = "ancient"))

latinstopwords <- as.data.frame(stopwords(language = "la", source = "ancient"))

tokenized.MT.full.stopwords <- tokenized.MT.full %>% anti_join(latinstopwords, tokenized.MT.full, join_by(stopwords(language = "la", source = "ancient")))
??anti_join

install.packages("dplyr")
library(dplyr)
englishstopwords <- as.data.frame(stop_words)
completestopwords <- merge(englishstopwords, latinstopwords, all.x = "TRUE", copy = TRUE)
tokenized.MT.full.stopwords <- tokenized.MT.full %>% anti_join(completestopwords)




