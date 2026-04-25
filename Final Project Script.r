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
englishstopwords <- (stop_words) %>% select(word)
customstopwords <- as_tibble("customstopwords.csv")
completestopwords <- bind_rows(englishstopwords, latinstopwords, customstopwords) %>% distinct()
tokenized.MT.full.stopwords <- tokenized.MT.full %>% anti_join(completestopwords)

tokenized.MT.full.stopwords %>% count(word, sort = TRUE)
install.packages("ggplot2")
library(ggplot2)
tokenized.MT.full.stopwords %>% count(word, sort = TRUE) %>% filter(n > 1000) %>% ggplot(aes(x = word, y = n, ylim = 5000)) + geom_point()

file_paths <- system.file("EnglishTexts/")
monasticonhibernicum <- readtext(paste("EnglishTexts/", "*.txt", sep = ""))
tokenizedMH <- monasticonhibernicum %>% unnest_tokens(word, text) %>% as_tibble()
tokenizedMH <- tokenizedMH %>% anti_join(stop_words)
tokenizedMH %>% count(word, sort = TRUE)
tokenizedMH %>% count(word, sort = TRUE) %>% filter(n > 350) %>% ggplot(aes(x = word, y = n, ylim = 5000)) + geom_point()

#Topic Models

#Latin
library(tm)
library(topicmodels)
install.packages("reshape2")
library(reshape2)
tidy.MT <- tokenized.MT.full.stopwords %>% filter('ocr', 'png', 'results') %>% filter(str_detect(word, "[a-z]$")) %>% count(doc_id, word)
MT.dtm <- tidy.MT %>% count(doc_id, word) %>% cast_dtm(doc_id, word, n)
MT.lda <- LDA(MT.dtm, k = 35, control = list(seed = 12345))
MT.lda
MT.topics <- tidy(MT.lda, matrix = "beta")
head(MT.topics)
topterms.MT <- MT.topics %>% arrange(desc(beta)) %>% group_by(topic) %>% slice(1:10)
topterms.MT %>% mutate(term = reorder_within(term, beta, topic)) %>%
ggplot(aes(beta, term, fill = factor(topic))) + geom_col(show.legend = FALSE) + facet_wrap(~ topic, scales = "free") + scale_y_reordered()