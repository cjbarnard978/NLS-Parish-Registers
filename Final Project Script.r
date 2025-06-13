Final Project Script #using info from our webscraping script project 
library(internetarchive)
library(tidyverse)
library(tidytext)
Registrumquery <- c("contributor" = "National Library of Scotland", "title" = "Registrum")

ia_search(Registrumquery, num_results = 19) %>% ia_get_items() %>% ia_metadata() %>% filter(field == "title" | field == "external-identifier")

registrummetadata <- as.data.frame(ia_search(Registrumquery, num_results = 19) %>% ia_get_items() %>% ia_metadata() %>% filter(field == "title" | field == "external-identifier"))

registrummetadata <- registrummetadata %>% pivot_wider(names_from = field, values_from = value)


dir.create("NLSParishRegisterTexts")

ia_search(Registrumquery, num_results = 19) %>% ia_get_items %>% ia_files %>% filter(type == "txt") %>% group_by(id) %>% ia_download(dir = "NLSParishRegisterTexts", overwrite = FALSE,) %>% glimpse()
library(readtext)
file_paths <- system.file("NLSParishRegisterTexts/")
NLStxtfull <- readtext(paste("NLSParishRegisterTexts/", "*.txt", sep = ""))
NLStxtmeta <- full_join(registrummetadata, NLStxtfull, by = c("id" ="doc_id")) %>% as_tibble()
write.csv(registrummetadata, "output.csv", row.names =FALSE, quote=FALSE)
write.csv(NLStxtmeta, "output.csv", row.names=FALSE, quote=FALSE)
write.csv(NLStxtfull, "output.csv", row.names=FALSE, quote=FALSE)

library("stopwords")
head(stopwords::data_stopwords_ancient)

tokenized.NLS.full <- NLStxtfull %>% unnest_tokens(word, text)
tokenized.NLS.full <- tokenized.NLS.full %>% anti_join(stop_words)
tokenized.NLS.full.stopwords <- tokenized.NLS.full %>% anti_join(stopwords(language = "la", source = "ancient"))
latinstopwords <- as.data.frame(stopwords(language = "la", source = "ancient"))
tokenized.NLS.full.stopwords <- tokenized.NLS.full %>% anti_join(latinstopwords, tokenized.NLS.full, join_by(stopwords(language = "la", source = "ancient") = word))
??anti_join
stopwordslist <- latinstopwords %>% cross_join(stopwords(language = "la", source = "ancient"), stop_words())