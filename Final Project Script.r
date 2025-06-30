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
write.csv(registrummetadata, "registrummetadata.csv", row.names =FALSE, quote=FALSE)
write.csv(NLStxtmeta, "NLStxtmeta.csv", row.names=FALSE, quote=FALSE)
write.csv(NLStxtfull, "NLStextfull.csv", row.names=FALSE, quote=FALSE)

library("stopwords")
library(quanteda)
??stopwords
head(stopwords::data_stopwords_ancient)

latinstopwords <- as_tibble(stopwords(language = "la", source = "ancient", simplify = TRUE))


tokenized.NLS.full <- NLStxtfull %>% unnest_tokens(word, text)
tokenized.NLS.full <- tokenized.NLS.full %>% anti_join(stop_words)
tokenized.NLS.full.stopwords <- tokenized.NLS.full %>% anti_join(stopwords(language = "la", source = "ancient"))

latinstopwords <- as.data.frame(stopwords(language = "la", source = "ancient"))

tokenized.NLS.full.stopwords <- tokenized.NLS.full %>% anti_join(latinstopwords, tokenized.NLS.full, join_by(stopwords(language = "la", source = "ancient") = word))
??anti_join
library(dplyr)
englishstopwords <- as.data.frame(stop_words)
completestopwords <- merge(englishstopwords, latinstopwords, all.x = "TRUE")
tokenized.NLS.full.stopwords <- tokenized.NLS.full %>% anti_join(completestopwords)


<working word list-sexuality: adulterium, adultero, inceste, incestus, castus>
<working word list-women and gender: puella, virginis, genetrix, conjunx>
<working word list-magic and heresy: incantatio, incantator, maleficium, veneficus, haereticus, magice, magicus>

#term frequency
sexualityTF <- tokenized.NLS.full.stopwords %>% filter(word == "adulterium"|word == "adultero" | word == "inceste" | word == "incestus")
womenandgenderTF <- tokenized.NLS.full.stopwords %>% filter(word == "puella"| word == "virginis" | word == "genetrix" | word == "conjunx")
magicandheresyTF <- tokenized.NLS.full.stopwords %>% filter(word == "incantatio" | word == "incantator" | word == "maleficium" | word == "veneficus"
| word == "haereticus" | word == "magice" | word == "magicus")
write.csv(sexualityTF, "sexualitytf.csv", row.names = FALSE, quote = FALSE)
write.csv(womenandgenderTF, "womenandgendertf.csv", row.names = FALSE, quote = FALSE)
write.csv(magicandheresyTF, "magicandheresytf.csv", row.names = FALSE, quote = FALSE)

wordcountsexuality <- sexualityTF %>% count(doc_id, word, sort = T) %>% group_by(doc_id)
wordcountwandg <- womenandgenderTF %>% count(doc_id, word, sort = T) %>% group_by(doc_id)
wordcountmandh <- magicandheresyTF %>% count(doc_id, word, sort = T) %>% group_by(doc_id)

write.csv(wordcountsexuality, "wordcountsexuality.csv", row.names = FALSE, quote = FALSE)
write.csv(wordcountwandg, "wordcountwomenandgender.csv", row.names = FALSE, quote = FALSE)
write.csv(wordcountmandh, "wordcountmagicandheresy.csv", row.names = FALSE, quote = FALSE)

<PLAGUE WORDS ADDED>
<diseasetf: morbus, aegrotatio, pestilentia, febris, 
diseasetf <- tokenized.NLS.full.stopwords %>% filter(word == "morbus" | word == "aegrotatio" | word == "pestilentia" | word == "febris")
wordcountdisease <- diseasetf %>% count(doc_id, word, sort = T) %>% group_by(doc_id)
<not productive> 

Topic Modeling 
library(tidytext)
library(tidyverse)
library(readtext)
library(tm)
library(topicmodels)

#<code pulled from 8500 worksheet 6-couldn't remember which package to open>

NLSstringdetech <- tokenized.NLS.full.stopwords %>% filter(str_detect(word, "[a-z']$"))
file_paths <- system.file("GlasgowAreaTexts/")
Glasgowtexts <- readtext(paste("GlasgowAreaTexts/", "*.txt", sep=""))
Glasgowtexts <- Glasgowtexts %>% unnest_tokens(word, text) 
Glasgowtexts %>% anti_join(stop_words)
Glasgowtexts <- Glasgowtexts %>% anti_join(completestopwords)
??anti_join
glasgowtextsDTM <- Glasgowtexts %>% count(doc_id, word) %>% cast_dtm(doc_id, word, n)
glasgowtextslda <- LDA(glasgowtextsDTM, k = 5, control = list(seed = 12345))
glasgowtextslda

glasgowtopics <- tidy(glasgowtextslda, matrix = "beta")
head(glasgowtopics)

Gtopic1 <- glasgowtopics %>% filter(topic == 1) %>% arrange(desc(beta)) %>% slice(1:20)
Gtopic2 <- glasgowtopics %>% filter(topic == 2) %>% arrange(desc(beta)) %>% slice(1:20)
Gtopic3 <- glasgowtopics %>% filter(topic == 3) %>% arrange(desc(beta)) %>% slice(1:20)
Gtopic4 <- glasgowtopics %>% filter(topic == 4) %>% arrange(desc(beta)) %>% slice(1:20)
Gtopic5 <- glasgowtopics %>% filter(topic == 5) %>% arrange(desc(beta)) %>% slice(1:20)


write.csv(Glasgowtexts, "glasgowtexts.csv", row.names = FALSE, quote = FALSE)
write.csv(Gtopic1, "glasgowtopic1.csv", row.names = FALSE, quote = FALSE)
write.csv(Gtopic2, "glasgowtopic2", row.names = FALSE, quote = FALSE )
write.csv(Gtopic3, "glasgowtopic3.csv", row.names = FALSE, quote = FALSE )
write.csv(Gtopic4, "glasgowtopic4.csv", row.names = FALSE, quote = FALSE)
write.csv(Gtopic5, "glasgowtopic5.csv", row.names = FALSE, quote = FALSE)

glasgowtextslda3 <- LDA(glasgowtextsDTM, k = 3, control = list(seed = 12345))
glasgowtopics3 <- tidy(glasgowtextslda3, matrix = "beta")
head(glasgowtopics3)
View(glasgowtopics3)
Gtopics1.3 <- glasgowtopics3 %>% filter(topic == 1) %>% arrange(desc(beta))
Gtopics2.3 <- glasgowtopics3 %>% filter(topic == 2) %>% arrange(desc(beta))
Gtopics3.3 <- glasgowtopics3 %>% filter(topic == 3) %>% arrange(desc(beta))

<Bishopric of Moray>



<Brechin and Arbroath>
