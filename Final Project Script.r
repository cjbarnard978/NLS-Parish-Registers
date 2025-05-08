Final Project Script #using info from our webscraping script project 
library(internetarchive)
library(tidyverse)

Registrumquery <- c("contributor" = "National Library of Scotland", "title" = "Registrum")

ia_search(Registrumquery, num_results = 19) %>% ia_get_items() %>% ia_metadata() %>% filter(field == "title" | field == "external-identifier")

registrummetadata <- as.data.frame(ia_search(Registrumquery, num_results = 19) %>% ia_get_items() %>% ia_metadata() %>% filter(field == "title" | field == "external-identifier"))

registrummetadata <- registrummetadata %>% pivot_wider(names_from = field, values_from = value)

dir.create("NLSParishRegisterTexts")

ia_search(Registrumquery, num_results = 19) %>% ia_get_items %>% ia_files %>% filter(type == "txt") %>% group_by(id) %>% ia_download(dir = "NLSParishRegisterTexts", overwrite = FALSE,) %>% glimpse()
