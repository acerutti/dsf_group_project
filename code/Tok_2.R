################################################################################
#                                                                              #
# DSF Group Project                                                            #
# Script 3: Tokenisation                                                       #
# 20.11.2021                                                                   #
#                                                                              #        
################################################################################

rm(list = ls())

load("data/rent_listings_raw.RData")

library(tidytext)
library(tidyverse)
library(lubridate)
library(textdata)
library(tidyr)

# we can only do tokenisation for observations with a "descr" or appartment description 

D_tok = data_analyzed %>% 
  filter(is.na(descr)==F) %>% 
  select(rowid,descr,furnished,rooms,balcony, home_type)


tok <- D_tok %>% unnest_tokens(word, descr, token = "words") 

improved_data_analyzed = data_analyzed

### furnished ----

furnished_words <- c("furnished", "furniture",
                     "meublé", "meubles", "meuble", "meublées", "meublée", "mobilier",
                     "möbliert", "möblierte", "möbel", "möbeln",
                     "ammobiliato", "mobilio", "mobili")

furnished_appartments <- tok %>% filter(furnished == 0) %>%
  filter(word %in% furnished_words) 

improved_data_analyzed[unique(furnished_appartments$rowid),"furnished"] <- 1 # 1723 obs

# problem: doesn't take into account "non-meublé" - trying to solve this with bigrams

negation <- c("non", "no", "nicht", "ohne", "pas", "senza")

bigrams <- D_tok %>% 
  unnest_tokens(bigram, descr, token = "ngrams", n = 2, drop = F)

no_furniture <- bigrams %>%
  filter(rowid %in% furnished_appartments$rowid) %>% #if remove this line get rid of the 2
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% negation) %>%
  filter(word2 %in% furnished_words) %>%
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE) # 110 obs 

improved_data_analyzed[unique(no_furniture$rowid),"furnished"] <- 0 # 108 obs

### balcony ----

balcony_words <- c("balcon", "balkon", "balcone", "balcony")

balcony_appartments <- tok %>% filter(balcony == 0) %>%
  filter(word %in% balcony_words) %>% 
  distinct(rowid, .keep_all = TRUE)

improved_data_analyzed[unique(balcony_appartments$rowid),"balcony"] <- 1 # 8997 obs

### home_type ----

homes = c("wohnung","attika","dachwohnung","terrassenwohnung","maisonette",      
              "studio","loft","ferienwohnung", "attique", "soffitta", "attico","sottotetto", "duplex")

new_home_types = tok %>% filter(word %in% homes)

new_home_types$home_type =tolower(new_home_types$home_type)

new_home_types = new_home_types %>% 
  filter(home_type != word) %>% # 7706 obs, but do we really want to overwrite?
  filter(word != "wohnung") %>% # 1967 obs, removing since its the "default" word
  filter(home_type == "wohnung") %>% # 1596 obs, #deciding not to overwrite the "special" hometypes
  distinct(rowid, .keep_all = TRUE) # 1417 obs (all of those where before i included more languages)

improved_data_analyzed[unique(balcony_appartments$rowid),"balcony"] <- 1
# I still need to do an ifelse and put the translations together

### number of rooms ----

number <- as.character(c(seq(from=1, to=10, by=0.5),
                         paste(seq(from=1, to = 10, by=1), sep= ",",5))) 

room <- as.character(c("zimmer", "pièces", "pièce", "pcs", "stanze", "räume", "camere"))

new_rooms <- bigrams %>%
  filter(is.na(rooms) == T) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% number) %>%
  filter(word2 %in% room) %>%
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE)  # those where it appears more than once... 
                                     # hoping that the 1st one is the correct one

improved_data_analyzed[unique(new_rooms$rowid),"rooms"] <- as.numeric( 
  ifelse(new_rooms$word1 %in% number[1:19], 
         new_rooms$word1, 
         (paste((substr(new_rooms$word1,1,1)), sep= ".",5))))

              # simplistic but it works: catches 472 more observations

# dealing with the 1/2 (appearing as 1 2), overwriting what was wrongly caught by the bigrams

trigrams <- D_tok %>% 
  filter(is.na(rooms) == T) %>%
  unnest_tokens(trigram, descr, token = "ngrams", n = 3, drop = F) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter((word2 == "1" & word3 =="2")) %>%
  unite(word2, word2, word3, sep = "/", remove = F) %>%
  filter(word1 %in% number) %>%
  distinct(rowid, .keep_all = TRUE)

improved_data_analyzed[unique(trigrams$rowid),"rooms"] <- as.numeric( 
  paste(trigrams$word1, sep = ".",5))

# trying stuff out with area on D ----
D <- rowid_to_column(D)

area_bigrams <- D %>% filter(is.na(area) == T) %>%
  unnest_tokens(bigram, descr, token = "ngrams", n = 2, drop = F) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% as.character(seq(from= 25, to = 200, by = 1))) %>%
  filter(word2 %in% c("m2", "qm")) %>%
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE) #334 obs

area_bigrams = area_bigrams %>% select(descr, area, word1, word2)


D[unique(area_bigrams$rowid),"area"] <- as.numeric(area_bigrams$word1) 

### DIAGNOSIS -----
balcony_appartments <- tok %>% filter(balcony == 0) %>%
  filter(word %in% balcony_words) %>% 
  distinct(rowid, .keep_all = TRUE)

improved_data_analyzed[unique(balcony_appartments$rowid),"balcony"] <- 1

D_tok %>% mutate(balcony_tok = ..)
















