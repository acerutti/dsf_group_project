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

# D Area ----

D %>% filter(is.na(area)) %>%  filter(is.na(descr)) %>% count()

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

### DIAGNOSIS AREA ---------

area_bigrams_test <- D %>% filter(!is.na(area)) %>%
  unnest_tokens(bigram, descr, token = "ngrams", n = 2, drop = F) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% as.character(seq(from= 25, to = 946, by = 1))) %>%
  filter(word2 %in% c("m2", "qm")) %>%
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE) 

area_bigrams_test %>% select(rowid, area, descr, word1, word2) %>%
  mutate(word1 = as.double(word1)) %>%
  mutate(area_prediction = ifelse(area != word1, 0,1)) %>%
  summarise(diagnosis = mean(area_prediction))

# this has an accuracy of 0.74 on the whole dataset, 
# but the deviations in those that are mispredicted is too high
# so we are not going to use it to "improve" the original dataset
# we can only do tokenisation for observations with a "descr" or appartment description 

D_tok = data_analyzed %>% 
  filter(is.na(descr)==F) %>% 
  select(rowid,descr,furnished,rooms,balcony, home_type)


tok <- D_tok %>% unnest_tokens(word, descr, token = "words") 

bigrams <- D_tok %>% 
  unnest_tokens(bigram, descr, token = "ngrams", n = 2, drop = F)

improved_data_analyzed = data_analyzed

### rooms ----

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

improved_data_analyzed[unique(trigrams$rowid),"rooms"] <- as.double( 
  paste(trigrams$word1, sep = ".",5))

# DIAGNOSIS ROOMS -----

new_rooms_tok <- bigrams %>%
  filter(is.na(rooms) == F) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% number) %>%
  filter(word2 %in% room) %>%
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE)  # those where it appears more than once... 
# hoping that the 1st one is the correct one

new_rooms_tok = new_rooms_tok %>% mutate(word_numeric = as.double(gsub(",", ".", word1)))

trigrams_test <- D_tok %>% 
  filter(is.na(rooms) == F) %>%
  unnest_tokens(trigram, descr, token = "ngrams", n = 3, drop = F) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter((word2 == "1" & word3 =="2")) %>%
  unite(word2, word2, word3, sep = "/", remove = F) %>%
  filter(word1 %in% number) %>%
  distinct(rowid, .keep_all = TRUE)

new_rooms_tok %>% left_join(trigrams_test %>% select(rowid, word1, word2), by = c("rowid" = "rowid")) %>%
  mutate(room_pred = ifelse(is.na(word1.y), word_numeric, as.double(word1.y) + 0.5)) %>%
  select(rowid, rooms, descr, room_pred) %>%
  mutate(score = ifelse(rooms == room_pred, 1,0)) %>% 
  summarise(diagnosis = mean(score))

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

home_type_words = c("wohnung","attika","dachwohnung","terrassenwohnung","maisonette",      
              "studio","loft","ferienwohnung", "attique", "soffitta", "attico","sottotetto", "duplex")

new_home_types = tok %>% filter(word %in% home_type_words)

new_home_types$home_type =tolower(new_home_types$home_type)

new_home_types = new_home_types %>% 
  filter(home_type != word) %>% # 7706 obs, but do we really want to overwrite?
  filter(word != "wohnung") %>% # 1967 obs, removing since its the "default" word
  filter(home_type == "wohnung") %>% # 1596 obs, #deciding not to overwrite the "special" hometypes
  distinct(rowid, .keep_all = TRUE) 

# I still need to do an ifelse and put the translations together

### DIAGNOSIS BALCONY -----

balcony_tok <- tok %>% filter(balcony == 1) %>% 
  filter(word %in% balcony_words) %>% 
  distinct(rowid, .keep_all = TRUE)

D_tok = D_tok %>% mutate(balcony_pred = ifelse(rowid %in% balcony_tok$rowid,1,0), 
                         balcony_score = ifelse(balcony_pred != balcony, 0, 1))

D_tok %>% filter(!rowid %in% balcony_appartments$rowid) %>%
  summarise(balcony_correct = mean(balcony_test))

mean(D_tok$balcony_score)


### DIAGNOSIS FURNISHED -----
furnished_tok <- tok %>% 
  filter(word %in% furnished_words)

no_furniture_tok <- bigrams %>%
  filter(rowid %in% furnished_appartments$rowid) %>% #if remove this line get rid of the 2
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% negation) %>%
  filter(word2 %in% furnished_words) %>%
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE)

D_tok = D_tok %>% mutate(furnished_tok = ifelse(rowid %in% furnished_tok$rowid,1,0), 
                         furnished_test = ifelse(furnished_tok != furnished, 0, 1))

D_tok %>% filter(!rowid %in% furnished_appartments$rowid) %>%
  summarise(furnished_correct = mean(furnished_test))

### final steps ----

dclean = improved_data_analyzed

save(D, data_analyzed, dclean , problems, file = "data/rent_listings.RData")




