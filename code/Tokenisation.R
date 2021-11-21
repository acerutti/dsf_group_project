#   tokenisation

library(tidytext)
library(tidyverse)
library(lubridate)
library(tsibble)
library(scales)
library(textdata)
library(tidyr)

D_selection = D_selection %>% 
  mutate(ID = row_number())

D_tok = D_selection %>% 
  filter(is.na(descr)==F) %>% 
  select(ID,descr,furnished,area,rooms)

tok <- D_tok %>% unnest_tokens(word, descr, token = "words")


### 
tok %>% filter(furnished == 0) %>%
  
  
  ### checking for the number of rooms
  room_number <- as.character(seq(from=1, to=10, by=0.5)) 

test <- tok %>% 
  filter(is.na(rooms) == T) %>%
  filter(word %in% as.character(seq(from=1.5, to=10.5, by=1)))

bigrams <- D_tok %>% unnest_tokens(bigram, descr, token = "ngrams", n = 2, drop = F)

bigrams <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% str_remove_all(stop_words$word, "'")) %>%
  filter(!word1 %in% str_remove_all(stop_words$word, "'")) %>%
  filter(!word1 %in% c("amp", "lt", "gt")) %>%
  filter(!word2 %in% c("amp", "lt", "gt")) %>%
  filter(!(str_detect(word1, "[a-z]") & str_detect(word2, "[a-z]"))) 
unite(bigram, word1, word2, sep = " ", remove = F)