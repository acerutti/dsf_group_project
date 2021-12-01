library(tidytext)
library(tidyverse)
library(textdata)


### TOKENIZATION----------------------------------------------------------------

# In this section, we want to improve our data sets by looking if we can find values
# in the appartment description (variable "descr") that were not captured in their
# respective column. The interesting variables are: "area", "rooms", "furnished", "balcony" and "home_type".

### working on D ---------------------------------------------------------------

# We start with the uncleaned data set D, to see if we can catch some "areas".
# It is necessary to work on D because, in the cleaning, we removed observations
# which had NAs in the "area".

# First, we are looking at how many observations in D have no "area", 
# and out of these, how many have a description. 

D %>% filter(is.na(area)) %>%  filter(is.na(descr)) %>% count() 

# a maximum of 5089 observations could have their area predicted

# To distinguish each observation from one another for when we tokenize, we assign
# a number to each observation:

D <- rowid_to_column(D) 

# We now tokenize and look for bigrams looking like a number followed by m2 or equivalent 

area_bigrams <- D %>% filter(is.na(area) == T) %>%
  unnest_tokens(bigram, descr, token = "ngrams", n = 2, drop = F) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% as.character(seq(from= 25, to = 946, by = 1))) %>%
  
  # 25 is the minimum area for a flat that we used by definition,
  # 946 is used because 945 is the highest "area" entry of D 
  
  filter(word2 %in% c("m2", "qm")) %>%
  
  # using "m" was too risky, because it would capture for example people talking 
  # about the distance to the supermarket
  
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE) %>% 
  
  # we use a simplification here: if several tokens fulfill the criteria for a single 
  # observation, we only keep the first one.
  
  select(rowid, descr, word1, word2)

# Those are 520 observations where we could detect an "area" thanks to tokenization
# This represents 520/5089 = 10% of the maximum we could have predicted.

# If we wanted to assign them to D to improve it, we would have used this command: 

# D = D %>% left_join(area_bigrams, by = c("rowid"= "rowid", "descr" = "descr")) %>% 
# mutate(area = ifelse(is.na(word1), area, word1)) %>%
# select(-c(word1, word2))

# However, we first want to run a "diagnosis", that is: how well this method 
# performs to detect areas on the data for which we have "area" entries.


### Diagnosis of "area" -------------------------------------------------------*

D %>% filter(!is.na(area)) %>%
  select(rowid, descr, area) %>%
  unnest_tokens(bigram, descr, token = "ngrams", n = 2, drop = F) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% as.character(seq(from= 25, to = 946, by = 1))) %>% 
  filter(word2 %in% c("m2", "qm")) %>%
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE) %>%
  mutate(word1 = as.double(word1)) %>%
  mutate(score = ifelse(area == word1, 1,0)) %>%
  mutate(error = as.double(word1)-area) %>%
  summarise(diagnosis = mean(score), MAE = mean(abs(error)), MAEm = MAE/mean(D$area, na.rm = T))

# This has an accuracy of 74% on the whole dataset, and a MAE of 12.9 m2 (put in 
# relation with the mean area: 16%). This is rather good, but we decided that 
# the trade-off between possibly including incorrect areas with large deviations and 
# obtaining 520 more observations (out of 77851) was not worth it.

# So we are not going to use tokenisation to improve the "areas" in D.

# We now move on to improving our cleaned data set, data_analyzed.

# Working on data_analyzed -----------------------------------------------------

### Preparatory steps

# d_tok is the dataset which is "tokenizable" because it contains descriptions 

d_tok = data_analyzed %>% 
  filter(is.na(descr)==F) %>% 
  select(rowid,descr,furnished,rooms,balcony, home_type)

# simple tokenization 

tok <- d_tok %>% unnest_tokens(word, descr, token = "words") 

# tokenizing bigrams

bigrams <- d_tok %>% 
  unnest_tokens(bigram, descr, token = "ngrams", n = 2, drop = F)

# working on improved_data_analyzed to be safe

improved_data_analyzed = data_analyzed

### rooms ----------------------------------------------------------------------

# We create a vector containing possible number of rooms. We've noticed some 
# descriptions use commas, other use dots.

number <- as.character(c(seq(from=1, to=10, by=0.5),
                         paste(seq(from=1, to = 10, by=1), sep= ",",5))) 

# We create another vector containing different ways to say rooms that might be used
# on the website (tokenisation brings everything to lowercase so no need to worry
# about uppercases) :

room <- as.character(c("zimmer", "pièces", "pièce", "pcs", "stanze", "räume", "camere"))

new_rooms <- bigrams %>%
  select(c(rowid, descr, rooms, bigram)) %>%
  filter(is.na(rooms) == T) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% number) %>%
  filter(word2 %in% room) %>%
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE)

# Note that rowid here does not correspond to rowid made on D previously

# Here, we catch 474 observations.

# Dealing with the 1/2 (appearing as 1 2), which was wrongly caught by the bigrams: 
# we would have for example "5 1/2 zimmer" captured as 2 zimmer

trigrams <- d_tok %>% 
  filter(is.na(rooms) == T) %>%
  select(c(rowid, descr, rooms)) %>%
  unnest_tokens(trigram, descr, token = "ngrams", n = 3, drop = F) %>% 
  separate(trigram, c("word_a", "word_b", "word_c"), sep = " ") %>%
  filter((word_b == "1" & word_c =="2")) %>%
  unite(word_b, word_b, word_c, sep = "/", remove = F) %>%
  select(-word_c) %>% 
  filter(word_a %in% number) %>%
  distinct(rowid, .keep_all = TRUE)

new_rooms = new_rooms %>% left_join(trigrams, by= c("rowid"="rowid", "rooms" = "rooms", "descr" = "descr")) %>%
  mutate(rooms_new = ifelse(is.na(word_a), as.double(gsub(",", ".", word1)), as.double(word_a) + 0.5)) %>%
  select(rowid, descr, rooms_new)
           
# This trick works because there is only when talking about rooms that there is 
# a format like "number" "1" "2" in the tokenization, corresponding to "number" "1/2".

# 55 observations is not so significant but this is important because without this 
# step, the 2 rooms category would be quite polluted

# Diagnosis of "rooms" --------------------------------------------------------*

# Before assigning the values captured through tokenization, we want to check
# if the method we use is convincing.

# rooms_diagnosis has all observations for which we can detect a number of rooms
# through tokenization

rooms_diagnosis <- bigrams %>%
  filter(is.na(rooms) == F) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% number) %>%
  filter(word2 %in% room) %>%
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE) %>%  
  mutate(word1 = as.double(gsub(",", ".", word1)))

# We correct the 1/2 like previously

trigrams_diagnosis <- d_tok %>% 
  filter(is.na(rooms) == F) %>%
  unnest_tokens(trigram, descr, token = "ngrams", n = 3, drop = F) %>% 
  separate(trigram, c("word_a", "word_b", "word_c"), sep = " ") %>%
  filter((word_b == "1" & word_c =="2")) %>%
  unite(word_b, word_b, word_c, sep = "/", remove = F) %>%
  select(-word_c) %>%
  filter(word_a %in% number) %>%
  distinct(rowid, .keep_all = TRUE) %>%
  select(rowid, rooms, word_a, word_b)

rooms_diagnosis %>% 
  left_join(trigrams_diagnosis, by = c("rowid" = "rowid", "rooms" = "rooms")) %>%
  mutate(room_pred = ifelse(is.na(word_a), word1, as.double(word_a) + 0.5)) %>%
  select(rowid, rooms, descr, room_pred) %>%
  mutate(score = ifelse(rooms == room_pred, 1,0)) %>% 
  mutate(error = room_pred-rooms) %>%
  summarise(diagnosis = mean(score), MAE = mean(abs(error)), MAEm = MAE/mean(data_analyzed$rooms, na.rm = T))

# We have an accuracy of 83% and a MAE of 0.153 rooms (put in relation to the mean 
# number of rooms, 4.5%), which is pretty good. Interestingly, for some of them 
# (i.e. rowid = 24), what we capture through tokenisation is correct whereas what
# is entered in the column "rooms" is incorrect. 

# We now assign the "rooms" their values where there were NAs in the dataset:

improved_data_analyzed = improved_data_analyzed %>% 
  left_join(new_rooms, by = c("rowid"= "rowid", "descr" = "descr")) %>%
  mutate(rooms = ifelse(is.na(rooms_new), rooms, rooms_new)) %>%
  select(-rooms_new)

# We left join our new findings to the data set and then overwrite the column 
# with the new value if it exists, then drop the column with the new values 
# since those are incorporated to the original column of the dataset.

### furnished ------------------------------------------------------------------

furnished_words <- c("furnished", "furniture",
                     "meublé", "meubles", "meuble", "meublées", "meublée", "mobilier",
                     "möbliert", "möblierte", "möbel", "möbeln",
                     "ammobiliato", "mobilio", "mobili")

# Making sure we don't say it's furnished if it says i.e. "not furnished"

negation <- c("non", "not", "no", "nicht", "ohne", "pas", "senza")

furnished_appartments <- bigrams %>%
  select(c(rowid, furnished, bigram)) %>%
  filter(furnished == 0) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% negation) %>%
  filter(word2 %in% furnished_words) %>%
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE) %>%
  mutate(new_furnished = 1) %>% 
  select(c(rowid, new_furnished))

# For "furnished" and "balcony", we do not run a diagnosis as it could be the case
# that the appartment has a balcony, it is indicated as such in the balcony column
# but not in the description. Moreover, the simple words used makes us more 
# confident to catch whether it is furnished or not. Finally, a mistake when it comes 
# to furnished yes or no is not as impactful as a quantitative mistake in the area 
# assigned for example.

improved_data_analyzed = improved_data_analyzed %>% 
  left_join(furnished_appartments, by = c("rowid" = "rowid")) %>%
  mutate(furnished = ifelse(is.na(new_furnished), furnished, new_furnished)) %>% 
  select(-new_furnished)

### balcony --------------------------------------------------------------------

balcony_words <- c("balcon", "balkon", "balcone", "balcony")

balcony_appartments <- bigrams %>% 
  filter(balcony == 0) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% negation) %>%
  filter(word2 %in% balcony_words) %>%
  unite(bigram, word1, word2, sep = " ", remove = F) %>%
  distinct(rowid, .keep_all = TRUE) %>% 
  mutate(new_balcony = 1) %>% 
  select(c(rowid, new_balcony))

improved_data_analyzed = improved_data_analyzed %>% 
  left_join(balcony_appartments, by = c("rowid" = "rowid")) %>%
  mutate(balcony = ifelse(is.na(new_balcony), balcony, new_balcony)) %>% 
  select(-new_balcony)

### home_type ------------------------------------------------------------------

home_type_words = c("wohnung","attika","dachwohnung","terrassenwohnung","maisonette",      
                    "studio","loft","ferienwohnung", "attique", "soffitta", "attico","sottotetto", "duplex")

new_home_types = tok %>% 
  select(rowid, home_type, word) %>%
  filter(word %in% home_type_words) %>% 
  mutate(home_type = tolower(home_type)) %>%
  filter(home_type != word) %>% 
  filter(word != "wohnung") %>% 
  
  # We don't want to overwrite anything by "wohnung" since it's the standard word
  
  filter(home_type == "wohnung") %>% 
  
  # We only want to overwrite Wohnungen and not one 'special' types by another 'special' type
  
  distinct(rowid, .keep_all = TRUE) %>%
  
  # we still need to put the translations back into their original meaning
  
  mutate(home_type_new = ifelse(word %in% c("wohnung","attika","dachwohnung","terrassenwohnung",
                                            "maisonette","studio","loft","ferienwohnung"), 
                                word,
                                
                                # If they're in the original unique home_types,
                                # We keep them as they are.
                                
                                ifelse(word %in% c("attique", "soffitta", "attico","sottotetto"), 
                                       "attika", 
                                       
                                       # All of those mean attika
                                       
                                       "maisonette")))
                                      
                                       # We are just left with "duplex", which is
                                       # is associated with maisonette.


# Since we put everything in lowercase during tokenization, we want to re-capitalize
# the home_types so we can left-join with the original data-set

cap <- function(x) {
  paste0(toupper(substring(x,1,1)), substring(x,2))
}

# We apply this function to all columns except the rowid

new_home_types[,2:4] = apply(new_home_types[,2:4], 2, cap)

# We assign the values

improved_data_analyzed = improved_data_analyzed %>% 
  left_join(new_home_types, by = c("rowid" = "rowid", "home_type" = "home_type")) %>%
  mutate(home_type = ifelse(is.na(home_type_new), home_type, home_type_new)) %>%
  select(-c(word,home_type_new))

### final steps ----------------------------------------------------------------

# In this nice plot we show you how many new observations we found which improved
# our data set 
results <- tibble(values = c(nrow(balcony_appartments), nrow(furnished_appartments),
                             nrow(new_rooms), nrow(new_home_types)), object = c("balcony", "furnished", "rooms", "home type"))

ggplot(data=results, aes(x=object, y=values)) +
  geom_bar(stat="identity") +
  theme_bw()


# Quick dimensions check

dim(data_analyzed)

dim(improved_data_analyzed)

# We overwrite the data_analyzed by the now improved data set (improved_data_analyzed)

data_analyzed = improved_data_analyzed

# We might be interested in filtering out Ferienwohnungen as they might not be
# rented out for a full month and thus the rent price/ m2 might be distorted

data_analyzed_ferien <- data_analyzed %>% filter(home_type != "Ferienwohnung") 
