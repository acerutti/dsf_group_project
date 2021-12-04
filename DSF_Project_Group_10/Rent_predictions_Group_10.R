###############################################################################*
#
# Data Science Fundamentals Group Project
# Fall 2021
# Final script 
# 05.12.2021
# Maximilian Georg Hans Bisges
# Alessandra Elivia Eugenia Cerutti
# Amelie Carla Hoa Madrona
# Jason Rosenthal
#
###############################################################################*

### Preparatory Steps

rm(list = ls())

# Loading all packages needed for this project

# Cleaning

library(xlsx)
library(tidyverse)

# Tokenization
library(tidytext)
library(textdata)

# Summary statistics
library(vtable) 
library(qwraps2)

# OLS
library(caret)              # cross-validation etc.
library(rsample)            # data partitions
library(ranger)             # random forests
library(randomForest)       # random forest 
library(janitor)            # use for variable name cleaning

# Boosted trees

library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(h2o)          # a java-based platform
library(pdp)          # model visualization
library(vip)          # devtools::install_github("koalaverse/vip")



### Please set your working directory to our group project folder!

### DATA CLEANING --------------------------------------------------------------

load("rent_listings.RData")

## Variable Selection ----------------------------------------------------------

# We first check which variables do not contain any data. 

# NAs per variable
lapply(D, function(x) table(is.na(x)))

# preliminary selection of important variables
relevant_variables <- c("rent_full", "area", "home_type", "GDENAMK", "GDENR", "KTKZ", "zipcode", 
                  "GKODE", "GKODN", "PLZNAME", "descr", "floors", "furnished", 
                  "lat", "lon", "date","month", "quarter_general",
                  "msregion", "rooms",  "year_built", "newly_built", "balcony", 
                  "Micro_rating_new", "Micro_rating_NoiseAndEmission_new", 
                  "Micro_rating_Accessibility_new", "Micro_rating_DistrictAndArea_new", 
                  "Micro_rating_SunAndView_new", "Micro_rating_ServicesAndNature_new",
                  "dist_to_haltst", "dist_to_highway", "dist_to_school_1",
                  "dist_to_train_stat", "apoth_pix_count_km2", 
                  "restaur_pix_count_km2", "superm_pix_count_km2")

###############################################################################*
# Notes on variables
################################################################################*
# Address and any GPS data is irrelevant
# area usable is not specified in most cases, use area
#
# balcony is unclear: NA comes from not specifying in description or no balcony?
# Assumption: Balcony NAs means balcony option was not ticked in listing (however,
# in our tokenization part, we attempt to extract that information from descr)
#
# does floors mean on which floor it is? we delete it because of incoherence
#
# what does msregion mean? MSRegion are defined regions by the BFS. Each region
# is homogeneous. https://www.bfs.admin.ch/bfs/de/home/statistiken/raum-umwelt/nomenklaturen/msreg.assetdetail.415729.html
#
# newly_built might have the same problem as the apartment features - if it's 
# not specified it has not got a 1 (we delete it)
#
# Is zipcode PLZ4? They are the same! Keep only one
# which(na.omit(D)$zipcode != na.omit(D)$PLZ4)
#
# the two columns with wgh_avg_... are identical - however we don't know what 
# this info refers to -> delete it
#
# Some further irrelevant variables which we discussed, we removed
#
###############################################################################*


## FILTERING METHOD APPROACH ------*
  # We filter outliers as defined by the boxplot system. This means we first
  # compute the ranges of admissible results for every AMR, and then drop the 
  # observations which are defined as outliers for every AMR. Note: we only drop
  # bottom outliers as we have issues with observations with lowest rent_full


# Legend of Arbeitsmarktregionen (AMR)
legend <- as_tibble(read.xlsx("legend.xlsx", sheetIndex = 1, header = T))
legend$MS.Regionen <- as.double(legend$MS.Regionen)
legend$Arbeitsmarktregionen.2018 <- as.double(legend$Arbeitsmarktregionen.2018)
legend$Arbeitsmarktgrossregionen.2018 <- as.double(legend$Arbeitsmarktgrossregionen.2018)
legend$BFS.Gde.nummer <- as.double(legend$BFS.Gde.nummer)

legend <- legend %>%
  select(MS.Regionen, Arbeitsmarktregionen.2018, Arbeitsmarktgrossregionen.2018, BFS.Gde.nummer)

# Arbeitsmarktregionen Names
amr_names <- read.xlsx("legend.xlsx", sheetIndex = 3, startRow = 2, header = T)
amr_names$Code <- as.double(amr_names$Code)

# Combine Arbeitsmarktregionen with its names
legend <- legend %>%
  left_join(amr_names, by = c("Arbeitsmarktregionen.2018" = "Code"))

# Calculate quartiles for the acceptance ranges
ms_quantiles <- D %>% 
  filter(!is.na(area)) %>%
  filter(area >= 25) %>%
  select(all_of(relevant_variables)) %>%
  mutate("rent_m2" = rent_full/area) %>%
  left_join(legend, by = c("msregion" = "MS.Regionen", "GDENR" = "BFS.Gde.nummer")) %>%
  group_by(Arbeitsmarktregionen.2018) %>%
  summarise(median = median(rent_m2), rent_iqr= IQR(rent_m2), q1 = quantile(rent_m2, probs = 0.25), q3 = quantile(rent_m2, probs = 0.75))

# Data which is going to be analyzed
data_analyzed <- D %>%
  filter(!is.na(area)) %>%
  filter(area >= 25) %>%
  select(all_of(relevant_variables)) %>%
  left_join(legend, by = c("msregion" = "MS.Regionen", "GDENR" = "BFS.Gde.nummer")) %>%
  mutate("rent_m2" = rent_full/area) %>%
  left_join(ms_quantiles, by = c("Arbeitsmarktregionen.2018" = "Arbeitsmarktregionen.2018")) %>%
  mutate(outlier = ifelse(rent_m2 < q1 - 1.5*rent_iqr, 1, 0)) %>%
  filter(outlier == 0) %>%
  rowid_to_column() %>%
  select(-c("floors", "median", "rent_iqr", "q1", "q3", "outlier"))


data_analyzed[,c("balcony", "furnished")] = apply(data_analyzed[,c("balcony", "furnished")], 2, function(x) replace_na(x,0))

# remove all intermediate objects
rm(list = setdiff(ls(), c("D", "data_analyzed", "problems")))


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

# Here, we catch 474 observations which had an NA value in the variable rooms.

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

# Plot to visualize results
ggplot(data=results, aes(x=object, y=values)) +
  geom_bar(stat="identity", fill = "coral", alpha = 0.7) +
  labs(title = "New observations obtained through text analysis", 
       y = "Number of observations",
       x = "Apartment features") +
  theme_bw()


# Quick dimensions check

dim(data_analyzed)

dim(improved_data_analyzed)

# We overwrite the data_analyzed by the now improved data set (improved_data_analyzed)

data_analyzed = improved_data_analyzed

# We might be interested in filtering out Ferienwohnungen as they might not be
# rented out for a full month and thus the rent price/ m2 might be distorted

data_analyzed <- data_analyzed %>% filter(home_type != "Ferienwohnung") 

# remove intermediate objects
rm(list = setdiff(ls(), c("D", "data_analyzed", "problems")))


### DATA ANALYSIS--------------------------------------------------------------

## 1. Summary statistics ----------------------------------------------------

# In a first step we want to get an overview over the dataset which is ready for
# analysis. This is done by retrieving some summary statistics.

st(data_analyzed %>% select(c(2,3,5,7,20,21,23,24))) # selected variables, whole dataset

st((data_analyzed %>% select(c(2,3,5,7,20,21,23,24))), group = 'KTKZ' , group.long = TRUE) # selected variables, summarized per canton

# Next we want to see how many different categories we have for certain variables

overview = data.frame(n_home_type = unique(data_analyzed$home_type) %>% length(),
                      n_GDENAMK = unique(data_analyzed$GDENAMK) %>% length(),
                      n_KTKZ = unique(data_analyzed$KTKZ) %>% length(),
                      n_PLZNAME = unique(data_analyzed$PLZNAME) %>% length(),
                      n_Arbeitsmarktregionen.2018 = unique(data_analyzed$Arbeitsmarktregionen.2018) %>% length(),
                      n_Arbeitsmarktgrossregionen.2018 = unique(data_analyzed$Arbeitsmarktgrossregionen.2018) %>% length())


## 2. Visualisations ----------------------------------------------------------

# In a second step we want to get an impression of the dataset by generating
# some plots. In addition, we try to see first correlations and we will run
# some simple regressions


## 2.1 Types of flats offered -------------------------------------------

  # Number of flats depending on size

flats_per_size <- data_analyzed %>% select(area, KTKZ) %>% mutate(flat_category = case_when(
  area <60  ~  "<60",
  area >=60 & area <=79  ~  "60-79",
  area >=80 & area <=99  ~  "80-99",
  area >=100 & area <=119  ~  "100-119",
  area >=120  & area <=159 ~  "120-159",
  area >=160  ~  ">160")
) %>%
  group_by(flat_category) %>%
  summarize(number_of_flats = length(flat_category))


flats_per_size$flat_category <- factor(flats_per_size$flat_category ,
                                       levels = c("<60", "60-79", "80-99", "100-119", "120-159", ">160")) # Rearrange the order

ggplot(data = flats_per_size, aes(x=flat_category, y = number_of_flats)) + geom_bar(stat = "identity", color ="black", fill = "darkblue") + labs(title = "Flat offerings in 2019 depending on flat size",
                                                                                                                                              x = "Flat size category",
                                                                                                                                              y = "Number of flat offerings")  +
  theme_bw(base_size = 20) 


#  Flat type and size depending on number of rooms

data_analyzed %>% filter(!is.na(rooms)) %>%

ggplot(aes(x = rooms, y = area, col = home_type)) +
  geom_point() +
  geom_hline(yintercept = mean(data_analyzed$area), col = "blue") + # average area in Switzerland
  geom_smooth(method = "lm", col = "black") +
  labs(title ="Flat size (area) depending on number of rooms by hometype",
       x = "Number of rooms",
       y = "Flat size (area)") +
  theme_bw(base_size = 20)


  # Number of flats offered depending on cantons

flats_canton <- data_analyzed %>% select(KTKZ) %>% mutate(flat = 1)
ggplot(data = flats_canton, aes(x=KTKZ, y = flat)) + geom_bar(stat = "identity", color ="darkblue") + labs(title = "Flat offerings per canton",
                                                                                                       x = "Canton",
                                                                                                       y = "Number of flat offerings") +
  theme_bw(base_size = 20)


## 2.2 Rent price (full / per m2) depending on area ----------------------------

  # Rent price full depending on area 

ggplot(data_analyzed, aes(x = area, y = rent_full)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +                             # add linear regressions
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue") + # add average rent price
  labs(title ="Rent price depending on flat size (area)",
       x = "Flat size (area)",
       y = "Rent price (full)") +
  theme_bw(base_size = 20)

  # Rent price full depending on area differed by canton

ggplot(data_analyzed, aes(x = area, y = rent_full, col = KTKZ)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", col = "black") +                             # add linear regressions
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue") + # add average rent price
  labs(title ="Rent price depending on flat size (area) by canton",
       x = "Flat size",
       y = "Rent price (full)") +
  theme_bw(base_size = 20)

  # Rent price per m2 depending on area 

ggplot(data_analyzed, aes(x = area, y = rent_m2)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +                             # add linear regressions
  geom_hline(yintercept = mean(data_analyzed$rent_m2), col = "blue") + # add average rent price
  labs(title ="Rent price per m2 depending on flat size (area)",
       x = "Flat size",
       y = "Rent price per m2")  +
  theme_bw(base_size = 20)

  # Rent price per m2 depending on area differed per canton

ggplot(data_analyzed, aes(x = area, y = rent_m2, col = KTKZ)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +                             # add linear regressions
  geom_hline(yintercept = mean(data_analyzed$rent_m2), col = "blue") + # add average rent price
  labs(title ="Rent price per m2 depending on flat size (area) by canton",
       x = "Flat size",
       y = "Rent price per m2")  +
  theme_bw(base_size = 20)


## 2.3 Average rents depending on region -------------------------

  # Average rent full per canton

avg_rent_per_canton <- data_analyzed %>%
  select(rent_full,KTKZ) %>%
  group_by(KTKZ) %>% 
  summarize(avg_cantonal_rent = mean(rent_full)) %>%
  arrange(avg_cantonal_rent)

ggplot(avg_rent_per_canton) + geom_point(aes(x = reorder(KTKZ, -avg_cantonal_rent), y = avg_cantonal_rent)) +
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue") +
  labs(title ="Average rent price (full) per canton",
       x = "Canton",
       y = "Average rent price (full)") +
  theme_bw(base_size = 20)


  # Average rent full per Arbeitsmarktgrossregion


avg_rent_per_Arbeitsmarktgrossregion <- data_analyzed %>%
  select(rent_full,Arbeitsmarktgrossregionen.2018) %>% na.omit() %>%
  group_by(Arbeitsmarktgrossregionen.2018) %>% 
  summarize(avg_Arbeitsmarktgrossregionen_rent = mean(rent_full))

ggplot(avg_rent_per_Arbeitsmarktgrossregion) + geom_point(aes(x = reorder(Arbeitsmarktgrossregionen.2018, -avg_Arbeitsmarktgrossregionen_rent), y = avg_Arbeitsmarktgrossregionen_rent)) +
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue") +
  labs(title ="Average rent price (full) per Arbeitsmarktgrossregion",
       x = "Arbeitsmarktgrossregion",
       y = "Average rent price (full)")+
  theme_bw(base_size = 20)


  # Average rent per m2 per canton

avg_rent_m2_per_canton <- data_analyzed %>%
  select(rent_m2,KTKZ) %>%
  group_by(KTKZ) %>% 
  summarize(avg_cantonal_rent_m2 = mean(rent_m2)) %>% 
  arrange(avg_cantonal_rent_m2)

ggplot(avg_rent_m2_per_canton) + geom_point(aes(x = reorder(KTKZ, -avg_cantonal_rent_m2), y = avg_cantonal_rent_m2)) +
  geom_hline(yintercept = mean(data_analyzed$rent_m2), col = "blue") +
  labs(title ="Average rent price per m2 per canton",
       x = "Canton",
       y = "Average rent price per m2")+
  theme_bw(base_size = 20)


  # Average rent per m2 per Arbeitsmarktgrossregion

avg_rent_m2_per_Arbeitsmarktgrossregion <- data_analyzed %>%
  select(rent_m2,Arbeitsmarktgrossregionen.2018) %>% na.omit() %>%
  group_by(Arbeitsmarktgrossregionen.2018) %>% 
  summarize(avg_Arbeitsmarktgrossregionen_rent_m2 = mean(rent_m2))

ggplot(avg_rent_m2_per_Arbeitsmarktgrossregion) + geom_point(aes(x = reorder(Arbeitsmarktgrossregionen.2018, -avg_Arbeitsmarktgrossregionen_rent_m2), y = avg_Arbeitsmarktgrossregionen_rent_m2)) +
  geom_hline(yintercept = mean(data_analyzed$rent_m2), col = "blue") +
  labs(title ="Average rent price per m2 per Arbeitsmarktgrossregion",
       x = "Arbeitsmarktgrossregion",
       y = "Average rent per m2") +
  theme_bw(base_size = 20)


## 2.4 Average rents depending on micro rating new ----------------------------

  # Rent price full depending on micro rating new

rent_per_micro_rating <- data_analyzed %>% select(rent_full, Micro_rating_new) %>% mutate(micro_rating_cluster = case_when(
  Micro_rating_new <1.5  ~  1,
  Micro_rating_new >=1.5 & Micro_rating_new <2.5  ~ 2,
  Micro_rating_new >=2.5 & Micro_rating_new <3.5  ~ 3,
  Micro_rating_new >=3.5 & Micro_rating_new <4.5  ~ 4,
  Micro_rating_new >=4.5 & Micro_rating_new <5.5 ~  5,
  Micro_rating_new >=5.5 & Micro_rating_new <6.5 ~  6,
  Micro_rating_new >=6.5 & Micro_rating_new <7.5 ~  7,
  Micro_rating_new >=7.5 & Micro_rating_new <8.5 ~  8,
  Micro_rating_new >=8.5 & Micro_rating_new <9.5 ~  9,
  Micro_rating_new >=9.5 & Micro_rating_new <10.5 ~  10
)
) %>% 
  group_by(micro_rating_cluster) %>%
  summarize(avg_rent = mean(rent_full))

regression_rent_micro_rating_new = lm(rent_full ~ (Micro_rating_new), data_analyzed) # regression of rent_full on all Micro_rating_new without rounding
regression_rent_micro_rating_new_2 = lm(avg_rent ~ (micro_rating_cluster), rent_per_micro_rating) # regression of avg_rent_full on clustered (rounded) micro_ratings_new

ggplot(rent_per_micro_rating) + geom_point(aes(x = micro_rating_cluster, y = avg_rent)) +
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue") + # average rent price in Switzerland
  geom_abline(intercept = regression_rent_micro_rating_new$coefficients[1], slope = regression_rent_micro_rating_new$coefficients[2], col = "red") +
  geom_abline(intercept = regression_rent_micro_rating_new_2$coefficients[1], slope = regression_rent_micro_rating_new_2$coefficients[2], col = "magenta") +
  labs(title ="Average rent depending on micro rating new (rounded)",
       x = "Micro Rating New (rounded)",
       y = "Average rent price (full)") +
  theme_bw(base_size = 20)


  # Rent price per m2 depending on micro rating new

rent_m2_per_micro_rating <- data_analyzed %>% select(rent_m2, Micro_rating_new) %>% mutate(micro_rating_cluster = case_when(
  Micro_rating_new <1.5  ~  1,
  Micro_rating_new >=1.5 & Micro_rating_new <2.5  ~ 2,
  Micro_rating_new >=2.5 & Micro_rating_new <3.5  ~ 3,
  Micro_rating_new >=3.5 & Micro_rating_new <4.5  ~ 4,
  Micro_rating_new >=4.5 & Micro_rating_new <5.5 ~  5,
  Micro_rating_new >=5.5 & Micro_rating_new <6.5 ~  6,
  Micro_rating_new >=6.5 & Micro_rating_new <7.5 ~  7,
  Micro_rating_new >=7.5 & Micro_rating_new <8.5 ~  8,
  Micro_rating_new >=8.5 & Micro_rating_new <9.5 ~  9,
  Micro_rating_new >=9.5 & Micro_rating_new <10.5 ~  10
)
) %>% 
  group_by(micro_rating_cluster) %>%
  summarize(avg_rent_m2 = mean(rent_m2))


regression_rent_m2_micro_rating_new = lm(rent_m2 ~ (Micro_rating_new), data_analyzed) # regression of rent_full on all Micro_rating_new without rounding
regression_rent_m2_micro_rating_new_2 = lm(avg_rent_m2 ~ (micro_rating_cluster), rent_m2_per_micro_rating) # regression of avg_rent_full on clustered (rounded) micro_ratings_new

ggplot(rent_m2_per_micro_rating) + geom_point(aes(x = micro_rating_cluster, y = avg_rent_m2)) +
  geom_hline(yintercept = mean(data_analyzed$rent_m2), col = "blue") + # average rent price in Switzerland
  geom_abline(intercept = regression_rent_m2_micro_rating_new$coefficients[1], slope = regression_rent_m2_micro_rating_new$coefficients[2], col = "red") +
  # geom_abline(intercept = regression_rent_m2_micro_rating_new_2$coefficients[1], slope = regression_rent_m2_micro_rating_new_2$coefficients[2], col = "magenta") +
  labs(title ="Average rent price per m2 depending on micro rating new (rounded)",
       x = "Micro Rating New (rounded)",
       y = "Average rent price per m2") +
  theme_bw(base_size = 20)



## 2.5 Average rent price depending on some additional variables --------------

  # Average price full depending on balcony

rent_balcony <-  data_analyzed %>% select (rent_full, balcony) %>%
  group_by(balcony) %>%
  summarize(avg_rent = mean(rent_full))

ggplot(rent_balcony) + geom_line(aes(x = balcony, y = avg_rent)) +
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue") + # average rent price in Switzerland
  labs(title ="Average rent price (full) depending on balcony",
       x = "Balcony",
       y = "Average rent price (full)") +
  theme_bw(base_size = 20)


  # Rent price full depending room number

avg_rent_per_room_size <- data_analyzed %>%
  select(rent_full,rooms)  %>%
  filter(!is.na(rooms)) %>%
  group_by(rooms) %>% 
  summarize(avg_rent_rooms = mean(rent_full,na.rm=TRUE)) %>% filter(rooms < 15)

avg_room_number <- data_analyzed$rooms %>% na.omit() %>% mean()


ggplot(data_analyzed) + geom_point(aes(x = rooms, y = rent_full)) +
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue")+ # average rent price in Switzerland
  labs(title ="Rent price (full) depending on number of rooms",
       x = "Number of rooms",
       y = " Rent price (full)") +
  theme_bw(base_size = 20)

  # Average rent price full depending room number

ggplot(avg_rent_per_room_size) + geom_point(aes(x = rooms, y = avg_rent_rooms)) +
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue")+ # average rent price in Switzerland
  geom_vline(xintercept = avg_room_number, col = "red") + # average number of rooms
  labs(title ="Average rent price (full) by room number",
       x = "Number of rooms",
       y = "Average rent price (full)") +
  theme_bw(base_size = 20)


## 2.6 Relationship between flat size and building year

lapply(data_analyzed, function(x) table(is.na(x)))

  # Flat size depending on building year (full time period)

size_per_yearbuilt_full <- data_analyzed %>%
  select(area, year_built) %>%
  na.omit()

ggplot(size_per_yearbuilt_full, aes(x = year_built, y = area)) +
  geom_point() +
  geom_hline(yintercept = mean(size_per_yearbuilt_full$area), col = "blue") +
  labs(title ="Flat size (area) depending on building year",
       x = "Year built",
       y = "Flat size (area)") +
  theme_bw(base_size = 20)

  # Flat size depending on building year (1900 to 2020)

size_per_yearbuilt_period <- data_analyzed %>%
  select(area, year_built) %>%
  na.omit() %>%
  filter(year_built >= "1900" & year_built <= "2020")

ggplot(size_per_yearbuilt_period, aes(x = year_built, y = area)) +
  geom_point() +
  geom_hline(yintercept = mean(size_per_yearbuilt_period$area), col = "blue") +
  labs(title ="Flat size (area) depending on building year from 1900 to 2020",
       x = "Year built",
       y = "Flat size (area)") +
  theme_bw(base_size = 20)


  # Average flat size per building year from 1900 to 2020


avg_size_per_yearbuilt <- data_analyzed %>%
  select(area, year_built) %>%
  na.omit() %>%
  group_by(year_built) %>% 
  summarize(avg_area_year = mean(area)) %>%
  filter(year_built >= "1900" & year_built <= "2020")

flats_1900_2020 <- data_analyzed %>%
  select(area, year_built) %>%
  na.omit() %>%
  filter(year_built >= "1900" & year_built <= "2020")

ggplot(avg_size_per_yearbuilt) + geom_line(aes(x = year_built, y = avg_area_year)) +
  geom_hline(yintercept = mean(flats_1900_2020$area), col = "blue") + # Average flat size for flats built between 1900 and 2020
  labs(title ="Average flat size depending on building year",
       x = "Year built",
       y = "Average flat size (area)") +
  theme_bw(base_size = 20)



# delete intermediate objects
rm(list = setdiff(ls(), c("D", "data_analyzed", "problems")))


### MODELLING-------------------------------------------------------------------


## Approaches -----------------------------------------------------------------*

# 1) Linear regression models with different features (cross-validated)
# 2) Random Forests (cross-validated)
# 3) Boosted Trees (cross-validated)

###############################################################################*
## Preliminary data cleaning --------------------------------------------------*
###############################################################################*

# Variables which can be used for modelling
modelling_vars <- c("rent_full", "area", "home_type", "furnished", "rooms", 
                    "Label", "balcony")

# Onehot-encode the Arbeitsmarktregionen and home_types
label_encoding <- dummyVars(~ Label, data = data_analyzed)
onehot_label <- as.data.frame(predict(label_encoding, data_analyzed))
hometype_encoding <- dummyVars(~ home_type, data = data_analyzed)
onehot_hometype <- as.data.frame(predict(hometype_encoding, data_analyzed))

# Data for modelling
Dmod <- data_analyzed %>%
  select(all_of(modelling_vars), 
         dplyr::starts_with("dist"),
         dplyr::starts_with("Micro")) %>%
  bind_cols(onehot_label) %>%
  bind_cols(onehot_hometype) %>%
  select(-c("Label", "home_type")) %>%
  drop_na() %>%
  clean_names() # makes titles clean 
# (capitals become lower-case, brackets deleted, etc.) 
# (e.g. Wil (SG) becomes wil_sg)

# Set up data frame where we can push all results of OLS models to
ols_results <- data.frame(matrix(ncol = 6, nrow = 5))
names(ols_results) <- c("model_nr", "model_type", "rmse_cv", "mae_cv", "mae", "mae/mean")




###############################################################################*
## OLS -------------------------------------------------------------------------
###############################################################################*

###############################################################################*
#
### OLS - Notes
# General approach: we model different OLS with different specified variables
# we check the errors (mse and mae) of models through cross-validation and a
# validation set approach
###############################################################################*
set.seed(123)



## Model 1: all variables ------------------------------------------------------

# Vars used: area, rooms, apartment types, furnished (yes/no), micro ratings,
# balcony (yes/no), distances to places of interest, arbeitsmarktregionen

# split into training/test
split <- initial_split(Dmod, prop = 0.8)             

# cross-validation split
data_ctrl <- trainControl(method = "cv", number = 10) 

# cross-validation
ols_1_cv <- train(rent_full ~ .,                    # model to fit
                  data = training(split),                        
                  trControl = data_ctrl,           # folds
                  method = "lm",                   # specifying regression model
                  na.action = na.pass)      
rmse_cv <- ols_1_cv$results[["RMSE"]]             
mae_cv <- ols_1_cv$results[["MAE"]]                 

# normal model
ols_1 <- lm(rent_full ~ ., data = training(split))


# error (validation set approach)
y_hat <- predict(ols_1, newdata = testing(split))
mae <- MAE(y_hat, testing(split)$rent_full)          


mae_mean <- MAE(y_hat, testing(split)$rent_full)/mean(testing(split)$rent_full)

ols_results[1,] <- c(1, "OLS", rmse_cv, mae_cv, mae, mae_mean)


## Model 2: Selected variables--------------------------------------------------

# Vars used: area, rooms, furnished (yes/no), balcony (yes/no), micro ratings,
# distances to places of interest

split <- initial_split(
  Dmod %>% 
    select(all_of(setdiff(modelling_vars, c("home_type", "Label"))), 
           micro_rating_new, 
           dplyr::starts_with("dist")
    ),
  prop = 0.8)

# cross-validation split
data_ctrl <- trainControl(method = "cv", number = 10) 

# cross-validation
ols_2_cv <- train(rent_full ~ .,                    # model to fit
                  data = training(split),                        
                  trControl = data_ctrl,           # folds
                  method = "lm",                   # specifying regression model
                  na.action = na.pass)      
rmse_cv <- ols_2_cv$results[["RMSE"]]               
mae_cv <- ols_2_cv$results[["MAE"]]                 

# normal model
ols_2 <- lm(rent_full ~ ., data = training(split))

# error (validation set approach)
y_hat <- predict(ols_2, newdata = testing(split))
mae <- MAE(y_hat, testing(split)$rent_full)      


mae_mean <- MAE(y_hat, testing(split)$rent_full)/mean(testing(split)$rent_full)

ols_results[2,] <- c(2, "OLS", rmse_cv, mae_cv, mae, mae_mean)



## Model 3: "very simple" ------------------------------------------------------

# Vars used: area, rooms, general micro rating, arbeitsmarktregionen

split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, micro_rating_new, starts_with("label"))
)

# cross-validation split
data_ctrl <- trainControl(method = "cv", number = 10) 

# cross-validation
ols_3_cv <- train(rent_full ~ .,                    # model to fit
                  data = training(split),                        
                  trControl = data_ctrl,           # folds
                  method = "lm",                   # specifying regression model
                  na.action = na.pass)      
rmse_cv <- ols_3_cv$results[["RMSE"]]               
mae_cv <- ols_3_cv$results[["MAE"]]                 

# normal model
ols_3 <- lm(rent_full ~ ., data = training(split))

# error (validation set approach)
y_hat <- predict(ols_3, newdata = testing(split))
mae <- MAE(y_hat, testing(split)$rent_full)         


mae_mean <- MAE(y_hat, testing(split)$rent_full)/mean(testing(split)$rent_full)

ols_results[3,] <- c(3, "OLS", rmse_cv, mae_cv, mae, mae_mean)


## Model 4: "squared variables" ------------------------------------------------

# Vars used: area, area squared, rooms, rooms squared, apartment types, 
# furnished (yes/no), micro ratings,
# balcony (yes/no), distances to places of interest, arbeitsmarktregionen

split <- initial_split(
  Dmod %>%
    mutate(area_sq = area^2) %>%
    mutate(rooms_sq = rooms^2)
)

# cross-validation split
data_ctrl <- trainControl(method = "cv", number = 10) 

# cross-validation
ols_4_cv <- train(rent_full ~ .,                    # model to fit
                  data = training(split),                        
                  trControl = data_ctrl,           # folds
                  method = "lm",                   # specifying regression model
                  na.action = na.pass)      
rmse_cv <- ols_4_cv$results[["RMSE"]]               
mae_cv <- ols_4_cv$results[["MAE"]]                 

# normal model
ols_4 <- lm(rent_full ~ ., data = training(split))

# error (validation set approach)
y_hat <- predict(ols_4, newdata = testing(split))
mae <- MAE(y_hat, testing(split)$rent_full)         


mae_mean <- MAE(y_hat, testing(split)$rent_full)/mean(testing(split)$rent_full) 

ols_results[4,] <- c(4, "OLS", rmse_cv, mae_cv, mae, mae_mean)



## Model 5: all variables including cantons, etc ------------------------------

# Vars used: area, rooms, apartment types, furnished (yes/no), micro ratings,
# balcony (yes/no), distances to places of interest, arbeitsmarktregionen,
# cantons, arbeitsmarktgrossregionen

split <- initial_split(
  data_analyzed %>%
    select(all_of(modelling_vars), 
           dplyr::starts_with("dist"),
           dplyr::starts_with("Micro"), 
           KTKZ, 
           Arbeitsmarktgrossregionen.2018) %>%
    drop_na()
)

# cross-validation split
data_ctrl <- trainControl(method = "cv", number = 10) 

# cross-validation
ols_5_cv <- train(rent_full ~ .,                    # model to fit
                  data = training(split),                        
                  trControl = data_ctrl,           # folds
                  method = "lm",                   # specifying regression model
                  na.action = na.pass)      
rmse_cv <- ols_5_cv$results[["RMSE"]]              
mae_cv <- ols_5_cv$results[["MAE"]]                 

# normal model
ols_5 <- lm(rent_full ~ ., data = training(split))

# error (validation set approach)
y_hat <- predict(ols_5, newdata = testing(split))
mae <- MAE(y_hat, testing(split)$rent_full)         


mae_mean <- MAE(y_hat, testing(split)$rent_full)/mean(testing(split)$rent_full) 

ols_results[5,] <- c(5, "OLS", rmse_cv, mae_cv, mae, mae_mean)





###############################################################################*
## Random Forests --------------------------------------------------------------
###############################################################################*
set.seed(123)
###############################################################################*
### RANDOM FOREST - NOTES
### We model some random forests in this section. We begin with a simple "vanilla"
### random forest with default values from randomForest. We then try some other
### random forest with a reduction in the amount of features. Once we have an overview
### on which model performs best (based on oob errors), we get into hyper-
### parameter tuning for that model
###############################################################################*

# statistics for model comparison
rf_results <- data.frame(matrix(ncol = 7, nrow = 5))
names(rf_results) <- c("model_nr", "model_type", "oob_rmse", "oob_mse", "mae", "mae/mean", "time")

## Model 1: All variables, vanilla attempt -------------------------------------

# Note: we first try a normal "vanilla" random forest with randomForest
# package. It should serve as a general benchmark for what follows

split <- initial_split(Dmod, prop = 0.8) 
Dmod_train <- analysis(split)
Dmod_test <- assessment(split)

# we use randomForest package as first result to plot oob errors vs. no. trees
# Note: randomForest parameters: 
# mtry = p/3 (40), sample = nrow(Dmod), ntree = 500, node size = 5

start <- Sys.time() # for measuring training time - get the start time
rf1 <- randomForest(
  formula = rent_full ~ .,
  data    = Dmod_train
)
fin <- Sys.time() # end time
time <- parse_number(as.character(difftime(fin, start, units = "mins")))

pdf("oob_errors.pdf")
plot(rf1)                             # plot oob errors vs. no. of trees:
dev.off()
# after some time mse doesn't improve
# by much

oob_mse <- rf1$mse[which.min(rf1$mse)]           
oob_rmse <- sqrt(oob_mse)

y_hat <- predict(rf1, Dmod_test) 
mae <- MAE(y_hat, Dmod_test$rent_full)       
mae_mean <- mae/mean(Dmod_test$rent_full)

rf_results[1,] <- c(1, "RF", oob_rmse, oob_mse, mae, mae_mean, time)


## Model 2: Selected Variables (same variables as ols model 2) -----------------

# Vars used: area, rooms, furnished (yes/no), balcony (yes/no), micro ratings,
# distances to places of interest
split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, furnished, balcony, 
           dplyr::starts_with("micro"), dplyr::starts_with("dist")),
  prop = 0.8
)
Dmod_train <- training(split)
Dmod_test <- testing(split)

start <- Sys.time() # for measuring training time - get the start time
rf2 <- ranger(
  formula       = rent_full ~ .,
  data          = Dmod_train,
  mtry          = 4,                # p/3 as in prev. model
  min.node.size = 5
)
fin <- Sys.time() # end time
time <- parse_number(as.character(difftime(fin, start, units = "mins")))

oob_mse <- rf2$prediction.error     # oob mse: 157119
oob_rmse <- sqrt(oob_mse)

mae <- MAE(predict(rf2, data = Dmod_test)$prediction, Dmod_test$rent_full)
# MAE: 271.66 (better than ols: 332.84)
mae_mean <- mae/mean(Dmod_test$rent_full)

rf_results[2,] <- c(2, "RF", oob_rmse, oob_mse, mae, mae_mean, time)



## Model 3: Selected Variables (same variables as ols model 3) -----------------

# Vars used: area, rooms, general micro rating, arbeitsmarktregionen
# Note: we use the ranger package for further models due to its faster
# implementation. Parameters remain the same as for the default vanilla 
# approach at the beginning
split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, micro_rating_new, dplyr::starts_with("label")),
  prop = 0.8
)
Dmod_train <- training(split)
Dmod_test <- testing(split)

start <- Sys.time() # for measuring training time - get the start time
rf3 <- ranger(
  formula       = rent_full ~ .,
  data          = Dmod_train,
  mtry          = 33,                # p/3 as in prev. model
  min.node.size = 5
)
fin <- Sys.time()
time <- parse_number(as.character(difftime(fin, start, units = "mins")))

oob_mse <- rf3$prediction.error
oob_rmse <- sqrt(oob_mse)

mae <- MAE(predict(rf3, data = Dmod_test)$prediction, Dmod_test$rent_full)

mae_mean <- mae/mean(Dmod_test$rent_full)

rf_results[3,] <- c(3, "RF", oob_rmse, oob_mse, mae, mae_mean, time)


###############################################################################*
## Hyper-parameter tuning - Random Forest---------------------------------------
###############################################################################*

###############################################################################*
### HYPER-PARAMETER TUNING - NOTES:
# We do a hyper-parameter tuning for a random forest with all the variables in
# modelling_vars, distance, and micro ratings
#
# Only run this section if you want to replicate the results!
###############################################################################*

#split <- initial_split(Dmod, prop = 0.8)
#Dmod_train <- analysis(split)

# we start with a hyper-parameter grid as introduced in the lecture
#hyper_grid <- expand.grid(
#  mtry       = seq(35, 45, by = 2),
#  node_size  = seq(3, 9, by = 2),
#  OOB_RMSE   = 0 # a place to dump results
#  sampe_size = c(.55, .632, .70, .80),
#)

#for(i in 1:nrow(hyper_grid)) {


#  model <- ranger(
#    formula         = rent_full ~ ., 
#    data            = Dmod_train, 
#    num.trees       = 500,
#    mtry            = hyper_grid$mtry[i],
#    write.forest    = FALSE,
#    min.node.size   = hyper_grid$node_size[i],
#    sample.fraction = hyper_grid$sampe_size[i],
#    oob.error       = TRUE,
#    verbose         = FALSE,
#    seed            = 123
#)

# add OOB error to grid
#hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)

# progress
#print(i)
#}


#hyper_grid %>% arrange(OOB_RMSE)

# Results (OOB_RMSE) don't wary by much (Goes from 320 to 328). 
# Nonetheless, here are the top 10 models


#      mtry  node_size  sampe_size    OOB_RMSE
#1    45         3        0.8         320.4696
#2    41         3        0.8         320.6417
#3    43         3        0.8         320.8151
#4    39         3        0.8         320.9273
#5    45         5        0.8         321.0673
#6    35         3        0.8         321.1452
#7    43         5        0.8         321.2615
#8    41         5        0.8         321.3123
#9    39         5        0.8         321.3756
#10   37         3        0.8         321.3880


## Model 4: Model obtained by hyper-parameter tuning ---------------------------

  # This is the model with the hyper-parameters which are obtained through
  # tuning.
split <- initial_split(Dmod, prop = 0.8)
Dmod_train <- training(split)
Dmod_test <- testing(split)

start <- Sys.time() # for measuring training time - get the start time
rf4 <- ranger(
  formula         = rent_full ~ .,
  data            = Dmod_train,
  mtry            = 45,
  min.node.size   = 3,
  sample.fraction = 0.8,
  seed            = 123
)
fin <- Sys.time()
time <- parse_number(as.character(difftime(fin, start, units = "mins")))

oob_mse <- rf4$prediction.error 
oob_rmse <- sqrt(oob_mse)

mae <- MAE(predict(rf4, data = Dmod_test)$prediction, Dmod_test$rent_full)
mae_mean <- mae/mean(Dmod_test$rent_full)                                             

  # MAE is higher than initially obtained random forest. This probably comes 
  # from having a smaller sample size of 0.8 instead of the full sample size
  # as the default randomForest package provides

rf_results[4,] <- c(4, "RF", oob_rmse, oob_mse, mae, mae_mean, time)


## Model 5: Model obtained by hyper-parameter tuning with sample size 1 --------
  # As we ascertained, we also try the predictive performance of the model
  # with hyper-parameter tuning and with a sample size of 1
split <- initial_split(Dmod, prop = 0.8)
Dmod_train <- training(split)
Dmod_test <- testing(split)

start <- Sys.time() # for measuring training time - get the start time
rf5 <- ranger(
  formula         = rent_full ~ .,
  data            = Dmod_train,
  mtry            = 45,
  min.node.size   = 3,
  sample.fraction = 1,
  seed            = 123
)
fin <- Sys.time()
time <- parse_number(as.character(difftime(fin, start, units = "mins")))

oob_mse <- rf5$prediction.error
oob_rmse <- sqrt(oob_mse)

mae <- MAE(predict(rf5, data = Dmod_test)$prediction, Dmod_test$rent_full)
mae_mean <- mae/mean(Dmod_test$rent_full)


rf_results[5,] <- c(5, "RF", oob_rmse, oob_mse, mae, mae_mean, time)


# some data frame operations for a nice table in the report
rf_results[,6:10] = apply(rf_results[,6:10], 2, function(x) as.numeric(x))
rf_results$mae_of_mean <- rf_results$mae_of_mean*100
rf_results <- data.frame(lapply(rf_results, function(y) if(is.numeric(y)) round(y, 2) else y)) 
rf_results$mae_of_mean <- rf_ressults$mae_of_mean/100

ols_results[,3:6] = apply(ols_results[,3:6], 2, function(x) as.numeric(x))
ols_results$mae_of_mean <- ols_results$mae_of_mean*100
ols_results <- data.frame(lapply(ols_results, function(y) if(is.numeric(y)) round(y, 2) else y)) 
ols_results$mae_of_mean <- ols_results$mae_of_mean/100

# save both results for now
write_csv(rf_results, "rf_results.csv")
write_csv(ols_results, "ols_results.csv")


###############################################################################*
## Boosted Trees --------------------------------------------------------------
###############################################################################*

###############################################################################*
### BOOSTING - NOTES
# 
# Data Dmod used for modelling
# We split the data into test and training set with a proportion of 0.8. 
# We set the parameter as variables that can be changed outside of the training
# function so that it is easier to save different training in our table and compare them. 

###############################################################################*





## Model 0: Manual parameter tuning -------------------------------------------

# create data frame where we can store the result for each tests of our manual tuning
hyperparameter_results <- data.frame(matrix(ncol = 10, nrow = 20))
names(hyperparameter_results) <- c( "model",  "n.trees", "interaction.depth", "shrinkage",
                              "cv.folds", "min_training_time", "rmse_training", 
                              "rmse_prediction", "mae_prediction", "mae_pred_mean")



## Training for finding of the best hyperparameters  --------------------------*
# Create training (80%) and test (20%) 
# Use set.seed for reproducibility (already done it the upper part)

Dmod_split <- initial_split(Dmod, prop = .8)
Dmod_train <- training(Dmod_split)
Dmod_test  <- testing(Dmod_split)

# Setting the parameters
# we changed here the hyperparameters manually for each test. 
# In the report we mentioned 12 tests, however we did not write 12 times the same
# code but simply changed the variables down here and stored the results. 

# Change here the variables for the training
model_type <- "gbm"
n.trees <- 600
shrinkage <- 0.1 #similar as learning rate
interaction.depth <- 6 #maximum depth of variable interactions
cv.folds <-  5 # cross validation

# for measuring training time - get the start time
start_time <- Sys.time() 

# fitting of the model
gbm.fit <- gbm(
  formula = rent_full ~ .,
  distribution = "gaussian",
  data = Dmod_train,
  n.trees = n.trees,
  interaction.depth = interaction.depth, 
  shrinkage = shrinkage, 
  cv.folds = cv.folds,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# get the end time
end_time <- Sys.time() 
end_time - start_time # calculate the length of the training - appears in the console

training_time <- difftime(end_time, start_time, units = "mins") #save the training time


# print results
print(gbm.fit)
# The best cross-validation iteration was 3149. 
# There were 119 predictors of which 114 had non-zero influence.

# get MSE and compute RMSE
rmse_training <- sqrt(min(gbm.fit$cv.error))


## Predicting -----------------------------------------------------------------*
# predict values for test data
pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, Dmod_test)


## Assessing Performance of the model -----------------------------------------*
# RMSE
rmse_prediction <- caret::RMSE(pred, Dmod_test$rent_full) 

# mae_prediction
mae_prediction <- caret::MAE(pred, Dmod_test$rent_full) 


# MAE/MEAN
mae_pred_mean <- caret::MAE(pred, Dmod_test$rent_full)/mean(Dmod_test$rent_full)



##Save the results after each iteration to table  -----------------------------*
## Always change the number of the row when saving the results of a new set of parameters 
# and the training. At the moment it is comment out because we set the best parameters already
# at the beginning. 

# hyperparameter_results[12, ] <- c(model_type, n.trees, interaction.depth, shrinkage, 
                          # cv.folds, training_time, rmse_training, rmse_prediction,
                          # mae_prediction, mae_pred_mean)


# Omit all the rows that are not filled 
# hyperparameter_results <- na.omit(hyperparameter_results)

# Save the data frame 
# write.csv(hyperparameter_results,"data/hyperparameter_results.csv", row.names = FALSE)


## Plots  ---------------------------------------------------------------------*
# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")

# plot for showing the importance of each variable
vip::vip(gbm.fit)






## Model 1: All variables, vanilla attempt -------------------------------------
# Set up data frame where we can push all results of all models to
boosting_result <- data.frame(matrix(ncol = 10, nrow = 5))
names(boosting_result) <- c( "model",  "n.trees", "interaction.depth", "shrinkage",
                              "cv.folds", "min_training_time", "rmse_training", 
                              "rmse_prediction", "mae_prediction", "mae_pred_mean")

# Training --------------------------------------------------------------------*

Dmod_split <- initial_split(Dmod, prop = .8)
Dmod_train <- training(Dmod_split)
Dmod_test  <- testing(Dmod_split)

# Setting the parameters
# Change here the variable for the training
model_type <- "gbm - model 1"
n.trees <- 600
shrinkage <- 0.1 #similar as learning rate
interaction.depth <- 6 #maximum depth of variable interactions
cv.folds <-  5 # cross validation

# for measuring training time - get the start time
start_time <- Sys.time() 

# fitting of the model
gbm.fit <- gbm(
  formula = rent_full ~ .,
  distribution = "gaussian",
  data = Dmod_train,
  n.trees = n.trees,
  interaction.depth = interaction.depth, 
  shrinkage = shrinkage, 
  cv.folds = cv.folds,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# get the end time
end_time <- Sys.time() 
end_time - start_time # calculate the length of the training - appears in the console

training_time <- difftime(end_time, start_time, units = "mins") #save the training time


# print results
print(gbm.fit)
# The best cross-validation iteration was 600. 
# There were 119 predictors of which 97 had non-zero influence.

# get MSE and compute RMSE
rmse_training <- sqrt(min(gbm.fit$cv.error)) # rmse_training: 321.408884619267


## Predicting -----------------------------------------------------------------*
# predict values for test data
pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, Dmod_test)



## Assessing Performance of the model -----------------------------------------*
# RMSE
rmse_prediction <- caret::RMSE(pred, Dmod_test$rent_full) 

# mae_prediction
mae_prediction <- caret::MAE(pred, Dmod_test$rent_full) 


# MAE/MEAN
mae_pred_mean <- caret::MAE(pred, Dmod_test$rent_full)/mean(Dmod_test$rent_full)



##Save the results to table  --------------------------------------------------*
boosting_result[1, ] <- c(model_type, n.trees, interaction.depth, shrinkage, 
                          cv.folds, training_time, rmse_training, rmse_prediction,
                          mae_prediction, mae_pred_mean)



## Model 2: Selected Variables (same variables as ols model 2) -----------------

# Vars used: area, rooms, furnished (yes/no), balcony (yes/no), micro ratings,
# distances to places of interest
split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, furnished, balcony, 
           dplyr::starts_with("micro"), dplyr::starts_with("dist")),
  prop = 0.8
)
Dmod_train <- training(split)
Dmod_test <- testing(split)


# Setting the parameters
# Change here the variable for the training
model_type <- "gbm - model 2"
n.trees <- 600
shrinkage <- 0.1 #similar as learning rate
interaction.depth <- 6 #maximum depth of variable interactions
cv.folds <-  5 # cross validation

# for measuring training time - get the start time
start_time <- Sys.time() 

# fitting of the model
gbm.fit <- gbm(
  formula = rent_full ~ .,
  distribution = "gaussian",
  data = Dmod_train,
  n.trees = n.trees,
  interaction.depth = interaction.depth, 
  shrinkage = shrinkage, 
  cv.folds = cv.folds,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# get the end time
end_time <- Sys.time() 
end_time - start_time # calculate the length of the training - appears in the console

training_time <- difftime(end_time, start_time, units = "mins") #save the training time


# print results
print(gbm.fit)
# 

# get MSE and compute RMSE
rmse_training <- sqrt(min(gbm.fit$cv.error))


## Predicting -----------------------------------------------------------------*
# predict values for test data
pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, Dmod_test)



## Assessing Performance of the model -----------------------------------------*
# RMSE
rmse_prediction <- caret::RMSE(pred, Dmod_test$rent_full) 

# mae_prediction
mae_prediction <- caret::MAE(pred, Dmod_test$rent_full) 


# MAE/MEAN
mae_pred_mean <- caret::MAE(pred, Dmod_test$rent_full)/mean(Dmod_test$rent_full)



##Save the results to table  --------------------------------------------------*
boosting_result[2, ] <- c(model_type, n.trees, interaction.depth, shrinkage, 
                          cv.folds, training_time, rmse_training, rmse_prediction,
                          mae_prediction, mae_pred_mean)



## Model 3: Selected Variables (same variables as ols model 3) -----------------

# Vars used: area, rooms, general micro rating, arbeitsmarktregionen
# Note: we use the ranger package for further models due to its faster
# implementation. Parameters remain the same as for the default vanilla 
# approach at the beginning
split <- initial_split(
  Dmod %>%
    select(rent_full, area, rooms, micro_rating_new, dplyr::starts_with("label")),
  prop = 0.8
)
Dmod_train <- training(split)
Dmod_test <- testing(split)

## Training  ------------------------------------------------------------------*
# Setting the parameters
# Change here the variable for the training
model_type <- "gbm - model 3"
n.trees <- 600
shrinkage <- 0.1 #similar as learning rate
interaction.depth <- 6 #maximum depth of variable interactions
cv.folds <-  5 # cross validation

# for measuring training time - get the start time
start_time <- Sys.time() 

# fitting of the model
gbm.fit <- gbm(
  formula = rent_full ~ .,
  distribution = "gaussian",
  data = Dmod_train,
  n.trees = n.trees,
  interaction.depth = interaction.depth, 
  shrinkage = shrinkage, 
  cv.folds = cv.folds,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# get the end time
end_time <- Sys.time() 
end_time - start_time # calculate the length of the training - appears in the console
# Time difference of 5.920291 mins

training_time <- difftime(end_time, start_time, units = "mins") #save the training time


# print results
print(gbm.fit)
# A gradient boosted model with gaussian loss function. 600 iterations were performed.
# The best cross-validation iteration was 591.
# There were 101 predictors of which 93 had non-zero influence.

# get MSE and compute RMSE
rmse_training <- sqrt(min(gbm.fit$cv.error))
# rmse_training: 338.465

## Predicting -----------------------------------------------------------------*
# predict values for test data
pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, Dmod_test)



## Assessing Performance of the model -----------------------------------------*
# RMSE
rmse_prediction <- caret::RMSE(pred, Dmod_test$rent_full) 
# rmse_prediction: 341.0365

# mae_prediction
mae_prediction <- caret::MAE(pred, Dmod_test$rent_full) 
# mae_prediction: 232.5631

# MAE/MEAN
mae_pred_mean <- caret::MAE(pred, Dmod_test$rent_full)/mean(Dmod_test$rent_full)
# mae_pred_mean: 0.1320951


##Save the results to table  --------------------------------------------------*
boosting_result[3, ] <- c(model_type, n.trees, interaction.depth, shrinkage, 
                          cv.folds, training_time, rmse_training, rmse_prediction,
                          mae_prediction, mae_pred_mean)



## Model 4: "squared variables" ------------------------------------------------

# Vars used: area, area squared, rooms, rooms squared, apartment types, 
# furnished (yes/no), micro ratings,
# balcony (yes/no), distances to places of interest, arbeitsmarktregionen

split <- initial_split(
  Dmod %>%
    mutate(area_sq = area^2) %>%
    mutate(rooms_sq = rooms^2)
)

Dmod_train <- training(split)
Dmod_test <- testing(split)

## Training  ------------------------------------------------------------------*
# Setting the parameters
# Change here the variable for the training
model_type <- "gbm - model 4"
n.trees <- 600
shrinkage <- 0.1 #similar as learning rate
interaction.depth <- 6 #maximum depth of variable interactions
cv.folds <-  5 # cross validation

# for measuring training time - get the start time
start_time <- Sys.time() 

# fitting of the model
gbm.fit <- gbm(
  formula = rent_full ~ .,
  distribution = "gaussian",
  data = Dmod_train,
  n.trees = n.trees,
  interaction.depth = interaction.depth, 
  shrinkage = shrinkage, 
  cv.folds = cv.folds,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# get the end time
end_time <- Sys.time() 
end_time - start_time # calculate the length of the training - appears in the console
# Time difference of 6.843948 mins

training_time <- difftime(end_time, start_time, units = "mins") #save the training time


# print results
print(gbm.fit)
# A gradient boosted model with gaussian loss function. 600 iterations were performed.
# The best cross-validation iteration was 600. There were 121 predictors of which 
# 97 had non-zero influence.

# get MSE and compute RMSE
rmse_training <- sqrt(min(gbm.fit$cv.error))
# rmse_training: 320.0584

## Predicting -----------------------------------------------------------------*
# predict values for test data
pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, Dmod_test)



## Assessing Performance of the model -----------------------------------------*
# RMSE
rmse_prediction <- caret::RMSE(pred, Dmod_test$rent_full) 
# rmse_prediction: 318.4705

# mae_prediction
mae_prediction <- caret::MAE(pred, Dmod_test$rent_full) 
# mae_prediction: 216.4146

# MAE/MEAN
mae_pred_mean <- caret::MAE(pred, Dmod_test$rent_full)/mean(Dmod_test$rent_full)
# mae_pred_mean: 0.1231721


##Save the results to table  --------------------------------------------------*
boosting_result[4, ] <- c(model_type, n.trees, interaction.depth, shrinkage, 
                          cv.folds, training_time, rmse_training, rmse_prediction,
                          mae_prediction, mae_pred_mean)



## Model 5: all variables including cantons, etc--------------------------------

# Vars used: area, rooms, apartment types, furnished (yes/no), micro ratings,
# balcony (yes/no), distances to places of interest, arbeitsmarktregionen,
# cantons, arbeitsmarktgrossregionen

split <- initial_split(
  data_analyzed %>%
    select(all_of(modelling_vars), 
           dplyr::starts_with("dist"),
           dplyr::starts_with("Micro"), 
           KTKZ, 
           Arbeitsmarktgrossregionen.2018) %>%
    drop_na()
)

## Training  ------------------------------------------------------------------*
# Setting the parameters
# Change here the variable for the training
model_type <- "gbm - model 5"
n.trees <- 600
shrinkage <- 0.1 #similar as learning rate
interaction.depth <- 6 #maximum depth of variable interactions
cv.folds <-  5 # cross validation

# for measuring training time - get the start time
start_time <- Sys.time() 

# fitting of the model
gbm.fit <- gbm(
  formula = rent_full ~ .,
  distribution = "gaussian",
  data = Dmod_train,
  n.trees = n.trees,
  interaction.depth = interaction.depth, 
  shrinkage = shrinkage, 
  cv.folds = cv.folds,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# get the end time
end_time <- Sys.time() 
end_time - start_time # calculate the length of the training - appears in the console
# Time difference of 6.898528 mins

training_time <- difftime(end_time, start_time, units = "mins") #save the training time


# print results
print(gbm.fit)
# A gradient boosted model with gaussian loss function.600 iterations were performed.
# The best cross-validation iteration was 600. There were 121 predictors of which 
# 99 had non-zero influence

# get MSE and compute RMSE
rmse_training <- sqrt(min(gbm.fit$cv.error))
# rmse_training: 320.319

## Predicting -----------------------------------------------------------------*
# predict values for test data
pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, Dmod_test)


## Assessing Performance of the model -----------------------------------------*
# RMSE
rmse_prediction <- caret::RMSE(pred, Dmod_test$rent_full) 
# rmse_prediction: 317.5295

# mae_prediction
mae_prediction <- caret::MAE(pred, Dmod_test$rent_full) 
# mae_prediction: 215.7215

# MAE/MEAN
mae_pred_mean <- caret::MAE(pred, Dmod_test$rent_full)/mean(Dmod_test$rent_full)
# mae_pred_mean: 0.1227776


##Save the results to table  --------------------------------------------------*
boosting_result[5, ] <- c(model_type, n.trees, interaction.depth, shrinkage, 
                          cv.folds, training_time, rmse_training, rmse_prediction,
                          mae_prediction, mae_pred_mean)



##Save the results after each model in csv  -----------------------------*

# Save the data frame 
# write.csv(boosting_result,"data/boosting_result.csv", row.names = FALSE)
