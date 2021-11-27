################################################################################
#
# DSF Group Project
# Script 4: Data Analysis
# 27.11.2021
#
################################################################################

rm(list = ls())

library(tidyverse) 
library(dplyr)
library(vtable) # for summary statistics


load("data/rent_listings_raw.RData")



###### Summary statistics ----


data_analyzed_for_summmary_statistics = data_analyzed %>% select(, c(2,3,5,7,13,20:36))

st(data_analyzed_for_summmary_statistics) # whole data set

st(data_analyzed_for_summmary_statistics, group = 'KTKZ', group.long = TRUE) # summary statistics per canton


  # Alternative summary statistics (does not give me a nice output)

library(tidyverse)
library(qwraps2)

test <- 
  list("rent_full" = 
         list("min" = ~ min(rent_full),
              "max" = ~ max(rent_full)))

test2 <- summary_table(dplyr::group_by(data_analyzed_for_summmary_statistics, KTKZ), test)


## Visualisations ----

# Rent price (full / per m2) depending on area

ggplot(data_analyzed, aes(x = area, y = rent_full)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +                             # add linear regressions
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue") + # add average rent price
  labs(title ="Rent price depending on flat size",
       x = "Flat size",
       y = "Rent price (full)")


ggplot(data_analyzed, aes(x = area, y = rent_full, col = KTKZ)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +                             # add linear regressions
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue") + # add average rent price
  labs(title ="Rent price depending on flat size",
       x = "Flat size",
       y = "Rent price (full)")


ggplot(data_analyzed, aes(x = area, y = rent_m2, col = KTKZ)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +                             # add linear regressions
  geom_hline(yintercept = mean(data_analyzed$rent_m2), col = "blue") + # add average rent price
  labs(title ="Rent m2 price depending on flat size",
       x = "Flat size",
       y = "Rent price (full)")


# Average rent (full / per m2) per canton
avg_rent_per_canton <- data_analyzed %>%
                                select(rent_full,KTKZ) %>%
                                group_by(KTKZ) %>% 
                                summarize(avg_cantonal_rent = mean(rent_full)) %>%
                                arrange(avg_cantonal_rent)


ggplot(avg_rent_per_canton) + geom_point(aes(x = reorder(KTKZ, -avg_cantonal_rent), y = avg_cantonal_rent)) +
                            geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue") +
                            labs(title ="Average rent per canton",
                             x = "Canton",
                             y = "Average Rent price (full)")
                          

avg_rent_m2_per_canton <- data_analyzed %>%
                            select(rent_m2,KTKZ) %>%
                            group_by(KTKZ) %>% 
                            summarize(avg_cantonal_rent_m2 = mean(rent_m2)) %>% 
                            arrange(avg_cantonal_rent_m2)

ggplot(avg_rent_m2_per_canton) + geom_point(aes(x = reorder(KTKZ, avg_cantonal_rent_m2), y = avg_cantonal_rent_m2)) +
  geom_hline(yintercept = mean(data_analyzed$rent_m2), col = "blue") +
  labs(title ="Average rent m2 per canton",
       x = "Canton",
       y = "Average Rent price per m2")


# Average rent per Arbeitsmarktgrossregion


avg_rent_per_Arbeitsmarktgrossregion <- data_analyzed %>%
                                            select(rent_full,Arbeitsmarktgrossregionen.2018) %>% na.omit() %>%
                                            group_by(Arbeitsmarktgrossregionen.2018) %>% 
                                            summarize(avg_Arbeitsmarktgrossregionen_rent = mean(rent_full))

ggplot(avg_rent_per_Arbeitsmarktgrossregion) + geom_point(aes(x = Arbeitsmarktgrossregionen.2018, y = avg_Arbeitsmarktgrossregionen_rent)) +
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue") +
  labs(title ="Average rent per Arbeitsmarktgrossregion",
       x = "Arbeitsmarktgrossregion",
       y = "Average Rent price (full)")



# Average rent per room size

avg_rent_per_room_size <- data_analyzed %>%
                              select(rent_full,rooms) %>%
                              group_by(rooms) %>% 
                              summarize(avg_rent_rooms = mean(rent_full)) %>% filter (rooms > "15")

avg_room_number <- data_analyzed$rooms %>% na.omit() %>% mean()




ggplot(data_analyzed) + geom_point(aes(x = rooms, y = rent_full)) +
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue")+ # average rent price in Switzerland
  labs(title ="Rent price depending on number of rooms",
       x = "NUmber of rooms",
       y = " Rent price (full)")


ggplot(avg_rent_per_room_size) + geom_point(aes(x = rooms, y = avg_rent_rooms)) +
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue")+ # average rent price in Switzerland
  geom_vline(xintercept = avg_room_number, col = "red") + # average number of rooms
      labs(title ="Average rent room number",
            x = "Number of rooms",
            y = "Average Rent price (full)")
  


# Average area per building year

lapply(data_analyzed, function(x) table(is.na(x)))

avg_size_per_yearbuilt <- data_analyzed %>%
                                select(area, year_built) %>%
                                na.omit() %>%
                                group_by(year_built) %>% 
                                summarize(avg_area_year = mean(area)) %>%
                                filter(year_built >= "1900" & year_built <= "2020")

ggplot(avg_size_per_yearbuilt) + geom_line(aes(x = year_built, y = avg_area_year)) +
  geom_hline(yintercept = mean(data_analyzed$area), col = "blue") +
  labs(title ="Average flat size depending on building year",
       x = "Year built",
       y = "Average flat size")


size_per_yearbuilt <- data_analyzed %>%
                        select(area, year_built) %>%
                        na.omit() %>%
                        filter(year_built >= "1900" & year_built <= "2020")


ggplot(size_per_yearbuilt, aes(x = year_built, y = area)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  geom_hline(yintercept = mean(size_per_yearbuilt$area), col = "blue") +
  labs(title ="Flat size depending on building year",
       x = "Year built",
       y = "Flat size")



# Flats depending cantons


flats_canton <- data_analyzed %>% select(KTKZ) %>% mutate(flat = 1)
ggplot(data = flats_canton, aes(x=KTKZ, y = flat)) + geom_bar(stat = "identity", color ="blue") + labs(title = "Flat offerings depending on canton",
                                                                                                     x = "Canton",
                                                                                                     y = "Number of flat offerings")
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

ggplot(data = flats_per_size, aes(x=flat_category, y = number_of_flats)) + geom_bar(stat = "identity", color ="blue", fill = "orange") + labs(title = "Flat offerings in 2019 depending on flat size",
                                                                                                                                x = "Flat category",
                                                                                                                                y = "Number of flat offerings")


# Rent price depending on balcony

rent_balcony <-  data_analyzed %>% select (rent_full, balcony) %>%
                  group_by(balcony) %>%
                  summarize(avg_rent = mean(rent_full))

ggplot(rent_balcony) + geom_line(aes(x = balcony, y = avg_rent)) +
  geom_hline(yintercept = mean(data_analyzed$rent_full), col = "blue") + # average rent price in Switzerland
  labs(title ="Average rent depending on balcony",
       x = "Balcony",
       y = "Average Rent price (full)")


# Rent price depending on micro rating new

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
       y = "Average Rent price (full)")


# Flat size depending on number of rooms

ggplot(data_analyzed, aes(x = rooms, y = area)) +
  geom_point() +
  geom_hline(yintercept = mean(data_analyzed$area), col = "blue") + # average area in Switzerland
  geom_smooth(method = "lm", col = "red") +
  labs(title ="Flat size (area) depending on number of rooms",
       x = "Number of rooms",
       y = "Flat size (area)")


