# Covid Visualization by Age Group
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
library(hrbrthemes)


# load in data
covid_data <- read.csv('covid_filtered.csv')
cases_by_age <- covid_data %>%  select(collection_week, previous_day_admission_adult_covid_confirmed_18.19_7_day_sum,
                                        previous_day_admission_adult_covid_confirmed_20.29_7_day_sum,
                                        previous_day_admission_adult_covid_confirmed_30.39_7_day_sum,
                                        previous_day_admission_adult_covid_confirmed_40.49_7_day_sum,
                                        previous_day_admission_adult_covid_confirmed_50.59_7_day_sum,
                                        previous_day_admission_adult_covid_confirmed_60.69_7_day_sum,
                                        previous_day_admission_adult_covid_confirmed_70.79_7_day_sum,
                                        previous_day_admission_adult_covid_confirmed_80._7_day_sum,
                                        previous_day_admission_pediatric_covid_confirmed_7_day_sum)


cases_by_age <- cases_by_age[order(as.Date(cases_by_age$collection_week, format="%m/%d/%Y")),]

# calculate total sums per date 
totals_by_age <- cases_by_age %>% group_by(collection_week) %>% summarize(tot_confirmed_18.19 = sum(previous_day_admission_adult_covid_confirmed_18.19_7_day_sum), 
                                                                          tot_confirmed_20.29 = sum(previous_day_admission_adult_covid_confirmed_20.29_7_day_sum), 
                                                                          tot_confirmed_30.39 = sum(previous_day_admission_adult_covid_confirmed_30.39_7_day_sum), 
                                                                          tot_confirmed_40.49 = sum(previous_day_admission_adult_covid_confirmed_40.49_7_day_sum),
                                                                          tot_confirmed_50.59 = sum(previous_day_admission_adult_covid_confirmed_50.59_7_day_sum),
                                                                          tot_confirmed_60.69 = sum(previous_day_admission_adult_covid_confirmed_60.69_7_day_sum),
                                                                          tot_confirmed_70.79 = sum(previous_day_admission_adult_covid_confirmed_70.79_7_day_sum),
                                                                          tot_confirmed_80 = sum(previous_day_admission_adult_covid_confirmed_80._7_day_sum), 
                                                                          tot_confirmed_peds = sum(previous_day_admission_pediatric_covid_confirmed_7_day_sum))

totals_by_age <- totals_by_age[order(as.Date(totals_by_age$collection_week, format="%m/%d/%Y")),]

totals_by_age$collection_week <- as.Date(totals_by_age$collection_week, format="%m/%d/%Y")

# rearrange dataframe in order to plot it as a grouped line graph
totals_18.19 <- totals_by_age %>% mutate(age = "18-19") %>% select(collection_week, age, tot_confirmed_18.19)
colnames(totals_18.19)[3] = "Cases"
totals_20.29 <- totals_by_age %>% mutate(age = "20-29") %>% select(collection_week, age, tot_confirmed_20.29)
colnames(totals_20.29)[3] = "Cases"
totals_30.39 <- totals_by_age %>% mutate(age = "30-39") %>% select(collection_week, age, tot_confirmed_30.39)
colnames(totals_30.39)[3] = "Cases"
totals_40.49 <- totals_by_age %>% mutate(age = "40-49") %>% select(collection_week, age, tot_confirmed_40.49)
colnames(totals_40.49)[3] = "Cases"
totals_50.59 <- totals_by_age %>% mutate(age = "50-59") %>% select(collection_week, age, tot_confirmed_50.59)
colnames(totals_50.59)[3] = "Cases"
totals_60.69 <- totals_by_age %>% mutate(age = "60-69") %>% select(collection_week, age, tot_confirmed_60.69)
colnames(totals_60.69)[3] = "Cases"
totals_70.79 <- totals_by_age %>% mutate(age = "70-79") %>% select(collection_week, age, tot_confirmed_70.79)
colnames(totals_70.79)[3] = "Cases"
totals_80 <- totals_by_age %>% mutate(age = "80+") %>% select(collection_week, age, tot_confirmed_80)
colnames(totals_80)[3] = "Cases"
totals_peds <- totals_by_age %>% mutate(age = "< 18") %>% select(collection_week, age, tot_confirmed_peds)
colnames(totals_peds)[3] = "Cases"
totals_rearranged <- bind_rows(totals_18.19, totals_20.29, totals_30.39, totals_40.49, totals_50.59, 
                               totals_60.69, totals_70.79, totals_80, totals_peds)
totals_by_age$collection_week <- as.Date(totals_by_age$collection_week, format="%m/%d/%Y")

# calculate maximums for each age group
max_peds <- totals_peds %>% filter(Cases == max(Cases)) # 2022-01-07, < 18,  2795
max_18.19 <- totals_18.19 %>% filter(Cases == max(Cases)) # 2022-01-07, 18-19, 470
max_20.29 <- totals_20.29 %>% filter(Cases == max(Cases)) # 2022-01-07, 20-29, 4194
max_30.39 <- totals_30.39 %>% filter(Cases == max(Cases)) # 2022-01-07, 30-39, 5450
max_40.49 <- totals_40.49 %>% filter(Cases == max(Cases)) # 2022-01-07, 40-49, 5069
max_50.59 <- totals_50.59 %>% filter(Cases == max(Cases)) # 2022-01-07, 50-59, 9110
max_60.69 <- totals_60.69 %>% filter(Cases == max(Cases)) # 2022-01-07, 60-69, 12884
max_70.79 <- totals_70.79 %>% filter(Cases == max(Cases)) # 2022-01-07, 70-79, 1279
max_80 <- totals_80 %>% filter(Cases == max(Cases)) # 2022-01-07, 80+, 11670

 
# plot the data as a spaghetti line plot, with subplots for each age group
 tmp <- totals_rearranged %>%
   mutate(age2=age)
 
 tmp %>%
   ggplot( aes(x=collection_week, y=Cases)) +
   geom_line( data=tmp %>% dplyr::select(-age), aes(group=age2), color="grey", size=0.5, alpha=0.5) +
   geom_line( aes(color=age), color="#69b3a2", size=1.2 )+
   #scale_color_viridis(discrete = TRUE) +
   theme_ipsum() +
   theme(
     legend.position="none",
     plot.title = element_text(size=14),
     panel.grid = element_blank(),
     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
   ) +
   ggtitle("Confirmed Covid-19 Cases by Age Group (2021-2023)") +
   facet_wrap(~age)




