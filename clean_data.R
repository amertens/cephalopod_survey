
rm(list=ls())
library(tidyverse)
library(here)
library(haven)

d <- read.csv(here("data", "Octopus Farming - FINAL DATA - 030725.csv"))


#explore data
table(d$CONSENT)
table(is.na(d$CONSENT))


#-------------------------------------------------------------------------------
# check messaging condition
#-------------------------------------------------------------------------------

d$hCell <- factor(d$hCell, levels = c(1:5), labels = c("Control", "Sentience + Wellfare", "Environmental", "Economics", "Public health"))
d <- d %>% rename(message = hCell)
table(d$message)


#-------------------------------------------------------------------------------
# clean variables
#-------------------------------------------------------------------------------

#1: Slovenia|2: France|3: Germany|4: Portugal|5: Italy|6: Poland|7: Spain|8: Sweden|9: Netherlands|10: Denmark|11: UK|12: Greece|13: Czechia|14: Belgium|99: None of the above
d$COUNTRY <- factor(d$COUNTRY, levels = c(1:14, 99), labels = c("Slovenia", "France", "Germany", "Portugal", "Italy", "Poland", "Spain", "Sweden", "Netherlands", "Denmark", "UK", "Greece", "Czechia", "Belgium", "None of the above"))
table(d$COUNTRY)

#1: City - A large settlement with a population of at least 50,000|2: Town - A mid-sized settlement with a population of somewhere between 1,000 and 50,000 people|3: Village - A small settlement with a population of somewhere between 100 and 1,000 people|4: Hamlet or Isolated Dwelling - A very small settlement with a population of less than 100 people|96: Prefer not to say
d$URBANRURAL <- factor(d$URBANRURAL, levels = c(1:4, 96), labels = c("City", "Town", "Village", "Hamlet or Isolated Dwelling", "Prefer not to say"))
table(d$URBANRURAL )

#Gender
#1: Male|2: Female|4: Non Binary|97: Prefer not to say|3: I identify in another way
d$Gender <- factor(d$Gender, levels = c(1,2,3,4, 97), labels = c("Male","Female","I identify in another way","Non Binary","Prefer not to say"))
table(d$Gender)

#Income
#1: Less than 15,000 EUR|2: 15,000 to 19,999 EUR|3: 20,000 to 29,999 EUR|4: 30,000 to 39,999 EUR|5: 40,000 to 49,999 EUR|6: 50,000 to 59,999 EUR|7: 60,000 to 69,999 EUR|
#8: 70,000 to 79,999 EUR|9: 80,000 to 89,999 EUR|10: 90,000 to 99,999 EUR|11: 100,000 to 149,999 EUR|12: 150,000 to 199,999 EUR|
#13: 200,000 to 249,999 EUR|14: 250,000 EUR and more|15: Less than 3 000 PLN (less than 251 PLN per month)|16: 3 001 - 6 000 PLN (251 - 500 PLN per month)|
#17: 6 001 - 12 000 PLN (501 - 1 000 PLN per month)|18: 12 001 - 18 000 PLN (1 001 - 1 500 PLN per month)|19: 18 001 - 24 000 PLN (1 501 - 2 000 PLN per month)|
#20: 24 001 - 30 000 PLN (2 001 - 2 500 PLN per month)|21: 30 001 - 36 000 PLN (2 501 - 3 000 PLN per month)|22: 36 001 - 48 000 PLN (3 001 - 4 000 PLN per month)|
#23: 48 001 - 60 000 PLN (4 001 - 5 000 PLN per month)|24: 60 001 - 72 000 PLN (5 001 - 6 000 PLN per month)|25: 72 001 - 84 000 PLN (6 001 - 7 000 PLN per month)|
#26: 84 001 - 96 000 PLN (7 001 - 8 000 PLN per month)|27: 96 001 - 108 000 PLN (8 001 - 9 000 PLN per month)|28: More than 108 000 PLN (more than 9 000 PLN per month)|
#29: Less than £20,000|30: £20,000 - £29,999|31: £30,000 - £39,999|32: £40,000 - £49,999|33: £50,000 - £59,999|34: £60,000 - £69,999|35: £70,000 - £79,999|36: £80,000 or more|
#37: Below 100,000 kr‎|38: 100,000 - 249,999 kr‎|39: 250,000-499,999 kr‎|40: 500,000 - 999,9999 kr‎|41: More than 1,000,000 kr|42: Below 100 000kr per year|43: 100 000kr-149 999kr|
#44: 200 000kr -249 999kr|45: 250 000kr -299 999kr|46: 300 000kr -399 999kr|47: 400 000kr -499 999kr|48: 500 000kr -599 999kr|49: 600 000kr or more per year|50: Less than 30,000 Kč|
#51: 30,000 Kč - 39,999 Kč|52: 40,000 Kč - 49,999 Kč|53: 50,000 Kč - 59,999 Kč|54: 60,000 Kč - 69,999 Kč|55: 70,000 Kč - 79,999 Kč|56: 80,000 Kč - 89,999 Kč|57: More than 90,000 Kč|
#97: Prefer not to say

d$INCOME <- factor(d$INCOME, levels = c(1:57, 97), 
                   labels = c("Less than 15,000 EUR", "15,000 to 19,999 EUR", "20,000 to 29,999 EUR", "30,000 to 39,999 EUR", 
                              "40,000 to 49,999 EUR", "50,000 to 59,999 EUR", "60,000 to 69,999 EUR", "70,000 to 79,999 EUR", 
                              "80,000 to 89,999 EUR", "90,000 to 99,999 EUR", "100,000 to 149,999 EUR", "150,000 to 199,999 EUR", 
                              "200,000 to 249,999 EUR", "250,000 EUR and more", "Less than 3 000 PLN (less than 251 PLN per month)", 
                              "3 001 - 6 000 PLN (251 - 500 PLN per month)", "6 001 - 12 000 PLN (501 - 1 000 PLN per month)", 
                              "12 001 - 18 000 PLN (1 001 - 1 500 PLN per month)", "18 001 - 24 000 PLN (1 501 - 2 000 PLN per month)", 
                              "24 001 - 30 000 PLN (2 001 - 2 500 PLN per month)", "30 001 - 36 000 PLN (2 501 - 3 000 PLN per month)", 
                              "36 001 - 48 000 PLN (3 001 - 4 000 PLN per month)", "48 001 - 60 000 PLN (4 001 -5.000 PLN per month)", 
                              "60.001-72.000 PLN (5.001-6.000 PLN per month)", "72.001-84.000 PLN (6.001-7.000 PLN per month)", 
                              "84.001-96.000 PLN (7.001-8.000 PLN per month)", "96.001-108.000 PLN (8.001-9.000 PLN per month)", 
                              "More than108.000PLN (more than9.000PLN per month)", "Less than £20,000", "£20,000 - £29,999",
                              "£30,000 - £39,999", "£40,000 - £49,999", "£50,000 - £59,999", "£60,000 - £69,999", 
                              "£70,000 - £79,999", "£80,000 or more", "Below 100,000 kr‎", "100,000 - 249,999 kr‎",
                              "250,000-499,999 kr‎", "500,000 - 999,9999 kr‎", "More than 1,000,000 kr",
                              "Below 100 000kr per year", "100 000kr-149 999kr", "200 000kr -249 999kr",
                              "250 000kr -299 999kr", "300 000kr -399 999kr", "400 000kr -499 999kr",
                              "500 000kr -599 999kr", "600 000kr or more per year", "Less than 30,000 Kč",
                              "30,000 Kč - 39,999 Kč", "40,000 Kč - 49,999 Kč", "50,000 Kč - 59,999 Kč",
                              "60,000 Kč - 69,999 Kč", "70,000 Kč - 79,999 Kč", "80,000 Kč - 89,999 Kč",
                              "More than 90,000 Kč", "Prefer not to say"))

d <- d %>%
  mutate(
    income_cat = case_when(
      INCOME == "Prefer not to say" ~ "No response",
      
      # EUR countries - Low income
      COUNTRY %in% c("Slovenia", "France", "Germany", "Portugal", "Italy", "Spain", "Netherlands", "Greece", "Belgium") &
        INCOME %in% c("Less than 15,000 EUR", "15,000 to 19,999 EUR", "20,000 to 29,999 EUR") ~ "Low",
      
      # EUR countries - Medium income
      COUNTRY %in% c("Slovenia", "France", "Germany", "Portugal", "Italy", "Spain", "Netherlands", "Greece", "Belgium") &
        INCOME %in% c("30,000 to 39,999 EUR", "40,000 to 49,999 EUR", "50,000 to 59,999 EUR") ~ "Medium",
      
      # EUR countries - High income
      COUNTRY %in% c("Slovenia", "France", "Germany", "Portugal", "Italy", "Spain", "Netherlands", "Greece", "Belgium") &
        INCOME %in% c("60,000 to 69,999 EUR", "70,000 to 79,999 EUR", "80,000 to 89,999 EUR", 
                      "90,000 to 99,999 EUR", "100,000 to 149,999 EUR", "150,000 to 199,999 EUR", 
                      "200,000 to 249,999 EUR", "250,000 EUR and more") ~ "High",
      
      # Poland - Low income (up to ~14k EUR equivalent)
      COUNTRY == "Poland" & 
        INCOME %in% c("Less than 3 000 PLN (less than 251 PLN per month)", 
                      "3 001 - 6 000 PLN (251 - 500 PLN per month)", 
                      "6 001 - 12 000 PLN (501 - 1 000 PLN per month)", 
                      "12 001 - 18 000 PLN (1 001 - 1 500 PLN per month)", 
                      "18 001 - 24 000 PLN (1 501 - 2 000 PLN per month)", 
                      "24 001 - 30 000 PLN (2 001 - 2 500 PLN per month)", 
                      "30 001 - 36 000 PLN (2 501 - 3 000 PLN per month)", 
                      "36 001 - 48 000 PLN (3 001 - 4 000 PLN per month)", 
                      "48 001 - 60 000 PLN (4 001 -5.000 PLN per month)") ~ "Low",
      
      # Poland - Medium income
      COUNTRY == "Poland" & 
        INCOME %in% c("60.001-72.000 PLN (5.001-6.000 PLN per month)", 
                      "72.001-84.000 PLN (6.001-7.000 PLN per month)", 
                      "84.001-96.000 PLN (7.001-8.000 PLN per month)", 
                      "96.001-108.000 PLN (8.001-9.000 PLN per month)") ~ "Medium",
      
      # Poland - High income
      COUNTRY == "Poland" & 
        INCOME == "More than108.000PLN (more than9.000PLN per month)" ~ "High",
      
      # UK - Low income
      COUNTRY == "UK" & 
        INCOME %in% c("Less than £20,000", "£20,000 - £29,999") ~ "Low",
      
      # UK - Medium income
      COUNTRY == "UK" & 
        INCOME %in% c("£30,000 - £39,999", "£40,000 - £49,999", "£50,000 - £59,999") ~ "Medium",
      
      # UK - High income
      COUNTRY == "UK" & 
        INCOME %in% c("£60,000 - £69,999", "£70,000 - £79,999", "£80,000 or more") ~ "High",
      
      # Sweden - Low income
      COUNTRY == "Sweden" & 
        INCOME %in% c("Below 100,000 kr‎", "100,000 - 249,999 kr‎", "Below 100 000kr per year", 
                      "100 000kr-149 999kr", "200 000kr -249 999kr") ~ "Low",
      
      # Sweden - Medium income
      COUNTRY == "Sweden" & 
        INCOME %in% c("250,000-499,999 kr‎", "500,000 - 999,9999 kr‎", "250 000kr -299 999kr", 
                      "300 000kr -399 999kr", "400 000kr -499 999kr", "500 000kr -599 999kr") ~ "Medium",
      
      # Sweden - High income
      COUNTRY == "Sweden" & 
        INCOME %in% c("More than 1,000,000 kr", "600 000kr or more per year") ~ "High",
      
      # Denmark - Low income
      COUNTRY == "Denmark" & 
        INCOME %in% c("Below 100 000kr per year", "100 000kr-149 999kr", "200 000kr -249 999kr") ~ "Low",
      
      # Denmark - Medium income
      COUNTRY == "Denmark" & 
        INCOME %in% c("250 000kr -299 999kr", "300 000kr -399 999kr", "400 000kr -499 999kr") ~ "Medium",
      
      # Denmark - High income
      COUNTRY == "Denmark" & 
        INCOME %in% c("500 000kr -599 999kr", "600 000kr or more per year") ~ "High",
      
      # Czechia - Low income
      COUNTRY == "Czechia" & 
        INCOME %in% c("Less than 30,000 Kč", "30,000 Kč - 39,999 Kč", "40,000 Kč - 49,999 Kč", 
                      "50,000 Kč - 59,999 Kč", "60,000 Kč - 69,999 Kč", "70,000 Kč - 79,999 Kč") ~ "Low",
      
      # Czechia - Medium income
      COUNTRY == "Czechia" & 
        INCOME == "80,000 Kč - 89,999 Kč" ~ "Medium",
      
      # Czechia - High income
      COUNTRY == "Czechia" & 
        INCOME == "More than 90,000 Kč" ~ "High",
      
      TRUE ~ NA_character_
    ),
    income_cat = factor(income_cat, levels = c("Low", "Medium", "High","No response"))
  )

# Verify the recoding
table(d$income_cat, d$COUNTRY)

# Age	How old are you?
summary(d$Age)
#1: 18-24|2: 25-34|3: 35-44|4: 45-54|5: 55-64|6: 65+
d$dAGE <- factor(d$dAGE, levels = c(1:6), labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
table(d$dAGE)

#   Q1	Before today, were you aware of plans to open an octopus farm in Spain?
#1: Yes, I know a lot about this|2: Yes, I know a little about this|3: Yes, I heard about this|4: No, I haven't heard about this before|97: Don't know
d$Q1 <- factor(d$Q1, levels = c(1:4, 97), labels = c("Yes, I know a lot about this", "Yes, I know a little about this", "Yes, I heard about this", "No, I haven't heard about this before", "Don't know"))
table(d$Q1)

#   Q2	To what extent, if at all, do you support or oppose plans to intensively farm octopuses?
#1: Strongly support|2: Moderately support|3: Slightly support|4: Neutral / No opinion|5: Slightly oppose|6: Moderately oppose|7: Strongly oppose|97: Don't know
d$Q2 <- factor(d$Q2, levels = c(1:7, 97), labels = c("Strongly support", "Moderately support", "Slightly support", "Neutral / No opinion", "Slightly oppose", "Moderately oppose", "Strongly oppose", "Don't know"))
table(d$Q2)

#   Q3a	: You said that you support octopus farming - what are the main reasons you support octopus farming?

#   Q3b	: You said that you oppose octopus farming - what are the main reasons you oppose octopus farming?

#   Q4	The welfare of octopus in planned intensive octopus farming operation: How knowledgeable do you feel about the following aspects of octopus farming?
#   Q4	The environmental impacts of the planned intensive farming operations: How knowledgeable do you feel about the following aspects of octopus farming?
#   Q4	The economic impacts of the planned intensive octopus farming operation: How knowledgeable do you feel about the following aspects of octopus farming?
#   Q4	The ability for octopus to feel pain, distress and other emotions: How knowledgeable do you feel about the following aspects of octopus farming?
#   Q4	The public health impacts of the planning intensive farming operations: How knowledgeable do you feel about the following aspects of octopus farming?
# 1: Not at all|2: Slightly knowledgeable|3: Moderately knowledgeable|4: Very knowledgeable|5: Extremely knowledgeable
d$Q4_1 <- factor(d$Q4_1, levels = c(1:5), labels = c("Not at all", "Slightly knowledgeable", "Moderately knowledgeable", "Very knowledgeable", "Extremely knowledgeable"))
table(d$Q4_1)
d$Q4_2 <- factor(d$Q4_2, levels = c(1:5), labels = c("Not at all", "Slightly knowledgeable", "Moderately knowledgeable", "Very knowledgeable", "Extremely knowledgeable"))
table(d$Q4_2)
d$Q4_3 <- factor(d$Q4_3, levels = c(1:5), labels = c("Not at all", "Slightly knowledgeable", "Moderately knowledgeable", "Very knowledgeable", "Extremely knowledgeable"))
table(d$Q4_3)
d$Q4_4 <- factor(d$Q4_4, levels = c(1:5), labels = c("Not at all", "Slightly knowledgeable", "Moderately knowledgeable", "Very knowledgeable", "Extremely knowledgeable"))
table(d$Q4_4)
d$Q4_5 <- factor(d$Q4_5, levels = c(1:5), labels = c("Not at all", "Slightly knowledgeable", "Moderately knowledgeable", "Very knowledgeable", "Extremely knowledgeable"))
table(d$Q4_5)

#   Q5	How often do you consume octopus as part of your diet?
#1: Never|2: A few times a year|3: At least once a month|4: Every week|5: Multiple times a week
d$Q5 <- factor(d$Q5, levels = c(1:5), labels = c("Never", "A few times a year", "At least once a month", "Every week", "Multiple times a week"))
table(d$Q5)


#   Q6	What impact, if any, would the availability of octopus from an octopus farming operation have on your octopus consumption?
#1: Significantly decrease|2: Decrease|3: Slightly decrease|4: No impact|5: Slightly increase|6: Increase|7: Significantly increase
d$Q6 <- factor(d$Q6, levels = c(1:7), labels = c("Significantly decrease", "Decrease", "Slightly decrease", "No impact", "Slightly increase", "Increase", "Significantly increase"))
table(d$Q6)

# Q7	...a ban in your country on the intensive farming of octopus: To what extent would you support or oppose....
# Q7	...a ban in your country on the sale of octopus coming from intensive farming operations: To what extent would you support or oppose....
# Q7	...a ban in your country on the use of public funds (i.e. tax funded subsidies) to support intensive octopus farming: To what extent would you support or oppose....
#1: Strongly support|2: Moderately support|3: Slightly support|4: Neutral / No opinion|5: Slightly oppose|6: Moderately oppose|7: Strongly oppose
d$Q7_1 <- factor(d$Q7_1, levels = c(7:1), labels = rev(c("Strongly support", "Moderately support", "Slightly support", "Neutral / No opinion", "Slightly oppose", "Moderately oppose", "Strongly oppose")))
table(d$Q7_1)
d$Q7_2 <- factor(d$Q7_2, levels = c(7:1), labels = rev(c("Strongly support", "Moderately support", "Slightly support", "Neutral / No opinion", "Slightly oppose", "Moderately oppose", "Strongly oppose")))
table(d$Q7_2)
d$Q7_3 <- factor(d$Q7_3, levels = c(7:1), labels = rev(c("Strongly support", "Moderately support", "Slightly support", "Neutral / No opinion", "Slightly oppose", "Moderately oppose", "Strongly oppose")))
table(d$Q7_3)

# Q9	Imagine there was an election and there was a candidate who was advocating for a ban on intensive octopus farming. If all their other policy positions remained the same would you be more or less likely to vote for that candidate?
#1: Much more likely|2: A little more likely|3: No difference|4: A little less likely|5: Much less likely|97: Don't know
d$Q9 <- factor(d$Q9, levels = c(1:5, 97), labels = c("Much more likely", "A little more likely", "No difference", "A little less likely", "Much less likely", "Don't know"))
table(d$Q9)


d <- d %>% rename(
  Q1_awareness = Q1,
  Q2_support = Q2,
  Q4_welfare_knowledge = Q4_1,
  Q4_environmental_knowledge = Q4_2,
  Q4_economic_knowledge = Q4_3,
  Q4_sentience_knowledge = Q4_4,
  Q4_public_health_knowledge = Q4_5,
  Q5_consumption_frequency = Q5,
  Q6_impact_on_consumption = Q6,
  Q7_ban_farming_support = Q7_1,
  Q7_ban_sale_support = Q7_2,
  Q7_ban_subsidies_support = Q7_3,
  Q9_vote_candidate_support = Q9
)

#drop unneeded/open ended variables
d <- d %>% subset(., select= -c(CONSENT,ResponseID,ResponseID,REGION, Gender_3_Other, INCOME, Q3a_1, Q3b_1, Q8a_1, Q8b_1))
head(d)
colnames(d)


d <- droplevels(d)

#-------------------------------------------------------------------------------
# save data
#-------------------------------------------------------------------------------
saveRDS(d, here("data", "cleaned_octopus_farming_data.RDS"))
