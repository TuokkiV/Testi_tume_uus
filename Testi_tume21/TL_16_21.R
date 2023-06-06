rm(list=ls())
library(tidyverse)
library(tidylog)
library(haven)
library(readxl)
#library(writexl)
library(crayon)
library(stringr)
library(psych)

crayon = function(x) cat(green(x), sep = "\n")
options("tidylog.display" = list(crayon))

# Tidy 1st sheet ("Aloita tästä") ----------------------------------------------

## Load data
esko_se = read_xlsx("C:/Users/03248355/Work Folders/Data/eskodata_se.xlsx")

## Retain only unique hetu (2 obs per id). Spread.
esko_se = esko_se %>% pivot_wider(names_from = Quest, values_from = Ans)
names(esko_se) = c("hetu_hash", "tunnus", "tester", "group")

## Add indicator for changing test language
esko_se = esko_se %>% 
  mutate(lang_change = str_length(tunnus) == 8,
         tunnus = str_sub(tunnus, 1, 6),
         group = tolower(group))

## Retain only the test result done in the correct language
esko_se = esko_se %>% 
  group_by(tunnus) %>% 
  arrange(-lang_change) %>% # TRUE is first 
  filter(row_number()==1) %>% 
  ungroup

# Tidy 2nd sheet ("Fonologinen tietoisuus - Alkuäänne") ------------------------

## Load data
esko_fonol = read_xlsx("C:/Users/03248355/Work Folders/Data/eskodata_se.xlsx", 3)

## Create a variable to indicate whether each item was correctly answered
esko_fonol <- esko_fonol %>%
  group_by(IDHash, IDCode) %>%
  mutate(
    Q1_AnsC_fon = ifelse(PreOrd == 1 & AnsC == 1, 1, 0),
    Q2_AnsC_fon = ifelse(PreOrd == 2 & AnsC == 1, 1, 0),
    Q3_AnsC_fon = ifelse(PreOrd == 3 & AnsC == 1, 1, 0),
    Q4_AnsC_fon = ifelse(PreOrd == 4 & AnsC == 1, 1, 0),
    Q5_AnsC_fon = ifelse(PreOrd == 5 & AnsC == 1, 1, 0),
    Q6_AnsC_fon = ifelse(PreOrd == 6 & AnsC == 1, 1, 0),
    Q7_AnsC_fon = ifelse(PreOrd == 7 & AnsC == 1, 1, 0),
    Q8_AnsC_fon = ifelse(PreOrd == 8 & AnsC == 1, 1, 0),
    Q9_AnsC_fon = ifelse(PreOrd == 9 & AnsC == 1, 1, 0),
    Q10_AnsC_fon = ifelse(PreOrd == 10 & AnsC == 1, 1, 0)
  )

## Group the data by ID
esko_fonol <- esko_fonol %>%
  group_by(IDHash, IDCode) %>%
  summarise(Q1_AnsC_fon = max(Q1_AnsC_fon, na.rm = TRUE),
  Q2_AnsC_fon = max(Q2_AnsC_fon, na.rm = TRUE),
  Q3_AnsC_fon = max(Q3_AnsC_fon, na.rm = TRUE),
  Q4_AnsC_fon = max(Q4_AnsC_fon, na.rm = TRUE),
  Q5_AnsC_fon = max(Q5_AnsC_fon, na.rm = TRUE),
  Q6_AnsC_fon = max(Q6_AnsC_fon, na.rm = TRUE),
  Q7_AnsC_fon = max(Q7_AnsC_fon, na.rm = TRUE),
  Q8_AnsC_fon = max(Q8_AnsC_fon, na.rm = TRUE),
  Q9_AnsC_fon = max(Q9_AnsC_fon, na.rm = TRUE),
  Q10_AnsC_fon = max(Q10_AnsC_fon, na.rm = TRUE))

#Add a variable for the total correct answers by an individual
esko_fonol  <- esko_fonol %>%
  mutate(Sum_AnsC_fon = sum(
    Q1_AnsC_fon,
    Q2_AnsC_fon,
    Q3_AnsC_fon,
    Q4_AnsC_fon,
    Q5_AnsC_fon,
    Q6_AnsC_fon,
    Q7_AnsC_fon,
    Q8_AnsC_fon,
    Q9_AnsC_fon,
    Q10_AnsC_fon))
         

# Tidy 3rd sheet ("Kirjaintuntemus") -------------------------------------------

## Load data
esko_kirjain = read_xlsx("C:/Users/03248355/Work Folders/Data/eskodata_se.xlsx", 4)

## Process data
esko_kirjain = esko_kirjain %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(Ans = str_replace_all(Ans, "[, ]", ""), # Remove commas and blank spaces from answers
         AnsCount = nchar(Ans) - 2) # Sum of identified letters is the number of characters - brackets

## Create a variable to indicate whether each item was correctly answered
esko_kirjain <- esko_kirjain %>%
  group_by(IDHash, IDCode) %>%
  mutate(
    Q1_AnsC_kt = ifelse(PreOrd == 1 & AnsCount > 0, AnsCount, 0),
    Q2_AnsC_kt = ifelse(PreOrd == 2 & AnsCount > 0, AnsCount, 0),
    Q3_AnsC_kt = ifelse(PreOrd == 3 & AnsCount > 0, AnsCount, 0))

#Group the data by ID and add the total correct answers -variable
esko_kirjain <- esko_kirjain %>%
  group_by(IDHash, IDCode) %>%
  summarise(Sum_AnsC_kt = sum(Q1_AnsC_kt,Q2_AnsC_kt,Q3_AnsC_kt),
            Q1_AnsC_kt = max(Q1_AnsC_kt),
            Q2_AnsC_kt = max(Q2_AnsC_kt),
            Q3_AnsC_kt = max(Q3_AnsC_kt))


# Tidy 4th sheet ("Lukutaito - Sanalistan lukeminen") --------------------------

## Load data
esko_lukutaito = read_xlsx("C:/Users/03248355/Work Folders/Data/eskodata_se.xlsx", 5)

## Create individual variables for correctly/ falsely answered items, total correctly 
## answered items in addition to individual variables for attempted items and
## total attempted items
esko_lukutaito = esko_lukutaito %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  group_by(IDHash, IDCode) %>%
  mutate(
    Q1_AnsC_lt = ifelse(SRFPreOrd == 1 & SRFAnsC == 1, 1, 0),
    Q2_AnsC_lt = ifelse(SRFPreOrd == 2 & SRFAnsC == 1, 1, 0),
    Q3_AnsC_lt = ifelse(SRFPreOrd == 3 & SRFAnsC == 1, 1, 0),
    Q4_AnsC_lt = ifelse(SRFPreOrd == 4 & SRFAnsC == 1, 1, 0),
    Q5_AnsC_lt = ifelse(SRFPreOrd == 5 & SRFAnsC == 1, 1, 0),
    Q6_AnsC_lt = ifelse(SRFPreOrd == 6 & SRFAnsC == 1, 1, 0),
    Q7_AnsC_lt = ifelse(SRFPreOrd == 7 & SRFAnsC == 1, 1, 0),
    Q8_AnsC_lt = ifelse(SRFPreOrd == 8 & SRFAnsC == 1, 1, 0),
    Q9_AnsC_lt = ifelse(SRFPreOrd == 9 & SRFAnsC == 1, 1, 0),
    Q10_AnsC_lt = ifelse(SRFPreOrd == 10 & SRFAnsC == 1, 1, 0),
    Q1_att_lt = ifelse(SRFPreOrd == 1,1,0),
    Q2_att_lt = ifelse(SRFPreOrd == 2,1,0),
    Q3_att_lt = ifelse(SRFPreOrd == 3,1,0),
    Q4_att_lt = ifelse(SRFPreOrd == 4,1,0),
    Q5_att_lt = ifelse(SRFPreOrd == 5,1,0),
    Q6_att_lt = ifelse(SRFPreOrd == 6,1,0),
    Q7_att_lt = ifelse(SRFPreOrd == 7,1,0),
    Q8_att_lt = ifelse(SRFPreOrd == 8,1,0),
    Q9_att_lt = ifelse(SRFPreOrd == 9,1,0),
    Q10_att_lt = ifelse(SRFPreOrd == 10,1,0)) %>%
  summarise(
    Q1_AnsC_lt = max(Q1_AnsC_lt),
    Q2_AnsC_lt = max(Q2_AnsC_lt),
    Q3_AnsC_lt = max(Q3_AnsC_lt),
    Q4_AnsC_lt = max(Q4_AnsC_lt),
    Q5_AnsC_lt = max(Q5_AnsC_lt),
    Q6_AnsC_lt = max(Q6_AnsC_lt),
    Q7_AnsC_lt = max(Q7_AnsC_lt),
    Q8_AnsC_lt = max(Q8_AnsC_lt),
    Q9_AnsC_lt = max(Q9_AnsC_lt),
    Q10_AnsC_lt = max(Q10_AnsC_lt),
    Sum_AnsC_lt = sum(
      Q1_AnsC_lt,
      Q2_AnsC_lt,
      Q3_AnsC_lt,
      Q4_AnsC_lt,
      Q5_AnsC_lt,
      Q6_AnsC_lt,
      Q7_AnsC_lt,
      Q8_AnsC_lt,
      Q9_AnsC_lt,
      Q10_AnsC_lt),
    Q1_att_lt = max(Q1_att_lt),
    Q2_att_lt = max(Q2_att_lt),
    Q3_att_lt = max(Q3_att_lt),
    Q4_att_lt = max(Q4_att_lt),
    Q5_att_lt = max(Q5_att_lt),
    Q6_att_lt = max(Q6_att_lt),
    Q7_att_lt = max(Q7_att_lt),
    Q8_att_lt = max(Q8_att_lt),
    Q9_att_lt = max(Q9_att_lt),
    Q10_att_lt = max(Q10_att_lt),
    Sum_att_lt = sum(
      Q1_att_lt,
      Q2_att_lt,
      Q3_att_lt,
      Q4_att_lt,
      Q5_att_lt,
      Q6_att_lt,
      Q7_att_lt,
      Q8_att_lt,
      Q9_att_lt,
      Q10_att_lt))

# Combine verbal assessment into one table -------------------------------------

esko_se = esko_se %>%
  rename(IDHash = hetu_hash, IDCode = tunnus)

## Merge the four datasets
esko_se = right_join(esko_se, esko_kirjain, by = c("IDHash", "IDCode"))
esko_se = right_join(esko_se, esko_fonol, by = c("IDHash", "IDCode")) 
esko_se = right_join(esko_se, esko_lukutaito, by = c("IDHash", "IDCode")) 

## create a separate dataframe for the correlation matrix
esko_sums = esko_se[,c("Sum_AnsC_lt","Sum_AnsC_kt","Sum_AnsC_fon")]

# Compute the descriptive statistics -------------------------------------------

## Compute the correlation matrix between the three tests
cor(esko_sums, method = "pearson")

## Calculate quantiles for the three tests
quantiles_kt <- quantile(esko_se$Sum_AnsC_kt, c(0.05, 0.25, 0.5, 0.75, 0.95))
quantiles_lt <- quantile(esko_se$Sum_AnsC_kt, c(0.05, 0.25, 0.5, 0.75, 0.95))
quantiles_fon <- quantile(esko_se$Sum_AnsC_kt, c(0.05, 0.25, 0.5, 0.75, 0.95))

## Print the quantiles for the three tests
print(quantiles_kt)
print(quantiles_lt)
print(quantiles_fon)

## Print the mean and sd for the three tests
print(mean(esko_se$Sum_AnsC_kt))
print(mean(esko_se$Sum_AnsC_lt))
print(mean(esko_se$Sum_AnsC_fon))
print(sd(esko_se$Sum_AnsC_kt))
print(sd(esko_se$Sum_AnsC_lt))
print(sd(esko_se$Sum_AnsC_fon))


## Compute the average score for each item in letter recognition
variable_names <- paste0("Q", 1:3, "_AnsC_kt")

## Loop through each variable and calculate the mean
for (variable in variable_names) {
  variable_mean <- mean(esko_se[[variable]])
  print(variable_mean)
}

## Compute the average score for each item in letter recognition
variable_names <- paste0("Q", 1:10, "_AnsC_lt")

## Loop through each variable and calculate the mean
for (variable in variable_names) {
  variable_mean <- mean(esko_se[[variable]])
  print(variable_mean)
}

## Compute the average score for each item in letter recognition
variable_names <- paste0("Q", 1:10, "_AnsC_fon")

## Loop through each variable and calculate the mean
for (variable in variable_names) {
  variable_mean <- mean(esko_se[[variable]])
  print(variable_mean)
}

## Compute the Cronbach alpha's for letter recognition
esko_kt = esko_se[,c("Q1_AnsC_kt","Q2_AnsC_kt","Q3_AnsC_kt")]
alpha(esko_kt)

## Compute the Cronbach alpha's for letter recognition
esko_lt = esko_se[,c("Q1_AnsC_lt","Q2_AnsC_lt","Q3_AnsC_lt","Q4_AnsC_lt","Q5_AnsC_lt"
                     ,"Q6_AnsC_lt","Q7_AnsC_lt","Q8_AnsC_lt","Q9_AnsC_lt","Q10_AnsC_lt")]
alpha(esko_lt)

## Compute the Cronbach alpha's for letter recognition
esko_fon = esko_se[,c("Q1_AnsC_fon","Q2_AnsC_fon","Q3_AnsC_fon","Q4_AnsC_fon","Q5_AnsC_fon",
                      "Q6_AnsC_fon","Q7_AnsC_fon","Q8_AnsC_fon","Q9_AnsC_fon","Q10_AnsC_fon")]
alpha(esko_fon)