rm(list=ls())
library(tidyverse)
library(tidylog)
library(haven)
library(readxl)
#library(writexl)
library(crayon)
library(stringr)

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

# Tidy 2nd sheet ("Sanavarasto") -----------------------------------------------

## Load data
esko_sanavarasto = read_xlsx("C:/Users/03248355/Work Folders/Data/eskodata_se.xlsx", 2)

## Process data
esko_sanavarasto = esko_sanavarasto %>%
  mutate()
  filter(!PreOrd %in% 1:2, # Drop practice questions
         IDCode != "admin@esko.fi") %>% # Drop test account
  group_by(IDHash, IDCode) %>%
  summarise(sum_sanavarasto = sum(AnsC, na.rm = TRUE), # Sum of correct answers
            time_sanavarasto = sum(Time, na.rm = TRUE) / 1000) # Total answer time in seconds

# Tidy 3rd sheet ("Fonologinen tietoisuus - Alkuäänne") ------------------------

## Load data
esko_fonol = read_xlsx("C:/Users/03248355/Work Folders/Data/eskodata_se.xlsx", 3)

## Process data
#esko_fonol = esko_fonol %>%
#  filter(PreOrd != 1, # Drop practice questions
#         IDCode != "admin@esko.fi") # Drop test account

esko_fonol <- esko_fonol %>%
  group_by(IDHash) %>%
  mutate(
    Q1_AnsC = ifelse(PreOrd == 1 & AnsC == 1, 1, 0),
    Q2_AnsC = ifelse(PreOrd == 2 & AnsC == 1, 1, 0),
    Q3_AnsC = ifelse(PreOrd == 3 & AnsC == 1, 1, 0),
    Q4_AnsC = ifelse(PreOrd == 4 & AnsC == 1, 1, 0),
    Q5_AnsC = ifelse(PreOrd == 5 & AnsC == 1, 1, 0),
    Q6_AnsC = ifelse(PreOrd == 6 & AnsC == 1, 1, 0),
    Q7_AnsC = ifelse(PreOrd == 7 & AnsC == 1, 1, 0),
    Q8_AnsC = ifelse(PreOrd == 8 & AnsC == 1, 1, 0),
    Q9_AnsC = ifelse(PreOrd == 9 & AnsC == 1, 1, 0),
    Q10_AnsC = ifelse(PreOrd == 10 & AnsC == 1, 1, 0),
    Q11_AnsC = ifelse(PreOrd == 11 & AnsC == 1, 1, 0)
  )

esko_fonol <- esko_fonol %>%
  group_by(IDHash, IDCode) %>%
  summarise(Q1_AnsC = sum(Q1_AnsC, na.rm = TRUE),
  Q2_AnsC = sum(Q2_AnsC, na.rm = TRUE),
  Q3_AnsC = sum(Q3_AnsC, na.rm = TRUE),
  Q4_AnsC = sum(Q4_AnsC, na.rm = TRUE),
  Q5_AnsC = sum(Q5_AnsC, na.rm = TRUE),
  Q6_AnsC = sum(Q6_AnsC, na.rm = TRUE),
  Q7_AnsC = sum(Q7_AnsC, na.rm = TRUE),
  Q8_AnsC = sum(Q8_AnsC, na.rm = TRUE),
  Q9_AnsC = sum(Q9_AnsC, na.rm = TRUE),
  Q10_AnsC = sum(Q10_AnsC, na.rm = TRUE),
  Q11_AnsC = sum(Q11_AnsC, na.rm = TRUE))

esko_fonol  <- esko_fonol %>%
  mutate(sum_AnsC = sum(Q1_AnsC,Q2_AnsC,Q3_AnsC,Q4_AnsC,Q5_AnsC,Q6_AnsC,Q7_AnsC,Q8_AnsC,Q9_AnsC,Q10_AnsC,Q11_AnsC))
              
print(mean(esko_fonol$sum_AnsC))
print(sd(esko_fonol$sum_AnsC))
print(mean(esko_fonol$Q1_AnsC))
              
              
              
              
              
              
              
              

# Tidy 4th sheet ("Kirjaintuntemus") -------------------------------------------

## Load data
esko_kirjain = read_xlsx("C:/Users/03248355/Work Folders/Data/eskodata_se.xlsx", 4)

## Process data
esko_kirjain = esko_kirjain %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(Ans = str_replace_all(Ans, "[, ]", ""), # Remove commas and blank spaces from answers
         AnsCount = nchar(Ans) - 2) # Sum of identified letters is the number of characters - brackets

esko_kirjain <- esko_kirjain %>%
  group_by(IDHash) %>%
  mutate(
    Q1_AnsC = ifelse(PreOrd == 1 & AnsCount > 0, AnsCount, 0),
    Q2_AnsC = ifelse(PreOrd == 2 & AnsCount > 0, AnsCount, 0),
    Q3_AnsC = ifelse(PreOrd == 3 & AnsCount > 0, AnsCount, 0))

esko_kirjain <- esko_kirjain %>%
  group_by(IDHash) %>%
  summarise(Sum_AnsC = sum(Q1_AnsC,Q2_AnsC,Q3_AnsC),
            Q1_AnsC = max(Q1_AnsC),
            Q2_AnsC = max(Q2_AnsC),
            Q3_AnsC = max(Q3_AnsC))

print(mean(esko_kirjain$Sum_AnsC))
print(sd(esko_kirjain$Sum_AnsC))
print(mean(esko_kirjain$Q1_AnsC))
print(mean(esko_kirjain$Q2_AnsC))
print(mean(esko_kirjain$Q3_AnsC))

# Tidy 5th sheet ("Lukutaito - Sanalistan lukeminen") --------------------------

## Load data
esko_lukutaito = read_xlsx("C:/Users/03248355/Work Folders/Data/eskodata_se.xlsx", 5)

## Process data
esko_lukutaito = esko_lukutaito %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  group_by(IDHash, IDCode) %>%
  summarise(sum_lukutaito = sum(SRFAnsC, na.rm = TRUE), # Sum of correct answers
            time_lukutaito = sum(SRFTime, na.rm = TRUE) / 1000, # Total answer time in seconds
            lang = ifelse(lang[1] == 1, "fi", "se")) # Add test language tag

# Combine verbal assessment into one table -------------------------------------

## Merge by full join so that assessments with missing values are not lost
esko_kielellinen = full_join(esko_sanavarasto, esko_fonol, by = c("IDHash", "IDCode", "lang"))
esko_kielellinen = full_join(esko_kielellinen, esko_kirjain, by = c("IDHash", "IDCode", "lang")) 
esko_kielellinen = full_join(esko_kielellinen, esko_lukutaito, by = c("IDHash", "IDCode", "lang")) 

# Keep only one test result per tunnus
esko_kielellinen = esko_kielellinen %>%
  rename(hetu_hash = IDHash, tunnus = IDCode) %>% # Rename columns
  mutate(lang_change = str_length(tunnus) == 8, # Add indicator for changing language
         tunnus = str_sub(tunnus, 1, 6)) %>% 
  relocate(lang, .after = tunnus) %>% 
  group_by(tunnus) %>%
  arrange(-lang_change) %>% # TRUE is first
  filter(row_number() == 1) %>% # Retain only assessments done in correct language
  select(-lang_change) %>% 
  ungroup
