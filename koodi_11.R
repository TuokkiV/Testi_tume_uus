# Code authors: 
# Anna Holvio, Ramin Izadi
# anna.holvio@vatt.fi, ramin.izadi@gmail.com

# Initial settings -------------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(tidylog)
library(haven)
library(readxl)
library(writexl)
library(crayon)
library(stringr)

crayon = function(x) cat(green(x), sep = "\n")
options("tidylog.display" = list(crayon))

# Tidy 1st sheet ("Aloita tästä") ----------------------------------------------

## Load data
esko = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx")
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx")

## Retain only unique hetu (2 obs per id). Spread.
esko =  esko %>% pivot_wider(names_from = Quest, values_from = Ans)
names(esko) = c("hetu_hash", "tunnus", "tester", "group", "missing")
esko = esko %>% select(-missing)

## Retain only unique hetu (2 obs per id). Spread.
esko_se = esko_se %>% pivot_wider(names_from = Quest, values_from = Ans)
names(esko_se) = c("hetu_hash", "tunnus", "tester", "group")

## Add indicator for changing test language
esko = esko %>% bind_rows(esko_se) %>% 
  mutate(lang_change = str_length(tunnus) == 8,
         tunnus = str_sub(tunnus, 1, 6),
         group = tolower(group))

## Retain only the test result done in the correct language
esko = esko %>% 
  group_by(tunnus) %>% 
  arrange(-lang_change) %>% # TRUE is first 
  filter(row_number()==1) %>% 
  ungroup

# Tidy 2nd sheet ("Sanavarasto") -----------------------------------------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 2)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 2)

esko_sanavarasto = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_sanavarasto = esko_sanavarasto %>%
  filter(!PreOrd %in% 1:2, # Drop practice questions
         IDCode != "admin@esko.fi") %>% # Drop test account
  group_by(IDHash, IDCode) %>%
  summarise(sum_sanavarasto = sum(AnsC, na.rm = TRUE), # Sum of correct answers
            time_sanavarasto = sum(Time, na.rm = TRUE) / 1000, # Total answer time in seconds
            lang = ifelse(lang[1] == 1, "fi", "se")) # Add test language tag

# Tidy 3rd sheet ("Fonologinen tietoisuus - Alkuäänne") ------------------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 3)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 3)

esko_fonol = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_fonol = esko_fonol %>%
  filter(PreOrd != 1, # Drop practice questions
         IDCode != "admin@esko.fi") %>% # Drop test account
  group_by(IDHash, IDCode) %>%
  summarise(sum_fonol = sum(AnsC, na.rm = TRUE), # Sum of correct answers
            time_fonol = sum(Time, na.rm = TRUE) / 1000, # Total answer time in seconds
            lang = ifelse(lang[1] == 1, "fi", "se")) # Add test language tag

# Tidy 4th sheet ("Kirjaintuntemus") -------------------------------------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 4)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 4)

esko_kirjain = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_kirjain = esko_kirjain %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(Ans = str_replace_all(Ans, "[, ]", ""), # Remove commas and blank spaces from answers
         AnsCount = nchar(Ans) - 2) %>% # Sum of identified letters is the number of characters - brackets
  group_by(IDHash, IDCode) %>%
  summarise(sum_kirjain = sum(AnsCount, na.rm = TRUE), # Sum of correct answers
            time_kirjain = sum(Time, na.rm = TRUE) / 1000, # Total answer time in seconds
            lang = ifelse(lang[1] == 1, "fi", "se")) # Add test language tag

# Tidy 5th sheet ("Lukutaito - Sanalistan lukeminen") --------------------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 5)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 5)

esko_lukutaito = bind_rows(esko_fi, esko_se, .id = "lang")

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

# Tidy 6th sheet ("Lukujonotaidot") --------------------------------------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 6)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 6)

## Pivot into wider format
esko_lukujono = bind_rows(esko_fi, esko_se, .id = "lang") %>%
  mutate(Quest = ifelse(substr(Quest, 1, 2) %in% c("Se", "S&"), "luvut_ylosp", "luvut_alasp")) %>%
  pivot_wider(names_from = Quest, values_from = Ans) # Pivot wide

# ## Manually clean answers in text format or larger than 31 in luvut_ylosp
# esko_lukujono_ylosp = esko_lukujono %>% 
#   select(-luvut_alasp) %>% 
#   filter(!luvut_ylosp %in% c(0:31))
# 
# # write_xlsx(esko_lukujono_ylosp, "2016 cohort full build/data output/esko_lukujono_ylosp.xlsx")

## Load the cleaned answers, replace the old ones, and process data
esko_lukujono_ylosp_fix = read_xlsx("2016 cohort full build/data input/evaluations_2021/esko_lukujono_ylosp_fix.xlsx")
esko_lukujono = left_join(esko_lukujono, esko_lukujono_ylosp_fix, by = c("IDHash", "IDCode", "lang")) %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(luvut_ylosp = ifelse(is.na(luvut_ylosp_fix), luvut_ylosp.x, luvut_ylosp_fix)) %>%
  select(-luvut_ylosp.x, -luvut_ylosp.y, -luvut_ylosp_fix) %>% 
  group_by(IDHash, IDCode) %>%
  mutate(luvut_alasp = ifelse(luvut_alasp %in% c("oikein", "rätt"), "1", "0"), # Recode answers
         luvut_alasp = as.integer(luvut_alasp),
         luvut_ylosp = as.integer(luvut_ylosp),
         lang = ifelse(lang[1] == 1, "fi", "se")) # Add test language tag 

# Tidy 9th sheet ("Määrien vertailu -tehtävä") ---------------------------------

## NB: Skipped the eighth sheet as it was practice questions

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 9)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 9)

esko_maarat_vert = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_maarat_vert = esko_maarat_vert %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(CRTAnsC = if_else(CRTAnsC == 0, -1 ,1), # Recode incorrect answers to -1 to subtract them from correct ones
         CRTAnsC = if_else(CRTPreOrd %in% c(1, 3, 4, 6, 7, 9, 11, 13, 14, 17, 19, 22, 23, 25, 26, 28), 
                           -1 * CRTAnsC, CRTAnsC)) %>% # Fix exercises with incorrect CRTCor value
  group_by(IDHash, IDCode) %>%
  summarise(sum_maarat_vert = sum(CRTAnsC, na.rm = TRUE), # Sum of correct answers (correct - incorrect)
            time_maarat_vert = sum(CRTtime, na.rm = TRUE) / 1000, # Total answer time in seconds
            lang = ifelse(lang[1] == 1, "fi", "se")) # Add test language tag

# Tidy 11th sheet ("Lukujen vertailu -tehtävä") --------------------------------

## NB: Skipped the tenth sheet as it was practice questions

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 11)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 11)

esko_luvut_vert = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_luvut_vert = esko_luvut_vert %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(NCAnsC = if_else(NCAnsC == 0, -1 ,1)) %>% 
  group_by(IDHash, IDCode) %>%
  summarise(sum_luvut_vert = sum(NCAnsC, na.rm = TRUE), # Sum of correct answers
            time_luvut_vert = sum(NCtime, na.rm = TRUE) / 1000, # Total answer time in seconds
            lang = ifelse(lang[1] == 1, "fi", "se")) # Add test language tag

# Tidy 12th sheet ("Lukumäärän tuottaminen") -----------------------------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 12)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 12)

esko_luvut_tuot = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_luvut_tuot = esko_luvut_tuot %>%
  filter(PreOrd != "1", # Drop practice questions
         IDCode != "admin@esko.fi") %>% # Drop test account
  group_by(IDHash, IDCode) %>%
  summarise(sum_luvut_tuot = sum(AnsC, na.rm = TRUE), # Sum of correct answers
            lang = ifelse(lang[1] == 1, "fi", "se")) # Add test language tag

# Tidy 13th sheet ("Laskutaito") -----------------------------------------------

# ## Clean answers in text format
# 
# ### Load data
# esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 13)
# esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 13)
# 
# ### Create esko_laskutaito
# esko_laskutaito = bind_rows(esko_fi, esko_se, .id = "lang") %>%
#   filter(!is.na(Quest)) %>% # Filter out rows with missing questions
#   mutate(Ans = ifelse(Ans == "-", NA, Ans), # Change answers with value "-" to NA
#          Quest = substring(Quest, 1, 1)) # Shorten questions for simplicity
# 
# ### Clean answers in incorrect format manually
# write_xlsx(esko_laskutaito, "C:/Users/03206466/Work Folders/Arviointidata/output/esko_laskutaito.xlsx")

## Load data
esko_laskutaito = read_xlsx("2016 cohort full build/data input/evaluations_2021/esko_laskutaito_fix.xlsx")

## Process data
esko_laskutaito = esko_laskutaito %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  rename(Ans_orig = 5, Ans = 6) %>%  # Rename the modified column Ans_fix to Ans and the original answer to Ans_orig
  mutate(AnsC = ifelse((Quest == 1 & Ans == 7) | 
                         (Quest == 2 & Ans == 2) |
                         (Quest == 3 & Ans == 10) |
                         (lang == 1 & Quest == 4 & Ans == 14) |
                         (lang == 2 & Quest == 4 & Ans == 10) |
                         (Quest == 6 & Ans == 14), 1, 0)) %>% # Define correct answers
  # NB: Finnish and Swedish tests have different numbering for the questions, although they are the same
  group_by(IDHash, IDCode) %>%
  summarise(sum_laskutaito = sum(AnsC, na.rm = TRUE), # Sum of correct answers
            lang = ifelse(lang[1] == 1, "fi", "se")) # Add test language tag

# Tidy 15th sheet ("Spatiaaliset suhteet -tehtävä") ----------------------------

## NB: Skipped the 14th sheet as it was practice questions

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 15)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 15)

esko_spatiaaliset = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_spatiaaliset = esko_spatiaaliset %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(Time = if_else(Time < 0, 0, Time), # Replace negative answering times
         #AnsC = if_else(AnsC == 0 & Ans != "[]", -1 , AnsC), # Change incorrect to -1
         AnsC = if_else(PreOrd == 24, 0, AnsC)) %>% # No correct answer defined for 24 so change all to 0
  group_by(IDHash, IDCode) %>%
  summarise(sum_spat = sum(AnsC, na.rm = TRUE), # Sum of correct answers
            time_spat = sum(Time, na.rm = TRUE) / 1000, # Total answer time in seconds
            lang = ifelse(lang[1] == 1, "fi", "se")) # Add test language tag

## NB: Includes negative answering times
# Combine mathematical assessment into one table -------------------------------

## Merge by full join so that assessments with missing values are not lost
esko_matemaattinen = full_join(esko_lukujono, esko_maarat_vert, by = c("IDHash", "IDCode", "lang"))
esko_matemaattinen = full_join(esko_matemaattinen, esko_luvut_vert, by = c("IDHash", "IDCode", "lang")) 
esko_matemaattinen = full_join(esko_matemaattinen, esko_luvut_tuot, by = c("IDHash", "IDCode", "lang")) 
esko_matemaattinen = full_join(esko_matemaattinen, esko_laskutaito, by = c("IDHash", "IDCode", "lang")) 
esko_matemaattinen = full_join(esko_matemaattinen, esko_spatiaaliset, by = c("IDHash", "IDCode", "lang")) 

# Keep only one test result per test taker
esko_matemaattinen = esko_matemaattinen %>%
  rename(hetu_hash = IDHash, tunnus = IDCode) %>% # Rename columns
  mutate(lang_change = str_length(tunnus) == 8, # Add indicator for changing language
         tunnus = str_sub(tunnus, 1, 6)) %>% 
  relocate(lang, .after = tunnus) %>% 
  group_by(tunnus) %>%
  arrange(-lang_change) %>% # TRUE is first
  filter(row_number() == 1) %>% # Retain only assessments done in correct language
  select(-lang_change) %>%
  #mutate(across(everything(), ~replace_na(.x, 0))) # Change NA to zero (non-answer because child was unable)
  ungroup

# Tidy 7th sheet ("Itsetunto") -------------------------------------------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 7)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 7)

esko_itsetunto = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_itsetunto = esko_itsetunto %>%
  filter(str_sub(Quest, 1, 4) != "<p>S", # Drop practice questions
         IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(Quest = word(Quest, 1, sep = "\\."), # Extract question number from string before "."
         Quest = str_remove(Quest, "<strong>"), # Remove "<strong>" so that Finnish questions are the same with Swedish
         Ans = str_remove_all(Ans, "[()]")) %>% # Remove brackets from answers to make them numeric
  pivot_wider(names_from = Quest, values_from = Ans) %>% # Pivot wide
  rename(hetu_hash = 2, tunnus = 3, pk_hauskaa = 4, pk_tylsaa = 5,
         ylpea = 6, tyytyvainen = 7, sairas = 8, paansarky_vatsakipu = 9,
         vanhemmat = 10, koti_mukavaa = 11, leikkii_kav = 12, tulee_toimeen = 13,
         parjaa_hyvin = 14, pk_mukavaa = 15) %>%
  mutate(lang = ifelse(lang == 1, "fi", "se"),
         lang_change = str_length(tunnus) == 8, # Add indicator for changing language
         tunnus = str_sub(tunnus, 1, 6)) %>% 
  mutate_at(vars(pk_hauskaa, pk_tylsaa, ylpea, tyytyvainen, sairas, 
                 paansarky_vatsakipu, vanhemmat, koti_mukavaa, leikkii_kav, 
                 tulee_toimeen, parjaa_hyvin, pk_mukavaa), as.integer) %>% 
  relocate(lang, .after = tunnus) %>% 
  group_by(tunnus) %>%
  arrange(-lang_change) %>% # TRUE is first
  filter(row_number() == 1) %>% # Retain only assessments done in correct language
  select(-lang_change) %>% 
  ungroup

## NB: Values correspond to (0) = "harvoin tai ei kovin usein", (1) = "joskus",
## (2) = "hyvin usein"

# Tidy 16th sheet ("Lisätietoja lapsesta ja testitilanteesta") -----------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 16)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 16)

esko_lisa = bind_rows(esko_fi, esko_se, .id = "lang")

esko_lisa = esko_lisa %>% 
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(Quest = ifelse(substr(Quest, 1, 2) %in% c("LI", "TI"), "lisatiedot", "kommentti")) %>% # Rename Finnish and Swedish questions the same
  group_by(IDHash, IDCode, Quest) %>%
  mutate(Ans = paste(Ans, collapse = " ")) %>% # Collapse the two lisatiedot rows into one per child 
  distinct %>%  # Remove duplicates (caused by the previous)
  pivot_wider(names_from = Quest, values_from = Ans) %>% # Pivot wide
  rename(hetu_hash = 2, tunnus = 3) %>%  # Rename columns
  mutate(kommentti = str_remove_all(kommentti, "NA"),
         kommentti = trimws(kommentti), # Remove leading (and trailing) whitespace
         laaja_viiv = case_when(str_detect(lisatiedot, "1\\.") ~ TRUE, TRUE ~ FALSE), # Present each option in its own column
         kielellinen_viiv = case_when(str_detect(lisatiedot, "2") ~ TRUE, TRUE ~ FALSE),
         muu_kielitaito = case_when(str_detect(lisatiedot, "3") ~ TRUE, TRUE ~ FALSE),
         nako_kuulo = case_when(str_detect(lisatiedot, "4") ~ TRUE, TRUE ~ FALSE),
         motoriset = case_when(str_detect(lisatiedot, "5") ~ TRUE, TRUE ~ FALSE),
         tarkkaavaisuus = case_when(str_detect(lisatiedot, "6") ~ TRUE, TRUE ~ FALSE),
         muut = case_when(str_detect(lisatiedot, "7") ~ TRUE, TRUE ~ FALSE),
         avustaja = case_when(str_detect(lisatiedot, "8") ~ TRUE, TRUE ~ FALSE),
         kayt_omaa_kielta = case_when(str_detect(lisatiedot, "9") ~ TRUE, TRUE ~ FALSE),
         varhennettu = case_when(str_detect(lisatiedot, "10") ~ TRUE, TRUE ~ FALSE),
         lang = ifelse(lang[1] == 1, "fi", "se"), # Test language tag
         tunnus = str_sub(tunnus, 1, 6)) %>% # Did not contain any duplicate IDs due to language change
  select(-lisatiedot) %>% 
  ungroup

# Tidy 17th sheet ("Lapsen tehtävästrategiat") ---------------------------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 17)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 17)

esko_strategiat = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_strategiat = esko_strategiat %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(Quest = str_sub(Quest, 1, 1)) %>% 
  pivot_wider(names_from = Quest, values_from = Ans) %>% # Pivot wide
  rename(hetu_hash = 2, tunnus = 3, tekee_muuta = 4, yrittaa_selvita = 5,
         luovuttaa = 6, aktiivisuus = 7, touhuilee = 8) %>%
  mutate(lang = ifelse(lang == 1, "fi", "se"),
         lang_change = str_length(tunnus) == 8, # Add indicator for changing language
         tunnus = str_sub(tunnus, 1, 6)) %>% 
  mutate_at(vars(tekee_muuta, yrittaa_selvita, luovuttaa, aktiivisuus, touhuilee),
            as.integer) %>% 
  relocate(lang, .after = tunnus) %>% 
  group_by(tunnus) %>%
  arrange(-lang_change) %>% # TRUE is first
  filter(row_number() == 1) %>% # Retain only assessments done in correct language
  select(-lang_change) %>% 
  ungroup

# Tidy 18th sheet ("Lapsen sosiaaliset taidot ja yksinäisyys") -----------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 18)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 18)

esko_sosiaaliset = bind_rows(esko_fi, esko_se, .id = "lang")

yksin_rename_1 = c("1. L", "1. B") # Numbers 1, 2 and 3 are each used twice for different questions
yksin_rename_2 = c("2. L", "2. B") # Rename the loneliness questions as 7, 8 and 9 later
yksin_rename_3 = c("3. L", "3. B")

## Process data
esko_sosiaaliset = esko_sosiaaliset %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(Quest = if_else(str_sub(Quest, 1, 4) %in% yksin_rename_1, "7",
                         if_else(str_sub(Quest, 1, 4) %in% yksin_rename_2, "8",
                                 if_else(str_sub(Quest, 1, 4) %in% yksin_rename_3, "9", Quest))),
         Quest = str_sub(Quest, 1, 1)) %>% 
  pivot_wider(names_from = Quest, values_from = Ans) %>% # Pivot wide
  rename(hetu_hash = 2, tunnus = 3, osallistuu = 4, kutsuu = 5,
         aloit_kesk = 6, yhteistyo = 7, hyva_kaveri = 8, hyvaksyy = 9,
         yksinainen = 10, jaa_ulkop = 11, vaik_ystavia = 12) %>%
  mutate(lang = ifelse(lang == 1, "fi", "se"),
         lang_change = str_length(tunnus) == 8, # Add indicator for changing language
         tunnus = str_sub(tunnus, 1, 6)) %>% 
  mutate_at(vars(osallistuu, kutsuu, aloit_kesk, yhteistyo, hyva_kaveri, hyvaksyy,
                 yksinainen, jaa_ulkop, vaik_ystavia), as.integer) %>% 
  relocate(lang, .after = tunnus) %>% 
  group_by(tunnus) %>%
  arrange(-lang_change) %>% # TRUE is first
  filter(row_number() == 1) %>% # Retain only assessments done in correct language
  select(-lang_change) %>% 
  ungroup

# Tidy 19th sheet ("Lapsen itsesäätelytaidot") ---------------------------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 19)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 19)

esko_itsesaately = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_itsesaately = esko_itsesaately %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(Quest = word(Quest, 1, sep = "\\.")) %>% # Extract question number from string before "."
  pivot_wider(names_from = Quest, values_from = Ans) %>% # Pivot wide
  rename(hetu_hash = 2, tunnus = 3, jakaa_toisten_kanssa = 4, sanallinen_viham = 5,
         fyysinen_viham = 6, yhteistyokykyinen = 7, jakaa_kavereiden_kanssa = 8,
         tottelee_aik = 9, ei_hossota = 10, noud_saantoja = 11, saattaa_loppuun = 12,
         saa_teht_valmiiksi = 13, yrittaa_uusia_teht = 14, keskittyy = 15,
         reagoi_ohjeisiin = 16, yrittaa_parhaansa = 17, loytaa_mater = 18,
         huomaa_virheitaan = 19, palaa_teht_pariin = 20) %>%
  mutate(lang = ifelse(lang == 1, "fi", "se"),
         lang_change = str_length(tunnus) == 8, # Add indicator for changing language
         tunnus = str_sub(tunnus, 1, 6)) %>% 
  mutate_at(vars(jakaa_toisten_kanssa, sanallinen_viham, fyysinen_viham, yhteistyokykyinen, 
                 jakaa_kavereiden_kanssa, tottelee_aik, ei_hossota, noud_saantoja, 
                 saattaa_loppuun, saa_teht_valmiiksi, yrittaa_uusia_teht, keskittyy,
                 reagoi_ohjeisiin, yrittaa_parhaansa, loytaa_mater, huomaa_virheitaan, 
                 palaa_teht_pariin), as.integer) %>% 
  relocate(lang, .after = tunnus) %>% 
  group_by(tunnus) %>%
  arrange(-lang_change) %>% # TRUE is first
  filter(row_number() == 1) %>% # Retain only assessments done in correct language
  select(-lang_change) %>% 
  ungroup

# Tidy 20th sheet ("Lapsen käyttäytyminen") ------------------------------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 20)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 20)

esko_kayttaytyminen = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_kayttaytyminen = esko_kayttaytyminen %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(Quest = word(Quest, 1, sep = "\\.")) %>% # Extract question number from string before "."
  pivot_wider(names_from = Quest, values_from = Ans) %>% # Pivot wide
  rename(hetu_hash = 2, tunnus = 3, huomioi_tunteet = 4, levoton = 5,
         valittaa_paansarkya = 6, jakaa_tavaroita = 7, kiukunpuuskia = 8,
         ei_kaipaa_seuraa = 9, tottelevainen = 10, huolestunut = 11, 
         tarjoutuu_auttamaan_jos_loukkaa = 12, hypistelee_jatk = 13, 
         ainakin_yksi_kaveri = 14, tappelee_kiusaa = 15, onneton = 16, 
         muiden_suosiossa = 17, helposti_hairiintyva = 18, pelokas_uusissa_til = 19, 
         kiltti_nuoremmille = 20, valehtelee = 21, silmatikku = 22,
         tarjoutuu_auttamaan_muita = 23, harkitsee = 24, varastaa = 25, 
         par_toimeen_aik_kanssa = 26, karsii_peloista = 27, saattaa_teht_loppuun = 28) %>%
  mutate(lang = ifelse(lang == 1, "fi", "se"),
         lang_change = str_length(tunnus) == 8, # Add indicator for changing language
         tunnus = str_sub(tunnus, 1, 6)) %>% 
  mutate_at(vars(huomioi_tunteet, levoton, valittaa_paansarkya, jakaa_tavaroita, kiukunpuuskia,
                 ei_kaipaa_seuraa, tottelevainen, huolestunut, tarjoutuu_auttamaan_jos_loukkaa, 
                 hypistelee_jatk, ainakin_yksi_kaveri, tappelee_kiusaa, onneton, 
                 muiden_suosiossa, helposti_hairiintyva, pelokas_uusissa_til, 
                 kiltti_nuoremmille, valehtelee, silmatikku, tarjoutuu_auttamaan_muita, 
                 harkitsee, varastaa, par_toimeen_aik_kanssa, karsii_peloista, 
                 saattaa_teht_loppuun), as.integer) %>% 
  relocate(lang, .after = tunnus) %>% 
  group_by(tunnus) %>%
  arrange(-lang_change) %>% # TRUE is first
  filter(row_number() == 1) %>% # Retain only assessments done in correct language
  select(-lang_change) %>% 
  ungroup

# Tidy 21st sheet ("Lapsen uteliaisuus ja onnellisuus") ------------------------

## Load data
esko_fi = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata.xlsx", 21)
esko_se = read_xlsx("2016 cohort full build/data input/evaluations_2021/eskodata_se.xlsx", 21)

esko_uteliaisuus_onnellisuus = bind_rows(esko_fi, esko_se, .id = "lang")

## Process data
esko_uteliaisuus_onnellisuus = esko_uteliaisuus_onnellisuus %>%
  filter(IDCode != "admin@esko.fi") %>% # Drop test account
  mutate(Quest = str_sub(Quest, 1, 1)) %>% # Extract question number
  pivot_wider(names_from = Quest, values_from = Ans) %>% # Pivot wide
  rename(hetu_hash = 2, tunnus = 3, pitaa_oppimisesta = 4, kivaa_oppia = 5,
         ei_miel_opettele = 6, myont_tulevaisuus = 7, nauttii_elamasta = 8,
         onnellinen = 9) %>%
  mutate(lang = ifelse(lang == 1, "fi", "se"),
         lang_change = str_length(tunnus) == 8, # Add indicator for changing language
         tunnus = str_sub(tunnus, 1, 6)) %>% 
  mutate_at(vars(pitaa_oppimisesta, kivaa_oppia, ei_miel_opettele, myont_tulevaisuus,
                 nauttii_elamasta, onnellinen), as.integer) %>% 
  relocate(lang, .after = tunnus) %>% 
  group_by(tunnus) %>%
  arrange(-lang_change) %>% # TRUE is first
  filter(row_number() == 1) %>% # Retain only assessments done in correct language
  select(-lang_change) %>% 
  ungroup
