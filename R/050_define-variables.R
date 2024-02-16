rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "010_paths-and-files.R"))
source(here::here("R", "020_libraries.R"))

# Import data

tracker <- readRDS(here::here(RDS_path, "010_tracker.rds"))
hrs1998_a <- readRDS(here::here(RDS_path, "010_hrs1998_a.rds"))
hrs1998_c <- readRDS(here::here(RDS_path, "010_hrs1998_c.rds"))
hrs2000_c <- readRDS(here::here(RDS_path, "010_hrs2000_c.rds"))
hrs2002_d <- readRDS(here::here(RDS_path, "010_hrs2002_d.rds"))
hrs2004_d <- readRDS(here::here(RDS_path, "010_hrs2004_d.rds"))
hrs2006_d <- readRDS(here::here(RDS_path, "010_hrs2006_d.rds"))
hrs2008_d <- readRDS(here::here(RDS_path, "010_hrs2008_d.rds"))
hrs2010_d <- readRDS(here::here(RDS_path, "010_hrs2010_d.rds"))
hrs2012_d <- readRDS(here::here(RDS_path, "010_hrs2012_d.rds"))
hrs2014_d <- readRDS(here::here(RDS_path, "010_hrs2014_d.rds"))
hrs2016_d <- readRDS(here::here(RDS_path, "010_hrs2016_d.rds"))
hrs2018_d <- readRDS(here::here(RDS_path, "010_hrs2018_d.rds"))
hrs2020_d <- readRDS(here::here(RDS_path, "010_hrs2020_d.rds"))
CODA_IDs <- readRDS(here::here(RDS_path, "040_CODA-IDS.Rds"))

# Begin building cohort

# Pull in the CODA IDs with non-zero weight from step 4

# First - join HHID and PN together to create unique IDs and filter for CODA participants

tracker_01 <- tracker %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs1998_c_01 <- hrs1998_c %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs2000_c_01 <- hrs2000_c %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs2002_d_01 <- hrs2002_d %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs2004_d_01 <- hrs2004_d %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs2006_d_01 <- hrs2006_d %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs2008_d_01 <- hrs2008_d %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs2010_d_01 <- hrs2010_d %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs2012_d_01 <- hrs2012_d %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs2014_d_01 <- hrs2014_d %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs2016_d_01 <- hrs2016_d %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs2018_d_01 <- hrs2018_d %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

hrs2020_d_01 <- hrs2020_d %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

###
# First, get mode of assessment data from tracker
###

tracker_02 <- tracker_01 %>% 
  dplyr::select(HRS_ID,
                FIWMODE, #98
                GIWMODE, #00
                HIWMODE, #02
                JIWMODE, #04
                KIWMODE, #06
                LIWMODE, #08
                MIWMODE, #10
                NIWMODE, #12
                OIWMODE, #14
                PIWMODE, #16
                QIWMODE, #18,
                RIWMODE) %>%  #20
  dplyr::mutate(FIWMODE = dplyr::na_if(FIWMODE, 8), # 8 = not ascertained
                GIWMODE = dplyr::na_if(GIWMODE, 8))

tracker_02 %>% 
  select(-HRS_ID) %>% 
  gtsummary::tbl_summary()

assessment_mode_long <- tracker_02 %>% 
  pivot_longer(!HRS_ID, names_to = "time", values_to = "mode") %>% 
  dplyr::mutate(Year = dplyr::case_when(time == "FIWMODE" ~ 1998,
                                        time == "GIWMODE" ~ 2000,
                                        time == "HIWMODE" ~ 2002,
                                        time == "JIWMODE" ~ 2004,
                                        time == "KIWMODE" ~ 2006,
                                        time == "LIWMODE" ~ 2008,
                                        time == "MIWMODE" ~ 2010,
                                        time == "NIWMODE" ~ 2012,
                                        time == "OIWMODE" ~ 2014,
                                        time == "PIWMODE" ~ 2016,
                                        time == "QIWMODE" ~ 2018,
                                        time == "RIWMODE" ~ 2020),
                person0webphone1 = dplyr::case_when(mode == 1 ~ 0,
                                                    mode == 2 ~ 1,
                                                    mode == 3 ~ 1),
                isbaseline = dplyr::case_when(Year == 1998 ~ 1,
                                              TRUE ~ 0))

# Get weights from 1998 (when CODA started)

weights_only <- tracker_01 %>%
  dplyr::select(HRS_ID, SECU, STRATUM, FWGTR)



# Consider further filtering based on Rodgers et al. (2003) and McArdle et al. (2007)

# eliminated any person who (a) was not
# a primary respondent, (b) had no data on gender or age, (c) had a
# sampling weight of zero, or (d) was under age 50. In addition, (e)
# in cases in which there were two persons per family, we randomly
# picked only one member of the family.

# Score cognition measure
# Going to use McArdle's longitudinal measure

# McArdle model:
# Episodic Memory Factor =~ Immediate Recall + Delayed Recall
# Mental Status =~ Serial 7s + Backward Counting + Dates + Names
# ALL MEASURES SCORED BY % CORRECT
# Also adjusts for age, education, gender, cohort (birth year), couple, and telephone vs FTF
# Age in years
# Years of education
# Gender (-.5 = M, .5 = F)
# Couple (-.5 = living alone, .5 = living in couple)
# tele vs ftf (-.5 = ftf, .5 = tele)
# Birth cohort
# Telephone vs FTF dummy coded

# McArdle et al puts everything on a % correct scale for analysis


hrs1998_scored <- hrs1998_c_01 %>%
  left_join(CODA_IDs, by = "HRS_ID") %>%
  
  # First score immediate recall
  # Items F1491M1 to F1491M11
  # 4 word list sets (1-10, 11-20, 21-30, 31-40) -- see item F1486_1
  # Entries of 50-53 are "wrong"
  # Entries of 95 were "invalid test"
  # Entries of 96-99 were "dk/NA/refused/none remembered"
  # Add up number of entries "below 40", multiply by 10 to put on 0-100
  
  dplyr::mutate(IMM_RC_1 = dplyr::case_when(F1491M1 < 41 ~ 1,
                                            between(F1491M1, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_2 = dplyr::case_when(F1491M2 < 41 ~ 1,
                                            between(F1491M2, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_3 = dplyr::case_when(F1491M3 < 41 ~ 1,
                                            between(F1491M3, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_4 = dplyr::case_when(F1491M4 < 41 ~ 1,
                                            between(F1491M4, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_5 = dplyr::case_when(F1491M5 < 41 ~ 1,
                                            between(F1491M5, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_6 = dplyr::case_when(F1491M6 < 41 ~ 1,
                                            between(F1491M6, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_7 = dplyr::case_when(F1491M7 < 41 ~ 1,
                                            between(F1491M7, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_8 = dplyr::case_when(F1491M8 < 41 ~ 1,
                                            between(F1491M8, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_9 = dplyr::case_when(F1491M9 < 41 ~ 1,
                                            between(F1491M9, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_10 = dplyr::case_when(F1491M10 < 41 ~ 1,
                                             between(F1491M10, 51, 99) ~ 0,
                                             TRUE ~ NA_real_),
                IMM_RC_11 = dplyr::case_when(F1491M11 < 41 ~ 1,
                                             between(F1491M11, 51, 99) ~ 0,
                                             TRUE ~ NA_real_)) %>%
  dplyr::mutate(IMM_RC_SUM = (rowSums(.[c("IMM_RC_1", "IMM_RC_2", "IMM_RC_3", "IMM_RC_4",
                                          "IMM_RC_5", "IMM_RC_6", "IMM_RC_7", "IMM_RC_8",
                                          "IMM_RC_9", "IMM_RC_10", "IMM_RC_11")], na.rm = TRUE)),
                IR = IMM_RC_SUM*10) %>%
  
  # Next score Delayed recall -- same procedures as imm recall
  # items are F1640M1-F1640M11
  
  dplyr::mutate(DEL_RC_1 = dplyr::case_when(F1640M1 < 41 ~ 1,
                                            between(F1640M1, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_2 = dplyr::case_when(F1640M2 < 41 ~ 1,
                                            between(F1640M2, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_3 = dplyr::case_when(F1640M3 < 41 ~ 1,
                                            between(F1640M3, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_4 = dplyr::case_when(F1640M4 < 41 ~ 1,
                                            between(F1640M4, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_5 = dplyr::case_when(F1640M5 < 41 ~ 1,
                                            between(F1640M5, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_6 = dplyr::case_when(F1640M6 < 41 ~ 1,
                                            between(F1640M6, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_7 = dplyr::case_when(F1640M7 < 41 ~ 1,
                                            between(F1640M7, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_8 = dplyr::case_when(F1640M8 < 41 ~ 1,
                                            between(F1640M8, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_9 = dplyr::case_when(F1640M9 < 41 ~ 1,
                                            between(F1640M9, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_10 = dplyr::case_when(F1640M10 < 41 ~ 1,
                                             between(F1640M10, 51, 99) ~ 0,
                                             TRUE ~ NA_real_),
                DEL_RC_11 = dplyr::case_when(F1640M11 < 41 ~ 1,
                                             between(F1640M11, 51, 99) ~ 0,
                                             TRUE ~ NA_real_)) %>%
  dplyr::mutate(DEL_RC_SUM = (rowSums(.[c("DEL_RC_1", "DEL_RC_2", "DEL_RC_3", "DEL_RC_4",
                                          "DEL_RC_5", "DEL_RC_6", "DEL_RC_7", "DEL_RC_8",
                                          "DEL_RC_9", "DEL_RC_10", "DEL_RC_11")], na.rm = TRUE)),
                DR = DEL_RC_SUM*10) %>%
  
  # Next score serial 7s
  # Every participant starts with a prompt of 100
  # sum correct scores, divide by 5, multiply by 100 for percent scale
  
  dplyr::mutate(S7_1 = dplyr::case_when(F1631 == 93 ~ 1, # 100 - 7 = 93
                                        F1631 != 93 ~ 0,
                                        TRUE ~ NA_real_),
                S7_2 = dplyr::case_when(F1632 == 86 ~ 1, # 93 - 7 = 86
                                        F1632 != 86 ~ 0,
                                        TRUE ~ NA_real_),
                S7_3 = dplyr::case_when(F1633 == 79 ~ 1, # 86 - 7 = 79
                                        F1633 != 79 ~ 0,
                                        TRUE ~ NA_real_),
                S7_4 = dplyr::case_when(F1634 == 72 ~ 1, # 79 - 7 = 72
                                        F1634 != 72 ~ 0,
                                        TRUE ~ NA_real_),
                S7_5 = dplyr::case_when(F1635 == 65 ~ 1, # 72 - 7 = 65
                                        F1635 != 65 ~ 0,
                                        TRUE ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  
  # Next score backwards counting (from 20)
  # scored where 1 = correct, 5 = incorrect
  # did not give weight to second attempts -- only n = 30 took a second attempt
  
  dplyr::mutate(BC = case_when(F1535 == 1 ~ 100,
                               F1535 == 5 ~ 0,
                               TRUE ~ NA_real_)) %>%
  
  # Next score names
  # F1649 - Name (Scissors)
  # F1650 - Name (Cactus)
  # F1651 - Name (President)
  # F1652 - Name (VP)
  # Take sum, divide by 4 (num of Qs), multiply by 100
  
  dplyr::mutate(Names_Scissors = dplyr::case_when(F1649 == 1 ~ 1,
                                                  F1649 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(F1650 == 1 ~ 1,
                                                F1650 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(F1651 == 1 ~ 1,
                                              F1651 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(F1652 == 1 ~ 1,
                                            F1652 == 5 ~ 0,
                                            TRUE ~ NA_real_)) %>%
  dplyr::mutate(NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  
  # Next score orientation
  # F1645  - Date (Month)
  # F1646  - Date (Day)
  # F1647  - Date (Year)
  # F1648  - Date (Day of week)
  
  # Take sum, divide by 4 (num of Qs), multiply by 100
  dplyr::mutate(Date_Month = dplyr::case_when(F1645 == 1 ~ 1,
                                              F1645 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(F1646 == 1 ~ 1,
                                            F1646 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(F1647 == 1 ~ 1,
                                             F1647 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(F1648 == 1 ~ 1,
                                            F1648 == 5 ~ 0,
                                            TRUE ~ NA_real_)) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100) %>%
  dplyr::select(
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 1998) # Add year variable

# 2000 Wave
# Same as 1998, just have to update var names

hrs2000_scored <- hrs2000_c_01 %>%
  left_join(CODA_IDs, by = "HRS_ID") %>%
  
  # First score immediate recall
  # Items G1666M1 to G1666M11
  # 4 word list sets (1-10, 11-20, 21-30, 31-40) -- see item F1486_1
  # Entries of 50-99 are "wrong"
  # Entries of 95 were "invalid test"
  # Entries of 96-99 were "dk/NA/refused/none remembered"
  # Add up number of entries "below 40", multiply by 10 to put on 0-100
  
  dplyr::mutate(IMM_RC_1 = dplyr::case_when(G1666M1 < 41 ~ 1,
                                            between(G1666M1, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_2 = dplyr::case_when(G1666M2 < 41 ~ 1,
                                            between(G1666M2, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_3 = dplyr::case_when(G1666M3 < 41 ~ 1,
                                            between(G1666M3, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_4 = dplyr::case_when(G1666M4 < 41 ~ 1,
                                            between(G1666M4, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_5 = dplyr::case_when(G1666M5 < 41 ~ 1,
                                            between(G1666M5, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_6 = dplyr::case_when(G1666M6 < 41 ~ 1,
                                            between(G1666M6, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_7 = dplyr::case_when(G1666M7 < 41 ~ 1,
                                            between(G1666M7, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_8 = dplyr::case_when(G1666M8 < 41 ~ 1,
                                            between(G1666M8, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_9 = dplyr::case_when(G1666M9 < 41 ~ 1,
                                            between(G1666M9, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                IMM_RC_10 = dplyr::case_when(G1666M10 < 41 ~ 1,
                                             between(G1666M10, 51, 99) ~ 0,
                                             TRUE ~ NA_real_),
                IMM_RC_11 = dplyr::case_when(G1666M11 < 41 ~ 1,
                                             between(G1666M11, 51, 99) ~ 0,
                                             TRUE ~ NA_real_)) %>%
  dplyr::mutate(IMM_RC_SUM = (rowSums(.[c("IMM_RC_1", "IMM_RC_2", "IMM_RC_3", "IMM_RC_4",
                                          "IMM_RC_5", "IMM_RC_6", "IMM_RC_7", "IMM_RC_8",
                                          "IMM_RC_9", "IMM_RC_10", "IMM_RC_11")], na.rm = TRUE)),
                IR = IMM_RC_SUM*10) %>%
  
  # Next score Delayed recall -- same procedures as imm recall
  # items are G1815M1-G1815M11
  
  dplyr::mutate(DEL_RC_1 = dplyr::case_when(G1815M1 < 41 ~ 1,
                                            between(G1815M1, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_2 = dplyr::case_when(G1815M2 < 41 ~ 1,
                                            between(G1815M2, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_3 = dplyr::case_when(G1815M3 < 41 ~ 1,
                                            between(G1815M3, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_4 = dplyr::case_when(G1815M4 < 41 ~ 1,
                                            between(G1815M4, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_5 = dplyr::case_when(G1815M5 < 41 ~ 1,
                                            between(G1815M5, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_6 = dplyr::case_when(G1815M6 < 41 ~ 1,
                                            between(G1815M6, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_7 = dplyr::case_when(G1815M7 < 41 ~ 1,
                                            between(G1815M7, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_8 = dplyr::case_when(G1815M8 < 41 ~ 1,
                                            between(G1815M8, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_9 = dplyr::case_when(G1815M9 < 41 ~ 1,
                                            between(G1815M9, 51, 99) ~ 0,
                                            TRUE ~ NA_real_),
                DEL_RC_10 = dplyr::case_when(G1815M10 < 41 ~ 1,
                                             between(G1815M10, 51, 99) ~ 0,
                                             TRUE ~ NA_real_),
                DEL_RC_11 = dplyr::case_when(G1815M11 < 41 ~ 1,
                                             between(G1815M11, 51, 99) ~ 0,
                                             TRUE ~ NA_real_)) %>%
  dplyr::mutate(DEL_RC_SUM = (rowSums(.[c("DEL_RC_1", "DEL_RC_2", "DEL_RC_3", "DEL_RC_4",
                                          "DEL_RC_5", "DEL_RC_6", "DEL_RC_7", "DEL_RC_8",
                                          "DEL_RC_9", "DEL_RC_10", "DEL_RC_11")], na.rm = TRUE)),
                DR = DEL_RC_SUM*10) %>%
  
  # Next score serial 7s
  # Every participant starts with a prompt of 100
  # sum correct scores, divide by 5, multiply by 100 for percent scale
  
  dplyr::mutate(S7_1 = dplyr::case_when(G1806 == 93 ~ 1, # 100 - 7 = 93
                                        G1806 != 93 ~ 0,
                                        TRUE ~ NA_real_),
                S7_2 = dplyr::case_when(G1807 == 86 ~ 1, # 93 - 7 = 86
                                        G1807 != 86 ~ 0,
                                        TRUE ~ NA_real_),
                S7_3 = dplyr::case_when(G1808 == 79 ~ 1, # 86 - 7 = 79
                                        G1808 != 79 ~ 0,
                                        TRUE ~ NA_real_),
                S7_4 = dplyr::case_when(G1809 == 72 ~ 1, # 79 - 7 = 72
                                        G1809 != 72 ~ 0,
                                        TRUE ~ NA_real_),
                S7_5 = dplyr::case_when(G1810 == 65 ~ 1, # 72 - 7 = 65
                                        G1810 != 65 ~ 0,
                                        TRUE ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  
  # Next score backwards counting (from 20)
  # scored where 1 = correct, 5 = incorrect
  # did not give weight to second attempts -- only n = 56 took a second attempt
  
  dplyr::mutate(BC = case_when(G1710 == 1 ~ 100,
                               G1710 == 5 ~ 0,
                               TRUE ~ NA_real_)) %>%
  
  # Next score names
  # F1649 - Name (Scissors)
  # F1650 - Name (Cactus)
  # F1651 - Name (President)
  # F1652 - Name (VP)
  # Take sum, divide by 4 (num of Qs), multiply by 100
  
  dplyr::mutate(Names_Scissors = dplyr::case_when(G1824 == 1 ~ 1,
                                                  G1824 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(G1825 == 1 ~ 1,
                                                G1825 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(G1826 == 1 ~ 1,
                                              G1826 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(G1827 == 1 ~ 1,
                                            G1827 == 5 ~ 0,
                                            TRUE ~ NA_real_)) %>%
  dplyr::mutate(NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  
  # Next score orientation
  # F1645  - Date (Month)
  # F1646  - Date (Day)
  # F1647  - Date (Year)
  # F1648  - Date (Day of week)
  
  # Take sum, divide by 4 (num of Qs), multiply by 100
  dplyr::mutate(Date_Month = dplyr::case_when(G1820 == 1 ~ 1,
                                              G1820 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(G1821 == 1 ~ 1,
                                            G1821 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(G1822 == 1 ~ 1,
                                             G1822 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(G1823 == 1 ~ 1,
                                            G1823 == 5 ~ 0,
                                            TRUE ~ NA_real_)) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100) %>%
  
  dplyr::select(
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 2000) # Add year variable

# 2002 is the beginning of the "2010-style" variable naming
# 2010-style solely means I started with 2010 and moved forward to 2020
# 2002 prefix is HD

hrs2002_scored <- hrs2002_d_01 %>%
  dplyr::left_join(CODA_IDs, by = "HRS_ID") %>%
  dplyr::mutate(IR = HD174*10, # on a scale from 0-10, multiply by 10 to score
                DR = HD184*10, # on a scale from 0-10, multiply by 10 to score
                BC = dplyr::case_when(HD124 == 1 ~ 100, # on a scale where 1 = correct, 5 = incorrect
                                      HD124 == 5 ~ 0,   # ignored second attempts
                                      TRUE ~ NA_real_),
                # Date and name variables are scored 1 = yes, 5 = no, 8 or 9 = don't know, NA, or refused - recoding
                Date_Month = dplyr::case_when(HD151 == 1 ~ 1,
                                              HD151 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(HD152 == 1 ~ 1,
                                            HD152 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(HD153 == 1 ~ 1,
                                             HD153 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(HD154 == 1 ~ 1,
                                            HD154 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Names_Scissors = dplyr::case_when(HD155 == 1 ~ 1,
                                                  HD155 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(HD156 == 1 ~ 1,
                                                HD155 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(HD157 == 1 ~ 1,
                                              HD157 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(HD158 == 1 ~ 1,
                                            HD158 == 5 ~ 0,
                                            TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100,
                # Take sum, divide by 4 (num of Qs), multiply by 100
                NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  # Take sum, divide by 4 (num of Qs), multiply by 100
  # serial 7s is scored based on the previous number -- so a correct answer for HD142 is 93. If a participant answers 94, but gets 94-7
  # right, then HD143 is correct
  dplyr::mutate(S7_1 = dplyr::case_when(HD142 == 100 - 7 ~ 1,
                                        HD142 != 100 - 7 ~ 0,
                                        HD142 > 997 ~ NA_real_),
                S7_2 = dplyr::case_when(HD143 == HD142 - 7 ~ 1,
                                        HD143 != HD142 - 7 ~ 0,
                                        HD143 > 997 ~ NA_real_),
                S7_3 = dplyr::case_when(HD144 == HD143 - 7 ~ 1,
                                        HD144 != HD143 - 7 ~ 0,
                                        HD144 > 997 ~ NA_real_),
                S7_4 = dplyr::case_when(HD145 == HD144 - 7 ~ 1,
                                        HD145 != HD144 - 7 ~ 0,
                                        HD145 > 997 ~ NA_real_),
                S7_5 = dplyr::case_when(HD146 == HD145 - 7 ~ 1,
                                        HD146 != HD145 - 7 ~ 0,
                                        HD146 > 997 ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  dplyr::select(
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 2002) # Add year variable

# 2004
# HD -> JD

hrs2004_scored <- hrs2004_d_01 %>%
  dplyr::left_join(CODA_IDs, by = "HRS_ID") %>%
  dplyr::mutate(IR = JD174*10, # on a scale from 0-10, multiply by 10 to score
                DR = JD184*10, # on a scale from 0-10, multiply by 10 to score
                BC = dplyr::case_when(JD124 == 1 ~ 100, # on a scale where 1 = correct, 5 = incorrect
                                      JD124 == 5 ~ 0,   # ignored second attempts
                                      TRUE ~ NA_real_),
                # Date and name variables are scored 1 = yes, 5 = no, 8 or 9 = don't know, NA, or refused - recoding
                Date_Month = dplyr::case_when(JD151 == 1 ~ 1,
                                              JD151 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(JD152 == 1 ~ 1,
                                            JD152 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(JD153 == 1 ~ 1,
                                             JD153 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(JD154 == 1 ~ 1,
                                            JD154 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Names_Scissors = dplyr::case_when(JD155 == 1 ~ 1,
                                                  JD155 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(JD156 == 1 ~ 1,
                                                JD155 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(JD157 == 1 ~ 1,
                                              JD157 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(JD158 == 1 ~ 1,
                                            JD158 == 5 ~ 0,
                                            TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100,
                # Take sum, divide by 4 (num of Qs), multiply by 100
                NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  # Take sum, divide by 4 (num of Qs), multiply by 100
  # serial 7s is scored based on the previous number -- so a correct answer for JD142 is 93. If a participant answers 94, but gets 94-7
  # right, then JD143 is correct
  dplyr::mutate(S7_1 = dplyr::case_when(JD142 == 100 - 7 ~ 1,
                                        JD142 != 100 - 7 ~ 0,
                                        JD142 > 997 ~ NA_real_),
                S7_2 = dplyr::case_when(JD143 == JD142 - 7 ~ 1,
                                        JD143 != JD142 - 7 ~ 0,
                                        JD143 > 997 ~ NA_real_),
                S7_3 = dplyr::case_when(JD144 == JD143 - 7 ~ 1,
                                        JD144 != JD143 - 7 ~ 0,
                                        JD144 > 997 ~ NA_real_),
                S7_4 = dplyr::case_when(JD145 == JD144 - 7 ~ 1,
                                        JD145 != JD144 - 7 ~ 0,
                                        JD145 > 997 ~ NA_real_),
                S7_5 = dplyr::case_when(JD146 == JD145 - 7 ~ 1,
                                        JD146 != JD145 - 7 ~ 0,
                                        JD146 > 997 ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  dplyr::select(
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 2004) # Add year variable

# 2006
# JD -> KD

hrs2006_scored <- hrs2006_d_01 %>%
  dplyr::left_join(CODA_IDs, by = "HRS_ID") %>%
  dplyr::mutate(IR = KD174*10, # on a scale from 0-10, multiply by 10 to score
                DR = KD184*10, # on a scale from 0-10, multiply by 10 to score
                BC = dplyr::case_when(KD124 == 1 ~ 100, # on a scale where 1 = correct, 5 = incorrect
                                      KD124 == 5 ~ 0,   # ignored second attempts
                                      TRUE ~ NA_real_),
                # Date and name variables are scored 1 = yes, 5 = no, 8 or 9 = don't know, NA, or refused - recoding
                Date_Month = dplyr::case_when(KD151 == 1 ~ 1,
                                              KD151 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(KD152 == 1 ~ 1,
                                            KD152 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(KD153 == 1 ~ 1,
                                             KD153 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(KD154 == 1 ~ 1,
                                            KD154 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Names_Scissors = dplyr::case_when(KD155 == 1 ~ 1,
                                                  KD155 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(KD156 == 1 ~ 1,
                                                KD155 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(KD157 == 1 ~ 1,
                                              KD157 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(KD158 == 1 ~ 1,
                                            KD158 == 5 ~ 0,
                                            TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100,
                # Take sum, divide by 4 (num of Qs), multiply by 100
                NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  # Take sum, divide by 4 (num of Qs), multiply by 100
  # serial 7s is scored based on the previous number -- so a correct answer for KD142 is 93. If a participant answers 94, but gets 94-7
  # right, then KD143 is correct
  dplyr::mutate(S7_1 = dplyr::case_when(KD142 == 100 - 7 ~ 1,
                                        KD142 != 100 - 7 ~ 0,
                                        KD142 > 997 ~ NA_real_),
                S7_2 = dplyr::case_when(KD143 == KD142 - 7 ~ 1,
                                        KD143 != KD142 - 7 ~ 0,
                                        KD143 > 997 ~ NA_real_),
                S7_3 = dplyr::case_when(KD144 == KD143 - 7 ~ 1,
                                        KD144 != KD143 - 7 ~ 0,
                                        KD144 > 997 ~ NA_real_),
                S7_4 = dplyr::case_when(KD145 == KD144 - 7 ~ 1,
                                        KD145 != KD144 - 7 ~ 0,
                                        KD145 > 997 ~ NA_real_),
                S7_5 = dplyr::case_when(KD146 == KD145 - 7 ~ 1,
                                        KD146 != KD145 - 7 ~ 0,
                                        KD146 > 997 ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  dplyr::select(
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 2006) # Add year variable

# 2008
# KD -> LD

hrs2008_scored <- hrs2008_d_01 %>%
  dplyr::left_join(CODA_IDs, by = "HRS_ID") %>%
  dplyr::mutate(IR = LD174*10, # on a scale from 0-10, multiply by 10 to score
                DR = LD184*10, # on a scale from 0-10, multiply by 10 to score
                BC = dplyr::case_when(LD124 == 1 ~ 100, # on a scale where 1 = correct, 5 = incorrect
                                      LD124 == 5 ~ 0,   # ignored second attempts
                                      TRUE ~ NA_real_),
                # Date and name variables are scored 1 = yes, 5 = no, 8 or 9 = don't know, NA, or refused - recoding
                Date_Month = dplyr::case_when(LD151 == 1 ~ 1,
                                              LD151 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(LD152 == 1 ~ 1,
                                            LD152 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(LD153 == 1 ~ 1,
                                             LD153 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(LD154 == 1 ~ 1,
                                            LD154 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Names_Scissors = dplyr::case_when(LD155 == 1 ~ 1,
                                                  LD155 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(LD156 == 1 ~ 1,
                                                LD155 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(LD157 == 1 ~ 1,
                                              LD157 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(LD158 == 1 ~ 1,
                                            LD158 == 5 ~ 0,
                                            TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100,
                # Take sum, divide by 4 (num of Qs), multiply by 100
                NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  # Take sum, divide by 4 (num of Qs), multiply by 100
  # serial 7s is scored based on the previous number -- so a correct answer for LD142 is 93. If a participant answers 94, but gets 94-7
  # right, then LD143 is correct
  dplyr::mutate(S7_1 = dplyr::case_when(LD142 == 100 - 7 ~ 1,
                                        LD142 != 100 - 7 ~ 0,
                                        LD142 > 997 ~ NA_real_),
                S7_2 = dplyr::case_when(LD143 == LD142 - 7 ~ 1,
                                        LD143 != LD142 - 7 ~ 0,
                                        LD143 > 997 ~ NA_real_),
                S7_3 = dplyr::case_when(LD144 == LD143 - 7 ~ 1,
                                        LD144 != LD143 - 7 ~ 0,
                                        LD144 > 997 ~ NA_real_),
                S7_4 = dplyr::case_when(LD145 == LD144 - 7 ~ 1,
                                        LD145 != LD144 - 7 ~ 0,
                                        LD145 > 997 ~ NA_real_),
                S7_5 = dplyr::case_when(LD146 == LD145 - 7 ~ 1,
                                        LD146 != LD145 - 7 ~ 0,
                                        LD146 > 997 ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  dplyr::select( # just getting scored measures 
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 2008) # Add year variable

# 2010 Wave
# MD174 - Number Good (IR)
# MD175 - Number Wrong (IR)
# MD176 - Number Forgotten (IR)

# MD184 - Number Good (DR)
# MD185 - Number Wrong (DR)
# MD186 - Number Forgotten (DR)

# MD142-MD146 Serial 7s (starting from 100, each item is 7 back; e.g. item 1 is 100-7, item 2 is 93-7, etc.)

# MD124 - Backwards Counting (From 20)
# DO NOT USE MD139 - Backwards Counting (From 86), IT WAS DISCONTINUED TWICE AND ONLY APPEARS IN 2010 AND 2012

# MD151 - Date (Month)
# MD152 - Date (Day)
# MD153 - Date (Year)
# MD154 - Date (Day of week)

# MD155 - Name (Scissors)
# MD156 - Name (Cactus)
# MD157 - Name (President)
# MD158 - Name (VP)


hrs2010_scored <- hrs2010_d_01 %>%
  dplyr::left_join(CODA_IDs, by = "HRS_ID") %>%
  dplyr::mutate(IR = MD174*10, # on a scale from 0-10, multiply by 10 to score
                DR = MD184*10, # on a scale from 0-10, multiply by 10 to score
                BC = dplyr::case_when(MD124 == 1 ~ 100, # on a scale where 1 = correct, 5 = incorrect
                                      MD124 == 5 ~ 0,   # ignored second attempts
                                      TRUE ~ NA_real_),
                # Date and name variables are scored 1 = yes, 5 = no, 8 or 9 = don't know, NA, or refused - recoding
                Date_Month = dplyr::case_when(MD151 == 1 ~ 1,
                                              MD151 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(MD152 == 1 ~ 1,
                                            MD152 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(MD153 == 1 ~ 1,
                                             MD153 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(MD154 == 1 ~ 1,
                                            MD154 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Names_Scissors = dplyr::case_when(MD155 == 1 ~ 1,
                                                  MD155 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(MD156 == 1 ~ 1,
                                                MD155 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(MD157 == 1 ~ 1,
                                              MD157 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(MD158 == 1 ~ 1,
                                            MD158 == 5 ~ 0,
                                            TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100,
                # Take sum, divide by 4 (num of Qs), multiply by 100
                NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  # Take sum, divide by 4 (num of Qs), multiply by 100
  # serial 7s is scored based on the previous number -- so a correct answer for MD142 is 93. If a participant answers 94, but gets 94-7
  # right, then MD143 is correct
  dplyr::mutate(S7_1 = dplyr::case_when(MD142 == 100 - 7 ~ 1,
                                        MD142 != 100 - 7 ~ 0,
                                        MD142 > 997 ~ NA_real_),
                S7_2 = dplyr::case_when(MD143 == MD142 - 7 ~ 1,
                                        MD143 != MD142 - 7 ~ 0,
                                        MD143 > 997 ~ NA_real_),
                S7_3 = dplyr::case_when(MD144 == MD143 - 7 ~ 1,
                                        MD144 != MD143 - 7 ~ 0,
                                        MD144 > 997 ~ NA_real_),
                S7_4 = dplyr::case_when(MD145 == MD144 - 7 ~ 1,
                                        MD145 != MD144 - 7 ~ 0,
                                        MD145 > 997 ~ NA_real_),
                S7_5 = dplyr::case_when(MD146 == MD145 - 7 ~ 1,
                                        MD146 != MD145 - 7 ~ 0,
                                        MD146 > 997 ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  dplyr::select( # just getting scored measures and depression items
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 2010) # Add year variable

# Score 2012 data
# MD -> ND

hrs2012_scored <- hrs2012_d_01 %>%
  dplyr::left_join(CODA_IDs, by = "HRS_ID") %>%
  dplyr::mutate(IR = ND174*10, # on a scale from 0-10, multiply by 10 to score
                DR = ND184*10, # on a scale from 0-10, multiply by 10 to score
                BC = dplyr::case_when(ND124 == 1 ~ 100, # on a scale where 1 = correct, 5 = incorrect
                                      ND124 == 5 ~ 0,   # ignored second attempts
                                      TRUE ~ NA_real_),
                # Date and name variables are scored 1 = yes, 5 = no, 8 or 9 = don't know, NA, or refused - recoding
                Date_Month = dplyr::case_when(ND151 == 1 ~ 1,
                                              ND151 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(ND152 == 1 ~ 1,
                                            ND152 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(ND153 == 1 ~ 1,
                                             ND153 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(ND154 == 1 ~ 1,
                                            ND154 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Names_Scissors = dplyr::case_when(ND155 == 1 ~ 1,
                                                  ND155 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(ND156 == 1 ~ 1,
                                                ND155 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(ND157 == 1 ~ 1,
                                              ND157 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(ND158 == 1 ~ 1,
                                            ND158 == 5 ~ 0,
                                            TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100,
                # Take sum, divide by 4 (num of Qs), multiply by 100
                NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  # Take sum, divide by 4 (num of Qs), multiply by 100
  # serial 7s is scored based on the previous number -- so a correct answer for ND142 is 93. If a participant answers 94, but gets 94-7
  # right, then ND143 is correct
  dplyr::mutate(S7_1 = dplyr::case_when(ND142 == 100 - 7 ~ 1,
                                        ND142 != 100 - 7 ~ 0,
                                        ND142 > 997 ~ NA_real_),
                S7_2 = dplyr::case_when(ND143 == ND142 - 7 ~ 1,
                                        ND143 != ND142 - 7 ~ 0,
                                        ND143 > 997 ~ NA_real_),
                S7_3 = dplyr::case_when(ND144 == ND143 - 7 ~ 1,
                                        ND144 != ND143 - 7 ~ 0,
                                        ND144 > 997 ~ NA_real_),
                S7_4 = dplyr::case_when(ND145 == ND144 - 7 ~ 1,
                                        ND145 != ND144 - 7 ~ 0,
                                        ND145 > 997 ~ NA_real_),
                S7_5 = dplyr::case_when(ND146 == ND145 - 7 ~ 1,
                                        ND146 != ND145 - 7 ~ 0,
                                        ND146 > 997 ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  dplyr::select( # just getting scored measures and depression items
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 2012) # Add year variable

# Score 2014 data
# MD -> OD

hrs2014_scored <- hrs2014_d_01 %>%
  dplyr::left_join(CODA_IDs, by = "HRS_ID") %>%
  dplyr::mutate(IR = OD174*10, # on a scale from 0-10, multiply by 10 to score
                DR = OD184*10, # on a scale from 0-10, multiply by 10 to score
                BC = dplyr::case_when(OD124 == 1 ~ 100, # on a scale where 1 = correct, 5 = incorrect
                                      OD124 == 5 ~ 0,   # ignored second attempts
                                      TRUE ~ NA_real_),
                # Date and name variables are scored 1 = yes, 5 = no, 8 or 9 = don't know, NA, or refused - recoding
                Date_Month = dplyr::case_when(OD151 == 1 ~ 1,
                                              OD151 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(OD152 == 1 ~ 1,
                                            OD152 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(OD153 == 1 ~ 1,
                                             OD153 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(OD154 == 1 ~ 1,
                                            OD154 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Names_Scissors = dplyr::case_when(OD155 == 1 ~ 1,
                                                  OD155 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(OD156 == 1 ~ 1,
                                                OD155 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(OD157 == 1 ~ 1,
                                              OD157 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(OD158 == 1 ~ 1,
                                            OD158 == 5 ~ 0,
                                            TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100,
                # Take sum, divide by 4 (num of Qs), multiply by 100
                NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  # Take sum, divide by 4 (num of Qs), multiply by 100
  # serial 7s is scored based on the previous number -- so a correct answer for OD142 is 93. If a participant answers 94, but gets 94-7
  # right, then OD143 is correct
  dplyr::mutate(S7_1 = dplyr::case_when(OD142 == 100 - 7 ~ 1,
                                        OD142 != 100 - 7 ~ 0,
                                        OD142 > 997 ~ NA_real_),
                S7_2 = dplyr::case_when(OD143 == OD142 - 7 ~ 1,
                                        OD143 != OD142 - 7 ~ 0,
                                        OD143 > 997 ~ NA_real_),
                S7_3 = dplyr::case_when(OD144 == OD143 - 7 ~ 1,
                                        OD144 != OD143 - 7 ~ 0,
                                        OD144 > 997 ~ NA_real_),
                S7_4 = dplyr::case_when(OD145 == OD144 - 7 ~ 1,
                                        OD145 != OD144 - 7 ~ 0,
                                        OD145 > 997 ~ NA_real_),
                S7_5 = dplyr::case_when(OD146 == OD145 - 7 ~ 1,
                                        OD146 != OD145 - 7 ~ 0,
                                        OD146 > 997 ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  dplyr::select( # just getting scored measures and depression items
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 2014) # Add year variable

# Score 2016 data
# MD -> PD

hrs2016_scored <- hrs2016_d_01 %>%
  dplyr::left_join(CODA_IDs, by = "HRS_ID") %>%
  dplyr::mutate(IR = PD174*10, # on a scale from 0-10, multiply by 10 to score
                DR = PD184*10, # on a scale from 0-10, multiply by 10 to score
                BC = dplyr::case_when(PD124 == 1 ~ 100, # on a scale where 1 = correct, 5 = incorrect
                                      PD124 == 5 ~ 0,   # ignored second attempts
                                      TRUE ~ NA_real_),
                # Date and name variables are scored 1 = yes, 5 = no, 8 or 9 = don't know, NA, or refused - recoding
                Date_Month = dplyr::case_when(PD151 == 1 ~ 1,
                                              PD151 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(PD152 == 1 ~ 1,
                                            PD152 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(PD153 == 1 ~ 1,
                                             PD153 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(PD154 == 1 ~ 1,
                                            PD154 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Names_Scissors = dplyr::case_when(PD155 == 1 ~ 1,
                                                  PD155 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(PD156 == 1 ~ 1,
                                                PD155 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(PD157 == 1 ~ 1,
                                              PD157 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(PD158 == 1 ~ 1,
                                            PD158 == 5 ~ 0,
                                            TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100,
                # Take sum, divide by 4 (num of Qs), multiply by 100
                NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  # Take sum, divide by 4 (num of Qs), multiply by 100
  # serial 7s is scored based on the previous number -- so a correct answer for PD142 is 93. If a participant answers 94, but gets 94-7
  # right, then PD143 is correct
  dplyr::mutate(S7_1 = dplyr::case_when(PD142 == 100 - 7 ~ 1,
                                        PD142 != 100 - 7 ~ 0,
                                        PD142 > 997 ~ NA_real_),
                S7_2 = dplyr::case_when(PD143 == PD142 - 7 ~ 1,
                                        PD143 != PD142 - 7 ~ 0,
                                        PD143 > 997 ~ NA_real_),
                S7_3 = dplyr::case_when(PD144 == PD143 - 7 ~ 1,
                                        PD144 != PD143 - 7 ~ 0,
                                        PD144 > 997 ~ NA_real_),
                S7_4 = dplyr::case_when(PD145 == PD144 - 7 ~ 1,
                                        PD145 != PD144 - 7 ~ 0,
                                        PD145 > 997 ~ NA_real_),
                S7_5 = dplyr::case_when(PD146 == PD145 - 7 ~ 1,
                                        PD146 != PD145 - 7 ~ 0,
                                        PD146 > 997 ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  dplyr::select( # just getting scored measures
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 2016) # Add year variable

# Score 2018 data
# MD -> QD

hrs2018_scored <- hrs2018_d_01 %>%
  dplyr::left_join(CODA_IDs, by = "HRS_ID") %>%
  dplyr::mutate(IR = QD174*10, # on a scale from 0-10, multiply by 10 to score
                DR = QD184*10, # on a scale from 0-10, multiply by 10 to score
                BC = dplyr::case_when(QD124 == 1 ~ 100, # on a scale where 1 = correct, 5 = incorrect
                                      QD124 == 5 ~ 0,   # ignored second attempts
                                      TRUE ~ NA_real_),
                # Date and name variables are scored 1 = yes, 5 = no, 8 or 9 = don't know, NA, or refused - recoding
                Date_Month = dplyr::case_when(QD151 == 1 ~ 1,
                                              QD151 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(QD152 == 1 ~ 1,
                                            QD152 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(QD153 == 1 ~ 1,
                                             QD153 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(QD154 == 1 ~ 1,
                                            QD154 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Names_Scissors = dplyr::case_when(QD155 == 1 ~ 1,
                                                  QD155 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(QD156 == 1 ~ 1,
                                                QD155 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(QD157 == 1 ~ 1,
                                              QD157 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(QD158 == 1 ~ 1,
                                            QD158 == 5 ~ 0,
                                            TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100,
                # Take sum, divide by 4 (num of Qs), multiply by 100
                NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  # Take sum, divide by 4 (num of Qs), multiply by 100
  # serial 7s is scored based on the previous number -- so a correct answer for QD142 is 93. If a participant answers 94, but gets 94-7
  # right, then QD143 is correct
  dplyr::mutate(S7_1 = dplyr::case_when(QD142 == 100 - 7 ~ 1,
                                        QD142 != 100 - 7 ~ 0,
                                        QD142 > 997 ~ NA_real_),
                S7_2 = dplyr::case_when(QD143 == QD142 - 7 ~ 1,
                                        QD143 != QD142 - 7 ~ 0,
                                        QD143 > 997 ~ NA_real_),
                S7_3 = dplyr::case_when(QD144 == QD143 - 7 ~ 1,
                                        QD144 != QD143 - 7 ~ 0,
                                        QD144 > 997 ~ NA_real_),
                S7_4 = dplyr::case_when(QD145 == QD144 - 7 ~ 1,
                                        QD145 != QD144 - 7 ~ 0,
                                        QD145 > 997 ~ NA_real_),
                S7_5 = dplyr::case_when(QD146 == QD145 - 7 ~ 1,
                                        QD146 != QD145 - 7 ~ 0,
                                        QD146 > 997 ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  dplyr::select( # just getting scored measures
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 2018) # Add year variable

# Score 2020 Data
# MD -> RD

hrs2020_scored <- hrs2020_d_01 %>%
  dplyr::left_join(CODA_IDs, by = "HRS_ID") %>%
  dplyr::mutate(IR = RD174*10, # on a scale from 0-10, multiply by 10 to score
                DR = RD184*10, # on a scale from 0-10, multiply by 10 to score
                BC = dplyr::case_when(RD124 == 1 ~ 100, # on a scale where 1 = correct, 5 = incorrect
                                      RD124 == 5 ~ 0,   # ignored second attempts
                                      TRUE ~ NA_real_),
                # Date and name variables are scored 1 = yes, 5 = no, 8 or 9 = don't know, NA, or refused - recoding
                Date_Month = dplyr::case_when(RD151 == 1 ~ 1,
                                              RD151 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Date_Day = dplyr::case_when(RD152 == 1 ~ 1,
                                            RD152 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Date_Year = dplyr::case_when(RD153 == 1 ~ 1,
                                             RD153 == 5 ~ 0,
                                             TRUE ~ NA_real_),
                Date_DOW = dplyr::case_when(RD154 == 1 ~ 1,
                                            RD154 == 5 ~ 0,
                                            TRUE ~ NA_real_),
                Names_Scissors = dplyr::case_when(RD155 == 1 ~ 1,
                                                  RD155 == 5 ~ 0,
                                                  TRUE ~ NA_real_),
                Names_Cactus = dplyr::case_when(RD156 == 1 ~ 1,
                                                RD155 == 5 ~ 0,
                                                TRUE ~ NA_real_),
                Names_Pres = dplyr::case_when(RD157 == 1 ~ 1,
                                              RD157 == 5 ~ 0,
                                              TRUE ~ NA_real_),
                Names_VP = dplyr::case_when(RD158 == 1 ~ 1,
                                            RD158 == 5 ~ 0,
                                            TRUE ~ NA_real_)
  ) %>%
  dplyr::mutate(DA = (rowSums(.[c("Date_Month", "Date_Day", "Date_Year", "Date_DOW")], na.rm = FALSE) / 4) * 100,
                # Take sum, divide by 4 (num of Qs), multiply by 100
                NM = (rowSums(.[c("Names_Scissors", "Names_Cactus", "Names_Pres", "Names_VP")], na.rm = FALSE) / 4) * 100) %>%
  # Take sum, divide by 4 (num of Qs), multiply by 100
  # serial 7s is scored based on the previous number -- so a correct answer for RD142 is 93. If a participant answers 94, but gets 94-7
  # right, then RD143 is correct
  dplyr::mutate(S7_1 = dplyr::case_when(RD142 == 100 - 7 ~ 1,
                                        RD142 != 100 - 7 ~ 0,
                                        RD142 > 997 ~ NA_real_),
                S7_2 = dplyr::case_when(RD143 == RD142 - 7 ~ 1,
                                        RD143 != RD142 - 7 ~ 0,
                                        RD143 > 997 ~ NA_real_),
                S7_3 = dplyr::case_when(RD144 == RD143 - 7 ~ 1,
                                        RD144 != RD143 - 7 ~ 0,
                                        RD144 > 997 ~ NA_real_),
                S7_4 = dplyr::case_when(RD145 == RD144 - 7 ~ 1,
                                        RD145 != RD144 - 7 ~ 0,
                                        RD145 > 997 ~ NA_real_),
                S7_5 = dplyr::case_when(RD146 == RD145 - 7 ~ 1,
                                        RD146 != RD145 - 7 ~ 0,
                                        RD146 > 997 ~ NA_real_)) %>%
  dplyr::mutate(S7 = (rowSums(.[c("S7_1", "S7_2", "S7_3", "S7_4", "S7_5")], na.rm = FALSE) / 5) * 100) %>%
  dplyr::select( # just getting scored measures
    HRS_ID, IR, DR, BC, DA, NM, S7, mplusid) %>%
  dplyr::mutate(Year = 2020) # Add year variable


# Stack it up!

scored_long <- bind_rows(hrs1998_scored,
                         hrs2000_scored,
                         hrs2002_scored,
                         hrs2004_scored,
                         hrs2006_scored,
                         hrs2008_scored,
                         hrs2010_scored,
                         hrs2012_scored,
                         hrs2014_scored,
                         hrs2016_scored,
                         hrs2018_scored,
                         hrs2020_scored) %>% 
  left_join(assessment_mode_long, by = c("HRS_ID", "Year"))

# Add weights and create indicator for years in study

long_w_weights <- scored_long %>% 
  left_join(weights_only, by = "HRS_ID") %>% 
  mutate(yrsinstudy = dplyr::case_when(Year == 1998 ~ 0,
                                       Year == 2000 ~ 2,
                                       Year == 2002 ~ 4,
                                       Year == 2004 ~ 6,
                                       Year == 2006 ~ 8,
                                       Year == 2008 ~ 10,
                                       Year == 2010 ~ 12,
                                       Year == 2012 ~ 14,
                                       Year == 2014 ~ 16,
                                       Year == 2016 ~ 18,
                                       Year == 2018 ~ 20,
                                       Year == 2020 ~ 22)) %>% 
  dplyr::mutate(
    vddr = minmax(DR/10),
    vdbc = dplyr::if_else(BC == 100, 1, 0),
    vdda = dplyr::case_when(DA == 0 ~ 0,
                            DA == 25 ~ 1,
                            DA == 50 ~ 2,
                            DA == 75 ~ 3,
                            DA == 100 ~ 4),
    vdnm = dplyr::case_when(NM == 0 ~ 0,
                            NM == 25 ~ 1,
                            NM == 50 ~ 2,
                            NM == 75 ~ 3,
                            NM == 100 ~ 4),
    vds7 = dplyr::case_when(S7 == 0 ~ 0,
                            S7 == 20 ~ 1,
                            S7 == 40 ~ 2,
                            S7 == 60 ~ 3,
                            S7 == 80 ~ 4,
                            S7 == 100 ~ 5)
  )

# Begin visualizing variables to see if any collapsing needs to be done

table2 <- long_w_weights %>% 
  dplyr::select(vddr, vdbc, vdda, vdnm, vds7, yrsinstudy) %>% 
  gtsummary::tbl_summary(by = yrsinstudy,
                         statistic = vddr ~ "{mean} ({sd})")

# Looks like we need to collapse vdnm for sure. vdda maybe.

long_w_weights_rescored <- long_w_weights %>% 
  dplyr::mutate(vdda_r = dplyr::case_when(vdda == 0 ~ 0,
                                        vdda == 1 ~ 0,
                                        vdda == 2 ~ 1,
                                        vdda == 3 ~ 2,
                                        vdda == 4 ~ 3),
                vdnm_r = dplyr::case_when(vdnm == 0 ~ 0,
                                        vdnm == 1 ~ 0,
                                        vdnm == 2 ~ 0,
                                        vdnm == 3 ~ 1,
                                        vdnm == 4 ~ 2))

checkrescoringda <- QSPtools::checkvar(long_w_weights_rescored,
                                    vdda, vdda_r)

checkrescoringnm <- QSPtools::checkvar(long_w_weights_rescored,
                                      vdnm, vdnm_r)

table6 <- long_w_weights_rescored %>% 
  dplyr::select(vddr, vdbc, vdda_r, vdnm_r, vds7, yrsinstudy) %>% 
  gtsummary::tbl_summary(by = yrsinstudy,
                         statistic = vddr ~ "{mean} ({sd})")



saveRDS(long_w_weights_rescored, here::here(RDS_path, "020_long_w_weights.rds"))


tracker_demos <- tracker_01 %>% 
  dplyr::select(HRS_ID, GENDER, HISPANIC, RACE, SCHLYRS, FAGE) %>% 
  dplyr::mutate(vdfem = dplyr::if_else(GENDER == 2, 1, 0),
                vdhisp = dplyr::case_when(HISPANIC == 0 ~ NA_real_,
                                          HISPANIC == 1 ~ 1,
                                          HISPANIC == 2 ~ 1,
                                          HISPANIC == 3 ~ 1,
                                          HISPANIC == 5 ~ 0),
                vdothrac = dplyr::if_else(RACE == 7, 1, 0),
                vdblack = dplyr::if_else(RACE == 2, 1, 0),
                vdedu = dplyr::na_if(SCHLYRS, 99),
                vdage = dplyr::na_if(FAGE, 999)) %>% 
  dplyr::select(HRS_ID, vdfem, vdhisp, vdothrac, vdblack, vdedu, vdage)

demos_table <- tracker_demos %>% 
  dplyr::select(-HRS_ID) %>% 
  gtsummary::tbl_summary(statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"))

saveRDS(tracker_demos, here::here(RDS_path, "020_demos.rds"))

save.image(here::here(RDS_path, "050_define-variables.Rdata"))
