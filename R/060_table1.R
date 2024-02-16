rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "010_paths-and-files.R"))
source(here::here("R", "020_libraries.R"))

# Import data

tracker <- readRDS(here::here(RDS_path, "010_tracker.rds"))
CODA_IDs <- readRDS(here::here(RDS_path, "040_CODA-IDS.Rds"))

tracker_01 <- tracker %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID)

tracker_demos <- tracker_01 %>% 
  dplyr::select(HRS_ID, GENDER, HISPANIC, RACE, SCHLYRS, FAGE, FWGTR) %>% 
  dplyr::mutate(vdfem = dplyr::if_else(GENDER == 2, 1, 0),
                vdhisp = dplyr::case_when(HISPANIC == 0 ~ NA_real_,
                                          HISPANIC == 1 ~ 1,
                                          HISPANIC == 2 ~ 1,
                                          HISPANIC == 3 ~ 1,
                                          HISPANIC == 5 ~ 0),
                vdrace = dplyr::case_when(RACE == 7 ~ "Other race",
                                          RACE == 2 ~ "Black",
                                          RACE == 1 ~ "White"),
                vdedu = dplyr::na_if(SCHLYRS, 99),
                vdage = dplyr::na_if(FAGE, 999)) %>% 
  dplyr::select(HRS_ID, vdfem, vdhisp, vdrace, vdedu, vdage, FWGTR)

table1svy <- survey::svydesign(id = ~HRS_ID, data = tracker_demos, weights = ~FWGTR) 

table1 <- table1svy %>% 
  gtsummary::tbl_svysummary(
    missing = "no",
    statistic = list(vdfem ~ "{p}%",
                     vdhisp ~ "{p}%",
                     vdrace ~ "{p}%",
                     vdedu ~ "{mean} ({sd})",
                     vdage ~ "{p}%"),
    digits = list(vdfem ~ 1,
                  vdhisp ~ 1,
                  vdrace ~ 1,
                  vdedu ~ c(2,2),
                  vdage ~ 1),
    include = c("vdfem", "vdhisp", "vdrace", "vdedu", "vdage")) %>% 
  gtsummary::modify_header(stat_0 = "**N = 2295**") %>%
  gtsummary::modify_footnote(stat_0 ~ "% for categorical variables, Mean (SD) for continuous variables. N = 1 participant chose not to report their race or ethnicity and are not shown in the table. Weighted estimates of proportions and means/SDs shown in table.")

save.image(here::here(RDS_path, "060_table1.Rdata"))