rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "010_paths-and-files.R"))
source(here::here("R", "020_libraries.R"))

# Import data

tracker <- readRDS(here::here(RDS_path, "010_tracker.rds"))

# Begin building cohort

# Need to get just CODA folks. Can pull from STUDY variable in tracker file


CODA_IDs <- tracker %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  dplyr::filter(STUDY == 21) %>% # N = 2464
  # 21 = CODA (Children of the Depression Age)
  dplyr::filter(FWGTR > 0) %>%                     # N = 2301, drop N = 163
  # We do not want any folks with a weight of 0
  dplyr::filter(FAGE > 66 & FAGE < 75) %>% 
  # Select for age band between 67 and 74
  dplyr::select(HRS_ID) %>%
  dplyr::mutate(mplusid = row_number()) # create ID key for mplus

# Get actual numbers for report

CODA_IDs_all <- tracker %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  dplyr::filter(STUDY == 21) %>% 
  nrow()

CODA_IDs_drop0wt <- tracker %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  dplyr::filter(STUDY == 21) %>% 
  dplyr::filter(FWGTR > 0) %>% 
  nrow()

CODA_IDs_age <- tracker %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  dplyr::filter(STUDY == 21) %>% 
  dplyr::filter(FWGTR > 0) %>% 
  dplyr::filter(FAGE < 66 | FAGE > 74) %>% 
  nrow()

Ndropped_0wt <- CODA_IDs_all - CODA_IDs_drop0wt

why_ineligible <- tracker %>%
  tidyr::unite(HRS_ID, c("HHID", "PN")) %>%
  dplyr::filter(STUDY == 21) %>% 
  dplyr::filter(FWGTR == 0) %>% 
  dplyr::select(FWHY0WGT, FIWTYPE)

why_ineligible$FWHY0WGT <- factor(why_ineligible$FWHY0WGT,
                                  levels = c(1, 3, 7),
                                  labels = c("Not cohort eligible this wave",
                                             "Nursing home resident",
                                             "xWGTR greater than zero or xIWTYPE greater than one"))

why_ineligible$FIWTYPE <- factor(why_ineligible$FIWTYPE,
                                  levels = c(1, 5, 99),
                                  labels = c("Core interview obtained",
                                             "Core interview not obtained",
                                             "Not in the sample this wave"))


table1 <- gtsummary::tbl_summary(why_ineligible)

table1checkvar <- QSPtools::checkvar(why_ineligible, FWHY0WGT, FIWTYPE)

saveRDS(CODA_IDs, here::here(RDS_path, "040_CODA-IDS.Rds"))
save.image(here::here(RDS_path, "040_select-obs.Rdata"))

