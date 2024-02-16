rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "010_paths-and-files.R"))
source(here::here("R", "020_libraries.R"))

# Import data

cognitiondata <- readRDS(here::here(RDS_path, "020_long_w_weights.rds")) 

baselinecog <- cognitiondata %>% 
  dplyr::filter(yrsinstudy == 0) %>% 
  dplyr::select(mplusid, vddr, vdbc, vdda_r, vdnm_r, vds7) %>% 
  dplyr::rename(vdda = vdda_r,
                vdnm = vdnm_r)

fs::dir_create(Mplus_path, "070_baseline-cog")
setwd(here::here(Mplus_path, "070_baseline-cog"))

## Model 1 fix variance estimate loading

model1_baselinecog_model <- mplusObject(
  MODEL = "GCP BY vddr* vdbc vdda vdnm vds7;
           GCP@1;",
  ANALYSIS = "ESTIMATOR = MLR;
              LINK = PROBIT",
  VARIABLE = "idvariable = mplusid;
              CATEGORICAL = vdbc vdda vdnm vds7;",
  OUTPUT = "TECH4; SVALUES;",
  usevariables = colnames(baselinecog),
  rdata = baselinecog,
)

model1_baseline <- mplusModeler(model1_baselinecog_model, modelout = "model1_baselinemeanstructure.inp", run = TRUE)

model1_results <- model1_baseline$results$parameters$unstandardized %>% 
  dplyr::filter((paramHeader == "GCP.BY" & param == "VDDR")| (paramHeader == "Intercepts" & param == "VDDR"))

## Model 2, free variance fix loading

model2_baselinecog_model <- mplusObject(
  MODEL = "GCP BY vddr@1.216 vdbc vdda vdnm vds7;
           GCP*
           [vddr@3.817];",
  ANALYSIS = "ESTIMATOR = MLR;
              LINK = PROBIT",
  VARIABLE = "idvariable = mplusid;
              CATEGORICAL = vdbc vdda vdnm vds7;",
  OUTPUT = "TECH4; SVALUES;",
  usevariables = colnames(baselinecog),
  rdata = baselinecog,
)

model2_baselinecog <- mplusModeler(model2_baselinecog_model, modelout = "model2_baselinemeanstructure.inp", run = TRUE)

model2_results <- as.numeric(model2_baselinecog$results$tech4$latCovEst)

### Repeat, but use survey weights

baselinecogwt <- cognitiondata %>% 
  tidyr::unite(SECU_R, c("SECU", "STRATUM"), sep = ".", remove = FALSE) %>%
  dplyr::filter(yrsinstudy == 0) %>% 
  dplyr::select(mplusid, vddr, vdbc, vdda_r, vdnm_r, vds7, FWGTR, SECU_R, STRATUM) %>% 
  dplyr::rename(vdda = vdda_r,
                vdnm = vdnm_r)

model1_baselinecogwt_model <- mplusObject(
  MODEL = "GCP BY vddr* vdbc vdda vdnm vds7;
           GCP@1;",
  ANALYSIS = "ESTIMATOR = MLR;
              LINK = PROBIT;
              TYPE = COMPLEX;",
  VARIABLE = "WEIGHT = FWGTR;
              CLUSTER = SECU_R;
              STRATIFICATION = STRATUM;
              idvariable = mplusid;
              CATEGORICAL = vdbc vdda vdnm vds7;",
  OUTPUT = "TECH4; SVALUES;",
  usevariables = colnames(baselinecogwt),
  rdata = baselinecogwt,
)

model1wt_baseline <- mplusModeler(model1_baselinecogwt_model, modelout = "model1wt_baselinemeanstructure.inp", run = TRUE)

model1wt_results <- model1wt_baseline$results$parameters$unstandardized %>% 
  dplyr::filter((paramHeader == "GCP.BY" & param == "VDDR")| (paramHeader == "Intercepts" & param == "VDDR"))

save.image(here::here(RDS_path, "070_baseline-cognition.Rdata"))