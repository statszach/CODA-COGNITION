rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "010_paths-and-files.R"))
source(here::here("R", "020_libraries.R"))

# Import data

cognitiondata <- readRDS(here::here(RDS_path, "020_long_w_weights.rds")) 

firstfourcog <- cognitiondata %>% 
  dplyr::filter(yrsinstudy < 7) %>% 
  dplyr::select(mplusid, vddr, vdbc, vdda_r, vdnm_r, vds7) %>% 
  dplyr::rename(vdda = vdda_r,
                vdnm = vdnm_r)

fs::dir_create(Mplus_path, "080_longitudinal-cog")
setwd(here::here(Mplus_path, "080_longitudinal-cog"))

## Model 3, fixed loadings on longitudinal data

model3_longcog_model <- mplusObject(
  MODEL = "GCP BY vddr@1.216 vdbc vdda vdnm vds7;
           GCP*;
          [vddr@3.817];",
  ANALYSIS = "ESTIMATOR = MLR;
              LINK = PROBIT",
  VARIABLE = "idvariable = mplusid;
              CATEGORICAL = vdbc vdda vdnm vds7;",
  OUTPUT = "TECH4; SVALUES;",
  usevariables = colnames(firstfourcog),
  rdata = firstfourcog,
)

model3_firstfourcog <- mplusModeler(model3_longcog_model, modelout = "model3_longcog.inp", run = TRUE)

model3_results <- as.numeric(model3_firstfourcog$results$tech4$latCovEst)

## Model 4, add multilevel specification

model4_longcog_model <- mplusObject(
  MODEL = "%WITHIN%
           GCP BY vddr@1.216 vdbc vdda vdnm vds7;
           GCP*;
           %BETWEEN%
           [vddr@3.817];",
  ANALYSIS = "ESTIMATOR = MLR;
              LINK = PROBIT;
              TYPE = TWOLEVEL;",
  VARIABLE = "CLUSTER = mplusid;
              CATEGORICAL = vdbc vdda vdnm vds7;",
  OUTPUT = "TECH4; SVALUES;",
  usevariables = colnames(firstfourcog),
  rdata = firstfourcog,
)

model4_firstfourcog <- mplusModeler(model4_longcog_model, modelout = "model4_longcog.inp", run = TRUE)

## Model 5: Add random regressions

firstfourwintercept <- cognitiondata %>% 
  dplyr::filter(yrsinstudy < 7) %>% 
  dplyr::select(mplusid, vddr, vdbc, vdda_r, vdnm_r, vds7, yrsinstudy) %>% 
  dplyr::rename(vdda = vdda_r,
                vdnm = vdnm_r,
                yrsfu = yrsinstudy) %>% 
  dplyr::mutate(k = 1)

model5_longcog_model <- mplusObject(
  MODEL = "%WITHIN%
           GCP BY vddr@1.216 vdbc vdda vdnm vds7;
           s | GCP on yrsfu;
           i | GCP on k;
           %BETWEEN%
           i with s;
           vddr-vds7@0;
           [vddr@3.817];",
  ANALYSIS = "ESTIMATOR = MLR;
              LINK = PROBIT;
              TYPE = TWOLEVEL RANDOM;",
  VARIABLE = "CLUSTER = mplusid;
              CATEGORICAL = vdbc vdda vdnm vds7;
              WITHIN = yrsfu k;",
  DATA = "VARIANCES = NOCHECK;",
  OUTPUT = "TECH4; SVALUES;",
  usevariables = colnames(firstfourwintercept),
  rdata = firstfourwintercept,
)

model5_firstfourcog <- mplusModeler(model5_longcog_model, modelout = "model5_longcog.inp", run = TRUE)

## Model 6 - add weights

firstfourwinterceptwt <- cognitiondata %>% 
  tidyr::unite(SECU_R, c("SECU", "STRATUM"), sep = ".", remove = FALSE) %>%
  dplyr::filter(yrsinstudy < 7) %>% 
  dplyr::select(mplusid, vddr, vdbc, vdda_r, vdnm_r, vds7, yrsinstudy, FWGTR, SECU_R, STRATUM) %>% 
  dplyr::rename(vdda = vdda_r,
                vdnm = vdnm_r,
                yrsfu = yrsinstudy) %>% 
  dplyr::mutate(k = 1)

model6_longcog_model <- mplusObject(
  MODEL = "%WITHIN%
           GCP BY vddr@1.216 vdbc vdda vdnm vds7;
           s | GCP on yrsfu;
           i | GCP on k;
           %BETWEEN%
           i with s;
           vddr-vds7@0;
           [vddr@3.817];",
  ANALYSIS = "ESTIMATOR = MLR;
              LINK = PROBIT;
              TYPE = COMPLEX TWOLEVEL RANDOM;
              MITERATIONS = 2000;",
  VARIABLE = "WEIGHT = FWGTR;
              CLUSTER = SECU_R mplusid;
              STRATIFICATION = STRATUM;
              CATEGORICAL = vdbc vdda vdnm vds7;
              WITHIN = yrsfu k;
              WTSCALE = UNSCALED;",
  DATA = "VARIANCES = NOCHECK;",
  OUTPUT = "TECH4; SVALUES;",
  usevariables = colnames(firstfourwinterceptwt),
  rdata = firstfourwinterceptwt)

model6_firstfourcog <- mplusModeler(model6_longcog_model, modelout = "model6_longcog.inp", run = TRUE)
