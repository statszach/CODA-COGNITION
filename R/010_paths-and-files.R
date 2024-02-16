data_path <- fs::path("R:", "BM_QuantitativeSciencesPrg", "STUDIES", "HRS", "POSTED", "DATA", "SOURCE")

dir_path <- fs::path("Z:", "Research", "HRS", "CODA-COGNITION", "R")

fs::dir_create(here::here(dir_path, "Images"))
images_path <- here::here(dir_path, "Images")

fs::dir_create(here::here(dir_path, "Tables"))
tables_path <- here::here(dir_path, "Tables")

fs::dir_create(here::here(dir_path, "Mplus"))
Mplus_path <- here::here(dir_path,  "Mplus")

fs::dir_create(here::here(dir_path, "RDS"))
RDS_path <- here::here(dir_path, "RDS")

minmax <- function(x){
  
  c_n <- (5 / (8*1000)) * sd(x, na.rm = T)
  c_d <- (10/ (8*1000)) * sd(x, na.rm = T)
  
  (x - min(x, na.rm = T) + c_n) / (max(x, na.rm = T) - min(x, na.rm = T) + c_d)
  
}
